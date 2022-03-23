-- MIT License

-- Copyright ©2022 Stephen Merrony

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;

with Debug_Logs;      use Debug_Logs;
with Devices.Bus;
with Status_Monitor;

package body Devices.Disk6239 is

    procedure Create_Blank (Image_Name : in String; OK : out Boolean) is
        Tmp_File     : Sector_IO.File_Type;
        Empty_Sector : constant Sector := (others => 0);
    begin
        Sector_IO.Create (Tmp_File, Out_File, Image_Name);
        for S in 1 .. Physical_Block_Size loop
            Sector_IO.Write (Tmp_File, Empty_Sector);
        end loop;
        Sector_IO.Close (Tmp_File);
        OK := True;
    exception
        when Error : others =>
            Loggers.Debug_Print
               (Debug_Log,
                "WARNING: Could not create disk image due to " &
                Exception_Information (Error));
            OK := False;
    end Create_Blank;

    protected body Drives is

        procedure Init (Debug_Logging : in Boolean) is
        begin
            State.Debug_Logging := Debug_Logging;
            --Devices.Bus.Actions.Set_Reset_Proc (Devices.DSKP, Reset'Access);
            --Devices.Bus.Actions.Set_Data_In_Proc (Devices.DSKP, Data_In'Access);
            --Devices.Bus.Actions.Set_Data_Out_Proc (Devices.DSKP, Data_Out'Access);
            State.Image_Attached := False;
            --State.Map_Enabled    := False;
            Reset;
            Status_Sender.Start;
        end Init;

        procedure Attach
           (Unit : in Natural; Image_Name : in String; OK : out Boolean)
        is
        begin
            if Unit /= 0 then
                raise Not_Yet_Implemented
                   with "DSKP - Multiple disks not yet supported";
            end if;
            Sector_IO.Open (State.Image_File, Inout_File, Image_Name);
            State.Image_Filename := To_Unbounded_String (Image_Name);
            State.Image_Attached := True;
            Devices.Bus.Actions.Set_Image_Attached (Devices.DSKP, Image_Name);
            OK := True;
        exception
            when Error : others =>
                Loggers.Debug_Print
                   (Debug_Log,
                    "WARNING: Could not open disk image due to " &
                    Exception_Information (Error));
                OK := False;
        end Attach;

        procedure Reset is
        begin
            -- Reset Mapping
            State.Mapping_Reg_A := 16#0000#; -- DMA over BMC
            State.Mapping_Reg_B := Map_Int_Bmc_Phys or Map_Upstream_Load or Map_Upstream_Hpt;

            -- Reset Int_Inf_Block
            State.Int_Inf_Block(0) := 8#101#;
            State.Int_Inf_Block(1) := Ucode_Rev;
            State.Int_Inf_Block(2) := 3;
            State.Int_Inf_Block(3) := Shift_Left (8, 11) or Max_Queued_CBs;
            State.Int_Inf_Block(4) := 0;
            State.Int_Inf_Block(5) := Shift_Left (11, 8);
            State.Int_Inf_Block(6) := 0;
            State.Int_Inf_Block(7) := 0;

            -- Reset Ctrl_Inf_Block
            State.Ctrl_Inf_Block(0) := 0;
            State.Ctrl_Inf_Block(1) := 0;

            -- Reset Unit_Inf_Block
            State.Unit_Inf_Block(0) := 0;
            State.Unit_Inf_Block(1) := Shift_Left (9, 12) or Ucode_Rev;
            State.Unit_Inf_Block(2) := Logical_Blocks_H;
            State.Unit_Inf_Block(3) := Logical_Blocks_L;
            State.Unit_Inf_Block(4) := Word_T(Bytes_Per_Sector);
            State.Unit_Inf_Block(5) := Word_T(User_Cylinders);
            State.Unit_Inf_Block(6) := Shift_Left (Word_T(Surfaces_Per_Disk * Heads_Per_Surface), 8) or
                                       (Word_T(Sectors_Per_Track) and 16#00ff#);

            -- Other data
            State.Command_Reg_B := 0;
            State.Command_Reg_C := 0;
            Set_PIO_Status_Reg_C (Status   => Stat_Xec_State_Reset_Done, 
                                  CCS      => 0, 
                                  Cmd_Echo => Pio_Reset, 
                                  RR       => Test_W_Bit (Word => State.Command_Reg_C, Bit_Num => 15));
            if State.Debug_Logging then
                Loggers.Debug_Print (Dskp_Log, "DEBUG: *** Reset *** via call to Drives.Reset");
            end if;                                
        end Reset;

        procedure Set_PIO_Status_Reg_C (Status, CCS : in Byte_T; Cmd_Echo : in Word_T; RR : in Boolean) is
        -- Set the SYNCHRONOUS standard refturn as per p.3-22
            Stat : Byte_T := Status;
        begin
            if Status = 0 and State.Is_Mapped then
                Stat := Stat_Xec_State_Mapped;
            end if;
            if RR or Cmd_Echo = Pio_Reset then
                State.Status_Reg_C := Shift_Left (Word_T(Stat), 12);
                State.Status_Reg_C := State.Status_Reg_C or Shift_Left (Word_T(CCS and 3), 10);
                State.Status_Reg_C := State.Status_Reg_C or Shift_Left (Cmd_Echo and 16#01ff#, 1);
                if RR then
                    State.Status_Reg_C := State.Status_Reg_C or 1;
                end if;
                if State.Debug_Logging then
                    Loggers.Debug_Print (Dskp_Log, "DEBUG: PIO (SYNCH) status C set to: " & 
                                                   Word_To_String (WD => State.Status_Reg_C, Base => Binary, Width => 16, Zero_Pad => True));
                end if;
            end if;
        end Set_PIO_Status_Reg_C;

        function Get_Status return Status_Rec is
            Stat : Status_Rec;
        begin
            Stat.Image_Attached := State.Image_Attached;
            Stat.Image_Filename := State.Image_Filename;
            Stat.Status_Reg_A   := State.Status_Reg_A;
            Stat.Status_Reg_B   := State.Status_Reg_B;
            Stat.Status_Reg_C   := State.Status_Reg_C;
            Stat.Sector_No      := State.Sector_No;
            Stat.Reads          := State.Reads;
            Stat.Writes         := State.Writes;
            return Stat;
        end Get_Status;
        
    end Drives;


    task body Status_Sender is
        Status : Status_Rec;
    begin
        accept Start do
            Ada.Text_IO.Put_Line ("INFO: DSKP Status Sender started");
        end Start;
        loop
            Status := Drives.Get_Status;
            Status_Monitor.Monitor.DSKP_Update (Status);
            delay Status_Period_S;
        end loop;
    end Status_Sender;

end Devices.Disk6239;
