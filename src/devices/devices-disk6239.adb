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
with Memory;         
with Memory_Channels; use Memory_Channels;
with Status_Monitor;

package body Devices.Disk6239 is

    procedure Create_Blank (Image_Name : String; OK : out Boolean) is
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

    procedure Init (Debug_Logging : Boolean) is
    begin
        Drives.Set_Debug_Logging (Debug_Logging);
        Devices.Bus.Actions.Set_Reset_Proc (Devices.DSKP, Drives.Reset'Access);
        Devices.Bus.Actions.Set_Data_In_Proc (Devices.DSKP, Drives.Data_In'Access);
        Devices.Bus.Actions.Set_Data_Out_Proc (Devices.DSKP, Drives.Data_Out'Access);
        --State.Map_Enabled    := False;
        -- Drives.Reset;
        CB_Processor.Start (Debug_Logging);
        Status_Sender.Start;
    end Init;


    protected body Drives is

        procedure Data_In (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T) is
        -- Data_In services the DIA/B/C I/O instructions
        begin
            case ABC is
                when A => Datum := State.Status_Reg_A;
                when B => Datum := State.Status_Reg_B;
                when C => Datum := State.Status_Reg_C;
                when N => 
                    raise Not_Yet_Implemented with "NIO Data_In not supported for DSKP";
            end case;
            if State.Debug_Logging then
                Loggers.Debug_Print (Dskp_Log, "DI" & ABC'Image & " [Read Status] Returning: " & Word_To_String(Datum, Binary, 16, true));
            end if;
            Handle_Flag (IO_Flag);
        end Data_In;

        procedure Data_Out (Datum : Word_T; ABC : IO_Reg_T; IO_Flag : IO_Flag_T) is
        -- Data_Out services the DOA/B/C I/O instructions
        begin
            if State.Debug_Logging then
                Loggers.Debug_Print (Dskp_Log, "DO" & ABC'Image);
            end if;
            case ABC is
                when A => State.Command_Reg_A := Datum;
                when B => State.Command_Reg_B := Datum;
                when C => State.Command_Reg_C := Datum;
                when N => 
                    raise Not_Yet_Implemented with "NIO Data_Out not supported for DSKP";
            end case;
            Handle_Flag (IO_Flag);
        end Data_Out;

        procedure Do_PIO_Command is
            Pio_Cmd : constant Word_T := Extract_PIO_Command (State.Command_Reg_C);
            Addr    : Phys_Addr_T;
        begin
            case Pio_Cmd is
                -- when Pio_Prog_Load => Program_Load;

                when Pio_Begin =>
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... BEGIN command, unit #" & State.Command_Reg_A'Image);
                    end if;
                    -- pretend we have successfully booted ourself
                    State.Status_Reg_B := 0;
                    Set_PIO_Status_Reg_C (Status => Stat_Xec_State_Begun, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Pio_Begin, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Get_Mapping =>
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... GET MAPPING command");
                    end if;
                    State.Status_Reg_A := State.Mapping_Reg_A;
                    State.Status_Reg_B := State.Mapping_Reg_B;
                    Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Get_Mapping, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Set_Mapping =>
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... SET MAPPING command");
                    end if;
                    State.Mapping_Reg_A := State.Command_Reg_A;
                    State.Mapping_Reg_B := State.Command_Reg_B;
                    State.Is_Mapped     := true;
                    Set_PIO_Status_Reg_C (Status => Stat_Xec_State_Mapped, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Set_Mapping, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Get_Interface =>
                    Addr := Phys_Addr_T(Dword_From_Two_Words (State.Command_Reg_A, State.Command_Reg_B));
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... GET INTERFACE INFO command");
                    end if;
                    for W in 0 .. Int_Inf_Blk_Size-1 loop
                        BMC_DCH.Write_Word_BMC_Chan (Addr, State.Int_Inf_Block(W));
                    end loop;
                    Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Get_Interface, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Set_Interface =>
                    Addr := Phys_Addr_T(Dword_From_Two_Words (State.Command_Reg_A, State.Command_Reg_B)) + 5;
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... SET INTERFACE INFO command");
                    end if;
                    BMC_DCH.Read_Word_BMC_Chan (Addr, State.Int_Inf_Block(5));
                    State.Int_Inf_Block(5) := State.Int_Inf_Block(5) and 16#ff00#;
                    BMC_DCH.Read_Word_BMC_Chan (Addr, State.Int_Inf_Block(6));
                    BMC_DCH.Read_Word_BMC_Chan (Addr, State.Int_Inf_Block(7));
                    Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Set_Interface, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

               when Get_Unit =>
                    Addr := Phys_Addr_T(Dword_From_Two_Words (State.Command_Reg_A, State.Command_Reg_B));
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... GET UNIT INFO command");
                        Loggers.Debug_Print (Dskp_Log, "... ... Destination Start Address: " & Addr'Image);
                    end if;                    
                    for W in 0 .. Unit_Inf_Blk_Size-1 loop
                        BMC_DCH.Write_Word_BMC_Chan (Addr, State.Unit_Inf_Block(W));
                        if State.Debug_Logging then
                            Loggers.Debug_Print (Dskp_Log, "... ... Word" & W'Image & ": " & 
                                Word_To_String (State.Unit_Inf_Block(W), Base => Binary, Width => 16, Zero_Pad => True));
                        end if;
                    end loop;
                    Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Get_Unit, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Set_Unit =>
                    Addr := Phys_Addr_T(Dword_From_Two_Words (State.Command_Reg_A, State.Command_Reg_B));
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... SET UNIT INFO command");
                    end if;                      
                    -- only the first word is writable according to p.2-16
		            -- TODO check no active CBs first
                    -- FIXME should this be a direct or BMC access???
                    -- BMC_DCH.Read_Word_BMC_Chan (Addr, State.Unit_Inf_Block(0));
                    State.Unit_Inf_Block(0) := Memory.RAM.Read_Word (Addr);
                    if State.Debug_Logging then
                            Loggers.Debug_Print (Dskp_Log, "... ... Overwrote work 0 of UIB with: "  & 
                                Word_To_String (WD => Memory.RAM.Read_Word (Addr), Base => Binary, Width => 16, Zero_Pad => True));
                    end if;
                    Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Set_Unit, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Pio_Reset =>
                    Reset;
                    -- Reset set the PIO_Status_Reg_C
                
                when Set_Controller =>
                    Addr := Phys_Addr_T(Dword_From_Two_Words (State.Command_Reg_A, State.Command_Reg_B));
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... SET CONTROLLER INFO command");
                    end if;            
                    -- FIXME should this be a direct or BMC access???      
                    -- BMC_DCH.Read_Word_BMC_Chan (Addr, State.Ctrl_Inf_Block(0));
                    -- BMC_DCH.Read_Word_BMC_Chan (Addr, State.Ctrl_Inf_Block(1));
                    State.Ctrl_Inf_Block(0) := Memory.RAM.Read_Word (Addr);
                    State.Ctrl_Inf_Block(1) := Memory.RAM.Read_Word (Addr + 1);
                    Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Set_Controller, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Start_List =>
                    Addr := Phys_Addr_T(Dword_From_Two_Words (State.Command_Reg_A, State.Command_Reg_B));
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... START LIST command");
                    end if;
                    -- TODO should check addr validity before starting processing
                    CB_Processor.Process (Start_CB_Addr => Addr);
                    State.Status_Reg_A := Upper_Word (Dword_T(Addr));
                    State.Status_Reg_B := Lower_Word (Dword_T(Addr));
                    Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Start_List, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when Unit_Status =>
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... GET UNIT STATUS command");
                    end if;                
                    State.Status_Reg_B := 0;
                    Set_W_Bit (State.Status_Reg_B, 2); -- READY
                    -- TODO may need to handle bit 3 'Busy' in the future
                     Set_PIO_Status_Reg_C (Status => 0, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Unit_Status, 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));

                when others =>
                    raise Not_Yet_Implemented with "Unknown PIO command for DSKP, " & Pio_Cmd'Image;
            end case;

        end Do_PIO_Command;

        function Extract_PIO_Command (Wd : Word_T) return Word_T is
            (Shift_Right (Wd and 16#3FE#, 1));

        function Get_Extended_Status_Size return Natural is
            Wd : Word_T;
        begin
            Wd := Shift_Right (State.Int_Inf_Block(5), 8);
            Wd := Wd and 16#000f#;
            return Natural(Word_To_Integer_16(Wd));
        end Get_Extended_Status_Size;

        -- function Get_Sector_No return Dword_T is
        --     (State.Sector_No);

        procedure Handle_Flag (IO_Flag : IO_Flag_T) is
        begin
            case IO_Flag is
                when C =>
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... C flag set, clearing DONE flag");
                    end if;   
                    Devices.Bus.States.Set_Done (Devices.DSKP, false);
                    -- TODO clear any pending interrupt
                    Set_PIO_Status_Reg_C (Status => Stat_Xec_State_Mapped, 
                                          CCS => Stat_Ccs_Pio_Cmd_Ok, 
                                          Cmd_Echo => Extract_PIO_Command (State.Command_Reg_C), 
                                          RR => Test_W_Bit (State.Command_Reg_C, 15));
                when S =>
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dskp_Log, "... S flag set");
                    end if; 
                    Devices.Bus.States.Set_Busy (Devices.DSKP, true);
                    Devices.Bus.States.Set_Done (Devices.DSKP, false);
                    Do_PIO_Command;
                    Devices.Bus.States.Set_Busy (Devices.DSKP, false);
                    -- set the DONE flag if the return bit was set
                    if Test_W_Bit (State.Command_Reg_C, 15) then
                        Devices.Bus.States.Set_Done (Devices.DSKP, true);
                    end if;
                when P =>
                    raise Not_Yet_Implemented with "P flag NYI in DSKP";
                when None =>
                    null; -- null or empty flag - nothing to do
            end case;
        end Handle_Flag;



        procedure Reset is
        begin
            -- Reset Mapping
            State.Mapping_Reg_A := 16#4000#; -- DMA over BMC
            State.Mapping_Reg_B := Map_Int_Bmc_Phys or Map_Upstream_Load or Map_Upstream_Hpt;
            State.Is_Mapped     := False;

            -- Reset Int_Inf_Block
            State.Int_Inf_Block(0) := 8#101#;
            State.Int_Inf_Block(1) := Ucode_Rev;
            State.Int_Inf_Block(2) := 3;
            State.Int_Inf_Block(3) := 16#4000# or Max_Queued_CBs;
            State.Int_Inf_Block(4) := 0;
            State.Int_Inf_Block(5) := 16#0b00#; -- 11 << 8
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
                Loggers.Debug_Print (Dskp_Log, "*** Reset *** via call to Drives.Reset");
            end if;                                
        end Reset;


        procedure Set_Debug_Logging (TF : Boolean) is
        begin
            State.Debug_Logging := TF;
        end Set_Debug_Logging;


        procedure Set_PIO_Status_Reg_C (Status, CCS : Byte_T; Cmd_Echo : Word_T; RR : Boolean) is
        -- Set the SYNCHRONOUS standard refturn as per p.3-22
            Stat : Byte_T := Status;
        begin
            if Status = 0 and State.Is_Mapped then
                Stat := Stat_Xec_State_Mapped;
            end if;
            if RR or (Cmd_Echo = Pio_Reset) then
                State.Status_Reg_C := Shift_Left (Word_T(Stat), 12);
                State.Status_Reg_C := State.Status_Reg_C or Shift_Left (Word_T(CCS and 3), 10);
                State.Status_Reg_C := State.Status_Reg_C or Shift_Left (Cmd_Echo and 16#01ff#, 1);
                if RR then
                    State.Status_Reg_C := State.Status_Reg_C or 1;
                end if;
                if State.Debug_Logging then
                    Loggers.Debug_Print (Dskp_Log, "PIO (SYNCH) status C set to: " & 
                                                   Word_To_String (WD => State.Status_Reg_C, Base => Binary, Width => 16, Zero_Pad => True));
                end if;
            end if;
        end Set_PIO_Status_Reg_C;

        procedure Set_Status_Reg (Reg : Stat_Reg_T; Contents : Word_T) is
        begin
            case Reg is
                when A => State.Status_Reg_A := Contents;
                when B => State.Status_Reg_B := Contents;
                when C => State.Status_Reg_C := Contents;
            end case;
        end Set_Status_Reg;
      
    end Drives;

    task body CB_Processor is

        Logging         : Boolean;
        Image_Attached  : Boolean := False;
        Image_Filename  : Unbounded_String;
        Image_File      : Sector_IO.File_Type;
        DG_Sector_No    : Dword_T := 1234;    -- DG Starts at 0, Ada at 1 :-/
        Reads, Writes   : Unsigned_64 := 0;
        This_CB_Addr,
        Chained_CB_Addr,
        Tmp_Addr        : Phys_Addr_T;
        CB_Length       : Positive;
        Active_CB       : CB_T;
        Op_Code         : Word_T;
        Physical_Transfers : Boolean;
        Physical_Addr,
        Read_Addr,
        Write_Addr      : Phys_Addr_T;
        -- Start_Sect   : Dword_T;
        Read_Buff, Write_Buff : Sector;
        New_Status      : Word_T;

        procedure Set_OK is
        begin
            if CB_Length > CB_Err_Status then
                Active_CB(CB_Err_Status) := 0;
            end if;
            if CB_Length > CB_Unit_Status then
                Active_CB(CB_Unit_Status) := 16#2000#; -- Shift_Left(1, 13); -- Ready
            end if;
            if CB_Length > CB_CB_Status then
                Active_CB(CB_CB_Status) := 1; -- finally, set the Done bit
            end if;  
        end Set_OK;

        procedure Position_Image (New_DG_Sector_No : Dword_T) is
            Offset : constant Positive_Count := Positive_Count(1 + Dword_To_Integer (New_DG_Sector_No) );
        begin
            Sector_IO.Set_Index (Image_File, Sector_IO.Count(Offset));
            DG_Sector_No := New_DG_Sector_No;
            -- TODO handle positioning exception
            if Logging then
                Loggers.Debug_Print (Dskp_Log, "... Positioned disk image to DG sector: " & DG_Sector_No'Image);
            end if;
        end Position_Image;

        procedure Read_Sector (Buffer : out Sector) is
        begin
            Sector_IO.Read(Image_File, Buffer);
            Reads := Reads + 1;
        end Read_Sector;

        procedure Write_Sector (Buffer : Sector) is
        begin
            Sector_IO.Write(Image_File, Buffer);
            Writes := Writes + 1;
        end Write_Sector;

    begin
        accept Start (Debug_Logging : Boolean) do
            Logging := Debug_Logging;
        end Start;
        loop
            select
                accept Attach (Unit : Natural; Image_Name : String; OK : out Boolean) do
                    if Unit /= 0 then
                        raise Not_Yet_Implemented
                        with "DSKP - Multiple disks not yet supported";
                    end if;
                    Sector_IO.Open (Image_File, Inout_File, Image_Name);
                    Image_Filename := To_Unbounded_String (Image_Name);
                    Image_Attached := True;
                    Image_Attached := True;
                    Devices.Bus.Actions.Set_Image_Attached (Devices.DSKP, Image_Name);
                    OK := True;
                exception
                    when Error : others =>
                        Loggers.Debug_Print (Debug_Log, "WARNING: Could not open disk image due to " &
                            Exception_Information (Error));
                        OK := False;
                end Attach;
            or
                accept Get_Stats (S : out Status_Rec) do
                    S.Image_Attached := Image_Attached;
                    S.Image_Filename := Image_Filename;
                    S.Sector_No      := DG_Sector_No;
                    S.Reads := Reads;
                    S.Writes := Writes;
                end Get_Stats;
            or 
                accept Program_Load do
                    if Logging then
                        Loggers.Debug_Print (Dskp_Log, "INFO: PROGRAM LOAD initiated");
                    end if;  
                    Position_Image (0);
                    Sector_IO.Read(Image_File, Read_Buff);
                    Write_Addr := 0;
                    for W_Ix in 0 .. Words_Per_Sector - 1 loop
                        BMC_DCH.Write_Word_BMC_Chan (Write_Addr, Read_Buff(W_Ix));
                    end loop;
                    Position_Image (1);
                    Sector_IO.Read(Image_File, Read_Buff);
                    for W_Ix in 0 .. Words_Per_Sector - 1 loop
                        BMC_DCH.Write_Word_BMC_Chan (Write_Addr, Read_Buff(W_Ix));
                    end loop;
                    if Logging then
                        Loggers.Debug_Print (Dskp_Log, "INFO: PROGRAM LOAD complete");
                    end if; 
                end Program_Load;
            or
                accept Process (Start_CB_Addr : Phys_Addr_T) do
                    This_CB_Addr := Start_CB_Addr;
                end Process;
                loop                        
                    CB_Length := CB_Min_Size + Drives.Get_Extended_Status_Size;
                    if Logging then
                        Loggers.Debug_Print (Dskp_Log, "... Processing CB, extended status size is:" & 
                                                    CB_Length'Image);
                    end if;
                    -- copy CB contents from host memory
                    Tmp_Addr := This_CB_Addr;
                    for Ix in 0 .. CB_Length - 1 loop
                        BMC_DCH.Read_Word_BMC_Chan (Tmp_Addr, Active_CB(Ix));
                    end loop;
                    if Logging then
                        Loggers.Debug_Print (Dskp_Log, "... CB: ");
                        for Ix in 0 .. CB_Length - 1 loop
                            Loggers.Debug_Print (Dskp_Log, "... ..." & Ix'Image & ":" & Active_CB(Ix)'Image);
                        end loop;
                    end if;

                    Op_Code := Active_CB(CB_Ina_Flags_Opcode) and 16#03ff#;
                    Chained_CB_Addr := Phys_Addr_T(Dword_From_Two_Words (Active_CB(CB_Link_Addr_High), 
                                                                        Active_CB(CB_Link_Addr_Low)));

                    if Logging then
                        Loggers.Debug_Print (Dskp_Log, "... CB OpCode:" & Op_Code'Image);
                        Loggers.Debug_Print (Dskp_Log, "... Chained CB Addr:" & Chained_CB_Addr'Image);
                    end if;

                    case Op_Code is
                        when CB_Op_No_Op =>
                            if Logging then
                                Loggers.Debug_Print (Dskp_Log, "... .. NO OP");
                            end if;
                            Set_OK;

                        when CB_Op_Recalibrate_Disk =>
                            if Logging then
                                Loggers.Debug_Print (Dskp_Log, "... .. RECALIBRATE");
                            end if;
                            Position_Image (0);
                            Set_OK;

                        when CB_Op_Read =>
                            Position_Image (Dword_From_Two_Words (Active_CB(CB_Dev_Addr_High), 
                                                                        Active_CB(CB_Dev_Addr_Low)));
                            if Test_W_Bit (Active_CB(CB_Pageno_List_Addr_High), 0) then
                                -- logical premapped host address
                                Physical_Transfers := false;
                                raise Not_Yet_Implemented with "DSKP CB READ from premapped logical address";
                            else
                                Physical_Transfers := true;
                                Physical_Addr  := Phys_Addr_T(Dword_From_Two_Words (Active_CB(CB_Txfer_Addr_High), 
                                                                                    Active_CB(CB_Txfer_Addr_Low)));
                            end if;
                            if Logging then
                                Loggers.Debug_Print (Dskp_Log, "... .. CB READ command, SECCNT:" & Active_CB(CB_Txfer_Count)'Image);
                                Loggers.Debug_Print (Dskp_Log, "... .. .. .... from DG sector :" & DG_Sector_No'Image);
                                Loggers.Debug_Print (Dskp_Log, "... .. .. .... to   phys addr :" & Physical_Addr'Image);
                            end if;

                            for Sect in 0 .. Dword_T(Active_CB(CB_Txfer_Count))-1 loop
                                if Sect > 0 then
                                    Position_Image (DG_Sector_No + 1);
                                end if;
                                Read_Sector (Read_Buff);
                                Write_Addr := Physical_Addr + (Phys_Addr_T(Sect) * Phys_Addr_T(Words_Per_Sector));
                                for W_Ix in 0 .. Words_Per_Sector - 1 loop
                                    BMC_DCH.Write_Word_BMC_Chan (Write_Addr, Read_Buff(W_Ix));
                                end loop;
                            end loop;
                            Set_OK;
                            if Logging then
                                Loggers.Debug_Print (Dskp_Log, "... .. CB READ Complete");
                            end if;

                        when CB_Op_Write =>
                            Position_Image (Dword_From_Two_Words (Active_CB(CB_Dev_Addr_High), 
                                                                        Active_CB(CB_Dev_Addr_Low)));
                            if Test_W_Bit (Active_CB(CB_Pageno_List_Addr_High), 0) then
                                -- logical premapped host address
                                Physical_Transfers := false;
                                raise Not_Yet_Implemented with "DSKP CB WRITE from premapped logical address";
                            else
                                Physical_Transfers := true;
                                Physical_Addr  := Phys_Addr_T(Dword_From_Two_Words (Active_CB(CB_Txfer_Addr_High), 
                                                                                    Active_CB(CB_Txfer_Addr_Low)));
                            end if;
                            if Logging then
                                Loggers.Debug_Print (Dskp_Log, "... .. CB WRITE command, SECCNT:" & Active_CB(CB_Txfer_Count)'Image);
                                Loggers.Debug_Print (Dskp_Log, "... .. .. ..... to DG sector   :" & DG_Sector_No'Image);
                                Loggers.Debug_Print (Dskp_Log, "... .. .. ..... from phys addr :" & Physical_Addr'Image);
                            end if;

                            for Sect in 0 .. Active_CB(CB_Txfer_Count) - 1 loop
                                if Sect > 0 then
                                    Position_Image (DG_Sector_No + 1);
                                end if;
                                Read_Addr := Physical_Addr + (Phys_Addr_T(Sect) * Phys_Addr_T(Words_Per_Sector));
                                for W_Ix in 0 .. Words_Per_Sector - 1 loop
                                    BMC_DCH.Read_Word_BMC_Chan (Read_Addr, Write_Buff(W_Ix));
                                end loop;
                                Write_Sector (Write_Buff);
                            end loop;
                            Set_OK;
                            if Logging then
                                Loggers.Debug_Print (Dskp_Log, "... .. CB WRITE Complete");
                            end if;
                        when others =>
                            raise Not_Yet_Implemented with "DSKP CB Command:" & Op_Code'Image;
                    end case;

                    -- write back the CB
                    Tmp_Addr := This_CB_Addr;
                    for Ix in 0 .. CB_Length-1 loop
                        BMC_DCH.Write_Word_BMC_Chan (Tmp_Addr, Active_CB(Ix));
                    end loop;

                    if Chained_CB_Addr = 0 then
                        -- end of chain - set ASYNCH status - See p.4-15
                        while Devices.Bus.States.Get_Busy (Devices.DSKP) or Devices.Bus.States.Get_Done (Devices.DSKP) loop
                            delay Async_Stat_Retry_Interval;
                        end loop;
                        New_Status := Shift_Left (Word_T(Stat_Xec_State_Mapped), 12) or (Word_T(Stat_Async_No_Errors) and 16#03ff#);
                        Drives.Set_Status_Reg (C, New_Status);
                        if Logging then
                                Loggers.Debug_Print (Dskp_Log, "ASYNCHRONOUS status C set to: " & 
                                    Word_To_String (WD => New_Status, Base => Binary, Width => 16, Zero_Pad => True));
                        end if;
                        Devices.Bus.States.Set_Done (Devices.DSKP, true);
                        exit;
                    else
                        This_CB_Addr := Chained_CB_Addr;
                    end if;
                end loop;

            or
                terminate;
            end select;
        end loop;
    end CB_Processor;

    task body Status_Sender is
        Status : Status_Rec;
    begin
        accept Start do
            Ada.Text_IO.Put_Line ("INFO: DSKP Status Sender started");
        end Start;
        loop
            CB_Processor.Get_Stats (Status);
            Status_Monitor.Monitor.DSKP_Update (Status);
            delay Status_Period_S;
        end loop;
    end Status_Sender;

end Devices.Disk6239;
