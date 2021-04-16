-- MIT License

-- Copyright (c) 2021 Stephen Merrony

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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

with Debug_Logs;     use Debug_Logs;
with Devices;
with Devices.Bus;
with Memory;         use Memory;
with Status_Monitor;

package body Devices.Disk6061 is

    protected body Drives is

        procedure Init (Debug_Logging : in Boolean) is
        begin
            State.Debug_Logging := Debug_Logging;
            Devices.Bus.Actions.Set_Reset_Proc (Devices.DPF, Reset'Access);
            Devices.Bus.Actions.Set_Data_In_Proc (Devices.DPF, Data_In'Access);
            Devices.Bus.Actions.Set_Data_Out_Proc (Devices.DPF, Data_Out'Access);
            State.Image_Attached := False;
            State.Map_Enabled    := False;
            Reset;
            Status_Sender.Start;
        end Init;

        procedure Reset is
        begin
            State.RW_Status        := 0;
            State.Instruction_Mode := Normal;
            State.Command          := 0;
            State.Cylinder         := 0;
            State.Surface          := 0;
            State.Sector           := 0;
            State.Sector_Cnt       := 0;
            State.Drive_Status     := Drive_Stat_Ready;
        end Reset;

        procedure Attach
           (Unit : in Natural; Image_Name : in String; OK : out Boolean)
        is
        begin
            if Unit /= 0 then
                raise Not_Yet_Implemented
                   with "DPF - Multiple disks not yet supported";
            end if;
            Sector_IO.Open (State.Image_File, Inout_File, Image_Name);
            State.Image_Filename := To_Unbounded_String (Image_Name);
            State.Image_Attached := True;
            Devices.Bus.Actions.Set_Image_Attached (Devices.DPF, Image_Name);
            OK := True;
        exception
            when Error : others =>
                Loggers.Debug_Print
                   (Debug_Log,
                    "WARNING: Could not open disk image due to " &
                    Exception_Information (Error));
                OK := False;
        end Attach;

        function Get_Status return Status_Rec is
            Stat : Status_Rec;
        begin
            Stat.Image_Attached := State.Image_Attached;
            Stat.Image_Filename := State.Image_Filename;
            Stat.Cylinder       := State.Cylinder;
            Stat.Surface        := State.Surface;
            Stat.Sector         := State.Sector;
            Stat.Reads          := State.Reads;
            Stat.Writes         := State.Writes;
            return Stat;
        end Get_Status;

        procedure Position_Image is
            Offset : Integer;
        begin
            Offset := ((Integer(State.Cylinder) * Surfaces_Per_Disk) +
                        (Integer(State.Surface) * Sectors_Per_Track) +
                        Integer(State.Sector)) 
                        + 1;
            Sector_IO.Set_Index (State.Image_File, Sector_IO.Count(Offset));
        end Position_Image;

        function Printable_Addr return String is
        begin
            return "DRV: " & State.Drive'Image &
                   ", CYL: " & State.Cylinder'Image &
                   ", SURF: " & State.Surface'Image &
                   ", SECT: " & State.Sector'Image &
                   ", SECCNT: " & State.Sector_Cnt'Image;
        end Printable_Addr;


        procedure Do_Command is
        begin
            State.Instruction_Mode := Normal; -- ??? is this right ???
            case State.Command is
                when Cmd_T'Pos(Recal) =>
                    State.Cylinder := 0;
                    State.Surface  := 0;
                    State.Sector   := 0;
                    Position_Image;
                    State.Drive_Status := Drive_Stat_Ready;
                    State.RW_Status := RW_Stat_RW_Done or RW_Stat_Drive_0_Done;
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dpf_Log, "... RECAL done " & Printable_Addr);
                    end if;
                when Cmd_T'Pos(Seek) =>
                    Position_Image;
                    State.RW_Status := RW_Stat_RW_Done or RW_Stat_Drive_0_Done;
                    State.Drive_Status := Drive_Stat_Ready;
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dpf_Log, "SEEK done  " & Printable_Addr);
                    end if;
                when Cmd_T'Pos(Read) =>
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dpf_Log, "... READ called  " & Printable_Addr);
                    end if;
                    State.RW_Status := 0;
                    while State.Sector_Cnt /= 0 loop
                       if State.Cylinder >= Word_T(Cylinders_Per_Disk) then
                            State.Drive_Status := Drive_Stat_Ready;
                            State.RW_Status := RW_Stat_RW_Done or RW_Stat_RW_Fault or RW_Stat_Cylinder;
                            return;
                       end if;
                       if State.Sector >= Word_T(Sectors_Per_Track) then
                            State.Sector_Cnt := 0;
                            State.Surface := State.Surface + 1;
                       end if;    
                       if State.Surface >= Word_T(Surfaces_Per_Disk) then
                            State.Drive_Status := Drive_Stat_Ready;
                            State.RW_Status := RW_Stat_RW_Done or RW_Stat_RW_Fault or RW_Stat_Illegal_Sector;
                            return;
                       end if;
                       Position_Image;
                       Sector_IO.Read(State.Image_File, State.Read_Buff);
                       for W_Ix in 0 .. Words_Per_Sector - 1 loop
                          BMC_DCH.Write_Word_BMC_16(State.Mem_Addr, State.Read_Buff(W_Ix));
                       end loop;
                       State.Sector := State.Sector + 1;
                       State.Sector_Cnt := State.Sector_Cnt + 1;
                       State.Reads := State.Reads + 1;
                    end loop;
                    State.RW_Status := RW_Stat_RW_Done or RW_Stat_Drive_0_Done;
                    if State.Debug_Logging then
                        Loggers.Debug_Print (Dpf_Log, "... READ done    " & Printable_Addr);
                    end if;
                when Cmd_T'Pos(Release) =>
                    -- I think this is a No-Op on a single CPU machine
                    null;

                when Cmd_T'Pos(Write) =>
                    declare
                       Datum : Word_T;
                    begin
                        if State.Debug_Logging then
                            Loggers.Debug_Print (Dpf_Log, "... WRITE called  " & Printable_Addr);
                        end if;
                        State.RW_Status := 0;
                        while State.Sector_Cnt /= 0 loop
                            if State.Cylinder >= Word_T(Cylinders_Per_Disk) then
                                    State.Drive_Status := Drive_Stat_Ready;
                                    State.RW_Status := RW_Stat_RW_Done or RW_Stat_RW_Fault or RW_Stat_Cylinder;
                                    return;
                            end if;
                            if State.Sector >= Word_T(Sectors_Per_Track) then
                                    State.Sector_Cnt := 0;
                                    State.Surface := State.Surface + 1;
                            end if;    
                            if State.Surface >= Word_T(Surfaces_Per_Disk) then
                                    State.Drive_Status := Drive_Stat_Ready;
                                    State.RW_Status := RW_Stat_RW_Done or RW_Stat_RW_Fault or RW_Stat_Illegal_Sector;
                                    return;
                            end if;
                            Position_Image;
                            for W_Ix in 0 .. Words_Per_Sector - 1 loop
                                BMC_DCH.Read_Word_BMC_16 (State.Mem_Addr, Datum);
                                State.Write_Buff(W_Ix) := Datum;
                            end loop;
                            Sector_IO.Write(State.Image_File, State.Write_Buff);
                            State.Sector := State.Sector + 1;
                            State.Sector_Cnt := State.Sector_Cnt + 1;
                            State.Writes := State.Writes + 1;
                        end loop;
                        if State.Debug_Logging then
                            Loggers.Debug_Print (Dpf_Log, "... WRITE done    " & Printable_Addr);
                        end if;
                        State.Drive_Status := Drive_Stat_Ready;
                        State.RW_Status := RW_Stat_RW_Done; -- or RW_Stat_Drive_0_Done;
                    end;

                when others =>
                    raise Not_Yet_Implemented with State.Command'Image;                                               
            end case;
        end Do_Command;

        procedure Handle_Flag (IO_Flag : in IO_Flag_T) is
        begin
            case IO_Flag is
                when S =>
                    Devices.Bus.States.Set_Busy (Devices.DPF, true);
                    Devices.Bus.States.Set_Done (Devices.DPF, false);
                    -- TODO stop any I/O
                    State.RW_Status := 0;
                    -- TODO start I/O timeout
                    Do_Command;
                    Devices.Bus.States.Set_Busy (Devices.DPF, false);
                    Devices.Bus.States.Set_Done (Devices.DPF, true);
                    Devices.Bus.States.Send_Interrupt (Devices.DPF);
                when C =>
                    Devices.Bus.States.Set_Busy (Devices.DPF, false);
                    Devices.Bus.States.Set_Done (Devices.DPF, false);
                    State.RW_Status := 0;
                when P => -- 'Reserved...'
                    Devices.Bus.States.Set_Busy (Devices.DPF, false);
                    State.RW_Status := 0;
                    Do_Command;
                    Devices.Bus.States.Send_Interrupt (Devices.DPF);
                when None =>
                    null;
            end case;
        end Handle_Flag;

        -- Data_In services the DIA/B/C I/O instructions
        procedure Data_In
           (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T)
        is
        begin
            case ABC is
                when A =>
                    case State.Instruction_Mode is
                        when Normal =>
                            Datum := State.RW_Status;
                        when Alt_1 =>
                            Datum := State.Mem_Addr;
                        when Alt_2 =>
                            Datum := 0; -- return 0 for ECC
                    end case;
                when B =>
                    case State.Instruction_Mode is
                        when Normal =>
                            Datum := State.Drive_Status and 16#feff#;
                        when Alt_1 =>
                            Datum := 16#8000# or (State.EMA and 16#001f#);
                        when Alt_2 =>
                            Datum := 0; -- return 0 for ECC
                    end case;  
                when C =>
                    Datum := 0;
                    if State.Map_Enabled then 
                        Datum := 16#8000#;
                    end if;
                    Datum := Datum or Shift_Left(State.Surface and 16#001f#,10);  
                    Datum := Datum or Shift_Left(State.Sector and 16#001f#,5); 
                    Datum := Datum or (Word_T(State.Sector_Cnt) and 16#001f#);
                when N =>
                    raise Not_Yet_Implemented with "N flag on data in to DPF";
            end case;
            if State.Debug_Logging then
                Loggers.Debug_Print (Dpf_Log, "DI" & ABC'Image & " [" & State.Instruction_Mode'Image & 
                    "] Returning: " & Dword_To_String(Dword_T(Datum), Binary, 16, true));
            end if;
                    
            Handle_Flag (IO_Flag);
        end Data_In;

        -- Data_Out implements the DOA/B/C instructions
        -- NIO is also routed here with a dummy abc flag value of N
        procedure Data_Out (Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T) is
        begin
           case ABC is
                when A =>
                    State.Command := Shift_Right (Datum and 16#0780#, 7);
                    State.Drive   := Shift_Right (Datum and 16#0060#, 5);
                    State.Drive   := Shift_Right (Datum and 16#0060#, 5);
                    if Test_W_Bit(Datum, 0) then
                        State.RW_Status := State.RW_Status and 2#1011_1100_0000_0100#; -- clear R/W status
                    end if;
                    if Test_W_Bit(Datum, 1) then
                        State.RW_Status := State.RW_Status and not RW_Stat_Drive_0_Done;
                    end if;
                    if Test_W_Bit(Datum, 2) then
                        State.RW_Status := State.RW_Status and not RW_Stat_Drive_1_Done;
                    end if;
                    if Test_W_Bit(Datum, 3) then
                        State.RW_Status := State.RW_Status and not RW_Stat_Drive_2_Done;
                    end if;
                    if Test_W_Bit(Datum, 4) then
                        State.RW_Status := State.RW_Status and not RW_Stat_Drive_3_Done;
                    end if;
                    State.Instruction_Mode := Normal;
                    if State.Command = Cmd_T'Pos (Set_Alt_Mode_1) then
                        State.Instruction_Mode := Alt_1;
                        if State.Debug_Logging then
                            Loggers.Debug_Print (Dpf_Log, "... Alt Mode 1 set");
                        end if;    
                    elsif State.Command = Cmd_T'Pos (Set_Alt_Mode_2) then
                        State.Instruction_Mode := Alt_2;
                        if State.Debug_Logging then
                            Loggers.Debug_Print (Dpf_Log, "... Alt Mode 2 set");
                        end if;
                    end if;
                    if State.Command = Cmd_T'Pos (No_Op) then
                        State.Instruction_Mode := Normal;
                        State.RW_Status := 0;
                        State.Drive_Status := Drive_Stat_Ready;
                        if State.Debug_Logging then
                            Loggers.Debug_Print (Dpf_Log, "... NO OP command done");
                        end if;
                    end if;
                    State.Last_DOA_Was_Seek := State.Command = Cmd_T'Pos (Seek);
                when B =>
                    if Test_W_Bit (Datum, 0) then
                        State.EMA := State.EMA or 16#01#;
                    else
                        State.EMA := State.EMA and 16#fe#;
                    end if;
                    State.Mem_Addr := Datum and 16#7fff#;
                when C =>
                    if State.Last_DOA_Was_Seek then
                        State.Cylinder := Datum and 16#03ff#; -- mask off lower 10 bits
                    else
                        declare
                           Tmp_Byte : Byte_T;
                        begin
                            State.Map_Enabled := Test_W_Bit (Datum, 0);
                            State.Surface := Shift_Right(Datum and 16#7c00#, 10);
                            State.Sector  := Shift_Right(Datum and 16#03e0#, 5);
                            Tmp_Byte := Byte_T(Datum and 16#001f#);
                            if Tmp_Byte /= 0 then
                                Tmp_Byte := Tmp_Byte or 16#e0#; -- sign extend
                            end if;
                            State.Sector_Cnt :=  Byte_To_Integer_8(Tmp_Byte);
                        end;
                    end if;
                when N => -- dummy value for NIO - we just handle the flag below
                    null;
           end case;
           Handle_Flag (IO_Flag);
        end Data_Out;

    end Drives;

    -- Create_Blank creates an empty disk file of the correct size for the disk6061 emulator to use
    procedure Create_Blank (Image_Name : in String; OK : out Boolean) is
        Tmp_File     : Sector_IO.File_Type;
        Empty_Sector : constant Sector := (others => 0);
    begin
        Sector_IO.Create (Tmp_File, Out_File, Image_Name);
        for S in 1 .. Sectors_Per_Disk loop
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

    task body Status_Sender is
        Status : Status_Rec;
    begin
        accept Start do
            Ada.Text_IO.Put_Line ("INFO: DPF Status Sender started");
        end Start;
        loop
            Status := Drives.Get_Status;
            Status_Monitor.Monitor.DPF_Update (Status);
            delay 0.5;
        end loop;
    end Status_Sender;

end Devices.Disk6061;
