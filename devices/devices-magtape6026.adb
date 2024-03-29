-- MIT License

-- Copyright (c)2021,2022 Stephen Merrony

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

with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;

with Debug_Logs;       use Debug_Logs;
with Devices;
with Devices.Bus;
with Memory;
with Memory_Channels; use Memory_Channels;
with Simh_Tapes;      use Simh_Tapes;
with Status_Monitor;

package body Devices.Magtape6026 is

    protected body Drives is

        procedure Init is
        begin
            Devices.Bus.Actions.Set_Reset_Proc (Devices.MTB, Reset'Access);
            Devices.Bus.Actions.Set_Data_In_Proc (Devices.MTB, Data_In'Access);
            Devices.Bus.Actions.Set_Data_Out_Proc (Devices.MTB, Data_Out'Access);
            State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady;
            State.Status_Reg_2 := SR_2_PE_Mode;
            --  Status_Sender.Start;
        end Init;

        procedure Reset is
        begin
            for Tape in 0 .. Max_Tapes loop
               if State.Image_Attached(Tape) then
                  Rewind (State.SIMH_File(Tape));
                end if;
            end loop;
            -- BOT is an error state!
            State.Status_Reg_1 := SR_1_Error or SR_1_HiDensity or SR_1_9Track or SR_1_BOT or SR_1_StatusChanged or SR_1_UnitReady;
            State.Status_Reg_2 := SR_2_PE_Mode;
            State.Mem_Addr_Reg := 0;
            State.Neg_Word_Count := 0;
            State.Current_Unit := 0;
            Loggers.Debug_Print (Mt_Log, "Unit reset via bus Reset call");
        end Reset;

        procedure Attach (Unit : Natural; Image_Name : String; OK : out Boolean) is
        begin
            -- Open (File => Img_File, Mode => In_File, Name => Image_Name);
            Open (File => State.SIMH_File(Unit), Mode => In_File, Name => Image_Name);
            State.Image_Filename(Unit) := To_Unbounded_String(Image_Name);
            State.Image_Attached(Unit) := true;
            State.Status_Reg_1 := SR_1_Error or SR_1_HiDensity or SR_1_9Track or SR_1_BOT or SR_1_StatusChanged or SR_1_UnitReady;
            State.Status_Reg_2 := SR_2_PE_Mode;
            Devices.Bus.Actions.Set_Image_Attached (Devices.MTB, Image_Name);
            OK := true;
        exception
            when Error : others =>
                Loggers.Debug_Print (Debug_Log, "WARNING: Could not open SimH tape image due to " & 
                    Exception_Information (Error)); 
                OK := false;
        end Attach;

        procedure Detach (Unit : Natural) is
        begin
            Close (File => State.SIMH_File(Unit));
            State.Image_Filename(Unit) := Null_Unbounded_String;
            State.Image_Attached(Unit) := false;
            State.Status_Reg_1 := SR_1_Error or SR_1_HiDensity or SR_1_9Track or SR_1_BOT or SR_1_StatusChanged or SR_1_UnitReady;
            State.Status_Reg_2 := SR_2_PE_Mode;
            Devices.Bus.Actions.Set_Image_Detached (Devices.MTB);
        end Detach;

        procedure Do_Command is
            Hdr, Trailer : Unsigned_32; 
            Img_Stream : Stream_Access;  
            RC : Mt_Stat;  
        begin
            -- currently only supporting one drive (Unit 0)...
            if State.Current_Unit /= 0 then
                Loggers.Debug_Print (Mt_Log, "WARNING: Command issued for Unit #" & State.Current_Unit'Image);
                State.Status_Reg_1 := SR_1_Error or SR_1_Illegal;
                State.Status_Reg_2 := SR_2_Error;
                return;
            end if;
            case State.Current_Cmd is
                when Read =>
                    Loggers.Debug_Print (Mt_Log, "DEBUG: *READ* command - Unit:" & State.Current_Unit'Image &
                        " Word Count: "  & State.Neg_Word_Count'Image &
                        " Location: " & State.Mem_Addr_Reg'Image);
                    Img_Stream := stream(State.SIMH_File(State.Current_Unit));
                    Read_Meta_Data (Img_Stream, Hdr);
                    if Hdr = Mtr_Tmk then
                        Loggers.Debug_Print (Mt_Log, "WARNING: *READ* - Header is EOF (Tape Mark) Indicator");
                        State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady or SR_1_EOF or SR_1_Error;
                    else
                        Loggers.Debug_Print (Mt_Log, "DEBUG: *READ* - Expecting to read blocksize:" & Hdr'Image);
                        declare
                           Tape_Rec : Mt_Rec(0..Integer(Hdr));
                           W : Natural := 0;
                           Wd : Word_T;
                        begin
                           Read_Record_Data (Img_Stream, Natural(Hdr), Tape_Rec);
                           while W < Natural(Hdr) loop
                              Wd := Word_From_Bytes(Byte_T(Tape_Rec(W)), Byte_T(Tape_Rec(W+1)));
                              W := W + 2;
                              BMC_DCH.Write_Word_DCH_Chan(State.Mem_Addr_Reg, Wd);
                              State.Neg_Word_Count := State.Neg_Word_Count + 1;
                              exit when State.Neg_Word_Count = 0;
                           end loop;
                           Read_Meta_Data (Img_Stream, Trailer);
                           Loggers.Debug_Print (Mt_Log, "DEBUG:  *READ* -" & W'Image & " bytes read");
                           Loggers.Debug_Print (Mt_Log, "DEBUG:  *READ* - Read SimH Trailer: " & Trailer'Image);
                           -- TODO Need to verify how status should be set here...
                           State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_StatusChanged or SR_1_UnitReady;
                        end;
                    end if;

                when Rewind =>
                    Loggers.Debug_Print (Mt_Log, "DEBUG: *REWIND* command - Unit:" & State.Current_Unit'Image);
                    Rewind (State.SIMH_File(State.Current_Unit));
                    State.Status_Reg_1 := SR_1_Error or SR_1_HiDensity or SR_1_9Track or SR_1_StatusChanged or SR_1_UnitReady or SR_1_BOT;

                when Space_Fwd =>
                    Loggers.Debug_Print (Mt_Log, "DEBUG: *SPACE FORWARD* command - Unit:" & State.Current_Unit'Image);
                    Img_Stream := stream(State.SIMH_File(State.Current_Unit));
                    if State.Neg_Word_Count = 0 then -- one whole file
                        Space_Forward (Img_Stream, 0, RC);
                        -- according to the simH source, MA should be set to # files/recs skipped
                        -- can't find any reference to this in the Periph Pgmrs Guide but it lets INSTL
                        -- progress further...
                        -- It seems to need the two's complement of the number...
                        State.Mem_Addr_Reg := 16#ffff_ffff#;
                        if RC = OK then
                            State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady or SR_1_EOF or SR_1_Error;
                        else
                            State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady or SR_1_EOT or SR_1_StatusChanged;
                        end if;
                    else
                        Space_Forward (Img_Stream, Integer(State.Neg_Word_Count), RC);
                        case RC is
                            when OK =>
                                State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady or SR_1_StatusChanged;
                            when Tmk =>
                                State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady or SR_1_EOF or SR_1_StatusChanged or SR_1_Error;
                            when InvRec =>
                                State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady or SR_1_DataError or SR_1_StatusChanged or SR_1_Error;
                            when others =>
                                raise Unexpected_Return_Code with "from SimH Space Forward command";
                        end case;
                        State.Mem_Addr_Reg := Phys_Addr_T(State.Neg_Word_Count);
                    end if;

                when others =>
                    Loggers.Debug_Print (Mt_Log, "WARNING: Command not yet implemented: " & State.Current_Cmd'Image);
                    raise Not_Yet_Implemented with "MTB Command " & State.Current_Cmd'Image;
            end case;
        end Do_Command;

        procedure Handle_Flag ( IO_Flag : IO_Flag_T) is 
        begin
            case IO_Flag is
                when S =>
                    Loggers.Debug_Print (Mt_Log, "... S flag set");
                    if State.Current_Cmd /= Rewind then
                        Devices.Bus.States.Set_Busy (Devices.MTB, true);
                    end if;
                    Devices.Bus.States.Set_Done (Devices.MTB, false);
                    Do_Command;
                    Devices.Bus.States.Set_Busy (Devices.MTB, false);
                    Devices.Bus.States.Set_Done (Devices.MTB, true);
                when C =>
                    Loggers.Debug_Print (Mt_Log, "... C flag set");
                    Devices.Bus.States.Set_Busy (Devices.MTB, false);
                    Devices.Bus.States.Set_Done (Devices.MTB, false);
                when P => -- 'Reserved...'
                    Loggers.Debug_Print (Mt_Log, "WARNING: P flag set - 'Reserved' command");
                when None =>
                    null;
            end case;
        end Handle_Flag;

        -- Data_In is called from Bus to implement DIx from the mt device
        procedure  Data_In (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T) is
        begin
            case ABC is
                when A => -- Read status register 1 - see p.IV-18 of Peripherals guide
                    Datum := State.Status_Reg_1;
                    Loggers.Debug_Print (Mt_Log, "DIA - Read SR1 - returning: " & Datum'Image);
                 when B => --Read memory addr register 1 - see p.IV-19 of Peripherals guide
                    Datum := Word_T(State.Mem_Addr_Reg and 16#0000_ffff#);
                    Loggers.Debug_Print (Mt_Log, "DIB - Read MA - returning: " & Datum'Image);  
                when C => -- Read status register 2 - see p.IV-18 of Peripherals guide
                    Datum := State.Status_Reg_2;
                    Loggers.Debug_Print (Mt_Log, "DIC - Read SR2 - returning: " & Datum'Image); 
                when others => null;    
            end case;
            Handle_Flag (IO_Flag);
        end Data_In;

        procedure Data_Out (Datum : Word_T; ABC : IO_Reg_T; IO_Flag : IO_Flag_T) is
        begin
            case ABC is
                when A => -- specify Command and Drive - p.IV-17
                    -- which command?
                    for C in Cmd_T'Range loop
                       if (Datum and Cmd_Mask) = Commands(C) then
                          State.Current_Cmd := C;
                          exit;
                        end if;
                    end loop;
                    --which unit?
                    State.Current_Unit := Natural(Datum and 16#0007#);
                    Loggers.Debug_Print (Mt_Log, "INFO: DOA - Specify Command: " & State.Current_Cmd'Image &
                        " Unit: " & State.Current_Unit'Image);
                when B =>
                    State.Mem_Addr_Reg := Phys_Addr_T (Datum);
                    Loggers.Debug_Print (Mt_Log, "INFO: DOB - Mem Addr set to: " & State.Mem_Addr_Reg'Image);
                when C =>
                    State.Neg_Word_Count := Word_To_Integer_16(Datum);
                    Loggers.Debug_Print (Mt_Log, "INFO: DOC - Neg. Word Count set to : " & State.Neg_Word_Count'Image);
                when N => -- TODO
                    Loggers.Debug_Print (Mt_Log, "WARNING: NIO - Flag is : " & Datum'Image ); 
            end case;
            Handle_Flag (IO_Flag);
        end Data_Out;

        function Get_Image_Name (Unit : Natural) return String is
        begin
           return To_String (State.Image_Filename(Unit));
        end Get_Image_Name;

        function Get_Status return Status_Rec is
            Status : Status_Rec;
        begin
            Status.Image_Attached := State.Image_Attached;
            Status.Image_Filename := State.Image_Filename;
            Status.Mem_Addr_Reg   := State.Mem_Addr_Reg;
            Status.Current_Cmd    := State.Current_Cmd;
            Status.Status_Reg_1   := State.Status_Reg_1;
            Status.Status_Reg_2   := State.Status_Reg_2;
            return Status;
        end Get_Status;

        -- Load_TBOOT - This proc fakes the ROM/SCP boot-from-tape routine.
        -- Rather than copying a ROM (how?) and executing that, we simply mimic its basic actions...
        --  * Load file 0 from tape (1 x 2k block)
        --  * Put the loaded code at physical location 0
        procedure Load_TBOOT is
            Unit : constant Integer := 0;
            Img_Stream : Stream_Access;
            Hdr, Trailer : Unsigned_32;
            TBOOT_Rec : Mt_Rec(0..2047); -- TBOOT Block is always 2KB
            TBOOT_Size_W : Integer;
            Byte_0, Byte_1 : Unsigned_8;
            B_Wd : Word_T;
            Mem_Ix : Integer := 0;
        begin
            Loggers.Debug_Print (Mt_Log, "Load_TBOOT called" );
            Rewind (State.SIMH_File(Unit));
            Loggers.Debug_Print (Mt_Log, "... Tape rewound" );
            Img_Stream := stream(State.SIMH_File(Unit));

        Read_Loop:
            loop
               Read_Meta_Data (Img_Stream, Hdr);
               case Hdr is
                  when Mtr_Tmk =>
                      exit Read_Loop;
                  when others =>
                     TBOOT_Size_W := Integer(Hdr) / 2;
                     Read_Record_Data (Img_Stream, Natural(Hdr), TBOOT_Rec);
                     for Wd_Ix in 0 .. TBOOT_Size_W - 1 loop
                        Byte_1 := TBOOT_Rec(Wd_Ix * 2);
                        Byte_0 := TBOOT_Rec((Wd_Ix * 2) + 1);
                        B_Wd  := Shift_Left(Word_T(Byte_1), 8) or Word_T(Byte_0);
                        Memory.RAM.Write_Word(Phys_Addr_T(Mem_Ix + Wd_Ix), B_Wd);
                     end loop;
                     Mem_Ix := Mem_Ix + TBOOT_Size_W;
                     Read_Meta_Data (Img_Stream , Trailer);
                     if Hdr /= Trailer then
                        Loggers.Debug_Print (Mt_Log, "ERROR: Load_TBOOT - Mismatched trailer record");
                     end if;
                end case;
            end loop Read_Loop;
            Rewind (State.SIMH_File(Unit));
            Loggers.Debug_Print (Mt_Log, "Load_TBOOT completed" );
        end Load_TBOOT;   

    end Drives;

    task body Status_Sender is
        Status :  Status_Rec;
    begin
      accept Start do
         Ada.Text_IO.Put_line ("INFO: MTB Status Sender started");
      end Start;
      loop
         Status := Drives.Get_Status;
         Status_Monitor.Monitor.MTB_Update (Status);
         delay Status_Period_S;
      end loop;
   end Status_Sender;

end Devices.Magtape6026;