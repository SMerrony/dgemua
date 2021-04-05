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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            
with Debug_Logs;             use Debug_Logs;
with Devices;
with Devices.Bus;
with Memory;
with Simh_Tapes;             use Simh_Tapes;
with Status_Monitor;

package body Devices.Magtape6026 is

    protected body Drives is

        procedure Init is
        begin

            --Devices.Bus.Actions.Set_Reset_Proc (Devices.TTI, Reset'Access);
            Devices.Bus.Actions.Set_Data_In_Proc (Devices.MTB, Data_In'Access);
            --Devices.Bus.Actions.Set_Data_Out_Proc (Devices.TTI, Data_Out'Access);
            State.Status_Reg_1 := SR_1_HiDensity or SR_1_9Track or SR_1_UnitReady;
            State.Status_Reg_2 := SR_2_PE_Mode;
            Status_Sender.Start;
        end Init;

        procedure Attach (Unit : in Natural; Image_Name : in String; OK : out Boolean) is
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

        procedure Detach (Unit : in Natural) is
        begin
            Close (File => State.SIMH_File(Unit));
            State.Image_Filename(Unit) := Null_Unbounded_String;
            State.Image_Attached(Unit) := false;
            State.Status_Reg_1 := SR_1_Error or SR_1_HiDensity or SR_1_9Track or SR_1_BOT or SR_1_StatusChanged or SR_1_UnitReady;
            State.Status_Reg_2 := SR_2_PE_Mode;
            Devices.Bus.Actions.Set_Image_Detached (Devices.MTB);
        end Detach;

        procedure Handle_Flag ( IO_Flag : IO_Flag_T) is 
        begin
            case IO_Flag is
                when S =>
                    Loggers.Debug_Print (Mt_Log, "... S flag set");
                    if State.Current_Cmd /= Cmd_Rewind then
                        Devices.Bus.States.Set_Busy (Devices.MTB, true);
                    end if;
                    Devices.Bus.States.Set_Done (Devices.MTB, false);
                    -- FIXME: Do_Command;
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
        procedure  Data_In (ABC : in Character; IO_Flag : in IO_Flag_T; Datum : out Word_T) is
        begin
            case ABC is
                when 'A' => -- Read status register 1 - see p.IV-18 of Peripherals guide
                    Datum := State.Status_Reg_1;
                    Loggers.Debug_Print (Mt_Log, "DIA - Read SR1 - returning: " & Datum'Image);
                 when 'B' => --Read memory addr register 1 - see p.IV-19 of Peripherals guide
                    Datum := Word_T(State.Mem_Addr_Reg);
                    Loggers.Debug_Print (Mt_Log, "DIB - Read MA - returning: " & Datum'Image);  
                when 'C' => -- Read status register 2 - see p.IV-18 of Peripherals guide
                    Datum := State.Status_Reg_2;
                    Loggers.Debug_Print (Mt_Log, "DIC - Read SR2 - returning: " & Datum'Image); 
                when others => null;    
            end case;
            Handle_Flag (IO_Flag);
        end Data_In;

        function Get_Image_Name (Unit : in Natural) return String is
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
            Hdr, Trailer : Dword_T;
            TBOOT_Rec : Mt_Rec(0..2047); -- TBOOT Block is always 2KB
            TBOOT_Size_W : Integer;
            Byte_0, Byte_1 : Byte_T;
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
         delay 0.5;
      end loop;
   end Status_Sender;

end Devices.Magtape6026;