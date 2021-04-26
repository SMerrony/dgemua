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

with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Text_IO;

with Interfaces; use Interfaces;

with DG_Types; use DG_Types;
with Memory;   use Memory;

-- Status_Monitor maintains a near real-time status screen available on STAT_PORT.
--
-- The screen uses DG DASHER control codes for formatting, so a DASHER terminal emulator
-- should be attached to it for good results.
--
-- The Monitor task waits for status updates
-- from known senders and upon receiving an update refreshes the display of that status
-- on the monitor page.  It is therefore the responsibility of the sender to update the
-- status as often as it sees fit.
package body Status_Monitor is

   function To_Float(TS : Time_Span) return Float is
      SC1, SC2, SC3 : Seconds_Count;
      TS1, TS2, TS3 : Time_Span;
   begin
      -- Use Split(Time_Of()) to split time span into seconds and fraction
      -- Repeat twice to get microseconds and fraction thereof
      Split(Time_Of(0, TS), SC1, TS1);
      Split(Time_Of(0, TS1*1000), SC2, TS2);
      Split(Time_Of(0, TS2*1000), SC3, TS3);
      -- NOTE: it is safe to multiply by 1000 because RM95 D.8(31)
      -- guarantees that Time_Span'Last is >= 3600 seconds.

      -- Finally do the conversion of the remaining time-span to duration
      -- and add to other pieces.
      return (Float(SC1)*1.0E9 + Float(SC2)*1.0E6 + Float(SC3)*1.0E3 + Float(To_Duration(TS3*1000))) / 1.0E9;
   end To_Float;

   task body Monitor is
      Receiver              : GNAT.Sockets.Socket_Type;
      Connection            : GNAT.Sockets.Socket_Type;
      Client                : GNAT.Sockets.Sock_Addr_Type;
      Channel               : GNAT.Sockets.Stream_Access;
      Radix                 : Number_Base_T := Octal;
      CPU_Stats             : Processor.CPU_Monitor_Rec;
      DPF_Stats             : Devices.Disk6061.Status_Rec;
      MTB_Stats             : Devices.Magtape6026.Status_Rec;
      Now, 
      Last_CPU_Time,
      Last_DPF_Time         : Time;
      CPU_Elapsed,
      DPF_Elapsed           : Time_Span;
      I_Count, Last_I_Count : Unsigned_64 := 0;
      MIPS                  : Float;
      MIPS_I                : Natural;
      DPF_IO_Count, Last_DPF_IO_Count : Unsigned_64 := 0;
      DPF_IOPS              : Float;
      DPF_IOPS_I            : Natural;
   begin
      loop
         select
            accept Start (Port : in GNAT.Sockets.Port_Type) do
               GNAT.Sockets.Create_Socket (Socket => Receiver);
               GNAT.Sockets.Set_Socket_Option
                 (Socket => Receiver, Level => GNAT.Sockets.Socket_Level,
                  Option =>
                    (Name => GNAT.Sockets.Reuse_Address, Enabled => True));
               GNAT.Sockets.Bind_Socket
                 (Socket  => Receiver,
                  Address =>
                    (Family => GNAT.Sockets.Family_Inet,
                     Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
                     Port   => Port));
               GNAT.Sockets.Listen_Socket (Socket => Receiver);
            end Start;
            GNAT.Sockets.Accept_Socket
              (Server => Receiver, Socket => Connection, Address => Client);
            Ada.Text_IO.Put_Line
              ("INFO: Status Monitor connected from " & GNAT.Sockets.Image (Client));
            Channel := GNAT.Sockets.Stream (Connection);
            String'Write
              (Channel,
               Dasher_Erase_Page & "                             " &
               Dasher_Underline & "MV/Emua Status" & Dasher_Normal &
               Dasher_NL);
         or
            accept CPU_Update (Stats : in Processor.CPU_Monitor_Rec) do
               CPU_Stats := Stats;
            end CPU_Update;
            Now := Clock;
            I_Count      := CPU_Stats.Instruction_Count - Last_I_Count;
            Last_I_Count := CPU_Stats.Instruction_Count;
            CPU_Elapsed := Now - Last_CPU_Time;
            MIPS := Float (I_Count) / To_Float(CPU_Elapsed);
            MIPS_I := Natural(MIPS) / 1000;
            Last_CPU_Time := Now;
            String'Write
               (Channel,
               Dasher_Write_Window_Addr & Character'Val (0) &
               Character'Val (CPU_Row_1) & Dasher_Erase_EOL);
            String'Write (Channel, "PC:  " & Dword_To_String (Dword_T(CPU_Stats.PC), Radix, 11, true) & 
                                    "  Interrupts: " & Boolean_To_YN (CPU_Stats.ION) &
                                    "     ATU: " & Boolean_To_YN (CPU_Stats.ATU) &
                                    "            KIPS: " & MIPS_I'Image );
            String'Write
               (Channel,
               Dasher_Write_Window_Addr & Character'Val (0) &
               Character'Val (CPU_Row_2) & Dasher_Erase_EOL);
            String'Write (Channel, "AC0: " &  Dword_To_String (CPU_Stats.AC(0), Radix, 11, true) &
                                    "  AC1: " &  Dword_To_String (CPU_Stats.AC(1), Radix, 11, true) &
                                    "  AC2: " &  Dword_To_String (CPU_Stats.AC(2), Radix, 11, true) &
                                    "  AC3: " &  Dword_To_String (CPU_Stats.AC(3), Radix, 11, true));
         or
            accept DPF_Update (Stats : in Devices.Disk6061.Status_Rec) do
               DPF_Stats := Stats;
            end DPF_Update;    
            Now := Clock;
            DPF_IO_Count := DPF_Stats.Reads + DPF_Stats.Writes - Last_DPF_IO_Count;
            Last_DPF_IO_Count := DPF_Stats.Reads + DPF_Stats.Writes;
            DPF_Elapsed := Now - Last_DPF_Time;
            DPF_IOPS := Float(DPF_IO_Count) / To_Float(DPF_Elapsed);
            DPF_IOPS_I := Natural(DPF_IOPS) / 1000;
            Last_DPF_Time := Now;
            String'Write
               (Channel,
               Dasher_Write_Window_Addr & Character'Val (0) &
               Character'Val (DPF_Row_1) & Dasher_Erase_EOL);
            String'Write
               (Channel,
               "DPF:  (DPF0) - Attached: " & Boolean_To_YN (DPF_Stats.Image_Attached) &
               "  Cyl: " & DPF_Stats.Cylinder'Image & 
               "  Surf: " & DPF_Stats.Surface'Image &
               "  Sect: " & DPF_Stats.Sector'Image &
               "  KIOPS: " & DPF_IOPS_I'Image);   
         or
            accept MTB_Update (Stats : in Devices.Magtape6026.Status_Rec) do
               MTB_Stats := Stats;
            end MTB_Update;               
            String'Write
               (Channel,
               Dasher_Write_Window_Addr & Character'Val (0) &
               Character'Val (MTB_Row_1) & Dasher_Erase_EOL);
            String'Write
               (Channel,
               "MTA:  (MTC0) - Attached: " & Boolean_To_YN (MTB_Stats.Image_Attached(0)) &
               "  Mem Addr: " & Dword_To_String (Dword_T(MTB_Stats.Mem_Addr_Reg), Radix, 12, true) & 
               "  Curr Cmd: " & MTB_Stats.Current_Cmd'Image);
            String'Write
               (Channel,
               Dasher_Write_Window_Addr & Character'Val (0) &
               Character'Val (MTB_Row_2) & Dasher_Erase_EOL);
            String'Write
               (Channel,
               "               Image file: " & To_String(MTB_Stats.Image_Filename(0)));

         or
            accept Stop do
               GNAT.Sockets.Close_Socket (Connection);
            end Stop;
         or
            terminate;
         end select;
      end loop;

   end Monitor;

end Status_Monitor;
