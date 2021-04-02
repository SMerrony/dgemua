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

with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Text_IO;

with Interfaces; use Interfaces;

with CPU;
with DG_Types; use DG_Types;
with Memory;   use Memory;

package body Status_Monitor is

   task body Monitor is
      Receiver              : GNAT.Sockets.Socket_Type;
      Connection            : GNAT.Sockets.Socket_Type;
      Client                : GNAT.Sockets.Sock_Addr_Type;
      Channel               : GNAT.Sockets.Stream_Access;
      Radix                 : Number_Base_T := Octal;
      Last_CPU_Time         : Ada.Calendar.Time;
      I_Count, Last_I_Count : Unsigned_64 := 0;
      MIPS                  : Float;
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
            accept CPU_Update (Stats : in CPU.CPU_Monitor_Rec) do
               I_Count      := Stats.Instruction_Count - Last_I_Count;
               Last_I_Count := Stats.Instruction_Count;
      -- // ips = float64(iCount) / (time.Since(lastCPUtime).Seconds() * 1000)
               MIPS := Float (I_Count);
               -- // lastCPUtime = time.Now()
               String'Write
                 (Channel,
                  Dasher_Write_Window_Addr & Character'Val (0) &
                  Character'Val (CPU_Row_1) & Dasher_Erase_EOL);
               -- "PC:  %011o   Interrupts: %s    ATU: %s     IPS: %.fk/sec"
               String'Write (Channel, "PC:  " & Memory.Dword_To_String (Dword_T(Stats.PC), Radix, 12, true) & 
                                      "  Interrupts: " & Memory.Boolean_To_YN (Stats.ION) &
                                      "      ATU: " & Memory.Boolean_To_YN (Stats.ATU) &
                                      "             MIPS: "  );
               String'Write
                 (Channel,
                  Dasher_Write_Window_Addr & Character'Val (0) &
                  Character'Val (CPU_Row_2) & Dasher_Erase_EOL);
               String'Write (Channel, "AC0: " &  Memory.Dword_To_String (Stats.AC(0), Radix, 12, true) &
                                      "  AC1: " &  Memory.Dword_To_String (Stats.AC(1), Radix, 12, true) &
                                      "  AC2: " &  Memory.Dword_To_String (Stats.AC(2), Radix, 12, true) &
                                      "  AC3: " &  Memory.Dword_To_String (Stats.AC(3), Radix, 12, true));
            end CPU_Update;
         or
            accept MTB_Update (Stats : in Devices.Magtape6026.Status_Rec) do
               String'Write
                 (Channel,
                  Dasher_Write_Window_Addr & Character'Val (0) &
                  Character'Val (MTB_Row_1) & Dasher_Erase_EOL);
               String'Write
                 (Channel,
                  "MTA:  (MTC0) - Attached: " & Memory.Boolean_To_YN (Stats.Image_Attached(0)) &
                  "  Mem Addr: " & Memory.Dword_To_String (Dword_T(Stats.Mem_Addr_Reg), Radix, 12, true) & 
                  "  Curr Cmd: " & Integer'Image(Stats.Current_Cmd));
               String'Write
                 (Channel,
                  Dasher_Write_Window_Addr & Character'Val (0) &
                  Character'Val (MTB_Row_2) & Dasher_Erase_EOL);
               String'Write
                 (Channel,
                  "               Image file: " & To_String(Stats.Image_Filename(0)));
            end MTB_Update;
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
