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

with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.Sockets;
with GNAT.String_Split;

with CPU;
with CPU.Decoder;
with Debug_Logs;
with Devices;
with Devices.Bus;
with Devices.Console;
with DG_Types; use DG_Types;
with Memory;
with Status_Monitor;



procedure MVEmuA is

   Sem_Ver : constant String := "v0.0.0";

   Debug_Logging : constant Boolean := true;
   Log_Dir       : constant Unbounded_String := To_Unbounded_String ("logs/");
    

   Console_Port : constant GNAT.Sockets.Port_Type := 10_000;
   Monitor_Port : constant GNAT.Sockets.Port_Type := 10_001;

   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;
   Channel    : GNAT.Sockets.Stream_Access;

   Command_Line : Unbounded_String;
   Command      : Unbounded_String;
   One_Char     : Character;

   procedure Show_Help is
   begin
      Devices.Console.TTOut.Put_String (Dasher_Erase_Page & "                          " &
         Dasher_Underline &"SCP-CLI Commands" & Dasher_Normal &
         "                          " & Dasher_Dim_On & "MV/Emulator" & Dasher_Dim_Off & Dasher_NL &
         " .                      - Display state of CPU" & Dasher_NL &
         " B #                    - Boot from device #" & Dasher_NL &
         " CO                     - COntinue CPU Processing" & Dasher_NL &
         " E A <#> | M [addr] | P - Examine/Modify Acc/Memory/PC" & Dasher_NL &
         " HE                     - HElp (show this)" & Dasher_NL &
         " RE                     - REset the system" & Dasher_NL &
         " SS                     - Single Step one instruction" & Dasher_NL &
         " ST <addr>              - STart processing at specified address" & Dasher_NL);
      Devices.Console.TTOut.Put_String ("                          " & Dasher_Underline &
         "Emulator Commands" &Dasher_Normal & Dasher_NL &
         " ATT <dev> <file> [RW]  - ATTach the image file to named device (RO)" & Dasher_NL &
         " BREAK/NOBREAK <addr>   - Set or clear a BREAKpoint" & Dasher_NL &
         " CHECK                  - CHECK validity of attached TAPE image" & Dasher_NL &
         " CREATE DPF|DSKP <file> - CREATE an empty/unformatted disk image" & Dasher_NL &
         " DET <dev>              - DETach any image file from the device" & Dasher_NL &
         " DIS <from> <to>|+<#>   - DISassemble physical memory range or # from PC" & Dasher_NL &
         " DO <file>              - DO (i.e. run) emulator commands from script <file>" & Dasher_NL &
         " EXIT                   - EXIT the emulator" & Dasher_NL &
         " LOAD <file>            - Load ASCII octal file directly into memory" & Dasher_NL &
         " SET LOGGING ON|OFF     - Turn on or off debug logging (logs dumped end of run)" & Dasher_NL &
         " SHOW BREAK/DEV/LOGGING - SHOW list of BREAKpoints/DEVices configured" & Dasher_NL);
   end Show_Help;

   procedure Clean_Exit is
   begin
      Devices.Console.TTOut.Put_String (Dasher_NL & " *** MV/Emulator stopping at user request ***" );
      Debug_Logs.Loggers.Debug_Logs_Dump (Log_Dir);
      GNAT.OS_Lib.OS_Exit (0);
   end Clean_Exit;

   procedure Do_Command (Cmd : in Unbounded_String) is
      Words : GNAT.String_Split.Slice_Set;
   begin
      GNAT.String_Split.Create (Words, To_String(Cmd), " ", GNAT.String_Split.Multiple);
      Command := To_Unbounded_String(GNAT.String_Split.Slice (Words, 1));
      if Command = "HE" then
         Show_Help;
      elsif Command = "exit" or Command = "EXIT" or command = "quit" or command = "QUIT" then
         Clean_Exit;
      else
         Devices.Console.TTOut.Put_String (Dasher_NL & " *** Unknown SCP-CLI Command ***");
      end if;
   end Do_Command;

begin
   GNAT.Sockets.Create_Socket (Socket => Receiver);
   GNAT.Sockets.Set_Socket_Option
     (Socket => Receiver, Level => GNAT.Sockets.Socket_Level,
      Option => (Name => GNAT.Sockets.Reuse_Address, Enabled => True));
   GNAT.Sockets.Bind_Socket
     (Socket  => Receiver,
      Address =>
        (Family => GNAT.Sockets.Family_Inet,
         Addr => GNAT.Sockets.Inet_Addr ("127.0.0.1"), Port => Console_Port));
   GNAT.Sockets.Listen_Socket (Socket => Receiver);
   loop
      GNAT.Sockets.Accept_Socket
        (Server => Receiver, Socket => Connection, Address => Client);
      Ada.Text_IO.Put_Line
        ("INFO: Console connected from " & GNAT.Sockets.Image (Client));
      Channel := GNAT.Sockets.Stream (Connection);

		-- The console is connected, now we can set up our emulated machine
		-- Here we are defining the hardware in our virtual machine
		-- Initially based on a minimally configured MV/10000 Model I...
		--   One CPU
		--   Console (TTI/TTO)
		--   One Tape Drive
		--   One HDD
		--   A generous(!) 16MB (8MW) RAM
		--   NO IACs, LPT or ISC
      Status_Monitor.Monitor.Start (Monitor_Port);
      Memory.RAM.Init (Debug_Logging);
      Devices.Bus.Actions.Init;
      Devices.Bus.Actions.Attach (Devices.BMC);
      Devices.Bus.Actions.Set_Reset_Proc (Devices.BMC, Memory.BMC_DCH.Reset'Access);
      Devices.Bus.Actions.Attach (Devices.SCP);
      Devices.Bus.Actions.Attach (Devices.CPU);
      CPU.Actions.Init;

      Devices.Bus.Actions.Attach (Devices.TTO);
      Devices.Console.TTOut.Init (Channel);
      Devices.Bus.Actions.Attach (Devices.TTI);
      Devices.Console.TTIn.Init;

      -- TODO - More devices...

      -- say hello...
      Devices.Console.TTOut.Put_Char (ASCII.FF);
      Devices.Console.TTOut.Put_String (" *** Welcome to the MV/Emulator - Type HE for help ***" & ASCII.LF);

      -- TODO - handle DO scripts

      -- the main SCP/console interaction loop
      CPU.Actions.Set_SCP_IO (true);
      loop
         Devices.Console.TTOut.Put_String (Dasher_NL & "SCP-CLI> ");
         -- get one line of input from the console - handle DASHER DELete key as corrector
         Command_Line := Null_Unbounded_String;
         loop
            One_Char := Character'Input (Channel);
            exit when One_Char = ASCII.CR;
            if One_Char = Dasher_Delete and length (Command_Line) > 0 then
               Devices.Console.TTOut.Put_Char (Dasher_Cursor_Left);
               Command_Line := Head(Command_Line, length (Command_Line) - 1);
            else
               Devices.Console.TTOut.Put_Char (One_Char);
               Command_Line := Command_Line & One_Char;
            end if;
         end loop;
         Ada.Text_IO.Put_Line ("DEBUG: Got SCP command: " & To_String(Command_Line));
         Do_Command (Command_Line);
      end loop;

   end loop;

end MVEmuA;
