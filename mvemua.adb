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
with GNAT.Sockets;

with CPU;
with CPU.Decoder;
with Debug_Logs;
with Devices;
with Devices.Bus;
with Devices.Console;
with Memory;


procedure MVEmuA is

   Sem_Ver : constant String := "v0.0.0";

   Debug_Logging : constant Boolean := true;

    

   Console_Port : constant GNAT.Sockets.Port_Type := 10_000;

   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;
   Channel    : GNAT.Sockets.Stream_Access;

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

      begin
         loop
            Character'Output (Channel, Character'Input (Channel));
         end loop;
      exception
         when others =>
            GNAT.Sockets.Close_Socket (Connection);
            return;
      end;
      -- GNAT.Sockets.Close_Socket (Connection);
   end loop;
end MVEmuA;
