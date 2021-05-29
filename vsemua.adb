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

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Ctrl_C;
with GNAT.OS_Lib;
with GNAT.Sockets;

with AOSVS.Agent;

with Debug_Logs;   use Debug_Logs;
with Decoder;
with DG_Types;     use DG_Types;
with Memory;       use Memory;
with Processor;

procedure VSEmua is

   Sem_Ver : constant String := "v0.0.0";

   Debug_Logging : Boolean := TRUE;
   Log_Dir       : constant String  := "logs/";
   Console_Port  : constant GNAT.Sockets.Port_Type := 10_000;

   Command_Line  : Unbounded_String;
   Command       : Unbounded_String;
   Arg_Num       : Positive := 1;
   PR_Arg_Num, 
   Root_Arg_Num,
   Args_Arg_Num  : Natural := 0;
   VS_Args_Arr   : AOSVS.Args_Arr;
   VS_Num_Args   : Positive;
   

   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;
   Con_Stream : GNAT.Sockets.Stream_Access;

   procedure Put_String(Con : in GNAT.Sockets.Stream_Access; S : in String) is
   begin
      for C of S loop
         Character'Output(Con, C);
      end loop;
   end Put_String;

   procedure Clean_Exit is
   begin
      -- TTOut.Put_String (Dasher_NL & " *** VS/Emulator stopping ***" );
      Loggers.Debug_Logs_Dump (Log_Dir);
      -- Dump_Memory_Readable ("mvemua.dmp");
      GNAT.OS_Lib.OS_Exit (0);
   end Clean_Exit;

begin
   while Arg_Num <= Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (Arg_num) = "-pr" then
         Arg_Num := Arg_Num + 1;
         PR_Arg_Num := Arg_Num;
      elsif Ada.Command_Line.Argument (Arg_num) = "-root" then
         Arg_Num := Arg_Num + 1;
         Root_Arg_Num := Arg_Num;  
      elsif Ada.Command_Line.Argument (Arg_num) = "-args" then
         Arg_Num := Arg_Num + 1;
         Args_Arg_Num := Arg_Num;     
      elsif Ada.Command_Line.Argument (Arg_num) = "-version" then
         Ada.Text_IO.Put_Line ("VS/Emua version " & Sem_Ver);
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      Arg_Num := Arg_Num + 1;
   end loop;

   if (PR_Arg_Num = 0) or (Root_Arg_Num = 0) then
      Ada.Text_IO.Put_Line ("ERROR: Please supply a PR file to run and a root directory");
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   VS_Args_Arr(0) := To_Unbounded_String(Ada.Command_Line.Argument (PR_Arg_num));
   VS_Num_Args := 1;

   if Args_Arg_Num > 0 then
      if Ada.Command_Line.Argument (Args_Arg_num)(1) = '"' then
         raise Not_Yet_Implemented with "Multiple program arguments";
      end if;
      -- we have a single argument...
      VS_Args_Arr(1) := To_Unbounded_String(Ada.Command_Line.Argument (Args_Arg_num));
      VS_Num_Args := 2;
      Ada.Text_IO.Put_Line ("INFO: Program argument: " & To_String(VS_Args_Arr(1)));
   end if;

   GNAT.Ctrl_C.Install_Handler(Clean_Exit'Unrestricted_Access);

   Ada.Text_IO.Put_Line ("INFO: Will not start until console connects...");
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
   Loggers.Init;
   GNAT.Sockets.Accept_Socket (Server => Receiver, Socket => Connection, Address => Client);
   Con_Stream := GNAT.Sockets.Stream (Connection);
   Put_String (Con_Stream, "Welcome to the VS/Emua AOS/VS Emulator" & Dasher_NL);
   Ada.Text_IO.Put_Line ("INFO: Console connected from " & GNAT.Sockets.Image (Client));

   Decoder.Init;
   RAM.Init (Debug_Logging);

   AOSVS.Agent.Actions.Init(Con_Stream);

   AOSVS.Start (PR_Name   => Ada.Command_Line.Argument (PR_Arg_num),
                Virt_Root => Ada.Command_Line.Argument (Root_Arg_num),
                Segment   => 7,
                Arg_Count => VS_Num_Args,
                Args      => VS_Args_Arr,
                Console   => Con_Stream,
                Logging   => Debug_Logging);

   Clean_Exit;

   -- exception
   --    when others =>
   --       Clean_Exit;

end VSEmua;
