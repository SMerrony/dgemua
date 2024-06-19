-- Copyright ©2021,2022 Stephen Merrony
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.Ctrl_C;
with GNAT.OS_Lib;
with GNAT.Sockets;

with AOSVS.Agent;

with Debug_Logs;   use Debug_Logs;
with Decoder;
with DG_Types;     use DG_Types;
with Memory;       use Memory;

procedure VSEmua is

   Sem_Ver : constant String := "v0.0.0";

   Debug_Logging : Boolean := True; 
   Log_Dir       : constant String  := "logs/";
   Console_Port  : constant GNAT.Sockets.Port_Type := 10_000;

   Arg_Num       : Positive := 1;
   PR_Arg_Num, 
   Root_Arg_Num,
   Dir_Arg_Num,
   Args_Arg_Num  : Natural := 0;
   VS_Args_Arr   : AOSVS.Args_Arr;
   VS_Num_Args   : Positive;
   

   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;
   Con_Stream : GNAT.Sockets.Stream_Access;

   procedure Put_String(Con : GNAT.Sockets.Stream_Access; S : String) is
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
      -- Put_Line ("DEBUG: Arg_Num:" & Arg_Num'Image & ", Arg: >>>" & Ada.Command_Line.Argument (Arg_num) & "<<<");
      if Ada.Command_Line.Argument (Arg_num) = "-pr" then
         Arg_Num := Arg_Num + 1;
         PR_Arg_Num := Arg_Num;
      elsif Ada.Command_Line.Argument (Arg_num) = "-root" then
         Arg_Num := Arg_Num + 1;
         Root_Arg_Num := Arg_Num;  
      elsif Ada.Command_Line.Argument (Arg_num) = "-dir" then
         Arg_Num := Arg_Num + 1;
         Dir_Arg_Num := Arg_Num;       
      elsif Ada.Command_Line.Argument (Arg_num) = "-args" then
         Arg_Num := Arg_Num + 1;
         Args_Arg_Num := Arg_Num; 
      elsif Ada.Command_Line.Argument (Arg_num) = "-fast" then
         Debug_Logging := False;
      elsif Ada.Command_Line.Argument (Arg_num) = "-version" then
         Ada.Text_IO.Put_Line ("VS/Emua version " & Sem_Ver);
         GNAT.OS_Lib.OS_Exit (0);
      elsif Ada.Command_Line.Argument (Arg_num) = "-h" or Ada.Command_Line.Argument (Arg_num) = "-help" then   
         Put_Line ("Usage: vsemua [-h|-help|-version] | -pr <pr_name> -root <virtual root dir> -dir <AOS/VS working dir> [-args ...] ");
         Put_Line ("Where -h or -help prints this help");
         Put_Line ("      -version    prints the version number and exits");
         Put_Line ("      -pr         specifies the AOS/VS .PR file name");
         Put_Line ("      -root       the local location of the virtual AOS/VS filesystem root");
         Put_Line ("      -dir        the AOS/VS working directory");
         Put_Line ("      -fast       deactivates detailled logging");
         Put_Line ("      -args       any arguments to pass to the program, surrounded with double-quotes");
         Put_Line ("Eg. ./vsemua -pr SPIGOT.PR -root /home/steve/Ada/dgemua/FILESYSTEM -dir :SAMPLES");
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      Arg_Num := Arg_Num + 1;
   end loop;

   if (PR_Arg_Num = 0) or (Root_Arg_Num = 0) or (Dir_Arg_Num = 0) then
      Put_Line ("ERROR: You must specify at least -pr, -root, and -dir");
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   VS_Args_Arr(0) := To_Unbounded_String(Ada.Command_Line.Argument (PR_Arg_num));
   VS_Num_Args := 1;

   if Args_Arg_Num > 0 then
      -- we should have a double-quote enclosed list of AOS/VS arguments or SWITCHES
      -- which are separated by spaces.
      -- N.B. Linux/GNAT/bash removes the double-quotes.

      --VS_Num_Args := 2;
      for C of Ada.Command_Line.Argument (Args_Arg_num) loop
         if C /= ' ' then
            VS_Args_Arr(VS_Num_Args) := VS_Args_Arr(VS_Num_Args) & C;
         else
            Put_Line("DEBUG: VS Arg: >>>" & To_String(VS_Args_Arr(VS_Num_Args)) & "<<<");
            VS_Num_Args := VS_Num_Args + 1;
         end if;
      end loop;
      Put_Line("DEBUG: VS Arg: >>>" & To_String(VS_Args_Arr(VS_Num_Args)) & "<<<");
      VS_Num_Args := VS_Num_Args + 1;

   end if;

   GNAT.Ctrl_C.Install_Handler(Clean_Exit'Unrestricted_Access);

   Put_Line ("INFO: Will not start until console connects...");
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
   Put_String (Con_Stream, Dasher_NL & "Welcome to the VS/Emua AOS/VS Emulator" & Dasher_NL);
   Put_Line ("INFO: Console connected from " & GNAT.Sockets.Image (Client));

   Decoder.Init;
   RAM.Init (Debug_Logging);

   AOSVS.Agent.Actions.Init(Con_Stream, Ada.Command_Line.Argument (Root_Arg_num));
   Put_Line ("INFO: Psuedo-AGENT initialised");

   AOSVS.Start (PR_Name   => Ada.Command_Line.Argument (PR_Arg_num),
                Dir       => Ada.Command_Line.Argument (Dir_Arg_num),
                Segment   => 7,
                Arg_Count => VS_Num_Args,
                Args      => VS_Args_Arr,
                Console   => Con_Stream,
                Logging   => Debug_Logging);

   Clean_Exit;

   exception
      when others =>
         Clean_Exit;

end VSEmua;
