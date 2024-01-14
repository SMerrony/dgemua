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
with Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; 
with Ada.Text_IO;

with GNAT.Ctrl_C;
with GNAT.OS_Lib;
with GNAT.Sockets;
with GNAT.String_Split;     use GNAT.String_Split;

with Interfaces;            use Interfaces;

with Decoder;
with Debug_Logs;            use Debug_Logs;
with Devices;
with Devices.Bus;
with Devices.Console;       use Devices.Console;
with Devices.DRP_SCP;
with Devices.Disk6061;
with Devices.Disk6239;
with Devices.Magtape6026;
with Devices.PIT_P;
with Devices.PSC_UPSC;
with DG_Types;              use DG_Types;
with Processor;
with Memory;                use Memory;
with Memory_Channels;       use Memory_Channels;
with Simh_Tapes;
with Status_Monitor;

procedure MVEmuA is

   Sem_Ver : constant String := "v0.0.0";

   Debug_Logging : Boolean := TRUE;
   Log_Dir       : constant String  := "logs/";
    

   Console_Port : constant GNAT.Sockets.Port_Type := 10_000;
   Monitor_Port : constant GNAT.Sockets.Port_Type := 10_001;

   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;

   Command_Line  : Unbounded_String;
   Command       : Unbounded_String;
   Arg_Num       : Positive := 1;
   Do_Script_Arg : Natural := 0;

   CPU : Processor.CPU_T;

   Breakpoints : Processor.BP_Sets.Set;

   Console_Radix  : Number_Base_T := Octal; -- default console I/O number base

   procedure Do_Command (Cmd : Unbounded_String);

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
         " CHECK <unit>|<file>    - CHECK validity of TAPE image" & Dasher_NL &
         " CREATE DPF|DSKP <file> - CREATE an empty/unformatted disk image" & Dasher_NL &
         " DET <dev>              - DETach any image file from the device" & Dasher_NL &
         " DIS <from> <to>|+<#>   - DISassemble physical memory range or # from PC" & Dasher_NL &
         " DO <file>              - DO (i.e. run) emulator commands from script <file>" & Dasher_NL &
         " EXIT | BYE             - EXIT the emulator" & Dasher_NL &
         " LOAD <file>            - Load ASCII octal file directly into memory" & Dasher_NL &
         " SET LOGGING ON|OFF     - Turn on or off debug logging (logs dumped end of run)" & Dasher_NL &
         " SHOW BREAK/DEV/LOGGING - SHOW list of BREAKpoints/DEVices configured" & Dasher_NL);
   end Show_Help;

   procedure Attach (Command : Slice_Set) is
   begin
      if Slice_Count (Command) < 3 then
         TTOut.Put_String (Dasher_NL & " *** ATT command requires arguments: <dev> and <image> ***");
         return;
      end if;
      declare
         Dev : String := Slice (Command, 2);
         Image_Name : String := Slice (Command, 3);
         OK : Boolean;
      begin
         if Debug_Logging then
            Loggers.Debug_Print (Debug_Log, "INFO: Attach called with parms " & Dev & " " & Image_Name);
         end if;  
         if Dev = "MTB" then
            Devices.Magtape6026.Drives.Attach (0, Image_Name, OK);
            if OK then
               TTOut.Put_String (Dasher_NL & " *** Tape Image Attached ***");
            else
               TTOut.Put_String (Dasher_NL & " *** Could not attach Tape Image ***");
            end if;
         elsif Dev = "DPF" then
            Devices.Disk6061.Drives.Attach (0, Image_Name, OK);
            if OK then
               TTOut.Put_String (Dasher_NL & " *** DPF Disk Image Attached ***");
            else
               TTOut.Put_String (Dasher_NL & " *** Could not attach DPF Disk Image ***");
            end if;
         elsif Dev = "DSKP" then
            Devices.Disk6239.CB_Processor.Attach (0, Image_Name, OK);
            if OK then
               TTOut.Put_String (Dasher_NL & " *** DSKP Disk Image Attached ***");
            else
               TTOut.Put_String (Dasher_NL & " *** Could not attach DSKP Disk Image ***");
            end if;   
         else
               TTOut.Put_String (Dasher_NL & " *** Unknown or unimplemented Device for ATT command ***");
         end if;
      end;
   end Attach;

   procedure Detach (Command : Slice_Set) is
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** DET command requires argument: <dev> ***");
         return;
      end if;
      declare
         Dev : String := Slice (Command, 2);
         --  OK : Boolean;
      begin
         if Debug_Logging then
            Loggers.Debug_Print (Debug_Log, "INFO: Detach called with parms " & Dev);
         end if;  
         if Dev = "MTB" then
            Devices.Magtape6026.Drives.Detach (0);
            TTOut.Put_String (Dasher_NL & " *** Tape Image Detached ***");
         --  elsif Dev = "DPF" then
         --     Devices.Disk6061.Drives.Attach (0, Image_Name, OK);
         --     if OK then
         --        TTOut.Put_String (Dasher_NL & " *** DPF Disk Image Attached ***");
         --     else
         --        TTOut.Put_String (Dasher_NL & " *** Could not attach DPF Disk Image ***");
         --     end if;
         --  elsif Dev = "DSKP" then
         --     Devices.Disk6239.CB_Processor.Attach (0, Image_Name, OK);
         --     if OK then
         --        TTOut.Put_String (Dasher_NL & " *** DSKP Disk Image Attached ***");
         --     else
         --        TTOut.Put_String (Dasher_NL & " *** Could not attach DSKP Disk Image ***");
         --     end if;   
         else
               TTOut.Put_String (Dasher_NL & " *** Unknown or unimplemented Device for ATT command ***");
         end if;
      end;
   end Detach;

   procedure Boot (Command : Slice_Set) is
      Dev : Dev_Num_T;
   begin
     if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** B command requires argument: <devicenumber> ***");
         return;
      end if;
      Dev := Dev_Num_T(String_To_Integer(Slice (Command, 2), Console_Radix));
      if not Devices.Bus.Actions.Is_Attached(Dev) then
         TTOut.Put_String (Dasher_NL & " *** Devices is not ATTached ***");
         return;
      end if;
      if not Devices.Bus.Actions.Is_Bootable(Dev) then
         TTOut.Put_String (Dasher_NL & " *** Devices is not Bootable ***");
         return;
      end if; 
      RAM.Init (Debug_Logging);  
      case Dev is
         when Devices.MTB =>
            Devices.Magtape6026.Drives.Load_TBOOT;
            Processor.Boot (CPU, Devices.MTB, 8#10#);
         when Devices.DPF =>
            Devices.Disk6061.Drives.Load_DKBT;
            Processor.Boot (CPU, Devices.DPF, 8#10#);
         when Devices.DSKP =>
            Devices.Disk6239.CB_Processor.Program_Load;
            Processor.Boot (CPU, Devices.DSKP, 8#10#);
         when others =>
            TTOut.Put_String (Dasher_NL & " *** Booting from that device is not yet implemented ***");
      end case;   
      exception
         when Constraint_Error =>
            TTOut.Put_String (Dasher_NL & " *** Invalid Device Number ***");
   end Boot;

   procedure Break_Clear (Command : Slice_Set) is
      BP_Addr : Phys_Addr_T;
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** NOBREAK command requires an address argument ***");
         return;
      end if;
      BP_Addr  := Phys_Addr_T(String_To_Dword (Slice (Command, 2), Console_Radix));
      Breakpoints.Exclude (BP_Addr);
      TTOut.Put_String (Dasher_NL & " *** BREAKpoint cleared ***");
   end Break_Clear;

   procedure Break_Set (Command : Slice_Set) is
      BP_Addr : Phys_Addr_T;
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** BREAK command requires an address argument ***");
         return;
      end if;
      BP_Addr  := Phys_Addr_T(String_To_Dword (Slice (Command, 2), Console_Radix));
      Breakpoints.Include (BP_Addr);
      TTOut.Put_String (Dasher_NL & " *** BREAKpoint set ***");
   end Break_Set;

   procedure Check (Command : Slice_Set) is
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** CHECK command requires argument: <tape_image> or <unit> ***");
         return;
      end if;
      declare
         Arg : String := Slice (Command, 2);
      begin
         if Arg'Length > 2 then
            TTOut.Put_String (Simh_Tapes.Scan_Image (Slice (Command, 2)));
         else
            declare
               Tape_Image : String := Devices.Magtape6026.Drives.Get_Image_Name(Integer'Value (Arg));
               OK : boolean;
            begin
               Devices.Magtape6026.Drives.Detach (Integer'Value (Arg));
               TTOut.Put_String (Simh_Tapes.Scan_Image (Tape_Image));
               Devices.Magtape6026.Drives.Attach (Integer'Value (Arg), Tape_Image, OK);
            end;
         end if;
      end;
   end Check;

   procedure Dump_Memory_Readable ( Filename : String) is
      use Ada.Streams.Stream_IO;
      Write_File : File_Type;
      Writer : Stream_Access;
      Addr : Phys_Addr_T := 0;
   begin
      Create (Write_File, Out_File, Filename);
      Writer := Stream(Write_File);
      while Addr < Phys_Addr_T(Mem_Size_Words) loop
         String'Write (Writer, Dword_To_String(Dword_T(Addr), Octal, 12, false) & " " &
            Dword_To_String(Dword_T(RAM.Read_Word(Addr)), Hex, 4, true) & Dasher_NL);
         Addr := Addr + 1;
      end loop;
   end Dump_Memory_Readable;

   procedure Clean_Exit is
   begin
      TTOut.Put_String (Dasher_NL & " *** MV/Emulator stopping at user request ***" );
      Debug_Logs.Loggers.Debug_Logs_Dump (Log_Dir);
      Dump_Memory_Readable ("mvemua.dmp");
      GNAT.OS_Lib.OS_Exit (0);
   end Clean_Exit;

   procedure Create_Blank (Command : Slice_Set) is
      OK : Boolean;
   begin
      if Slice_Count (Command) < 3 then
         TTOut.Put_String (Dasher_NL & " *** CREATE command requires arguments: DPF and <filename> ***");
         return;
      end if;
      if Slice (Command, 2) = "DPF" then
         TTOut.Put_String (Dasher_NL & "Attempting to CREATE new empty DPF-type disk image, please wait...");
         Devices.Disk6061.Create_Blank(Slice(Command,3), OK);
         if OK then
            TTOut.Put_String (Dasher_NL & "Empty MV/Emua DPF-type disk image created");
         else 
            TTOut.Put_String (Dasher_NL & " *** Error: could not create empty disk image ***");
         end if;
      end if;
      if Slice (Command, 2) = "DSKP" then
         TTOut.Put_String (Dasher_NL & "Attempting to CREATE new empty DSKP-type disk image, please wait...");
         Devices.Disk6239.Create_Blank(Slice(Command,3), OK);
         if OK then
            TTOut.Put_String (Dasher_NL & "Empty MV/Emua DSKP-type disk image created");
         else 
            TTOut.Put_String (Dasher_NL & " *** Error: could not create empty disk image ***");
         end if;
      end if;
   end Create_Blank;

   procedure Disassemble (Command : Slice_Set) is
      Low_Addr, High_Addr : Phys_Addr_T;
   begin
      if Slice_Count (Command) < 3 then
         TTOut.Put_String (Dasher_NL & " *** DIS command requires two address arguments ***");
         return;
      end if;
      Low_Addr  := Phys_Addr_T(String_To_Dword (Slice (Command, 2), Console_Radix));
      High_Addr := Phys_Addr_T(String_To_Dword (Slice (Command, 3), Console_Radix));
      TTOut.Put_String (Processor.Disassemble_Range(Low_Addr, High_Addr, Console_Radix));
   end Disassemble;

   procedure Do_Script (Command : Slice_Set) is
      use Ada.Text_IO;
      Do_File : File_Type;
      Script_Line : Unbounded_String;
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** DO command requires argument: <script_file> ***");
         return;
      end if;
      Open (Do_File, In_File, Slice (Command, 2));
      while not End_Of_File (Do_File) loop
         Script_Line := To_Unbounded_String (Get_Line (Do_File));
         if Ada.Strings.Unbounded.Element(Script_Line, 1) /= '#' then
            TTOut.Put_String (Dasher_NL & To_String (Script_Line));
            Do_Command (Script_Line);
         end if;
      end loop;
      Close (Do_File);
   exception
      when Name_Error =>
         TTOut.Put_String (Dasher_NL & " *** DO command script cannot be opened ***");
   end Do_Script;

   procedure Examine (Command : Slice_Set) is
      Addr : Phys_Addr_T;
      New_Val_US : Unbounded_String;
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** E(xamine) command requires an argument ***");
         return;
      end if;
      if Slice (Command, 2) = "M" then
         if Slice_Count (Command) /= 3 then
            TTOut.Put_String (Dasher_NL & " *** E M command requires one address argument ***");
            return;
         end if;
         Addr  := Phys_Addr_T(String_To_Dword (Slice (Command, 3), Console_Radix));
         if Addr > Max_Phys_Addr then
            TTOut.Put_String (Dasher_NL & " *** E(xamine) M(emory) - address beyond physical limit ***");
            return;
         end if;
         TTOut.Put_String (Dasher_NL & "Location " & Slice (Command, 3) &
                           " contains " & Word_To_String (WD => RAM.Read_Word (Addr), 
                                                          Base => Console_Radix, 
                                                          Width => 11, 
                                                          Zero_Pad => false) &
                           " - Enter new val or just NEWLINE> ");
         SCP_Handler.SCP_Get_Line (New_Val_US);
         declare
            New_Val_S : String := To_String (New_Val_US);
            New_Val   : Word_T;
         begin
            New_Val := Word_T(String_To_Dword(Str => New_Val_S, Base => Console_Radix));
            RAM.Write_Word (Addr, New_Val);
            TTOut.Put_String (Dasher_NL & "Location " & Slice (Command, 3) &
                              " contains " & Word_To_String (WD => RAM.Read_Word (Addr), 
                                                             Base => Console_Radix, 
                                                             Width => 11, 
                                                             Zero_Pad => false));
         exception
            when others =>
               TTOut.Put_String (Dasher_NL & " *** Could not parse new value ***");
         end;
      else         
         TTOut.Put_String (Dasher_NL & " *** Expecting A, M, or P for E(xamine) command ***");
      end if;

   exception
      when Constraint_Error =>
         TTOut.Put_String (Dasher_NL & " *** E(xamine) M(emory) - invalid address ***");
   end Examine;

   procedure Run is
      I_Counts : Processor.Instr_Count_T;
      I_Count  : Unsigned_64;
      Start_Time : Time;
      Elapsed : Time_Span;
   begin
      Processor.Prepare_For_Running (CPU);
      SCP_Handler.Set_SCP_IO(false);  

      Start_Time := Clock;

      Processor.Run (CPU, Debug_Logging, Console_Radix, Breakpoints, I_Counts);

      Elapsed := Clock - Start_Time;
      I_Count := Processor.Get_Instruction_Count (CPU);
      SCP_Handler.Set_SCP_IO(true);   
      TTOut.Put_String (Dasher_NL & " *** MV/Emua executed " & Unsigned_64'Image(I_Count) & 
                        " instructions in" & Duration'Image(To_Duration(Elapsed)) & " seconds ***");

      Ada.Text_IO.Put_Line ("Instruction Execution Count by Mnemonic");
      for I in I_Counts'Range loop
         if I_Counts(I) > 0 then
            Ada.Text_IO.Put(I'Image & ASCII.HT & I_Counts(I)'Image & Dasher_NL);
         end if;
      end loop;

   
   exception
      when others =>
         Elapsed := Clock - Start_Time;
         I_Count := Processor.Get_Instruction_Count (CPU);
         SCP_Handler.Set_SCP_IO(true);   
         TTOut.Put_String (Dasher_NL & " *** MV/Emua executed " & Unsigned_64'Image(I_Count) & 
                           " instructions in" & Duration'Image(To_Duration(Elapsed)) & " seconds ***");
         Ada.Text_IO.Put_Line ("Instruction Execution Count by Mnemonic");
         for I in I_Counts'Range loop
            if I_Counts(I) > 0 then
               Ada.Text_IO.Put(I'Image & ASCII.HT & I_Counts(I)'Image & Dasher_NL);
            end if;
         end loop;                  
         raise;
   end run;

   procedure Set (Command : Slice_Set) is
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** SET command requires 2 arguments ***");
         return;
      end if;
      declare
         use Ada.Containers;
         What   : String := Slice (Command, 2);
         Set_To : String := Slice (Command, 3);
      begin
         if What = "LOGGING" then
            if Set_To = "ON" then
               Debug_Logging := true;
               TTOut.Put_String (Dasher_NL & " *** Logging turned on, CPU will run slow ***");
            elsif Set_To = "OFF" then
               Debug_Logging := false;
               TTOut.Put_String (Dasher_NL & " *** Logging turned off ***");
            else
               TTOut.Put_String (Dasher_NL & " *** Value must be 'ON' or 'OFF' ***");
            end if;
            -- TODO Add calls here when new devices are added...
            Processor.Set_Debug_Logging(CPU, Debug_Logging);
            BMC_DCH.Set_Logging(Debug_Logging);
            Devices.Disk6061.Drives.Set_Logging (Debug_Logging);
         else
           TTOut.Put_String (Dasher_NL & " *** Unknown or Unimplemented SET command");
         end if;
      end; 
   end Set;

   procedure Show (Command : Slice_Set) is
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** SHOW command requires argument: BREAK|DEV|LOGGING ***");
         return;
      end if;
      declare
         use Ada.Containers;
         What : String := Slice (Command, 2);
      begin
         if What = "DEV" then
            TTOut.Put_String (Dasher_NL & Devices.Bus.Actions.Get_Printable_Device_List);
         elsif What = "BREAK" then
            if Breakpoints.Length = 0 then
               TTOut.Put_String (Dasher_NL & " *** No Breakpoints are set ***");
            else
               TTOut.Put_String (Dasher_NL & " *** Breakpoints: ");
               for BP of Breakpoints loop
                  TTOut.Put_String (Dword_To_String(Dword_T(BP), Console_Radix, 12, false));
               end loop;
            end if;
         elsif What = "LOGGING" then  
            TTOut.Put_String (Dasher_NL & " *** Logging is turned ");
            if Debug_Logging then
               TTOut.Put_String ("ON ***");
            else
               TTOut.Put_String ("OFF ***");
            end if;
         else
            TTOut.Put_String (Dasher_NL & " *** Unknown or Unimplemented SHOW command");
         end if;
      end;
   end Show;

   procedure Single_Step is
      Disass : Unbounded_String;
   begin
      TTOut.Put_String (Dasher_NL & Processor.Get_Compact_Status(CPU, Console_Radix));
      Processor.Single_Step(CPU, Console_Radix, Disass);
      TTOut.Put_String (Dasher_NL & To_String(Disass));
      TTOut.Put_String (Dasher_NL & Processor.Get_Compact_Status(CPU, Console_Radix));
   exception   
      when Error: others =>
         TTOut.Put_String (Dasher_NL & Ada.Exceptions.Exception_Message(Error));
   end Single_Step;

   procedure Start (Command : Slice_Set) is
      Addr : Phys_Addr_T;
   begin
      if Slice_Count (Command) < 2 then
         TTOut.Put_String (Dasher_NL & " *** ST (Start at) command requires an address argument ***");
         return;
      end if;
      Addr  := Phys_Addr_T(String_To_Dword (Slice (Command, 2), Console_Radix));
      CPU.PC := Addr;
      Run;
   end Start;

   procedure Do_Command (Cmd : Unbounded_String) is
      Words : Slice_Set;
   begin
      Create (Words, To_String(Cmd), " ", Multiple);
      Command := To_Unbounded_String(Slice (Words, 1));
      -- SCP-like commands...
      if Command = "." then
         TTOut.Put_String (Dasher_NL & Processor.Get_Compact_Status(CPU, Console_Radix));
      elsif Command = "B" then
         Boot (Words);
      elsif Command = "CO" then
         Run;
      elsif Command = "E" then
         Examine (Words);
      elsif Command = "HE"  or Command = "HELP" then
         Show_Help;
      elsif Command = "SS" then
         Single_Step;
      elsif Command = "ST" then
         Start (Words);
      
      -- enulator commands
      elsif Command = "ATT" then
         Attach (Words);
      elsif Command = "BREAK" then
         Break_Set (Words);   
      elsif Command = "CHECK" then
         Check (Words);
      elsif Command = "CREATE" then
         Create_Blank (Words);  
      elsif Command = "DET" then
         Detach (Words);
      elsif Command = "DIS" then
         Disassemble (Words);
      elsif Command = "DO" then
         Do_Script (Words);        
      elsif Command = "exit" or Command = "EXIT" 
         or Command = "quit" or Command = "QUIT" 
         or Command = "bye"  or Command = "BYE" then
         Clean_Exit;
      elsif Command = "NOBREAK" then
         Break_Clear (Words);   
      elsif Command = "SET" then
         Set (Words);   
      elsif Command = "SHOW" or Command = "SHO" or Command = "SH" then
         Show (Words);
      else
         Devices.Console.TTOut.Put_String (Dasher_NL & " *** Unknown SCP-CLI Command ***");
      end if;
   end Do_Command;

----------
-- MAIN --
----------

begin
   while Arg_Num <= Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (Arg_num) = "-do" then
         Arg_Num := Arg_Num + 1;
         Do_Script_Arg := Arg_Num;
      elsif Ada.Command_Line.Argument (Arg_num) = "-version" then
         Ada.Text_IO.Put_Line ("MV/Emua version " & Sem_Ver);
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      Arg_Num := Arg_Num + 1;
   end loop;

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
   loop
      GNAT.Sockets.Accept_Socket
        (Server => Receiver, Socket => Connection, Address => Client);
      Ada.Text_IO.Put_Line
        ("INFO: Console connected from " & GNAT.Sockets.Image (Client));

		-- The console is connected, now we can set up our emulated machine
		-- Here we are defining the hardware in our virtual machine
		-- Initially based on a minimally configured MV/10000 Model I...
		--   One CPU
		--   Console (TTI/TTO)
		--   One Tape Drive
		--   One or two HDDs
		--   A generous(!) 16MB (8MW) RAM
      --   A PIT (Programmable Interval Timer)
		--   NO IACs, LPT or ISC

      Decoder.Init;
      Devices.Bus.Actions.Init;
      CPU := Processor.Make;

      Status_Monitor.Monitor.Start (Monitor_Port);
      RAM.Init (Debug_Logging);
      -- Devices.Bus.Actions.Init;
      Devices.Bus.Actions.Connect (Devices.BMC);
      Devices.Bus.Actions.Set_Reset_Proc (Devices.BMC, BMC_DCH.Reset'Access);

      Devices.Bus.Actions.Connect (Devices.SCP);
      Devices.DRP_SCP.Init (Debug_Logging);

      Devices.Bus.Actions.Connect (Devices.CPU);
      -- Devices.Bus.Actions.Set_Reset_Proc (Devices.CPU, Processor.Actions.Reset'Access);

      Devices.Bus.Actions.Connect (Devices.PIT);
      Devices.PIT_P.Init;

      Devices.Bus.Actions.Connect (Devices.PSC);
      Devices.PSC_UPSC.Init (Debug_Logging);

      Devices.Bus.Actions.Connect (Devices.TTO);
      Devices.Console.TTOut.Init (Connection);
      Devices.Bus.Actions.Connect (Devices.TTI);
      Devices.Console.TTIn.Init;
      Console_Handler.Start (Connection);

      Devices.Bus.Actions.Connect (Devices.MTB);
      Devices.Magtape6026.Drives.Init;
      Devices.Magtape6026.Status_Sender.Start;

      Devices.Bus.Actions.Connect (Devices.DPF);
      Devices.Disk6061.Drives.Init (Debug_Logging);
      Devices.Disk6061.Status_Sender.Start;

      Devices.Bus.Actions.Connect (Devices.DSKP);
      Devices.Disk6239.Init (Debug_Logging);


      -- say hello...
      Devices.Console.TTOut.Put_Char (ASCII.FF);
      Devices.Console.TTOut.Put_String (" *** Welcome to the MV/Emulator - Type HE for help ***" & ASCII.LF);

      -- Handle any DO script - N.B. will not pass here until start-up script is complete...
      if Do_Script_Arg > 0 then
         Do_Command (To_Unbounded_String ("DO " &  Ada.Command_Line.Argument (Do_Script_Arg)));
      end if;

      Processor.Status_Sender.Start (CPU);

      -- the main SCP/console interaction loop
      SCP_Handler.Set_SCP_IO (true);
      loop
         Devices.Console.TTOut.Put_String (Dasher_NL & "SCP-CLI> ");
         SCP_Handler.SCP_Get_Line (Command_Line);
         Ada.Text_IO.Put_Line ("DEBUG: Got SCP command: " & To_String(Command_Line));
         Do_Command (Command_Line);
      end loop;

   end loop;

   exception
      when Error: Processor.CPU_Halt =>
         TTOut.Put_String (Dasher_NL & " *** MV/Emulator stopping due to CPU HALT instruction ***" );
         Loggers.Debug_Print (Debug_Log," *** MV/Emulator stopping due to CPU HALT instruction ***" );
         Loggers.Debug_Logs_Dump (Log_Dir);
         GNAT.OS_Lib.OS_Exit (0);

      when Error: others =>
         Ada.Text_IO.Put_Line("ERROR: " & Ada.Exceptions.Exception_Information(Error));
         TTOut.Put_String (Dasher_NL & " *** MV/Emulator stopping due to unhandled error ***" );
         -- TTOut.Put_String (Dasher_NL & Ada.Exceptions.Exception_Information(Error));
         Loggers.Debug_Print (Debug_Log, "ERROR: " & Ada.Exceptions.Exception_Information(Error));
         Loggers.Debug_Logs_Dump (Log_Dir);
         GNAT.OS_Lib.OS_Exit (0);

end MVEmuA;
