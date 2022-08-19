--  Copyright ©2021,2022 Stephen Merrony
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Directories;
with Ada.Text_IO;

with Debug_Logs; use Debug_Logs;

package body AOSVS.Agent is

   protected body Actions is

      function Get_Free_Channel return Natural is
         Chan_No : Integer := -1;
      begin
         for C in Agent_Chans'Range loop
            if Agent_Chans(C).Opener_PID = 0 then
               Chan_No := C;
               exit;
            end if;
         end loop;
         if Chan_No = -1 then
            raise NO_MORE_CHANNELS;
         end if;
         return Natural(Chan_No);
      end Get_Free_Channel;

      procedure Init (Cons : GNAT.Sockets.Stream_Access; Virt_Root : String) is
      begin
         --  Fake some in-use PIDs
         for P in PID_T'Range loop
            PIDs_In_Use(P) := (P < 5);
         end loop;
         Console := Cons;
         Virtual_Root := To_Unbounded_String(Virt_Root);
         for C in Agent_Chans'Range loop
            if C < 2 then
               Agent_Chans(C).Opener_PID := 1; --  fake chans 0 & 1 in use
            else
               Agent_Chans(C).Opener_PID := 0;
            end if;
         end loop;
      end Init;

      procedure Allocate_PID (
         PR_Name         : Unbounded_String;
         Num_Invocation_Args : Natural;
         Invocation_Args : Args_Arr;
         Working_Dir     : Unbounded_String;
			Sixteen_Bit     : Boolean;
			Proc_Name       : Unbounded_String;
         User_Name       : Unbounded_String;
			PID             : out PID_T) is
      begin
         --  get 1st unused PID
         for P in PID_T'Range loop
            if not PIDs_In_Use(P) then
               PID := P;
               PIDs_In_Use(P) := true;
               exit;
            end if;
         end loop;
         Per_Process_Data(PID).PR_Name             := PR_Name;
         Per_Process_Data(PID).Num_Invocation_Args := Num_Invocation_Args;
         Per_Process_Data(PID).Invocation_Args     := Invocation_Args;
         Per_Process_Data(PID).Working_Directory   := Working_Dir;
         Per_Process_Data(PID).Searchlist          := Working_Dir & Dasher_Null & Dasher_Null;
         Per_Process_Data(PID).Sixteen_Bit         := Sixteen_Bit;
         Per_Process_Data(PID).Proc_Name           := Proc_Name;
         Per_Process_Data(PID).User_Name           := User_Name;
         Per_Process_Data(PID).Console             := Console;
         Per_Process_Data(PID).Default_ACL         := User_Name & Character'Val(0) &
                                                      "<?FACO+?FACW+?FACA+?FACR+?FACE>" &
                                                      Character'Val(0) & Character'Val(0);
         Device_Chars.Include ("@CONSOLE", Default_Chars);
         Device_Chars.Include ("@INPUT",   Default_Chars);
         Device_Chars.Include ("@OUTPUT",  Default_Chars);
         Ada.Text_IO.Put_Line ("DEBUG: AGENT: Assigned PID " & PID'Image &
                   " to Process Name: " & To_String(Proc_Name));
         Loggers.Debug_Print (Sc_Log,"AGENT: Assigned PID:" & PID'Image & " for program: " & To_String(PR_Name));
         Loggers.Debug_Print (Sc_Log,"-----   Working Dir : " & To_String (Working_Dir));
      end Allocate_PID;

      procedure Allocate_TID (PID : PID_T;
                              TID : out Word_T) is
      begin
         for T in Per_Process_Data(PID).TIDs_In_Use'Range loop
            if not Per_Process_Data(PID).TIDs_In_Use(T) then
               Per_Process_Data(PID).TIDs_In_Use(T) := true;
               TID := Word_T(T);
               exit;
            end if;
            raise NO_MORE_TIDS;
         end loop;
      end Allocate_TID;

      function Get_Proc_Name (PID : PID_T) return String is
         (To_String(Per_Process_Data(PID).Proc_Name));

       function Get_Virtual_Root return Unbounded_String is
         (Virtual_Root);

      function Get_Searchlist (PID : PID_T) return Unbounded_String is
			(Per_Process_Data(PID).Searchlist);

      --  Sys Call Support below...

      --  File I/O...

		procedure File_Open (PID     : Word_T;
                           Path    : String;
                           Options,               --  ?ISTI
                           File_Type : Word_T; --  ?ISTO
                           Rec_Len : Integer;
                           Chan_No : out Word_T;
                           Err     : out Word_T) is
         Chan_Num    : Natural;
         Format_Bits : constant Word_T := Options and 7;
         Create, Create_Or_Error, Create_If_Reqd, Recreate, Read_Only : Boolean;
         F_New       : Ada.Text_IO.File_Type;
      begin
         Err := 0;
         Chan_Num := Get_Free_Channel;
         Agent_Chans(Chan_Num).Opener_PID := 0; --  ensure set to zero so can be resused if open fails
         Agent_Chans(Chan_Num).Path := To_Unbounded_String (Path);
         Loggers.Debug_Print (Sc_Log,"-----  ?ISTI: " & Word_To_String (Options, Binary, 16, true) & " " & Word_To_String (Options, Octal, 6, true));
         Loggers.Debug_Print (Sc_Log,"-----  ?ISTO: " & Word_To_String (File_Type, Binary, 16, true) & " " & Word_To_String (File_Type, Octal, 6, true));

         --  parse creation options
         Create_If_Reqd  := Test_W_Bit (Options, PARU_32.OF1B);
         Create_Or_Error := Test_W_Bit (Options, PARU_32.OF2B); --  for a new file
         Recreate        := (not Create_If_Reqd) and Create_Or_Error;
         Create          := Create_If_Reqd or Create_Or_Error;

         --  parse R/W options
         Read_Only := Test_W_Bit (Options, PARU_32.OPIB) and not Test_W_Bit (Options, PARU_32.OPOB);
         Agent_Chans(Chan_Num).Read  := Test_W_Bit (Options, PARU_32.OPIB);
         Agent_Chans(Chan_Num).Write := Test_W_Bit (Options, PARU_32.OPOB);

         --  parse Format Field options which are at end of File_Type
         Loggers.Debug_Print (Sc_Log,"-----  Type:" & Format_Bits'Image);
         case Format_Bits is
            when PARU_32.RTDY => Agent_Chans(Chan_Num).Rec_Format := Dynamic;
            when PARU_32.RTDS => Agent_Chans(Chan_Num).Rec_Format := Data_Sensitive;
            when PARU_32.RTFX => Agent_Chans(Chan_Num).Rec_Format := Fixed_Length;
            when PARU_32.RTVR => Agent_Chans(Chan_Num).Rec_Format := Variable_Length;
            when PARU_32.RTUN => Agent_Chans(Chan_Num).Rec_Format := Undefined_Length;
            when PARU_32.RTVB => Agent_Chans(Chan_Num).Rec_Format := Variable_Block;
            when others =>
               --  raise Unknown_Record_Type with "Unknown format for ?OPEN";
               Agent_Chans(Chan_Num).Rec_Format := Data_Sensitive; --  TODO check this, is D/S the default?
         end case;

         Loggers.Debug_Print (Sc_Log, "-----  Default Record Type: " & Agent_Chans(Chan_Num).Rec_Format'Image);

         Agent_Chans(Chan_Num).Rec_Len := Rec_Len;
         Loggers.Debug_Print (Sc_Log, "-----  Default Record Length:" & Rec_Len'Image);

         if Path(Path'First) = '@' then
            if (Path = "@CONSOLE") or (Path = "@INPUT") or (Path = "@OUTPUT") or (Path = "@OUT") then
               Agent_Chans(Chan_Num).Con        := Console;
               Agent_Chans(Chan_Num).Is_Console := true;
               Agent_Chans(Chan_Num).Echo       := true;
            else
               raise Not_Yet_Implemented with "Cannot handle unknown generic files: " & Path;
            end if;
         else
            Agent_Chans(Chan_Num).Is_Console := false;

            if Recreate and Ada.Directories.Exists (Path) then
               Loggers.Debug_Print (Sc_Log, "------  Deleting old file");
               Ada.Directories.Delete_File (Path);
            end if;

            if Create then
               if Create_Or_Error and Ada.Directories.Exists (Path) then
                  Loggers.Debug_Print (Sc_Log, "------  Uh-oh - it already exists...");
                  Err := PARU_32.ERNAE;
                  return;
               end if;
               Ada.Text_IO.Create (F_New, Ada.Text_IO.Out_File, Path);
               Loggers.Debug_Print (Sc_Log, "-----  Created new file");
               Ada.Text_IO.Close (F_New);
            end if;

            if not Ada.Directories.Exists (Path) then
               Loggers.Debug_Print (Sc_Log, "------  File did not exist to ?OPEN");
               Err := PARU_32.ERFDE;
               return;
            end if;

            Loggers.Debug_Print (Sc_Log, "-----  Attempting to open: " & Path);
             Agent_Chans(Chan_Num).Access_Method := Direct;
             Direct_IO.Open (Agent_Chans(Chan_Num).File_Direct, 
                             (if Read_Only then Direct_IO.In_File else Direct_IO.Inout_File),
                             Path);

            Loggers.Debug_Print (Sc_Log, "-----  Channel number:" & Chan_Num'Image & " opened");
         end if;

         --  check for errors

         if Rec_Len > 0 then
            Agent_Chans(Chan_Num).Rec_Len := Rec_Len;
         end if;

         Agent_Chans(Chan_Num).Opener_PID := PID_T(PID);

         Chan_No := Word_T(Chan_Num);

      end File_Open;

      procedure File_Close (Chan_No : Natural; Err : out Word_T) is
      begin
         Err := 0;
         if Chan_No /= 0 then
            if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
               Err := PARU_32.ERACU;
            else
               if Agent_Chans(Integer(Chan_No)).Is_Console then
                  null; --  ignore ?CLOSE on console device for now
               else
                  case Agent_Chans(Integer(Chan_No)).Access_Method is
                     when Block =>  Block_IO.Close (Agent_Chans(Chan_No).File_Block);
                     when Shared => null; --  TODO
                     when Direct => Direct_IO.Close (Agent_Chans(Chan_No).File_Direct);
                     when Stream => 
                        null;
                        -- Ada.Direct_IO.Close (Agent_Chans(Chan_No).File_Direct);
                        -- Ada.Direct_IO.Close (Agent_Chans(Chan_No).File_Out_Stream);
                        -- Ada.Direct_IO.Close (Agent_Chans(Chan_No).File_Append_Stream);
                  end case;
                  Agent_Chans(Chan_No).Opener_PID := 0;
               end if;
            end if;
         end if;
      end File_Close;

      function Get_Default_ACL (PID : PID_T) return String is
         (To_String (Per_Process_Data(PID).Default_ACL));

      function Get_Device_For_Channel(Chan_No : Word_T) return Unbounded_String is
         Dev : Unbounded_String;
      begin
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            Dev := To_Unbounded_String ("***ERROR***");
         else
            Dev := Agent_Chans(Integer(Chan_No)).Path;
         end if;
         return Dev;
      end Get_Device_For_Channel;

      function Get_Default_Len_For_Channel (Chan_No : Word_T) return Integer is
         (Agent_Chans(Integer(Chan_No)).Rec_Len);

      procedure Block_File_Open (PID       : Word_T;
                                 Path      : String;
                                 Exclusive : Boolean;
                                 Chan_No   : out Word_T;
                                 File_Type : out Word_T;
                                 File_Size : out Integer_32;
                                 Err       : out Word_T) is
         Chan_Num : Natural;
         File_Length : Ada.Directories.File_Size;
      begin
         Err := 0;
         Chan_Num := Get_Free_Channel;
         Agent_Chans(Chan_Num).Opener_PID := 0; --  ensure set to zero so can be resused if open fails
         Agent_Chans(Chan_Num).Path := To_Unbounded_String (Path);
         Agent_Chans(Chan_Num).Is_Console := false;
         Agent_Chans(Chan_Num).Access_Method := Block;
         if not Ada.Directories.Exists (Path) then
            Loggers.Debug_Print (Sc_Log, "------  File did not exist to ?GOPEN: >>>" & Path & "<<<");
            Err := PARU_32.ERFDE;
            return;
         end if;
         Block_IO.Open (Agent_Chans(Chan_Num).File_Block, Block_IO.Inout_File, Path);
         File_Type := PARU_32.FUDF; --  FIXME always returns UDF file type atm.
         File_Length := Ada.Directories.Size (Path);
         File_Size   := Integer_32(File_Length);
         Agent_Chans(Chan_Num).Opener_PID := PID_T(PID);
         Chan_No := Word_T(Chan_Num);
      end Block_File_Open;

      procedure File_Read (Chan_No : Word_T;
                              Is_Extended,
                              Is_Absolute,
                              Open_Fmt    : Boolean;
							         Rec_Fmt     : Record_Format_T;
                              Rec_Len     : Integer;
                              Bytes       : out Byte_Arr_T;
                              Transferred : out Word_T;
                              Err         : out Word_T) is
         Actual_Rec_Fmt : Record_Format_T := Rec_Fmt;
         Byte_Ix : Integer := Bytes'First;
         Char    : Character;
         Byte    : Byte_T;
      begin
         Loggers.Debug_Print (Sc_Log, "-----  Path: " & To_String (Agent_Chans(Integer(Chan_No)).Path));
         Loggers.Debug_Print (Sc_Log, "-----  Record Length:" & Rec_Len'Image);
         Loggers.Debug_Print (Sc_Log, "-----  Index of first byte:" & Bytes'First'Image);
         Err := 0;
         Transferred := 0;
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            raise Channel_Not_Open with "?READ";
         end if;
         if Open_Fmt then
            Actual_Rec_Fmt := Agent_Chans(Integer(Chan_No)).Rec_Format;
         end if;
         if Agent_Chans(Integer(Chan_No)).Is_Console then
            case Actual_Rec_Fmt is
               when Data_Sensitive =>
                  Bytes(Byte_Ix) := 0;
                  loop
                     Character'Read (Agent_Chans(Integer(Chan_No)).Con, Char);
                     Byte := Char_To_Byte(Char);
                     --  ECHO
                     if Agent_Chans(Integer(Chan_No)).Echo then
                        Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Byte);
                        if Character'Val(Byte) = Dasher_CR then
                           Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Char_To_Byte(Dasher_NL));
                        end if;
                     end if;
                     Bytes(Byte_Ix) := Byte;
                     Bytes(Byte_Ix+1) := 0;
                     Bytes(Byte_Ix+2) := 0;
                     --  TODO Handle Delete char
                     Byte_Ix := Byte_Ix + 1;
                     Transferred := Transferred + 1;
                     exit when Char = Dasher_NL or Char = Dasher_CR or Char = Dasher_FF;
                  end loop;
               when Dynamic =>
                  --  Rec_Len is the fixed # of Bytes to read
                  for B in 0 .. Rec_Len - 1 loop
                     -- Direct_IO.Read(Agent_Chans(Integer(Chan_No)).Con, Bytes(B));
                     Character'Read (Agent_Chans(Integer(Chan_No)).Con, Char);
                     Byte := Char_To_Byte(Char);
                     Bytes (B) := Byte;
                     Transferred := Transferred + 1;
                  end loop;
               when others =>
                  raise Not_Yet_Implemented with "NYI - ?READ this Record Type from CONSOLE";
            end case;
         else --  not a CONSOLE device, a real file...
            case Actual_Rec_Fmt is
               when Data_Sensitive =>
                  Bytes (Byte_Ix) := 0;
                  loop
                     if Direct_IO.End_Of_File (Agent_Chans (Integer (Chan_No)).File_Direct) then
                        Err := PARU_32.EREOF;
                        return;
                     end if;
                     Direct_IO.Read(Agent_Chans(Integer(Chan_No)).File_Direct, Bytes(Byte_Ix));
                     --  Character'Read (Agent_Chans (Integer (Chan_No)).Stream_In_Acc, Char);
                     --  Byte := Char_To_Byte(Char);
                     --  Bytes (Byte_Ix) := Byte;
                     -- Bytes (Byte_Ix + 1) := 0;
                     -- Bytes (Byte_Ix + 2) := 0;
                     Char := Byte_To_Char (Bytes(Byte_Ix));
                     
                     -- TODO - Testing...
                     exit when Byte_Ix = Bytes'Last;

                     Byte_Ix := Byte_Ix + 1;
                     Transferred := Transferred + 1;

                     exit when Char = Dasher_NL or Char = Dasher_CR or Char = Dasher_FF;

                  end loop;
                  Loggers.Debug_Print (Sc_Log, "-----  Read: >>>" & To_String (Byte_Arr_To_Unbounded (Bytes (0 .. Byte_Ix))) & "<<<");
               when Dynamic =>
                  --  Rec_Len is the fixed # of Bytes to read
                  if not Direct_IO.Is_Open (Agent_Chans (Integer (Chan_No)).File_Direct) then
                     raise Channel_Not_Open;
                  end if;
                  for B in 0 .. Rec_Len - 1 loop
                     if Direct_IO.End_Of_File (Agent_Chans (Integer (Chan_No)).File_Direct) then
                        Err := PARU_32.EREOF;
                        return;
                     end if;
                     Direct_IO.Read(Agent_Chans(Integer(Chan_No)).File_Direct, Bytes(B));
                     --  Character'Read (Agent_Chans (Integer (Chan_No)).Stream_In_Acc, Char);
                     --  Byte := Char_To_Byte (Char);
                     --  Bytes (Byte_Ix) := Byte;
                     Transferred := Transferred + 1;
                  end loop;
               when Variable_Length =>
                  --  4-byte ASCII record length + 4, followed by raw data
                  declare
                     Byte     : Byte_T;
                     RL_ASCII : String(1..4);
                     Rec_Len  : Integer := 0;
                  begin
                     Direct_IO.Read(Agent_Chans(Integer(Chan_No)).File_Direct, Byte);
                     RL_ASCII(1) := Byte_To_Char (Byte);
                     Direct_IO.Read(Agent_Chans(Integer(Chan_No)).File_Direct, Byte);
                     RL_ASCII(2) := Byte_To_Char (Byte);
                     Direct_IO.Read(Agent_Chans(Integer(Chan_No)).File_Direct, Byte);
                     RL_ASCII(3) := Byte_To_Char (Byte);
                     Direct_IO.Read(Agent_Chans(Integer(Chan_No)).File_Direct, Byte);
                     RL_ASCII(4) := Byte_To_Char (Byte);                                          
                     Rec_Len := Integer'Value(RL_ASCII) - 4; --  4 additional bytes included for the length header
                     Loggers.Debug_Print (Sc_Log, "-----  Found record of length:" & Rec_Len'Image);
                     for B in 0 .. Rec_Len - 1 loop
                        Direct_IO.Read(Agent_Chans(Integer(Chan_No)).File_Direct, Bytes(Byte_Ix + 4));
                        --  Character'Read (Agent_Chans(Integer(Chan_No)).Stream_In_Acc, Char);
                        --  Byte := Char_To_Byte(Char);
                        --  Bytes(Byte_Ix+4) := Byte;
                        Transferred := Transferred + 1;
                     end loop;
                     Loggers.Debug_Print (Sc_Log, "-----  Transferred count set to:" & Transferred'Image);
                     --  for W in 0 .. (Rec_Len - 1) / 2 loop
                     --     Tmp := Bytes(W*2);
                     --     Bytes(W*2) := Bytes((W*2)+1);
                     --     Bytes((W*2)+1) := Tmp;
                     --  end loop;
                     --  Transferred := Word_T(Rec_Len)+ 4;
                  end;
               when others =>
                  raise Not_Yet_Implemented with "NYI: non-datasensitive/dynamic real file READs";
            end case;
         end if;
      end File_Read;

      procedure File_Read_Blocks (Chan_No     : Word_T;
                                    Num_Blocks  : Unsigned_8;
                                    Start_Block : Unsigned_32;
                                    Buffer_Addr : Phys_Addr_T;
                                    Transferred : out Word_T;
                                    Err         : out Word_T) is
         Block_Ix  : Block_IO.Count := Block_IO.Count(Start_Block);
         Mem_Addr  : Phys_Addr_T    := Buffer_Addr;
         Tmp_Block : Block_T;
         use Block_IO;
      begin
         Err := 0;
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            raise Channel_Not_Open with "?RDB";
         end if;
         Block_Ix := Block_Ix + 1; --  Ada blocks start at #1, AOS/VS at #0
         Block_IO.Set_Index (Agent_Chans(Integer(Chan_No)).File_Block, Block_Ix);
         Transferred := 0;
         for Blk in 1.. Num_Blocks loop
            Block_IO.Read (Agent_Chans(Integer(Chan_No)).File_Block, Tmp_Block);
            for W in Tmp_Block'Range loop
               RAM.Write_Word (Mem_Addr, Swap_Bytes (Tmp_Block(W))); --  EEK - had to swap bytes... <=======
               --RAM.Write_Word (Mem_Addr, Tmp_Block(W));
               Mem_Addr := Mem_Addr + 1;
               Transferred := Transferred + 2;
            end loop;
            Block_Ix := Block_Ix + 1;
            Block_IO.Set_Index (Agent_Chans(Integer(Chan_No)).File_Block, Block_Ix);
         end loop;
      exception
         when Block_IO.End_Error =>
            Err := PARU_32.EREOF;

      end File_Read_Blocks;

      procedure File_Write (Chan_No : Word_T;
                              Is_Extended,
                              Is_Absolute,
                              Open_Fmt    : Boolean;
                              Rec_Fmt     : Record_Format_T;
                              Rec_Len     : Integer;
                              Bytes_BA    : Dword_T;
                              Position    : Integer;
                              Transferred : out Word_T;
                              Err         : out Word_T) is
         Actual_Rec_Fmt : Record_Format_T := Rec_Fmt;
         DS_Len   : Integer := 0;
         Max_Len  : constant Integer := (if Rec_Len = (-1) then Agent_Chans(Integer(Chan_No)).Rec_Len else Rec_Len);
         Byte     : Byte_T;
      begin
         Err := 0;
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            raise Channel_Not_Open with "?WRITE";
         end if;
         if Open_Fmt then
            Actual_Rec_Fmt := Agent_Chans(Integer(Chan_No)).Rec_Format;
         end if;
         Loggers.Debug_Print (Sc_Log, "------  Path: " & To_String (Agent_Chans(Integer(Chan_No)).Path));

         if Agent_Chans(Integer(Chan_No)).Is_Console then
            case Actual_Rec_Fmt is
               when Data_Sensitive =>
                  loop
                     Byte := RAM.Read_Byte_BA( Bytes_BA + Dword_T(DS_Len));
                     --  TODO handle custom delimiter tables
                     --  exit when Byte = 0;
                     DS_Len := DS_Len + 1;
                     Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Byte);
                     exit when Byte = 0 or Byte = 8#12# or Byte = 8#14# or Byte = 8#15#;
                     exit when DS_Len = Max_Len;
                  end loop;
                  Transferred := Integer_16_To_Word(Integer_16(DS_Len));
                  --  Loggers.Debug_Print (Sc_Log,"-----  No of D/S bytes written:" & Transferred'Image);
                  Loggers.Debug_Print (Sc_Log,"-----  Wrote: " & RAM.Read_String_BA (Bytes_BA, False));
               when Dynamic =>
                  declare
                     Bytes : constant Byte_Arr_T := RAM.Read_Bytes_BA(Bytes_BA, Rec_Len);
                  begin
                     for B in Bytes'Range loop
                        Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Bytes(B));
                     end loop;
                     Transferred := Word_T(Bytes'Length);
                     Loggers.Debug_Print (Sc_Log,"-----  Wrote: " & To_String (Byte_Arr_To_Unbounded(Bytes)));
                  end;
                  if Integer (Transferred) /= Rec_Len then
                     raise IO_Error with "mismatch between requested and actual bytes written";
                  end if;
               when others =>
                  raise Not_Yet_Implemented with "NYI - non-DS or Dynamic";
            end case;
         else --  not to the console
            case Actual_Rec_Fmt is
               when Dynamic =>
                  declare
                     Bytes : constant Byte_Arr_T := RAM.Read_Bytes_BA(Bytes_BA, Rec_Len);
                  begin
                     for B in Bytes'Range loop
                        Direct_IO.Write (Agent_Chans(Integer(Chan_No)).File_Direct, Bytes(B));
                     end loop;
                     Transferred := Word_T(Bytes'Length);
                  end;
                  if Integer (Transferred) /= Rec_Len then
                     raise IO_Error with "mismatch between requested and actual bytes written";
                  end if;
               when others =>
                  raise Not_Yet_Implemented with "NYI - Non-dynamic ?WRITE to a real file";
            end case;
         end if;
      end File_Write;

      --  CLI environment...

		function Get_Nth_Arg (PID : Word_T; Arg_Num : Word_T) return Unbounded_String is
         Nth : constant Integer := Integer(Arg_Num);
      begin
         if Nth > Per_Process_Data(PID_T(PID)).Invocation_Args'Last then
            raise No_Such_Argument with Arg_Num'Image;
         end if;
         Loggers.Debug_Print (Sc_Log,"------  Get_Nth_Arg returning: >>>" & To_String(Per_Process_Data(PID_T(PID)).Invocation_Args(Nth)) & "<<<");
         return Per_Process_Data(PID_T(PID)).Invocation_Args(Nth);
      end Get_Nth_Arg;

      function Get_Num_Args (PID : Word_T) return Natural is
         (Per_Process_Data(PID_T(PID)).Num_Invocation_Args);

      function Get_PR_Name (PID : Word_T) return Unbounded_String is
         (Per_Process_Data(PID_T(PID)).PR_Name);

	   function Get_User_Name (PID : Word_T) return Unbounded_String is
         (Per_Process_Data(PID_T(PID)).User_Name);

      function Get_Working_Directory (PID : Word_T) return String is
         (To_String(Per_Process_Data(PID_T(PID)).Working_Directory));

      function Get_Superuser (PID : Word_T) return Boolean is
         (Per_Process_Data(PID_T(PID)).Superuser_On);

      procedure Set_Superuser (PID : Word_T; SU : Boolean) is
      begin
         Per_Process_Data(PID_T(PID)).Superuser_On := SU;
      end Set_Superuser;

      --  Terminal I/O...
      procedure Get_Default_Chars (Device : Unbounded_String;
									        WD_1, WD_2, WD_3 : out Word_T) is
      --  for the moment, we return the same basic defaults for any device...
      begin
         WD_1 := Default_Chars(1);
         WD_2 := Default_Chars(2);
         WD_3 := Default_Chars(3);
      end Get_Default_Chars;

      procedure Get_Current_Chars (Device : Unbounded_String;
									        WD_1, WD_2, WD_3 : out Word_T) is
         Chars : constant Chars_Arr := Device_Chars(To_String(Device));
      begin
         WD_1 := Chars(1);
         WD_2 := Chars(2);
         WD_3 := Chars(3);
      end Get_Current_Chars;

      function Get_Console_For_PID (PID : PID_T) return GNAT.Sockets.Stream_Access is
         Console : GNAT.Sockets.Stream_Access;
      begin
         for AC in Agent_Chans'Range loop
            if (Agent_Chans(AC).Opener_PID = PID) and (Agent_Chans(AC).Is_Console) then
               Console := Agent_Chans(AC).Con;
               exit;
            end if;
         end loop;
         --  TODO handle not found
         return Console;
      end Get_Console_For_PID;

      procedure Send_Msg (Dest_PID : Word_T; Msg : String; Send_PID : Word_T) is
         Out_Msg : constant String := Dasher_NL & "From PID" & Send_PID'Image & ": " & Msg & Dasher_NL;
         Con     : constant GNAT.Sockets.Stream_Access := Get_Console_For_PID(PID_T(Dest_PID));
      begin
         String'Write (Con, Out_Msg);
      end Send_Msg;

      --  IPCs...
      procedure I_Lookup (PID : Word_T; Filename : String;
							Glob_Port : out Dword_T;
							F_Type : out Word_T;
							Err    : out Word_T) is
      begin
         Err := 0;
         if IPCs.Contains (Filename) then
            Glob_Port := IPCs(Filename).Global_Port;
            F_Type    := IPCs(Filename).File_Type;
            Loggers.Debug_Print (Sc_Log,"-----  IPC Lookup succeeded for: " & Filename);
         else
            Err := PARU_32.ERFDE;
            Loggers.Debug_Print (Sc_Log,"-----  IPC Lookup failed for: " & Filename);
         end if;
      end I_Lookup;

      procedure I_Create (PID : Word_T; Filename : String; Local_Port : Word_T;
		                    Err : out Word_T) is
         I_New_F  : Ada.Text_IO.File_Type;
         New_IPC  : Agent_IPC_T;
      begin
         Err := 0;
         --  if the IPC file already exists we delete it (FIXUP would do this on a real system)
         if Ada.Directories.Exists (Filename) then
            Loggers.Debug_Print (Sc_Log, "-------  Deleting stale IPC file");
            Ada.Directories.Delete_File(Filename);
         end if;
         Ada.Text_IO.Create (I_New_F, Ada.Text_IO.Out_File, Filename);
         Ada.Text_IO.Close  (I_New_F);
         --  TODO 'register' the IPC internally
         New_IPC.Owner_PID  := PID_T(PID);
         New_IPC.Name       := To_Unbounded_String(Filename);
         New_IPC.Local_Port := Local_Port;
         New_IPC.Global_Port := Encode_Global_Port(PID_T(PID), 7, Local_Port); --  FIXME Ring hard-coded to 7
         IPCs.Include (Filename, New_IPC);

         Loggers.Debug_Print (Sc_Log, "-------  Created IPC file: " & Filename);
      end I_Create;

      --  Shared Files...
      procedure Shared_Open (PID : PID_T; S_Path : String; Read_Only : Boolean;
							        Chan_No : out Word_T; Err : out Word_T) is
         C      : Natural;
      begin
         Err := 0;
         if not Ada.Directories.Exists (S_Path) then
            Loggers.Debug_Print (Sc_Log, "------  File does not exist");
            Err := PARU_32.ERFDE;
            return;
         end if;
         C := Get_Free_Channel;
         Agent_Chans(C).Opener_PID := 0; --  ensure set to zero so it can be resused if this open fails
         Block_IO.Open (Agent_Chans(C).File_Shared, (if Read_Only then Block_IO.In_File else Block_IO.Inout_File), S_Path);
         Agent_Chans(C).Opener_PID := PID;
         Agent_Chans(C).Path       := To_Unbounded_String(S_Path);
         Agent_Chans(C).Is_Console := false;
         Agent_Chans(C).Read       := true;
         Agent_Chans(C).Write      := not Read_Only;
         Agent_Chans(C).For_Shared := true;
         Agent_Chans(C).Rec_Len    := 1024; --  not used
         Chan_No := Word_T(C);
      exception
         when others =>
            Err := PARU_32.ERFAD;
      end Shared_Open;

      procedure Shared_Read (PID         : PID_T;
                             Chan_No     : Natural;
                             Base_Addr   : Phys_Addr_T;
                             Num_Pages   : Natural;
                             Start_Block : Natural;
                             Page_Arr    : out Page_Arr_T;
                             Err         : out Word_T) is
         Num_Blocks : constant Natural := Num_Pages * 4; --  4 disk blocks per memory page
         Blocks     : Block_Arr_T(1..Num_Blocks);
         for Blocks'Address use Page_Arr'Address;
      begin
         Err := 0;
         Block_IO.Set_Index (Agent_Chans(Chan_No).File_Shared, Block_IO.Count(Start_Block+1));
         for B in 1 .. Num_Blocks loop
            if Block_IO.End_Of_File (Agent_Chans(Chan_No).File_Shared) then
               --  it seems AOS/VS magics pages into existence
               null;
            else
               Block_IO.Read (Agent_Chans(Chan_No).File_Shared, Blocks(B));
            end if;
         end loop;


      end Shared_Read;

   end Actions;

end AOSVS.Agent;
