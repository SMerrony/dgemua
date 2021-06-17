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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Interfaces;  use Interfaces;

with Debug_Logs; use Debug_Logs;
with PARU_32;

package body AOSVS.Agent is

   protected body Actions is

      function Get_Free_Channel return Natural is
         Chan_No : Integer := 0;
      begin
         for C in Agent_Chans'Range loop
            if Agent_Chans(C).Opener_PID = 0 then
               Chan_No := C;
               exit;
            end if;
         end loop;
         if Chan_No = 0 then
            raise NO_MORE_CHANNELS;
         end if;
         return Natural(Chan_No);
      end Get_Free_Channel;

      Procedure Init (Cons : in GNAT.Sockets.Stream_Access; Virt_Root : in String) is
      begin
         -- Fake some in-use PIDs 
         for P in PID_T'Range loop
            PIDs_In_Use(P) := (P < 5);
         end loop;
         Console := Cons;
         Virtual_Root := To_Unbounded_String(Virt_Root);
      end Init;
      		
      procedure Allocate_PID (
         PR_Name         : in Unbounded_String;
         Num_Invocation_Args : in Natural;
         Invocation_Args : in Args_Arr;
         Working_Dir     : in Unbounded_String;
			Sixteen_Bit     : in Boolean;
			Proc_Name       : in Unbounded_String;
         User_Name       : in Unbounded_String;
			PID             : out PID_T) is
      begin
         -- get 1st unused PID
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
         Per_Process_Data(PID).Sixteen_Bit         := Sixteen_Bit;
         Per_Process_Data(PID).Proc_Name           := Proc_Name;
         Per_Process_Data(PID).User_Name           := User_Name;
         Per_Process_Data(PID).Console             := Console;
         Device_Chars.Include ("@CONSOLE", Default_Chars);
         Device_Chars.Include ("@INPUT",   Default_Chars);
         Device_Chars.Include ("@OUTPUT",  Default_Chars);
         Ada.Text_IO.Put_Line ("DEBUG: AGENT: Assigned PID " & PID'Image &
                   " to Process Name: " & To_String(Proc_Name));
         Loggers.Debug_Print (Sc_Log,"AGENT: Assigned PID:" & PID'Image & " for program: " & To_String(PR_Name)); 
         Loggers.Debug_Print (Sc_Log,"-----  Working Dir : " & To_String (Working_Dir));
      end Allocate_PID;

      procedure Allocate_TID (PID : in PID_T; 
                              TID : out Word_T) is
      begin
         for T in Per_Process_Data(PID).TIDs_In_Use'Range loop
            if not Per_Process_Data(PID).TIDs_In_Use(T) then
               Per_Process_Data(PID).TIDs_In_Use(T) := true;
               TID := Word_T(T);
               exit;
            end if;
            -- exhausted, return 0 which is invalid
            TID := 0;
         end loop;
      end Allocate_TID;

      function Get_Proc_Name (PID : in PID_T) return String is
         (To_String(Per_Process_Data(PID).Proc_Name));
   
      -- Sys Call Support below...

      -- File I/O...

		procedure File_Open (PID     : in Word_T; 
                           Path    : in String;
                           Options,               -- ?ISTI
                           File_Type : in Word_T; -- ?ISTO
                           Rec_Len : in Integer;
                           Chan_No : out Word_T;
                           Err     : out Word_T) is
         Chan_Num : Natural;
         Format_Bits : Word_T := Options and 7;
         Create, Create_Or_Error, Create_If_Reqd, Recreate, Read_Only : Boolean;
         F_New  : Ada.Text_IO.File_Type;
      begin
         Err := 0;
         Chan_Num := Get_Free_Channel;
         Agent_Chans(Chan_Num).Opener_PID := 0; -- ensure set to zero so can be resused if open fails
         Agent_Chans(Chan_Num).Path := To_Unbounded_String (Path);
         Loggers.Debug_Print (Sc_Log,"----- ?ISTI: " & Word_To_String (Options, Binary, 16, true));
         Loggers.Debug_Print (Sc_Log,"----- ?ISTO: " & Word_To_String (File_Type, Binary, 16, true));

         -- parse creation options
         Create_If_Reqd  := Test_W_Bit (Options, PARU_32.OF1B);
         Create_Or_Error := Test_W_Bit (Options, PARU_32.OF2B); -- for a new file
         Recreate        := (not Create_If_Reqd) and Create_Or_Error;
         Create          := Create_If_Reqd or Create_Or_Error;

         -- parse R/W options
         Read_Only := Test_W_Bit (Options, PARU_32.OPIB) and not Test_W_Bit (Options, PARU_32.OPOB);
         Agent_Chans(Chan_Num).Read  := Test_W_Bit (Options, PARU_32.OPIB);
         Agent_Chans(Chan_Num).Write := Test_W_Bit (Options, PARU_32.OPOB);         

         -- parse Format Field options which are at end of File_Type
         Loggers.Debug_Print (Sc_Log,"----- Type:" & Format_Bits'Image);
         Agent_Chans(Chan_Num).Dynamic      := (Format_Bits = PARU_32.RTDY);
         Agent_Chans(Chan_Num).Data_Sens    := (Format_Bits = PARU_32.RTDS);
         Agent_Chans(Chan_Num).Fixed_Length := (Format_Bits = PARU_32.RTFX);
         Agent_Chans(Chan_Num).Var_Length   := (Format_Bits = PARU_32.RTVR);
         Agent_Chans(Chan_Num).Undefined    := (Format_Bits = PARU_32.RTUN);
         Agent_Chans(Chan_Num).IBM_VB       := (Format_Bits = PARU_32.RTVB);
         if Agent_Chans(Chan_Num).Data_Sens then
            Loggers.Debug_Print (Sc_Log, "----- Data Sensitive");
        end if;
        if Agent_Chans(Chan_Num).Dynamic then
            Loggers.Debug_Print (Sc_Log, "----- Dynamic");
        end if;

         if Path(Path'First) = '@' then
            if (Path = "@CONSOLE") or (Path = "@INPUT") or (Path = "@OUTPUT") then
               Agent_Chans(Chan_Num).Con        := Console;
               Agent_Chans(Chan_Num).Is_Console := true;
               Agent_Chans(Chan_Num).Echo       := true;
            else
               raise Not_Yet_Implemented with "Cannot handle unknown generic files";
            end if;
         else 
            Agent_Chans(Chan_Num).Is_Console := false;

            if Recreate and Ada.Directories.Exists (Path) then
               Loggers.Debug_Print (Sc_Log, "------ Deleting old file");
               Ada.Directories.Delete_File (Path);
            end if;

            if Create then
               if Create_Or_Error and Ada.Directories.Exists (Path) then
                  Loggers.Debug_Print (Sc_Log, "------ Uh-oh - it already exists...");
                  Err := PARU_32.ERNAE;
                  return;
               end if;
               Ada.Text_IO.Create (F_New, Ada.Text_IO.Out_File, Path);
               Loggers.Debug_Print (Sc_Log, "----- Created new file");
               Ada.Text_IO.Close (F_New);
            end if;

            Loggers.Debug_Print (Sc_Log, "----- Attempting to open: " & Path);
            Ada.Streams.Stream_IO.Open (Agent_Chans(Chan_Num).File_Stream, (if Read_Only then Ada.Streams.Stream_IO.In_File else Ada.Streams.Stream_IO.Out_File), Path);

         end if;

         -- check for errors

         if Rec_Len > 0 then
            Agent_Chans(Chan_Num).Rec_Len := Rec_Len;
         end if;

         Agent_Chans(Chan_Num).Opener_PID := PID_T(PID); 

         Chan_No := Word_T(Chan_Num);

      end File_Open;

      procedure File_Close (Chan_No : in Natural; Err : out Word_T) is
      begin
         Err := 0;
         if Chan_No /= 0 then
            if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
               Err := PARU_32.ERACU;
            else
               if Agent_Chans(Integer(Chan_No)).Is_Console then
                  null; -- ignore ?CLOSE on console device for now
               else
                  Ada.Streams.Stream_IO.Close (Agent_Chans(Chan_No).File_Stream);
                  Agent_Chans(Chan_No).Opener_PID := 0;
               end if;
            end if;
         end if;
      end File_Close;
      
      function Get_Device_For_Channel(Chan_No : in Word_T) return Unbounded_String is
         Dev : Unbounded_String;
      begin
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            Dev := To_Unbounded_String ("***ERROR***");
         else
            Dev := Agent_Chans(Integer(Chan_No)).Path;
         end if;
         return Dev;
      end Get_Device_For_Channel;

      procedure File_Read (Chan_No : in Word_T;
                              Is_Extended,
                              Is_Absolute,
                              Is_Dynamic,
                              Is_DataSens : in Boolean;
                              Rec_Len     : in Integer;
                              Bytes       : in out Byte_Arr_T;
                              Transferred : out Word_T;
                              Err         : out Word_T) is
         Byte_Ix : Integer := Bytes'First;
         Byte    : Byte_T;
      begin
         Err := 0;
         Transferred := 0;
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            raise Channel_Not_Open with "?READ";
         end if;
         if Agent_Chans(Integer(Chan_No)).Is_Console then
            loop
               Byte_T'Read (Agent_Chans(Integer(Chan_No)).Con, Byte);
               -- ECHO 
               if Agent_Chans(Integer(Chan_No)).Echo then
                   Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Byte);
                   if Character'Val(Byte) = Dasher_CR then 
                      Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Char_To_Byte(Dasher_NL));
                  end if;
               end if;
               Bytes(Byte_Ix) := Byte;
               Bytes(Byte_Ix+1) := 0;
               if (Byte = Character'Pos(Dasher_NL)) or (Byte = Character'Pos(Dasher_CR)) then               
                  Bytes(Byte_Ix) := 0;
                  --Byte_Ix := Byte_Ix + 1;
                  exit;
               end if;
               -- TODO Handle Delete char

               Byte_Ix := Byte_Ix + 1;
            end loop;
            Transferred := Word_T(Byte_Ix) - 1;
         else
            raise Not_Yet_Implemented with "physical file reads";
         end if;


      end File_Read;

      procedure File_Write (Chan_No : in Word_T;
                              Defaults,
                              Is_Extended,
                              Is_Absolute,
                              Is_Dynamic,
                              Is_DataSens : in Boolean;
                              Rec_Len     : in Integer;
                              Bytes_BA    : in Dword_T;
                              Position    : in Integer;
                              Transferred : out Word_T;
                              Err         : out Word_T) is
         Too_Long : Boolean := false;
         DS_Len   : Integer := 0;
         Max_Len  : Integer := (if Rec_Len = (-1) then Agent_Chans(Integer(Chan_No)).Rec_Len else Rec_Len);
         Byte     : Byte_T;
         T_Dyn, T_DS : Boolean;
      begin
         Err := 0;
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            raise Channel_Not_Open with "?WRITE";
         end if;
         if Defaults then
            T_Dyn := Agent_Chans(Integer(Chan_No)).Dynamic;
            T_DS  := Agent_Chans(Integer(Chan_No)).Data_Sens;
            Loggers.Debug_Print (Sc_Log,"------ ... Type: " & (if T_Dyn then "Dynamic" else "Data Sensitive"));
         else
            T_Dyn := Is_Dynamic;
            T_DS  := Is_DataSens;
         end if;
         if Agent_Chans(Integer(Chan_No)).Is_Console then
            if T_DS then
               loop
                  Byte := RAM.Read_Byte_BA( Bytes_BA + Dword_T(DS_Len));
                  exit when Byte = 0; -- FIXME other terminators exist...
                  DS_Len := DS_Len + 1;
                  Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Byte);
                  exit when DS_Len = Max_Len;
               end loop;
               Transferred := Integer_16_To_Word(Integer_16(DS_Len));
               Loggers.Debug_Print (Sc_Log,"----- No of D/S bytes written:" & Transferred'Image);
            else
               declare
                  Bytes : Byte_Arr_T := RAM.Read_Bytes_BA(Bytes_BA, Rec_Len);
               begin
                  for B in Bytes'Range loop
                     Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Bytes(B));
                  end loop;
                  Transferred := Word_T(Bytes'Length);
               end;
            end if;
         else
            raise Not_Yet_Implemented with "?WRITE to real file";
         end if;
      end File_Write;


      -- CLI environment...

		function Get_Nth_Arg (PID : in Word_T; Arg_Num : in Word_T) return Unbounded_String is
         Nth : Integer := Integer(Arg_Num);
      begin
         if Nth > Per_Process_Data(PID_T(PID)).Invocation_Args'Last then
            raise No_Such_Argument with Arg_Num'Image;
         end if;
         return Per_Process_Data(PID_T(PID)).Invocation_Args(Nth);
      end Get_Nth_Arg;

      function Get_Num_Args (PID : in Word_T) return Natural is
         (Per_Process_Data(PID_T(PID_T(PID))).Num_Invocation_Args);

      function Get_PR_Name (PID : in Word_T) return Unbounded_String is
         (Per_Process_Data(PID_T(PID)).PR_Name);

	   function Get_User_Name (PID : in Word_T) return Unbounded_String is
         (Per_Process_Data(PID_T(PID)).User_Name);

      function Get_Working_Directory (PID : in Word_T) return String is 
         (To_String(Per_Process_Data(PID_T(PID)).Working_Directory));

      function Get_Superuser (PID : in Word_T) return Boolean is
         (Per_Process_Data(PID_T(PID)).Superuser_On);

      procedure Set_Superuser (PID : in Word_T; SU : in Boolean) is
      begin
         Per_Process_Data(PID_T(PID)).Superuser_On := SU;
      end Set_Superuser;

      -- Terminal I/O...
      procedure Get_Default_Chars (Device : in Unbounded_String;
									        WD_1, WD_2, WD_3 : out Word_T) is
      -- for the moment, we return the same basic defaults for any device...
      begin
         WD_1 := Default_Chars(1);
         WD_2 := Default_Chars(2);
         WD_3 := Default_Chars(3);
      end Get_Default_Chars;

      procedure Get_Current_Chars (Device : in Unbounded_String;
									        WD_1, WD_2, WD_3 : out Word_T) is
         Chars : Chars_Arr := Device_Chars(To_String(Device));
      begin
         WD_1 := Chars(1);
         WD_2 := Chars(2);
         WD_3 := Chars(3);
      end Get_Current_Chars;

      function Get_Console_For_PID (PID : in PID_T) return GNAT.Sockets.Stream_Access is
         Console : GNAT.Sockets.Stream_Access;
      begin
         for AC in Agent_Chans'Range loop
            if (Agent_Chans(AC).Opener_PID = PID) and (Agent_Chans(AC).Is_Console) then
               Console := Agent_Chans(AC).Con;
               exit;
            end if;            
         end loop;
         -- TODO handle not found
         return Console;
      end Get_Console_For_PID;

      procedure Send_Msg (Dest_PID : in Word_T; Msg : in String; Send_PID : in Word_T) is
         Out_Msg : String := Dasher_NL & "From PID" & Send_PID'Image & ": " & Msg & Dasher_NL;
         Con     : GNAT.Sockets.Stream_Access := Get_Console_For_PID(PID_T(Dest_PID));
      begin
         String'Write (Con, Out_Msg);
      end Send_Msg;

      -- IPCs...
      procedure I_Lookup (PID : in Word_T; Filename : in String;
							Glob_Port : out Dword_T; 
							F_Type : out Word_T;
							Err    : out Word_T) is
         IPC_Path : String := Agent.Actions.Get_Working_Directory(PID) & "/" & Filename;
      begin
         Err := 0;
         if IPCs.Contains (IPC_Path) then
            Glob_Port := IPCs(IPC_Path).Global_Port;
            F_Type    := IPCs(IPC_Path).File_Type;
            Loggers.Debug_Print (Sc_Log,"----- IPC Lookup succeeded for: " & IPC_Path);
         else
            Err := PARU_32.ERFDE;
            Loggers.Debug_Print (Sc_Log,"----- IPC Lookup failed for: " & IPC_Path);
         end if;
      end I_Lookup;

      procedure I_Create (PID : in Word_T; Filename : in String; Local_Port : in Word_T;
		                    Err : out Word_T) is
         I_New_F  : Ada.Text_IO.File_Type;
         New_IPC  : Agent_IPC_T;
         CWD_Filename : String := Get_Working_Directory(PID) & "/" & Filename;
      begin
         Err := 0;
         -- if the IPC filealready exists we delete it (FIXUP would do this on a real system)
         if Ada.Directories.Exists (CWD_Filename) then
            Loggers.Debug_Print (Sc_Log, "------- Deleting stale IPC file");
            Ada.Directories.Delete_File(CWD_Filename);
         end if;
         Ada.Text_IO.Create (I_New_F, Ada.Text_IO.Out_File, CWD_Filename);
         Ada.Text_IO.Close  (I_New_F);
         -- TODO 'register' the IPC internally
         New_IPC.Owner_PID  := PID_T(PID);
         New_IPC.Name       := To_Unbounded_String(CWD_Filename);
         New_IPC.Local_Port := Local_Port;
         New_IPC.Global_Port := Encode_Global_Port(PID_T(PID), 7, Local_Port); -- FIXME Ring hard-coded to 7
         IPCs.Include (CWD_Filename, New_IPC);

         Loggers.Debug_Print (Sc_Log, "------- Created IPC file: " & CWD_Filename);
      end I_Create;

      -- Shared Files...
      procedure Shared_Open (PID : in PID_T; S_Path : in String; Read_Only : in Boolean;
							        Chan_No : out Word_T; Err : out Word_T) is
         C      : Natural;
         SFile  : Block_IO.File_Type;
      begin
         Err := 0;
         C := Get_Free_Channel;
         Agent_Chans(C).Opener_PID := 0; -- ensure set to zero so it can be resused if this open fails
         Block_IO.Open (Agent_Chans(C).File_Shared, (if Read_Only then Block_IO.In_File else Block_IO.Inout_File), S_Path);
         Agent_Chans(C).Opener_PID := PID;
         Agent_Chans(C).Path       := To_Unbounded_String(S_Path);
         Agent_Chans(C).Is_Console := false;
         Agent_Chans(C).Read       := true;
         Agent_Chans(C).Write      := not Read_Only;
         Agent_Chans(C).For_Shared := true;
         Agent_Chans(C).Rec_Len    := 1024; -- not used
         Chan_No := Word_T(C);
      exception
         when others =>
            Err := PARU_32.ERFAD;
      end Shared_Open;

      procedure Shared_Read (PID         : in PID_T;
                             Chan_No     : in Natural;
                             Base_Addr   : in Phys_Addr_T;
                             Num_Pages   : in Natural;
                             Start_Block : in Natural;
                             Page_Arr    : in out Page_Arr_T;
                             Err         : out Word_T) is
         Num_Blocks : Natural := Num_Pages * 4; -- 4 disk blocks per memory page
         Blocks : Block_Arr_T(1..Num_Blocks);
         for Blocks'Address use Page_Arr'Address;
      begin
         Err := 0;
         Block_IO.Set_Index (Agent_Chans(Chan_No).File_Shared, Block_IO.Count(Start_Block+1));
         for B in 1 .. Num_Blocks loop
            if Block_IO.End_Of_File (Agent_Chans(Chan_No).File_Shared) then
               -- it seems AOS/VS magics pages into existence
               null;
            else
               Block_IO.Read (Agent_Chans(Chan_No).File_Shared, Blocks(B));
            end if;
         end loop;


      end Shared_Read;
      
   end Actions;

end AOSVS.Agent;
