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

with Ada.Text_IO; use Ada.Text_IO;

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

      Procedure Init (Cons : in GNAT.Sockets.Stream_Access) is
      begin
         -- Fake some in-use PIDs 
         for P in PID_T'Range loop
            PIDs_In_Use(P) := (P < 5);
         end loop;
         Console := Cons;
      end Init;
      		
      procedure Allocate_PID (
         PR_Name         : in Unbounded_String;
         Num_Invocation_Args : in Natural;
         Invocation_Args : in Args_Arr;
			Virtual_Root    : in Unbounded_String;
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
         Per_Process_Data(PID).PR_Name         := PR_Name;
         Per_Process_Data(PID).Num_Invocation_Args := Num_Invocation_Args;
         Per_Process_Data(PID).Invocation_Args := Invocation_Args;
         Per_Process_Data(PID).Virtual_Root    := Virtual_Root;
         Per_Process_Data(PID).Working_Directory := Virtual_Root;
         Per_Process_Data(PID).Sixteen_Bit     := Sixteen_Bit;
         Per_Process_Data(PID).Proc_Name       := Proc_Name;
         Per_Process_Data(PID).User_Name       := User_Name;
         Per_Process_Data(PID).Console         := Console;
         Device_Chars.Include("@CONSOLE", Default_Chars);
         Device_Chars.Include("@INPUT", Default_Chars);
         Device_Chars.Include("@OUTPUT", Default_Chars);
         Put_Line ("DEBUG: AGENT: Assigned PID " & PID'Image &
                   " to Process Name: " & To_String(Proc_Name));
         Loggers.Debug_Print (Sc_Log,"AGENT: Assigned PID:" & PID'Image & " for program: " & To_String(PR_Name)); 
         Loggers.Debug_Print (Sc_Log,"-----  Working Dir : " & To_String (Virtual_Root));
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

      -- Sys Call Support below...

      -- File I/O...

		procedure File_Open (PID     : in Word_T; 
                           Path    : in String;
                           Mode    : in Word_T;
                           Rec_Len : in Integer;
                           Chan_No : out Word_T;
                           Err     : out Word_T) is
         Chan_Num : Natural;
      begin
         Err := 0;
         Chan_Num := Get_Free_Channel;
         Agent_Chans(Chan_Num).Opener_PID := 0; -- ensure set to zero so can be resused if open fails
         Agent_Chans(Chan_Num).Path := To_Unbounded_String (Path);
         -- parse creation options
         -- parse R/W options
         Agent_Chans(Chan_Num).Data_Sens := ((Mode and 7) = PARU_32.RTDS); -- TODO add the rest

         if Path(Path'First) = '@' then
            if (Path = "@CONSOLE") or (Path = "@INPUT") or (Path = "@OUTPUT") then
               Agent_Chans(Chan_Num).Con := Console;
               Agent_Chans(Chan_Num).Is_Console := true;
            else
               raise Not_Yet_Implemented with "Cannot handle unknown generic files";
            end if;
         elsif (Path(Path'First) /= ':') and (Per_Process_Data(PID_T(PID)).Virtual_Root /= "") then
             raise Not_Yet_Implemented with "Real (relative) file opening";
         else 
            raise Not_Yet_Implemented with "Real (abolute) file opening";
         end if;

         -- check for errors

         if Rec_Len > 0 then
            Agent_Chans(Chan_Num).Rec_Len := Rec_Len;
         end if;

         Agent_Chans(Chan_Num).Opener_PID := PID_T(PID); 

         Chan_No := Word_T(Chan_Num);

      end File_Open;

      procedure File_Close (Chan_No : in Word_T; Err : out Word_T) is
      begin
         Err := 0;
         if Chan_No /= 0 then
            if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
               Err := PARU_32.ERACU;
            else
               if Agent_Chans(Integer(Chan_No)).Is_Console then
                  null; -- ignore ?CLOSE on console device for now
               else
                  raise Not_Yet_Implemented with "Real file closing";
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

      procedure File_Write (Chan_No : in Word_T;
                              Is_Extended,
                              Is_Absolute,
                              Is_Dynamic,
                              Is_DataSens : in Boolean;
                              Rec_Len     : in Integer;
                              Bytes       : in Byte_Arr_T;
                              Position    : in Integer;
                              Transferred : out Word_T;
                              Err         : out Word_T) is
         Max_Len  : Integer;
         Too_Long : Boolean := false;
         DS_Len   : Integer;
      begin
         Err := 0;
         if Agent_Chans(Integer(Chan_No)).Opener_PID = 0 then
            raise Channel_Not_Open with "?WRITE";
         end if;
         if (not Is_Dynamic) and (Is_DataSens or Agent_Chans(Integer(Chan_No)).Data_Sens) then
            Max_Len := Rec_Len;
            if Rec_Len = -1 then
               Max_Len := Agent_Chans(Integer(Chan_No)).Rec_Len;
            end if;
            Get_Data_Sensitive_Portion (Bytes, Max_Len, DS_Len, Too_Long);
            Loggers.Debug_Print (Sc_Log,"----- Found D/S bytes:" & DS_Len'Image);
            if Too_Long then
               Err := PARU_32.ERLTL;
            end if;
         end if;
         if not Too_Long then
            if Agent_Chans(Integer(Chan_No)).Is_Console then
               if (not Is_Dynamic) and (Is_DataSens or Agent_Chans(Integer(Chan_No)).Data_Sens) then
                  for B in 0 .. DS_Len -1 loop
                     Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Bytes(B));
                  end loop;
                  Transferred := Integer_16_To_Word(Integer_16(DS_Len));
                  Loggers.Debug_Print (Sc_Log,"----- No of D/S bytes written:" & Transferred'Image);
               else
                  for B in Bytes'Range loop
                     Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Bytes(B));
                  end loop;
                  Transferred := Word_T(Bytes'Length);
               end if;
            else
               raise Not_Yet_Implemented with "?WRITE to real file";
            end if;
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

      procedure I_Lookup (PID : in Word_T; Filename : in String;
							Glob_Port : out Integer; 
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

      procedure Shared_Open (PID : in PID_T; S_Path : in String; Read_Only : in Boolean;
							        Chan_No : out Word_T; Err : out Word_T) is
         C      : Natural;
         SFile  : Page_IO.File_Type;
      begin
         Err := 0;
         C := Get_Free_Channel;
         Agent_Chans(C).Opener_PID := 0; -- ensure set to zero so it can be resused if this open fails
         Page_IO.Open (Agent_Chans(C).File_Shared, (if Read_Only then Page_IO.In_File else Page_IO.Inout_File), S_Path);
         Agent_Chans(C).Opener_PID := PID;
         Agent_Chans(C).Path       := To_Unbounded_String(S_Path);
         Agent_Chans(C).IS_Console := false;
         Agent_Chans(C).Read       := true;
         Agent_Chans(C).Write      := not Read_Only;
         Agent_Chans(C).For_Shared := true;
         Agent_Chans(C).Rec_Len    := 1204; -- not used
         Chan_No := Word_T(C);
      exception
         when others =>
            Err := PARU_32.ERFAD;
      end Shared_Open;
      
   end Actions;

end AOSVS.Agent;
