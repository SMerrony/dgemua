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

with PARU_32;

package body AOSVS.Agent is

   protected body Actions is

      Procedure Init (Cons : in GNAT.Sockets.Stream_Access) is
      begin
         -- Fake some in-use PIDs 
         for P in PID_T'Range loop
            PIDs_In_Use(P) := (P < 5);
         end loop;
         Console := Cons;
      end Init;
      		
      procedure Allocate_PID (Invocation_Args : in Args_Arr;
								Virtual_Root    : in Unbounded_String;
								Sixteen_Bit     : in Boolean;
								Proc_Name       : in Unbounded_String;
								-- Console         : in GNAT.Sockets.Stream_Access;
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
         Per_Process_Data(PID).Invocation_Args := Invocation_Args;
         Per_Process_Data(PID).Virtual_Root    := Virtual_Root;
         Per_Process_Data(PID).Sixteen_Bit     := Sixteen_Bit;
         Per_Process_Data(PID).Proc_Name       := Proc_Name;
         Per_Process_Data(PID).Console         := Console;
         Put_Line ("DEBUG: AGENT: Assigned PID " & PID'Image &
                   " to Process Name: " & To_String(Proc_Name));
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

		procedure File_Open (PID     : in Word_T; 
                           Path    : in String;
                           Mode    : in Word_T;
                           Rec_Len : in Integer;
                           Chan_No : out Word_T;
                           Err     : out Word_T) is
         Ag_Chan : Agent_Channel_T;
      begin
         Err := 0;
         Ag_Chan.Path := To_Unbounded_String (Path);
         -- parse creation options
         -- parse R/W options

         if Path(Path'First) = '@' then
            if (Path = "@CONSOLE") or (Path = "@INPUT") or (Path = "@OUTPUT") then
               Ag_Chan.Con := Console;
               Ag_Chan.Is_Console := true;
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
            Ag_Chan.Rec_Len := Rec_Len;
         end if;
         Chan_No := 0;
         for C in Agent_Chans'Range loop
            if Agent_Chans(C).Opener_PID = 0 then
               Agent_Chans(C) := Ag_Chan;
               Chan_No := Word_T(C);
               exit;
            end if;
         end loop;
         if Chan_No = 0 then
            raise NO_MORE_CHANNELS;
         end if;
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

      procedure File_Write (Chan_No : in Word_T;
                              Is_Extended,
                              Is_Absolute,
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
         if Is_DataSens then
            Max_Len := Rec_Len;
            if Rec_Len = -1 then
               Max_Len := Agent_Chans(Integer(Chan_No)).Rec_Len;
            end if;
            Get_Data_Sensitive_Portion (Bytes, Max_Len, DS_Len, Too_Long);
            if Too_Long then
               Err := PARU_32.ERLTL;
            end if;
         end if;
         if not Too_Long then
            if Agent_Chans(Integer(Chan_No)).Is_Console then
               if Is_DataSens then
                  for B in 0 .. DS_Len loop
                     Byte_T'Write (Agent_Chans(Integer(Chan_No)).Con, Bytes(B));
                  end loop;
                  Transferred := Word_T(DS_Len);
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

		function Get_Nth_Arg (PID : in Word_T; Arg_Num : in Word_T) return Unbounded_String is
         Nth : Integer := Integer(Arg_Num);
      begin
         if Nth > Per_Process_Data(PID_T(PID)).Invocation_Args'Last then
            raise No_Such_Argument;
         end if;
         return Per_Process_Data(PID_T(PID)).Invocation_Args(Nth);
      end Get_Nth_Arg;

      function Get_Num_Args (PID : in Word_T) return Dword_T is
         (Dword_T(Per_Process_Data(PID_T(PID_T(PID))).Invocation_Args'Length));

   end Actions;

end AOSVS.Agent;