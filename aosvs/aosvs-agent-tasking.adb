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

with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with Interfaces; use Interfaces;

with AOSVS.Connection;
with AOSVS.File_IO;
with AOSVS.File_Management;
with AOSVS.IPC;
with AOSVS.Sys_Memory;
with AOSVS.Multitasking;
with AOSVS.Process;
with AOSVS.System;
with Debug_Logs; use Debug_Logs;
with Memory;     use Memory;
with Processor;  use Processor;
with Processor.Eagle_Stack_P;

package body AOSVS.Agent.Tasking is

   procedure Create_Task
     (PID : in PID_T;
      -- TID : in Word_T;
      Priority : in Word_T; PR_Addrs : in PR_Addrs_T;
      Console  : in GNAT.Sockets.Stream_Access;
      Logging  : in Boolean)
   is
      Task_Data    : Task_Data_T;
      TID          : Word_T;
      Initial_Task : VS_Task;
   begin
      Task_Data.PID := Word_T (PID);
      Loggers.Debug_Print (Sc_Log, "Setting up task for PID:" & PID'Image);
      Task_Data.Sixteen_Bit := False; -- TODO
      Task_Data.Start_Addr  := PR_Addrs.PR_Start;
      Task_Data.Ring_Mask   := PR_Addrs.PR_Start and 16#7000_0000#;
      Task_Data.WFP         :=
        (if PR_Addrs.WFP /= 0 then PR_Addrs.WFP else PR_Addrs.WSP);
      Task_Data.WSP  := PR_Addrs.WSP;
      Task_Data.WSB  := PR_Addrs.WSB;
      Task_Data.WSL  := PR_Addrs.WSL;
      Task_Data.WSFH := PR_Addrs.WSFH;

      Task_Data.Debug_Logging := Logging;

      AOSVS.Agent.Actions.Allocate_TID (PID, TID);
      if TID = 0 then
         raise NO_MORE_TIDS with "PID: " & PID'Image;
      end if;
      Task_Data.TID := TID;
      Loggers.Debug_Print (Sc_Log, "... got TID:" & TID'Image);

      Initial_Task.Start (Task_Data, Console);

   end Create_Task;

   function Get_Unique_TID (PID : in PID_T; TID : in Word_T) return Word_T is
      (Shift_Left(Word_T(PID), 8) or TID);

   task body VS_Task is
      CPU          : Processor.CPU_T;
      Adj_WSFH     : Phys_Addr_T;
      I_Counts     : Processor.Instr_Count_T;
      Syscall_Trap : Boolean;
      Syscall_OK   : Boolean;
      Return_Addr  : Phys_Addr_T;
      Call_ID      : Word_T;
      Task_Data    : Task_Data_T;
      Error_Code   : Dword_T;
      Flags        : Byte_T;
      Msg_Len      : Integer;
      Term_Msg     : Unbounded_String;
      Cons         : GNAT.Sockets.Stream_Access;
      Dummy        : Dword_T;
   begin
      CPU := Processor.Make;
      accept Start (TD : in Task_Data_T; Console : in GNAT.Sockets.Stream_Access) do
         Set_PC (CPU, TD.Start_Addr);
         CPU.WFP  := TD.WFP;
         CPU.WSP  := TD.WSP;
         CPU.WSB  := TD.WSB;
         CPU.WSL  := TD.WSL;
         Adj_WSFH := (CPU.PC and 16#7000_0000#) or Memory.WSFH_Loc;
         RAM.Write_Word (Adj_WSFH, Word_T (TD.WSFH)); -- FIXME is this right???
         Loggers.Debug_Print (Sc_Log, "Adjusted WSFH set to: " & Dword_To_String (Dword_T(Adj_WSFH), Hex, 8, true) &
                                      " Containing: " &Dword_To_String (RAM.Read_Dword(Adj_WSFH), Hex, 8, true));
         CPU.ATU := True;
         Set_Debug_Logging (CPU, TD.Debug_Logging);
         Task_Data := TD;
         Cons := Console;
      -- end Start;

      loop
         -- run the processor until a system call trap...
         Processor.VRun (CPU, TD.Debug_Logging, Octal, I_Counts, Syscall_Trap);

         if Syscall_Trap then
            Return_Addr := Phys_Addr_T (CPU.AC (3));
            if Task_Data.Sixteen_Bit then
               raise Processor.Not_Yet_Implemented with "16-bit task";
            else
               Call_ID := RAM.Read_Word(Phys_Addr_T(RAM.Read_Dword (Phys_Addr_T (CPU.WSP - 2))));
               -- Loggers.Debug_Print (Sc_Log, "System Call # is: " & Dword_To_String (Dword_T(Call_ID), Octal, 6));
            end if;
            case Call_ID is
               when 8#000# => Syscall_OK := AOSVS.File_Management.Sys_CREATE (CPU, Task_Data.PID);
               when 8#001# => Syscall_OK := AOSVS.File_Management.Sys_DELETE (CPU, Task_Data.PID);
               -- 2 RENAME
               when 8#003# => Syscall_OK := AOSVS.Sys_Memory.Sys_MEM  (CPU, Task_Data.PID, Task_Data.TID, Task_Data.Ring_Mask);
               when 8#014# => Syscall_OK := AOSVS.Sys_Memory.Sys_MEMI (CPU, Task_Data.PID, Task_Data.TID, Task_Data.Ring_Mask);
               when 8#027# => Syscall_OK := AOSVS.IPC.Sys_ILKUP   (CPU, Task_Data.PID, Task_Data.TID);
               when 8#036# => Syscall_OK := AOSVS.System.Sys_GTOD (CPU);
               when 8#041# => Syscall_OK := AOSVS.System.Sys_GDAY (CPU);
               when 8#044# => Syscall_OK := AOSVS.Sys_Memory.Sys_SSHPT (CPU, Task_Data.PID, Task_Data.Ring_Mask);
               when 8#060# => Syscall_OK := AOSVS.Sys_Memory.Sys_SPAGE (CPU, Task_Data.PID,Task_Data.TID);
               when 8#063# => Syscall_OK := AOSVS.Sys_Memory.Sys_SOPEN (CPU, Task_Data.PID,Task_Data.TID);
               when 8#072# => Syscall_OK := AOSVS.Process.Sys_GUNM     (CPU, Task_Data.PID);
               when 8#073# => Syscall_OK := AOSVS.Sys_Memory.Sys_GSHPT(CPU, Task_Data.PID, Task_Data.Ring_Mask);
               when 8#111# => Syscall_OK := AOSVS.File_Management.Sys_GNAME (CPU, Task_Data.PID);
               when 8#113# => Syscall_OK := AOSVS.Process.Sys_SUSER (CPU, Task_Data.PID);
               when 8#116# => Syscall_OK := AOSVS.Process.Sys_PNAME (CPU, Task_Data.PID);
               when 8#127# => Syscall_OK := AOSVS.Process.Sys_DADID (CPU, Task_Data.PID);
               when 8#157# => Syscall_OK := AOSVS.System.Sys_SINFO  (CPU);
               -- when 8#166# => Syscall_OK := AOSVS.File_Management.Sys_DACL (CPU, Task_Data.PID);
               when 8#167# => Syscall_OK := AOSVS.Connection.Sys_CON (CPU, Task_Data.PID);
               when 8#170# => Syscall_OK := AOSVS.Connection.Sys_DCON (CPU, Task_Data.PID);
               when 8#171# => Syscall_OK := AOSVS.Connection.Sys_SERVE (CPU, Task_Data.PID);
               when 8#251# => Syscall_OK := AOSVS.Process.Sys_RNGPR    (CPU, Task_Data.PID);
               when 8#263# => Syscall_OK := AOSVS.Multitasking.Sys_WDELAY (CPU, Task_Data.PID, Task_Data.TID);
               when 8#300# => Syscall_OK := AOSVS.File_IO.Sys_OPEN  (CPU, Task_Data.PID, Task_Data.TID);
               when 8#301# => Syscall_OK := AOSVS.File_IO.Sys_CLOSE (CPU, Task_Data.PID, Task_Data.TID);
               when 8#302# => Syscall_OK := AOSVS.File_IO.Sys_READ  (CPU, Task_Data.PID, Task_Data.TID);
               when 8#303# => Syscall_OK := AOSVS.File_IO.Sys_WRITE (CPU, Task_Data.PID, Task_Data.TID, Task_Data.Debug_Logging);
               when 8#307# => Syscall_OK := AOSVS.System.Sys_GTMES  (CPU, Task_Data.PID, Task_Data.TID);
               when 8#310# => -- ?RETURN - handled differently
                  Loggers.Debug_Print (Sc_Log, "?RETURN");
                  Error_Code  := CPU.AC(0);
                  Flags       := Byte_T(Get_DW_Bits(CPU.AC(2), 16, 8));
                  Msg_Len     := Integer(Unsigned_8(Get_DW_Bits(CPU.AC(2), 24, 8)));
                  if Msg_Len > 0 then
                     Term_Msg := Byte_Arr_To_Unbounded(RAM.Read_Bytes_BA(CPU.AC(1), Msg_Len));
                  end if;
                  exit;
               when 8#311# => Syscall_OK := AOSVS.System.Sys_ERMSG (CPU);   
               when 8#312# => Syscall_OK := AOSVS.File_IO.Sys_GCHR (CPU, Task_Data.PID);
               when 8#313# => Syscall_OK := AOSVS.File_IO.Sys_SCHR (CPU, Task_Data.PID);
               when 8#316# => Syscall_OK := AOSVS.File_IO.Sys_SEND (CPU, Task_Data.PID);
               when 8#330# => Syscall_OK := AOSVS.System.Sys_EXEC  (CPU, Task_Data.PID, Task_Data.TID); -- !!!
               when 8#333# => Syscall_OK := AOSVS.Multitasking.Sys_UIDSTAT (CPU, Task_Data.PID, Task_Data.TID);
               when 8#336# => Syscall_OK := AOSVS.File_Management.Sys_RECREATE (CPU, Task_Data.PID);
               when 8#505# => Syscall_OK := AOSVS.Multitasking.Sys_KILAD (CPU, Task_Data.PID, Task_Data.Kill_Addr);
               when 8#525# => Syscall_OK := AOSVS.Multitasking.Sys_REC   (CPU, Task_Data.PID, Task_Data.TID);
               when 8#542# => Syscall_OK := AOSVS.Multitasking.Sys_IFPU  (CPU); 
               when 8#573# => Syscall_OK := AOSVS.Process.Sys_SYSPRV     (CPU, Task_Data.PID);
               when others =>
                  raise System_Call_Not_Implemented with "Octal call #:" & Word_To_String(Call_ID, Octal, 5);
            end case;
            Processor.Eagle_Stack_P.WS_Pop(CPU, Dummy);
            CPU.AC(3) := Dword_T(CPU.WFP);
            if Syscall_OK then
               CPU.PC := Return_Addr + 1; -- normal return
            else
               CPU.PC := Return_Addr;     -- error return
            end if;
         else
            -- VRun has stopped, but we're not at a system call...
            exit;
         end if;
      end loop;

      if Length(Term_Msg) > 0 then
         for C of To_String(Term_Msg) loop
            Character'Output(Cons, C);
         end loop;
      end if;

      end Start; -- just for testing

      exception
         when E : others =>
            Loggers.Debug_Logs_Dump ("logs/");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message(E));
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

   end VS_Task;

end AOSVS.Agent.Tasking;
