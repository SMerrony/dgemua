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

with Ada.Exceptions;
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
     (PID : PID_T;
      -- TID : Word_T;
      Priority : Word_T; PR_Addrs : PR_Addrs_T;
      Console  : GNAT.Sockets.Stream_Access;
      Logging  : Boolean)
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

      Task_Data.Is_TLOCKed := false;

      AOSVS.Agent.Actions.Allocate_TID (PID, TID);
      Task_Data.TID  := TID;
      Task_Data.UTID := 1; -- FIXME UTID vs. TID
      Loggers.Debug_Print (Sc_Log, "... got TID:" & TID'Image);

      Initial_Task.Start (Task_Data, Console);

   end Create_Task;

   function Get_Unique_TID (PID : PID_T; TID : Word_T) return Word_T is
      -- (Shift_Left (Word_T(PID), 8) or TID);
      ( 1 );

   task body VS_Task is
      CPU          : Processor.CPU_T;
      Adj_WSFH     : Phys_Addr_T;
      I_Counts     : Processor.Instr_Count_T;
      Syscall_Trap : Boolean;
      Syscall_OK   : Boolean;
      Return_Addr  : Phys_Addr_T;
      Call_ID_Addr : Phys_Addr_T;
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
      accept Start (TD : Task_Data_T; Console : GNAT.Sockets.Stream_Access) do
         Set_PC (CPU, TD.Start_Addr);
         CPU.WFP  := TD.WFP;
         CPU.WSP  := TD.WSP;
         CPU.WSB  := TD.WSB;
         CPU.WSL  := TD.WSL;
         Adj_WSFH := (CPU.PC and 16#7000_0000#) or Memory.WSFH_Loc;
         RAM.Write_Word (Adj_WSFH, Word_T (TD.WSFH)); -- FIXME is this right???
         Loggers.Debug_Print (Sc_Log, "Adjusted WSFH set to: " & Dword_To_String (Dword_T(Adj_WSFH), Octal, 11, true) &
                                      " Containing: " &Word_To_String (RAM.Read_Word(Adj_WSFH), Octal, 6, true));
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
               Call_ID_Addr := Phys_Addr_T(RAM.Read_Dword (Phys_Addr_T (CPU.WSP - 2)));
               Call_ID := RAM.Read_Word(Call_ID_Addr);
               Loggers.Debug_Print (Sc_Log, "System Call # is: " & Dword_To_String (Dword_T(Call_ID), Octal, 6));
            end if;
            case Call_ID is
               when 8#000# => Syscall_OK := File_Management.Sys_CREATE (CPU, Task_Data.PID);
               when 8#001# => Syscall_OK := File_Management.Sys_DELETE (CPU, Task_Data.PID);
               when 8#002# => Syscall_OK := File_Management.Sys_RENAME (CPU, Task_Data.PID);
               when 8#003# => Syscall_OK := Sys_Memory.Sys_MEM  (CPU, Task_Data.PID, Task_Data.TID, Task_Data.Ring_Mask);
               when 8#007# => Syscall_OK := File_IO.Sys_RDB (CPU, Task_Data.PID);
               when 8#014# => Syscall_OK := Sys_Memory.Sys_MEMI (CPU, Task_Data.PID, Task_Data.TID, Task_Data.Ring_Mask);
               when 8#026# => Syscall_OK := IPC.Sys_IREC (CPU, Task_Data.PID);
               when 8#027# => Syscall_OK := IPC.Sys_ILKUP   (CPU, Task_Data.PID, Task_Data.TID);
               when 8#036# => Syscall_OK := System.Sys_GTOD (CPU);
               when 8#041# => Syscall_OK := System.Sys_GDAY (CPU);
               when 8#044# => Syscall_OK := Sys_Memory.Sys_SSHPT (CPU, Task_Data.PID, Task_Data.Ring_Mask);
               when 8#056# => Syscall_OK := File_IO.Sys_GOPEN (CPU, Task_Data.PID);
               when 8#057# => Syscall_OK := File_IO.Sys_GCLOSE (CPU, Task_Data.PID);
               when 8#060# => Syscall_OK := Sys_Memory.Sys_SPAGE (CPU, Task_Data.PID,Task_Data.TID);
               when 8#063# => Syscall_OK := Sys_Memory.Sys_SOPEN (CPU, Task_Data.PID,Task_Data.TID);
               when 8#072# => Syscall_OK := Process.Sys_GUNM     (CPU, Task_Data.PID);
               when 8#077# => Syscall_OK := File_Management.Sys_FSTAT (CPU, Task_Data.PID);
               when 8#073# => Syscall_OK := Sys_Memory.Sys_GSHPT(CPU, Task_Data.PID, Task_Data.Ring_Mask);
               when 8#102# => Syscall_OK := Process.Sys_GLIST (CPU, Task_Data.PID);
               when 8#111# => Syscall_OK := File_Management.Sys_GNAME (CPU, Task_Data.PID);
               when 8#113# => Syscall_OK := Process.Sys_SUSER (CPU, Task_Data.PID);
               when 8#116# => Syscall_OK := Process.Sys_PNAME (CPU, Task_Data.PID);
               when 8#127# => Syscall_OK := Process.Sys_DADID (CPU, Task_Data.PID);
               when 8#157# => Syscall_OK := System.Sys_SINFO  (CPU);
               -- when 8#161# => Syscall_OK := Sys_Memory.Sys_SCLOSE (CPU, Task_Data.PID,Task_Data.TID);
               when 8#166# => Syscall_OK := File_Management.Sys_DACL (CPU, Task_Data.PID);
               when 8#167# => Syscall_OK := Connection.Sys_CON   (CPU, Task_Data.PID);
               when 8#170# => Syscall_OK := Connection.Sys_DCON  (CPU, Task_Data.PID);
               when 8#171# => Syscall_OK := Connection.Sys_SERVE (CPU, Task_Data.PID);
               -- when 8#172# => Syscall_OK := Connection.Sys_RESIGN (CPU, Task_Data.PID);
               when 8#251# => Syscall_OK := Process.Sys_RNGPR    (CPU, Task_Data.PID);
               when 8#263# => Syscall_OK := Multitasking.Sys_WDELAY (CPU, Task_Data.PID, Task_Data.TID);
               when 8#300# => Syscall_OK := File_IO.Sys_OPEN  (CPU, Task_Data.PID, Task_Data.TID);
               when 8#301# => Syscall_OK := File_IO.Sys_CLOSE (CPU, Task_Data.PID, Task_Data.TID);
               when 8#302# => Syscall_OK := File_IO.Sys_READ  (CPU, Task_Data.PID, Task_Data.TID);
               --when 8#303# => Syscall_OK := raise System_Call_Not_Implemented; 
               when 8#303# => Syscall_OK := File_IO.Sys_WRITE (CPU, Task_Data.PID, Task_Data.TID, Task_Data.Debug_Logging);
                  --raise System_Call_Not_Implemented;
               when 8#307# => Syscall_OK := System.Sys_GTMES  (CPU, Task_Data.PID, Task_Data.TID);
               when 8#310# => -- ?RETURN - handled differently
                  Loggers.Debug_Print (Sc_Log, "?RETURN");
                  Error_Code  := CPU.AC(0); -- FIXME using this?
                  Flags       := Byte_T(Get_DW_Bits(CPU.AC(2), 16, 8));
                  Msg_Len     := Integer(Unsigned_8(Get_DW_Bits(CPU.AC(2), 24, 8)));
                  if Msg_Len > 0 then
                     Term_Msg := Byte_Arr_To_Unbounded(RAM.Read_Bytes_BA(CPU.AC(1), Msg_Len));
                  end if;
                  exit;
               when 8#311# => Syscall_OK := System.Sys_ERMSG (CPU);   
               when 8#312# => Syscall_OK := File_IO.Sys_GCHR (CPU, Task_Data.PID);
               when 8#313# => Syscall_OK := File_IO.Sys_SCHR (CPU, Task_Data.PID);
               when 8#316# => Syscall_OK := File_IO.Sys_SEND (CPU, Task_Data.PID);
               when 8#330# => Syscall_OK := System.Sys_EXEC  (CPU, Task_Data.PID, Task_Data.TID); -- !!!
               when 8#333# => Syscall_OK := Multitasking.Sys_UIDSTAT (CPU, Task_Data.PID, Task_Data.TID);
               when 8#336# => Syscall_OK := File_Management.Sys_RECREATE (CPU, Task_Data.PID);
               -- when 8#351# => Syscall_OK := ...Sys_GTNAM...
               when 8#476# => Syscall_OK := Multitasking.Sys_TLOCK (CPU, Task_Data.Is_TLOCKed);
               when 8#503# => Syscall_OK := Multitasking.Sys_PRI   (CPU, Task_Data.PID, Task_Data.TID);
               when 8#505# => Syscall_OK := Multitasking.Sys_KILAD (CPU, Task_Data.PID, Task_Data.Kill_Addr);
               when 8#525# => Syscall_OK := Multitasking.Sys_REC   (CPU, Task_Data.PID, Task_Data.TID);
               when 8#542# => Syscall_OK := Multitasking.Sys_IFPU  (CPU); 
               when 8#573# => Syscall_OK := Process.Sys_SYSPRV     (CPU, Task_Data.PID);
               -- when 8#576# => Syscall_OK := ?XPSTAT
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
            for I in I_Counts'Range loop
               if I_Counts(I) > 0 then
                  Ada.Text_IO.Put(I'Image & ASCII.HT & I_Counts(I)'Image & Dasher_NL);
               end if;
            end loop;
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
            for I in I_Counts'Range loop
               if I_Counts(I) > 0 then
                  Ada.Text_IO.Put(I'Image & ASCII.HT & I_Counts(I)'Image & Dasher_NL);
               end if;
            end loop;
   end VS_Task;

end AOSVS.Agent.Tasking;
