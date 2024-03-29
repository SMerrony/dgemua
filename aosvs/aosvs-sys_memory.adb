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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Interfaces;  use Interfaces;

with AOSVS.Agent;
with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with PARU_32;

package body AOSVS.Sys_Memory is

   function Sys_GSHPT (CPU : CPU_T; PID : Word_T; Ring : Phys_Addr_T) return Boolean is
   begin
      Loggers.Debug_Print (Sc_Log, "?GSHPT");
      CPU.AC(0) := RAM.Get_First_Shared_Page and 16#0003_ffff#;
      CPU.AC(1) := RAM.Get_Num_Shared_Pages;
      Loggers.Debug_Print (Sc_Log, "----- First: " & Dword_To_String (CPU.AC(0), Hex, 8) & 
                           "(Hex), Number: " & Dword_To_String (CPU.AC(1), Decimal, 8) & "(Dec.)");
      return true;
   end Sys_GSHPT;

   function Sys_MEM (CPU : CPU_T; PID : Word_T; TID : Word_T; Ring_Mask : Phys_Addr_T) return Boolean is
      I32 : constant Integer_32 := Integer_32(RAM.Get_First_Shared_Page) - Integer_32(RAM.Get_Last_Unshared_Page) - 4;
      Max_Unsh_Addr : Phys_Addr_T;
   begin
      Loggers.Debug_Print (Debug_Log, "?MEM"); Loggers.Debug_Print (Sc_Log, "?MEM");
      -- No. Unshared Pages Available
      CPU.AC(0) := (if I32 < 0 then 0 else Dword_T(I32)); -- Not sure why we need the 4-page gap...
      -- No. Unshared Pages currently in use
      CPU.AC(1) := RAM.Get_Num_Unshared_Pages;
      -- Hignest Unshared addr in logical addr space 
      Max_Unsh_Addr := (Phys_Addr_T (Shift_Left (RAM.Get_Last_Unshared_Page + 1, 10)) - 1) or Ring_Mask;
      CPU.AC(2) := Dword_T(Max_Unsh_Addr);
      Loggers.Debug_Print (Sc_Log, "---- Unshared available:" &  CPU.AC_I32(0)'Image &
                                   " Unshared in use:" & CPU.AC_I32(1)'Image &
                                   " Highest unshared address: " & Dword_To_String (CPU.AC(2), Octal, 11, true));
      return true;
   end Sys_MEM;

   function Sys_MEMI (CPU : CPU_T; PID : Word_T; TID : Word_T; Ring_Mask : Phys_Addr_T) return Boolean is
      Change : constant Integer_32 := Dword_To_Integer_32(CPU.AC(0));
      Last_Unshared : Dword_T := RAM.Get_Last_Unshared_Page;
      Max_Unsh_Addr : Phys_Addr_T;
   begin
      Loggers.Debug_Print (Debug_Log, "?MEMI" & Change'Image); 
      Loggers.Debug_Print (Sc_Log, "?MEMI" & Change'Image);
      Loggers.Debug_Print (Sc_Log, "----- Before: Last Unshared page: " &  Dword_To_String (RAM.Get_Last_Unshared_Page, Hex, 8) &
                           " 1st shared page: " & Dword_To_String (RAM.Get_First_Shared_Page, Hex, 8));
      if Change > 0 then
         -- adding pages
         if Dword_T(Change) > (RAM.Get_First_Shared_Page - RAM.Get_Last_Unshared_Page) then
         CPU.AC(0) := Dword_T(PARU_32.ERMEM);
         Loggers.Debug_Print (Sc_Log, "----- Requested more pages than available");
         return false;
         end if;
         for P in 1 .. Integer(Change) loop
            Last_Unshared := Last_Unshared + 1;
            RAM.Map_Page (Natural(Last_Unshared), false);  
            -- Loggers.Debug_Print (Sc_Log, "----- Mapped page : " & Dword_To_String (Last_Unshared, Hex, 8));
         end loop;
         Max_Unsh_Addr := (Phys_Addr_T (Shift_Left (RAM.Get_Last_Unshared_Page + 1, 10)) - 1) or Ring_Mask;
         -- CPU.AC(1) := ((RAM.Get_Last_Unshared_Page * Dword_T(Memory.Words_Per_Page)) - 1) or Dword_T(Ring_Mask); 
         CPU.AC(1) := Dword_T (Max_Unsh_Addr);
         Loggers.Debug_Print (Sc_Log, "----- After:  Last Unshared page: " &  Dword_To_String (RAM.Get_Last_Unshared_Page, Hex, 8) &
                           " 1st shared page: " & Dword_To_String (RAM.Get_First_Shared_Page, Hex, 8));
         -- Loggers.Debug_Print (Sc_Log, "----- Mapped for addresses up to " & Dword_To_String (Dword_T(Max_Unsh_Addr), Octal, 11));                  
         Loggers.Debug_Print (Sc_Log, "----- Mapped for addresses up to " & Dword_To_String (CPU.AC(1), Octal, 11));
      elsif Change < 0 then
         -- removing pages
         raise Processor.Not_Yet_Implemented with "Removing unshared pages with ?MEMI";
      else
         Loggers.Debug_Print (Sc_Log, "----- AC0 was zero - changing nothing");
         Max_Unsh_Addr := (Phys_Addr_T (Shift_Left (RAM.Get_Last_Unshared_Page + 1, 10)) - 1) or Ring_Mask;
         CPU.AC(1) := Dword_T (Max_Unsh_Addr);
      end if;
      return true;
   end Sys_MEMI;

   function Sys_SOPEN (CPU : CPU_T; PID, TID : Word_T) return Boolean is
      SO_Name : constant String := To_Upper (RAM.Read_String_BA (CPU.AC(0), false));
      SO_Path : constant String := To_String(Agent.Actions.Get_Virtual_Root) &
                           Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                           ":" & SO_Name); 
      Chan_No, Err : Word_T;
   begin
      Loggers.Debug_Print (Sc_Log, "?SOPEN Path: " & SO_Name);
      Loggers.Debug_Print (Sc_Log, "------ Resolved to local file: " & SO_Path);
      if CPU.AC(1) /= 16#ffff_ffff# then
         raise Processor.Not_Yet_Implemented with "?SOPEN of specific channel";
      end if;
      Agent.Actions.Shared_Open (PID_T(PID), SO_Path, (CPU.AC(2) = 0), Chan_No, Err);
      if Err /= 0 then
         CPU.AC(0) := Dword_T(Err);
         return false;
      end if;
      CPU.AC(1) := Dword_T(Chan_No);
      Loggers.Debug_Print (Sc_Log, "---- Allocated channel No." & Chan_No'Image);
      return true;
   end Sys_Sopen;

   function Sys_SPAGE (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
      Chan_No     : constant Natural     := Natural(Lower_Word (CPU.AC(1)));
      Pkt_Addr    : constant Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
      Mem_Pages   : constant Natural     := Natural(RAM.Read_Word(Pkt_Addr + PARU_32.PSTI) and 16#00ff#) / 4;
      Page_Arr    : Page_Arr_T(1 .. Mem_Pages);
      Start_Addr  : constant Phys_Addr_T := Phys_Addr_T(RAM.Read_Dword(Pkt_Addr + PARU_32.PCAD));
      Start_Block : constant Natural     := Natural(RAM.Read_Dword(Pkt_Addr + PARU_32.PRNH)); -- Disk block addr (not page #)
      Err         : Word_T;
   begin
      Loggers.Debug_Print (Sc_Log, "?SPAGE - Channel No." & Chan_No'Image);
      Loggers.Debug_Print (Sc_Log, "------ - Mem Pages. " & Mem_Pages'Image);
      Loggers.Debug_Print (Sc_Log, "------ - Base Addr. " & Dword_To_String (RAM.Read_Dword(Pkt_Addr + PARU_32.PCAD), Octal, 11));
      Loggers.Debug_Print (Sc_Log, "------ - 1st Block. " & Dword_To_String (RAM.Read_Dword(Pkt_Addr + PARU_32.PRNH), Octal, 11));
      Agent.Actions.Shared_Read (PID_T(PID), Chan_No, Start_Addr, Mem_Pages, Start_Block, Page_Arr, Err);
      if Err /= 0 then
         CPU.AC(0) := Dword_T(Err);
         return false;
      end if;

      return true;
   end Sys_SPAGE;

   function Sys_SSHPT (CPU : CPU_T; PID : Word_T; Ring : Phys_Addr_T) return Boolean is
      Increase, Page_Num : Integer;
   begin
      Loggers.Debug_Print (Sc_Log, "?SSHPT");
      Loggers.Debug_Print (Sc_Log, "----- First: " & Dword_To_String (CPU.AC(0), Hex, 8) & 
                           "(Hex), Number: " & Dword_To_String (CPU.AC(1), Decimal, 8) & "(Dec.)");
      if CPU.AC(1) < RAM.Get_Num_Shared_Pages then
         raise Processor.Not_Yet_Implemented with "Removing shared pages via ?SSHPT";
      end if;
      Increase := Integer(CPU.AC(1)) - Integer(RAM.Get_Num_Shared_Pages);
      Loggers.Debug_Print (Sc_Log, "----- Change Requested:" & Increase'Image & "(Dec.)");
      -- At the beginning?
      Page_Num := Integer(CPU.AC(0));
      if CPU.AC(0) < (RAM.Get_First_Shared_Page and 16#0003_ffff#) then
         for P in 0 .. Increase - 1 loop
            if RAM.Page_Mapped (Page_Num + P) then
               CPU.AC(0) := Dword_T(PARU_32.ERMEM);
               return false;
            end if;
            RAM.Map_Page (P, true);
         end loop;
      else -- add at end
         while RAM.Get_Num_Shared_Pages < CPU.AC(1) loop
            RAM.Map_Page (Integer(RAM.Get_First_Shared_Page + RAM.Get_Num_Shared_Pages), true);
            Loggers.Debug_Print (Sc_Log, "----- Mapping Page:" & 
               Dword_To_String(RAM.Get_First_Shared_Page + RAM.Get_Num_Shared_Pages, Hex, 8));
         end loop;
      end if;
      return true;
   end Sys_SSHPT;
end AOSVS.Sys_Memory;