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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Interfaces;  use Interfaces;

with AOSVS.Agent;
with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with PARU_32;

package body AOSVS.Sys_Memory is

   function Sys_GSHPT (CPU : in out CPU_T; PID : in Word_T; Ring : in Phys_Addr_T) return Boolean is
   begin
      Loggers.Debug_Print (Sc_Log, "?GSHPT");
      CPU.AC(0) := RAM.Get_First_Shared_Page and 16#0003_ffff#;
      CPU.AC(1) := RAM.Get_Num_Shared_Pages;
      Loggers.Debug_Print (Sc_Log, "----- First: " & Dword_To_String (CPU.AC(0), Hex, 8) & 
                           "(Hex), Number: " & Dword_To_String (CPU.AC(1), Decimal, 8) & "(Dec.)");
      return true;
   end Sys_GSHPT;

   function Sys_MEM (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T; Ring_Mask : in Phys_Addr_T) return Boolean is
      I32 : Integer_32 := Integer_32(RAM.Get_First_Shared_Page) - Integer_32(RAM.Get_Last_Unshared_Page) - 4;
   begin
      Loggers.Debug_Print (Debug_Log, "?MEM"); Loggers.Debug_Print (Sc_Log, "?MEM");
      -- No. Unshared Pages Available
      CPU.AC(0) := (if I32 < 0 then 0 else Dword_T(I32)); -- Not sure why we need the 4-page gap...
      -- No. Unshared Pages currently in use
      CPU.AC(1) := RAM.Get_Num_Unshared_Pages;
      -- Hignest Unshared addr in logical addr space 
      CPU.AC(2) := ((RAM.Get_Last_Unshared_Page - 1) * Dword_T(Memory.Words_Per_Page)) or Dword_T(Ring_Mask); 
      return true;
   end Sys_MEM;

   function Sys_MEMI (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T; Ring_Mask : in Phys_Addr_T) return Boolean is
      Change : Integer_32 := Dword_To_Integer_32(CPU.AC(0));
      Last_Unshared : Dword_T := RAM.Get_Last_Unshared_Page;
   begin
      Loggers.Debug_Print (Debug_Log, "?MEMI"); Loggers.Debug_Print (Sc_Log, "?MEMI");
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
            Loggers.Debug_Print (Sc_Log, "----- Mapped page : " & Dword_To_String (Last_Unshared, Hex, 8));
         end loop;
         CPU.AC(1) := ((RAM.Get_Last_Unshared_Page * Dword_T(Memory.Words_Per_Page)) - 1) or Dword_T(Ring_Mask); 
         Loggers.Debug_Print (Sc_Log, "----- Mapped for addresses up to " & Dword_To_String (CPU.AC(1), Octal, 11));
      elsif Change < 0 then
         -- removing pages
         raise Processor.Not_Yet_Implemented with "Removing unshared pages with ?MEMI";
      else
         Loggers.Debug_Print (Sc_Log, "----- AC0 was zero - doing nothing");
      end if;
      return true;
   end Sys_MEMI;

   function Sys_SOPEN (CPU : in out CPU_T; PID, TID : in Word_T) return Boolean is
      SO_Name : String := To_Upper (RAM.Read_String_BA (CPU.AC(0), false));
      SO_Path : String := To_String(Agent.Actions.Get_Virtual_Root) &
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

   function Sys_SPAGE (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
      Chan_No     : Natural     := Natural(Lower_Word (CPU.AC(1)));
      Pkt_Addr    : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
      Mem_Pages   : Natural     := Natural(RAM.Read_Word(Pkt_Addr + PARU_32.PSTI) and 16#00ff#) / 4;
      Page_Arr    : Page_Arr_T(1 .. Mem_Pages);
      Start_Addr  : Phys_Addr_T := Phys_Addr_T(RAM.Read_Dword(Pkt_Addr + PARU_32.PCAD));
      Start_Block : Natural     := Natural(RAM.Read_Dword(Pkt_Addr + PARU_32.PRNH)); -- Disk block addr (not page #)
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

   function Sys_SSHPT (CPU : in out CPU_T; PID : in Word_T; Ring : in Phys_Addr_T) return Boolean is
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