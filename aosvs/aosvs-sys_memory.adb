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

with Interfaces; use Interfaces;

with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with PARU_32;

package body AOSVS.Sys_Memory is

    function Sys_MEM (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T; Ring_Mask : in Phys_Addr_T) return Boolean is
    begin
        Loggers.Debug_Print (Debug_Log, "?MEM"); Loggers.Debug_Print (Sc_Log, "?MEM");
        -- No. Unshared Pages Available
        CPU.AC(0) := RAM.Get_First_Shared_Page - RAM.Get_Last_Unshared_Page - 4; -- Not sure why we need th 4-page gap...
        -- No. Unshared Pages currently in use
        CPU.AC(1) := (RAM.Get_Last_Unshared_Page - 
                      (Dword_T(Ring_Mask) / Dword_T(Memory.Words_Per_Page))) / 
                     Dword_T(Memory.Words_Per_Page);
        -- Hignest Unshared addr in logical addr space 
        CPU.AC(2) := (RAM.Get_Last_Unshared_Page * Dword_T(Memory.Words_Per_Page)) or Dword_T(Ring_Mask); 
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
          CPU.AC(1) := (RAM.Get_Last_Unshared_Page * Dword_T(Memory.Words_Per_Page)) or Dword_T(Ring_Mask); 
       elsif Change < 0 then
          -- removing pages
          raise Processor.Not_Yet_Implemented with "Removing unshared pages with ?MEMI";
       else
          Loggers.Debug_Print (Sc_Log, "----- AC0 was zero - doing nothing");
       end if;
       return true;
    end Sys_MEMI;

end AOSVS.Sys_Memory;