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

with AOSVS.Agent.Tasking;
with Debug_Logs; use Debug_Logs;
with Memory;     use Memory;
with PARU_32;

package body AOSVS.Multitasking is

    function Sys_KILAD (CPU : in out CPU_T; PID : in Word_T; Kill_Addr : out Phys_Addr_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?KILAD");
        Kill_Addr := Phys_Addr_T(RAM.Read_Dword(Phys_Addr_T(CPU.AC(0))));
        return true;
    end Sys_KILAD;

    function Sys_UIDSTAT (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Req_TID  : Dword_T     := CPU.AC(1);
        Pkt_Addr : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
    begin
        Loggers.Debug_Print (Sc_Log, "?UIDSTAT");
        if Req_TID /= 16#ffff_ffff# then
            raise Not_Yet_Implemented with "?UIDSTAT for another TID";
        end if;
        RAM.Write_Word(Pkt_Addr, Agent.Tasking.Get_Unique_TID(PID_T(PID), TID));
        RAM.Write_Word(Pkt_Addr + 1, 0);
        RAM.Write_Word(Pkt_Addr + 2, TID);
        RAM.Write_Word(Pkt_Addr + 3, 0);
        return true;
    end Sys_UIDSTAT;

    function Sys_WDELAY (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Int_Delay : Integer := Integer(Dword_To_Integer_32(CPU.AC(0)));
        Secs      : Duration   := Duration(0.001) * Int_Delay;
    begin
        Loggers.Debug_Print (Sc_Log, "?WDELAY");
        delay Secs;
        return true;
    end Sys_WDELAY;

end AOSVS.Multitasking;