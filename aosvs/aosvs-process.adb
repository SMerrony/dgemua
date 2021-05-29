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

with AOSVS.Agent;
with Debug_Logs; use Debug_Logs;
with Memory;     use Memory;
with PARU_32;    use PARU_32;

package body AOSVS.Process is

    function Sys_DADID (CPU : in out CPU_T; PID : in Word_T) return Boolean is
    begin
        -- TODO fake response....
        Loggers.Debug_Print (Sc_Log, "?DADID (Faking response)");
        CPU.AC (0) := Dword_T (PID) - 1;
        return True;
    end Sys_DADID;

    function Sys_GUNM (CPU : in out CPU_T; PID : in Word_T) return Boolean is
        Pkt_Addr  : Phys_Addr_T := Phys_Addr_T (CPU.AC (2));
        User_Name : Unbounded_String;
    begin
        Loggers.Debug_Print (Sc_Log, "?GUNM");
        case CPU.AC (1) is
            when 16#ffff_ffff# =>
                raise AOSVS.Agent.Not_Yet_Implemented
                   with "?GUNM via target process";
            when 0 =>
                raise AOSVS.Agent.Not_Yet_Implemented with "?GUNM via PID";
            when others =>
                if CPU.AC (0) /= 16#ffff_ffff# then
                    Loggers.Debug_Print (Sc_Log, "----- Invalid parameters");
                    CPU.AC (0) := Dword_T (ERPNM);
                    return False;
                end if;
                User_Name := AOSVS.Agent.Actions.Get_User_Name (PID);
                RAM.Write_String_BA (CPU.AC (2), To_String (User_Name));
                CPU.AC (0) := 0;        -- Claim NOT to be in Superuser mode
                CPU.AC (1) := 16#001f#; -- Claim to have nearly all privileges
        end case;
        Loggers.Debug_Print
           (Sc_Log, "----- Returning: " & To_String (User_Name));
        return True;
    end Sys_GUNM;

    function Sys_SYSPRV (CPU : in out CPU_T; PID : in Word_T) return Boolean is
        Pkt_Addr : Phys_Addr_T := Phys_Addr_T (CPU.AC (2));
        P_FUNC   : Word_T      := RAM.Read_Word (Pkt_Addr + 2);
    begin
        Loggers.Debug_Print (Sc_Log, "?SYSPRV");
        -- TODO
        return True;
    end Sys_SYSPRV;

end AOSVS.Process;
