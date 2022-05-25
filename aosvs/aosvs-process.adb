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

with AOSVS.Agent;
with Debug_Logs; use Debug_Logs;
with Memory;     use Memory;
with PARU_32;    use PARU_32;

package body AOSVS.Process is

    function Sys_DADID (CPU : CPU_T; PID : Word_T) return Boolean is
    begin
        -- TODO fake response....
        Loggers.Debug_Print (Sc_Log, "?DADID (Faking response)");
        CPU.AC (1) := Dword_T (PID) - 1;
        return True;
    end Sys_DADID;

    function Sys_GLIST  (CPU : CPU_T; PID : Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?GLIST");
        Loggers.Debug_Print (Debug_Log, "?GLIST");
        RAM.Write_String_BA (CPU.AC(1), To_String (AOSVS.Agent.Actions.Get_Searchlist (PID_T(PID))));
        Loggers.Debug_Print (Sc_Log, "------ Returning: " & To_String (AOSVS.Agent.Actions.Get_Searchlist (PID_T(PID))));
        return True;
    end Sys_GLIST;

    function Sys_GUNM (CPU : CPU_T; PID : Word_T) return Boolean is
        User_Name : Unbounded_String;
    begin
        Loggers.Debug_Print (Sc_Log, "?GUNM");
        case CPU.AC (1) is
            when 16#ffff_ffff# =>
                raise AOSVS.Agent.Not_Yet_Implemented
                   with "?GUNM via target process";
            when 0 =>
                if CPU.AC(0) = 16#ffff_ffff# then
                    User_Name := AOSVS.Agent.Actions.Get_User_Name (PID);
                    RAM.Write_String_BA (CPU.AC (2), To_String (User_Name));
                    CPU.AC (0) := 0;        -- Claim NOT to be in Superuser mode
                    CPU.AC (1) := 16#001f#; -- Claim to have nearly all privileges
                else  
                    raise AOSVS.Agent.Not_Yet_Implemented with "?GUNM via another PID"; 
                end if;
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

    function Sys_PNAME  (CPU : CPU_T; PID : Word_T) return Boolean is
        Pname_BA : constant Dword_T := CPU.AC(0);
    begin
        Loggers.Debug_Print (Sc_Log, "?PNAME");
        case CPU.AC(1) is
            when 16#ffff_ffff# =>   -- get own PID
                CPU.AC(1) := Dword_T(PID);
                if Pname_BA /= 0 then
                    RAM.Write_String_BA (Pname_BA,Agent.Actions.Get_Proc_Name(PID_T(PID)));
                end if;
            when 0 =>               -- get PID of named target process
                raise AOSVS.Agent.Not_Yet_Implemented with "?PNAME for named proc";
            when others =>          -- get proc name for given PID in AC0
                raise AOSVS.Agent.Not_Yet_Implemented with "?PNAME for another PID";
        end case;
        return true;
    end Sys_PNAME;

    function Sys_RNGPR  (CPU : CPU_T; PID : Word_T) return Boolean is
        Pkt_Addr  : constant Phys_Addr_T := Phys_Addr_T (CPU.AC (2));
        Buff_BA   : constant Dword_T     := RAM.Read_Dword (Pkt_Addr + RNGBP);
        Ring_Num  : constant Integer     := Integer(Word_To_Integer_16(RAM.Read_Word (Pkt_Addr + RNGNM)));
        Buff_Len  : constant Integer     := Integer(Word_To_Integer_16(RAM.Read_Word (Pkt_Addr + RNGLB)));
    begin
        Loggers.Debug_Print (Sc_Log, "?RNGPR - Ring: " & Ring_Num'Image);
        if CPU.AC(0) /= 16#ffff_ffff# then
            raise Not_Yet_Implemented with "?RNGPR for other/named procs";
        end if;
        declare 
            PR_S : constant Unbounded_String := ':' & AOSVS.Agent.Actions.Get_PR_Name(PID);
        begin
            if Length(PR_S) > Buff_Len then
                CPU.AC(0) := Dword_T(ERIRB);
                return false;
            end if;
            RAM.Write_String_BA (Buff_BA, To_String(PR_S));
            RAM.Write_Word (Pkt_Addr + RNGLB, Word_T(Length(PR_S)));
            Loggers.Debug_Print (Sc_Log, "------ Returning: " & To_String(PR_S));
        end;
        return true;
    end Sys_RNGPR;

    function Sys_SUSER (CPU : CPU_T; PID : Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?SUSER");
        case CPU.AC(0) is
            when 0 =>
                CPU.AC(0) := (if AOSVS.Agent.Actions.Get_Superuser(PID) then 16#ffff_ffff# else 1);
            when 16#ffff_ffff# =>
                AOSVS.Agent.Actions.Set_Superuser(PID, true);
                Loggers.Debug_Print (Sc_Log, "------ Superuser turned on");
            when 1 =>
                AOSVS.Agent.Actions.Set_Superuser(PID, false);
                Loggers.Debug_Print (Sc_Log, "------ Superuser turned off");
            when others =>
                CPU.AC(0) := Dword_T(ERPRE);
                return false;
        end case;
        return true;
    end Sys_SUSER;

    function Sys_SYSPRV (CPU : CPU_T; PID : Word_T) return Boolean is
        Pkt_Addr : constant Phys_Addr_T := Phys_Addr_T (CPU.AC (2));
        P_FUNC   : constant Word_T      := RAM.Read_Word (Pkt_Addr + 2);
    begin
        Loggers.Debug_Print (Sc_Log, "?SYSPRV");
        -- TODO
        return True;
    end Sys_SYSPRV;

end AOSVS.Process;
