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

with AOSVS.Agent.Tasking;
with Debug_Logs; use Debug_Logs;
with Memory;     use Memory;
with PARU_32;    use PARU_32;

package body AOSVS.Multitasking is

    function Sys_IFPU (CPU : CPU_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?IFPU");
        Loggers.Debug_Print (Debug_Log, "?IFPU");
        -- STUB
        return true;
    end Sys_IFPU;

    function Sys_KILAD (CPU : CPU_T; PID : Word_T; Kill_Addr : out Phys_Addr_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?KILAD");
        Loggers.Debug_Print (Debug_Log, "?KILAD");
        Kill_Addr := Phys_Addr_T(RAM.Read_Dword(Phys_Addr_T(CPU.AC(0))));
        return true;
    end Sys_KILAD;

    function Sys_PRI (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?PRI");
        Loggers.Debug_Print (Debug_Log, "?PRI");
        Loggers.Debug_Print (Sc_Log, "---- PID:" & PID'Image & " TID:" & TID'Image &
                             " Requested new Priority:" & CPU.AC_I32(0)'Image &
                             " ***IGNORING***");
        return true;
    end Sys_PRI;

    function Sys_REC (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?REC");
        raise Not_Yet_Implemented with "?REC - NYI";
        return true;
    end Sys_REC;

    function Sys_TLOCK (CPU : CPU_T; Is_Locked : in out Boolean) return Boolean is
    begin
        if Is_Locked then
            CPU.AC_Wd(0) :=  PARU_32.TALOCK or PARU_32.TMYRING; --  TODO not exactly right...
        end if;
        Is_Locked := not Is_Locked;
        return true;
    end Sys_TLOCK;

    function Sys_UIDSTAT (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
        Req_TID  : constant Dword_T     := CPU.AC(1);
        Pkt_Addr : constant Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
    begin
        Loggers.Debug_Print (Sc_Log, "?UIDSTAT");
        Loggers.Debug_Print (Debug_Log, "?UIDSTAT");
        Loggers.Debug_Print (Sc_Log, "-------- Returning UTID: " & Word_To_String(Agent.Tasking.Get_Unique_TID(PID_T(PID), TID), Decimal, 2) &
            ", STID: " & Word_To_String(TID, Decimal, 2));
        if Req_TID /= 16#ffff_ffff# then
            raise Not_Yet_Implemented with "?UIDSTAT for another TID";
        end if;
        RAM.Write_Word(Pkt_Addr + UUID, Agent.Tasking.Get_Unique_TID(PID_T(PID), TID));
        RAM.Write_Word(Pkt_Addr + UTSTAT, 0);
        RAM.Write_Word(Pkt_Addr + UTID, TID);
        RAM.Write_Word(Pkt_Addr + UTPRI, 0);
        return true;
    end Sys_UIDSTAT;

    function Sys_WDELAY (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
        Int_Delay : constant Integer := Integer(Dword_To_Integer_32(CPU.AC(0)));
        Secs      : constant Duration   := Duration(0.001) * Int_Delay;
    begin
        Loggers.Debug_Print (Sc_Log, "?WDELAY - Secs:" & Secs'Image);
        Loggers.Debug_Print (Debug_Log, "?WDELAY");
        delay Secs;
        return true;
    end Sys_WDELAY;

end AOSVS.Multitasking;