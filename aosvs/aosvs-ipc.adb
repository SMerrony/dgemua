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

with Ada.Characters.Handling;   use Ada.Characters.Handling;

with AOSVS.Agent;
with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with PARU_32;

package body AOSVS.IPC is

    function Sys_ILKUP  (CPU : CPU_T; PID, TID : Word_T) return Boolean is
        I_Name : constant String := To_Upper (RAM.Read_String_BA (CPU.AC(0), false));
        I_Path : constant String := To_String(Agent.Actions.Get_Virtual_Root) &
                           Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                           ":" & I_Name); 
        G_Port : Dword_T;
        F_Type : Word_T;
        Err    : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?ILKUP - filename: " & I_Name);
        Loggers.Debug_Print (Sc_Log, "------   Resolved to local file: " & I_Path);
        AOSVS.Agent.Actions.I_Lookup(PID, I_Path, G_Port, F_Type, Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        CPU.AC(1) := G_Port;
        CPU.AC(2) := Dword_T(F_Type);
        Loggers.Debug_Print (Sc_Log, "------ Global Port No. " & Dword_To_String(G_Port, binary, 32, true));
        return true;
    end Sys_ILKUP;

    function Sys_IREC   (CPU : CPU_T; PID : Word_T) return Boolean is
        -- TODO ?IREC is just stubbed for the simplest case
    begin
        Loggers.Debug_Print (Sc_Log, "?IREC");
        Loggers.Debug_Print (Debug_Log, "?IREC");
        Dump_Packet (CPU.AC_PA(2), PARU_32.IPRLTH);
        if (RAM.Read_Word (CPU.AC_PA(2) + PARU_32.ISFL) and PARU_32.IFNBK) = 0 then
            raise Not_Yet_Implemented with "wait for IPC";
        else
            CPU.AC_Wd(0) := 0; -- PARU_32.ERNOR;
            return false;
        end if;
        -- return true;
    end Sys_IREC;

end AOSVS.IPC;