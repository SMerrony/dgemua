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

with Interfaces;  use Interfaces;

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
        Pkt_Addr    : constant Phys_Addr_T := CPU.AC_PA(2);
        Origin_Port : constant Dword_T     := RAM.Read_Dword (Pkt_Addr + PARU_32.IOPH);
        Dest_Port   : constant Word_T      := RAM.Read_Word  (Pkt_Addr + PARU_32.IDPN);
        Rx_Buff_Len : constant Unsigned_16 := Unsigned_16(RAM.Read_Word  (Pkt_Addr + PARU_32.ILTH));
        Buff_Addr   : Phys_Addr_T := Phys_Addr_T(RAM.Read_Dword (Pkt_Addr + PARU_32.IPTR));
    begin
        Loggers.Debug_Print (Sc_Log, "?IREC");
        Loggers.Debug_Print (Debug_Log, "?IREC");
        Dump_Packet (Pkt_Addr, PARU_32.IPRLTH);
        if (RAM.Read_Word (Pkt_Addr + PARU_32.ISFL) and PARU_32.IFNBK) = 0 then
            raise Not_Yet_Implemented with "wait for IPC";
        end if;
        -- special case where the caller is looking for a CLI-format args Message
        --if Origin_Port = 0 and Dest_Port = 0 then
        if Dest_Port = 0 then
            -- code similiar to ?GTMES in aosvs-system.AUTOBAUD
            Loggers.Debug_Print (Sc_Log, "----- Request is for initial CLI-format message");
            declare
                Res_US      : Unbounded_String;
                Num_Args    : Word_T;
            begin
                Res_US := AOSVS.Agent.Actions.Get_PR_Name (PID);
                Num_Args := Word_T(AOSVS.Agent.Actions.Get_Num_Args (PID));
                if Num_Args > 0 then
                    for A in 1 .. Num_Args loop
                    Res_US := Res_US & " " &  AOSVS.Agent.Actions.Get_Nth_Arg(PID, A);
                    end loop;
                end if;
                Res_US := Res_US & ASCII.NUL;
                -- write out word length of response
                RAM.Write_Word (Pkt_Addr + PARU_32.ILTH, Word_T((Length(Res_US)+1)/2));
                Buff_Addr := Shift_Left (Buff_Addr, 1); -- convert to byte address
                RAM.Write_String_BA (Dword_T(Buff_Addr), To_String (Res_US));
                Loggers.Debug_Print (Sc_Log, "----- Returning: " & To_String (Res_US));
            end;
        else
            raise Not_Yet_Implemented with "nowait, normal IPC";
        end if;      
        return true;
    end Sys_IREC;

end AOSVS.IPC;