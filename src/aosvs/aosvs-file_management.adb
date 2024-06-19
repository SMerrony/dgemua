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
with Ada.Directories;
with Ada.Text_IO;

with AOSVS.Agent;
with Debug_Logs;    use Debug_Logs;
with Memory;        use Memory;
with PARU_32;

package body AOSVS.File_Management is

    function Sys_CREATE (CPU : CPU_T; PID : Word_T) return Boolean is
        C_Name     : constant String := To_Upper (RAM.Read_String_BA (CPU.AC(0), false));
        C_Path     : constant String := To_String(Agent.Actions.Get_Virtual_Root) &
                               Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                               ":" & C_Name); 
        Pkt_Addr   : constant Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        File_Type  : constant Word_T := RAM.Read_Word (Pkt_Addr + PARU_32.CFTYP) and 16#00ff#;
        Err        : Word_T := 0;
    begin
        Loggers.Debug_Print (Sc_Log, "?CREATE - filename: " & C_Name & " Type No." & File_Type'Image);
        Loggers.Debug_Print (Sc_Log, "-------   Resolved to local file: " & C_Path);
        case File_Type is
            when PARU_32.FIPC =>
                declare
                   Local_Port : constant Word_T := RAM.Read_Word (Pkt_Addr + PARU_32.CPOR);
                begin
                   if Local_Port = 0 then
                      CPU.AC(0) := Dword_T(PARU_32.ERIVP);
                      return false;
                   end if;
                   AOSVS.Agent.Actions.I_Create(PID, C_Path, Local_Port, Err);
                   if Err /= 0 then
                      CPU.AC(0) := Dword_T(Err);
                      return false;
                   end if;
                end;
            when others =>
                raise AOSVS.Agent.Not_Yet_Implemented with "?CREATE type No." & File_Type'Image;
        end case;
        return true;
    end Sys_CREATE;

    function Sys_DACL (CPU : CPU_T; PID : Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?DACL");
        if CPU.AC(0) /= 0 then
            raise AOSVS.Agent.Not_Yet_Implemented with "?DACL set or change mode";
        end if;
        RAM.Write_String_BA (CPU.AC(1), AOSVS.Agent.Actions.Get_Default_ACL (PID_T(PID)));
        CPU.AC(0) := 1; -- Default ACL is ON
        Loggers.Debug_Print (Sc_Log, "----- Returning: " & AOSVS.Agent.Actions.Get_Default_ACL (PID_T(PID)));
        return True;
    end Sys_DACL;

    function Sys_DELETE (CPU : CPU_T; PID : Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?DELETE");
        if CPU.AC(0) = 0 then
            raise AOSVS.Agent.Not_Yet_Implemented with "?DELETE via channel";
        end if;
        declare
            D_Name : constant String := To_Upper (RAM.Read_String_BA (CPU.AC(0), false));
            D_Path : constant String := Agent.Actions.Get_Working_Directory(PID) & "/" & D_Name; 
        begin
            if not Ada.Directories.Exists(D_Path) then
                CPU.AC(0) := Dword_T(PARU_32.ERFDE);
                return false;
            end if;
            Ada.Directories.Delete_File(D_Path);
        exception
            when others =>
                CPU.AC(0) := Dword_T(PARU_32.ERWAD); -- Write access denied...
                Loggers.Debug_Print (Sc_Log, "------- Failed!");
                return false;
        end;
        return true;
    end Sys_DELETE;

    function Sys_FSTAT    (CPU : CPU_T; PID : Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?FSTAT");
        Loggers.Debug_Print (Debug_Log, "?FSTAT");
        if Test_DW_Bit (CPU.AC(1), 0) then
            --  AC0 should contain a channel number
            null; --  FIXME
        else
            --  AC0 should contain a b.p. to a Pathname
            null; --  FIXME
        end if;
        return true;
    end Sys_FSTAT;



    function Sys_GNAME (CPU : CPU_T; PID : Word_T) return Boolean is
        In_Name_BA  : constant Dword_T := CPU.AC(0);
        Out_Name_BA : constant Dword_T := CPU.AC(1);
        Out_Buflen  : constant Natural := Natural(CPU.AC(2));
        In_Name_Str : constant String  := RAM.Read_String_BA(In_Name_BA, false);
        Tmp_US      : Unbounded_String;
    begin
        Loggers.Debug_Print (Sc_Log, "?GNAME for: '" & In_Name_Str & "'");
        if In_Name_Str = "=" then
            declare
               CWD : constant String := Colonify_Path (Agent.Actions.Get_Working_Directory (PID));
            begin
               if CWD'Length > Out_Buflen then
                  CPU.AC(0) := Dword_T(PARU_32.ERIRB);
                  return false;
               end if;
               RAM.Write_String_BA(Out_Name_BA, CWD);
               CPU.AC(2) := Dword_T(CWD'Length);
            end;
        else 
            if In_Name_Str(In_Name_Str'First) = '@' then
                Tmp_US := To_Unbounded_String (":PER:" & Colonify_Path(In_Name_Str));
            else
                declare
                   Tmp_File : Ada.Text_IO.File_Type;
                begin
                   Ada.Text_IO.Open (Tmp_File,  Ada.Text_IO.In_File, In_Name_Str);
                   Ada.Text_IO.Close (Tmp_File);
                   Tmp_US := To_Unbounded_String (Colonify_Path(In_Name_Str));
                   if Length(Tmp_US) > Out_Buflen then
                      CPU.AC(0) := Dword_T(PARU_32.ERIRB);
                   return false;
               end if;
                exception
                   when others =>
                      CPU.AC(0) := Dword_T(PARU_32.ERFDE); -- always returning not exist on error...
                      return false;
                end;
            end if;
            RAM.Write_String_BA(Out_Name_BA, To_String(Tmp_US));
            CPU.AC(2) := Dword_T(Length(Tmp_US));
        end if;
        Loggers.Debug_Print (Sc_Log, "------ Returning: '" & RAM.Read_String_BA(Out_Name_BA, false) & 
            "', Length: " & CPU.AC(2)'Image);
        return true;
    end Sys_GNAME;

    function Sys_RECREATE (CPU : CPU_T; PID : Word_T) return Boolean is
        R_Name : constant String := To_Upper (RAM.Read_String_BA (CPU.AC(0), false));
        R_Path : constant String := To_String(Agent.Actions.Get_Virtual_Root) &
                           Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                           ":" & R_Name); 
        R_New  : Ada.Text_IO.File_Type;
    begin
        Loggers.Debug_Print (Sc_Log, "?RECREATE file: " & R_Name);
        Loggers.Debug_Print (Sc_Log, "--------- Resolved to local file: " & R_Path);
        if not Ada.Directories.Exists(R_Path) then
            CPU.AC(0) := Dword_T(PARU_32.ERFDE);
            return false;
        end if;
        Ada.Directories.Delete_File(R_Path);
        Ada.Text_IO.Create(R_New, Ada.Text_IO.Out_File, R_Path);
        Ada.Text_IO.Close(R_New);
        return true;
    exception
        when others =>
            CPU.AC(0) := Dword_T(PARU_32.ERFAD); -- File access denied...
            return false;
    end Sys_RECREATE;

    function Sys_RENAME (CPU : CPU_T; PID : Word_T) return Boolean is
        New_Name : constant String := To_Upper (RAM.Read_String_BA (CPU.AC(1), false));
        New_Path : constant String := To_String(Agent.Actions.Get_Virtual_Root) &
                           Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                           ":" & New_Name);
    begin
        Loggers.Debug_Print (Sc_Log, "?RENAME");
        Loggers.Debug_Print (Debug_Log, "?RENAME");
        if CPU.AC(0) = 0 then
            raise Not_Yet_Implemented with "NYI - ?RENAME packet";
        end if;
        declare
            Old_Name : constant String := To_Upper (RAM.Read_String_BA (CPU.AC(0), false));
            Old_Path : constant String := To_String(Agent.Actions.Get_Virtual_Root) &
                           Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                           ":" & Old_Name);
        begin
            Loggers.Debug_Print (Sc_Log, "------- Old name: " & Old_Name & " to: " & New_Name);
            Loggers.Debug_Print (Sc_Log, "------- Old path: " & Old_Path & " to: " & New_Path);
            Ada.Directories.Rename (Old_Path, New_Path);
        end;
        return true;
    end Sys_RENAME;

end AOSVS.File_Management;