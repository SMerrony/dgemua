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
with Ada.Directories;
with Ada.Text_IO;

with AOSVS.Agent;
with Debug_Logs;    use Debug_Logs;
with Memory;        use Memory;
with PARU_32;

package body AOSVS.File_Management is

    function Sys_CREATE (CPU : in out CPU_T; PID : in Word_T) return Boolean is
    
    begin
        Loggers.Debug_Print (Sc_Log, "?CREATE");
        raise AOSVS.Agent.Not_Yet_Implemented with "?CREATE";
        return true;
    end Sys_CREATE;

    function Sys_DELETE (CPU : in out CPU_T; PID : in Word_T) return Boolean is
    begin
        Loggers.Debug_Print (Sc_Log, "?DELETE");
        if CPU.AC(0) = 0 then
            raise AOSVS.Agent.Not_Yet_Implemented with "?DELETE via channel";
        end if;
        declare
            D_Name : String := To_Upper (RAM.Read_String_BA (CPU.AC(0)));
            D_Path : String := Agent.Actions.Get_Working_Directory(PID) & "/" & D_Name; 
        begin
            if not Ada.Directories.Exists(D_Path) then
                CPU.AC(0) := Dword_T(PARU_32.ERFDE);
                return false;
            end if;
            Ada.Directories.Delete_File(D_Path);
        exception
            when others =>
                CPU.AC(0) := Dword_T(PARU_32.ERWAD); -- Write access denied...
                return false;
        end;
        return true;
    end Sys_DELETE;

    function Sys_RECREATE (CPU : in out CPU_T; PID : in Word_T) return Boolean is
        R_Name : String := To_Upper (RAM.Read_String_BA (CPU.AC(0)));
        R_Path : String := Agent.Actions.Get_Working_Directory(PID) & "/" & R_Name; 
        R_New  : Ada.Text_IO.File_Type;
    begin
        Loggers.Debug_Print (Sc_Log, "?RECREATE file: " & R_Path);
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

end AOSVS.File_Management;