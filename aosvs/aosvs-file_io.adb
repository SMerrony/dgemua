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

with Ada.Characters.Handling;

with AOSVS.Agent;
with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with PARU_32;     use PARU_32;

package body AOSVS.File_IO is

    function Sys_OPEN (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Chan_No, Err : Word_T;
        Pkt_Addr  : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        File_Spec : Word_T      := RAM.Read_Word(Pkt_Addr + ISTI);
        File_Type : Word_T      := RAM.Read_Word(Pkt_Addr + ISTO);
        Rec_Len   : Integer     := Integer(Word_To_Integer_16(RAM.Read_Word(Pkt_Addr + IRCL)));
        Path_Name : Dword_T     := RAM.Read_Dword(Pkt_Addr + IFNP);
        Path      : String      := Ada.Characters.Handling.To_Upper(RAM.Read_String_BA(Path_Name));
    begin
        Loggers.Debug_Print (Sc_Log, "?OPEN Pathname: " & Path);
        AOSVS.Agent.Actions.File_Open (PID, Path, File_Spec, Rec_Len, Chan_No, Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        RAM.Write_Word(Pkt_Addr + ICH, Chan_No);
        Loggers.Debug_Print (Sc_Log, "----- Returned channel No. " & Chan_No'Image);
        return true;
    end Sys_OPEN;

    function Sys_CLOSE (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Pkt_Addr    : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        Chan_No     : Word_T      := RAM.Read_Word(Pkt_Addr + ICH);
        Err         : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?CLOSE");
        AOSVS.Agent.Actions.File_Close(Chan_No, Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        return true;
    end Sys_Close;

    function Sys_WRITE (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Pkt_Addr    : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        Chan_No     : Word_T      := RAM.Read_Word(Pkt_Addr + ICH);
        File_Spec   : Word_T      := RAM.Read_Word(Pkt_Addr + ISTI);
        Is_Extd     : Boolean     := ((File_Spec and Word_T(IPKL)) /= 0);
        Is_Abs      : Boolean     := ((File_Spec and Word_T(IPST)) /= 0);
        Is_DataSens : Boolean     := ((File_Spec and Word_T(RTDS)) /= 0);
        Rec_Len     : Integer     := Integer(Word_To_Integer_16(RAM.Read_Word(Pkt_Addr + IRCL)));
        Bytes       : Byte_Arr_T(0 .. Rec_Len - 1) := RAM.Read_Bytes_BA (RAM.Read_Dword(Pkt_Addr + IBAD), Rec_Len);
        Position    : Integer     := Integer(RAM.Read_Dword(Pkt_Addr + IRNH));
        Txfrd, Err  : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?WRITE");
        AOSVS.Agent.Actions.File_Write (Chan_No,
                                        Is_Extd,
                                        Is_Abs,
                                        Is_DataSens,
                                        Rec_Len,
                                        Bytes,
                                        Position,
                                        Txfrd,
                                        Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        RAM.Write_Word(Pkt_Addr + IRLR, Txfrd);
        return true;
    end Sys_Write;

end AOSVS.File_IO;