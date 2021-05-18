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

package body AOSVS.System is

    function Sys_GTMES (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Pkt_Addr    : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        P_Greq      : Word_T      := RAM.Read_Word (Pkt_Addr + GREQ);
        P_Gnum      : Word_T      := RAM.Read_Word (Pkt_Addr + GNUM);
        Err         : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?GTMES");
        Err := 0;
        case P_Greq is
        when GARG =>
            declare
               Arg_US  : Unbounded_String := AOSVS.Agent.Actions.Get_Nth_Arg(PID, P_Gnum);
               Arg_Int : Integer;
            begin
               Arg_Int := Integer'Value(To_String(Arg_US));
               -- no exception - it's a simple integer argument
               CPU.AC(1) := Dword_T(Arg_Int);
               CPU.AC(0) := Dword_T(Length (Arg_US));
            exception
               when others =>
                  -- not an integer...
                  CPU.AC(1) := 16#FFFF_FFFF#;
                  CPU.AC(0) := Dword_T(Length (Arg_US));
                  RAM.Write_String_BA (RAM.Read_Dword(Pkt_Addr + GRES), To_String (Arg_US));
            end;
        when others =>
            raise AOSVS.Agent.Not_Yet_Implemented with "?GTMES request type " & P_Greq'Image;
        end case;
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        return true;
    end Sys_GTMES;

end AOSVS.System;