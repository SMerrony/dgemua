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

with Ada.Calendar;
with Ada.Characters.Handling;

with GNAT.Calendar;

with Interfaces;  use Interfaces;

with AOSVS.Agent;
with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with PARU_32;     use PARU_32;

package body AOSVS.System is

    function Sys_ERMSG  (CPU : in out CPU_T) return Boolean is
        Supplied_Buf_Len : Unsigned_8 := Unsigned_8(Get_DW_Bits(CPU.AC(1), 16, 8));
        ERMES_Chan       : Unsigned_8 := Unsigned_8(Get_DW_Bits(CPU.AC(1), 24, 8));
        Default_Response : String := "UNKNOWN ERROR CODE " & CPU.AC(0)'Image;
    begin
        Loggers.Debug_Print (Sc_Log, "?ERMSG - Octal code: " & Dword_To_String (CPU.AC(0), Octal, 8));
        if ERMES_Chan /= 255 then
            raise Not_Yet_Implemented with "Custom ERMES files";
        end if;
        -- TODO - actually look it up!
        RAM.Write_String_BA (CPU.AC(2), Default_Response);
        CPU.AC(0) := Dword_T(Default_Response'Last);
        return true;
    end Sys_ERMSG;

    function Sys_EXEC (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Pkt_Addr    : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        XRFNC       : Word_T      := RAM.Read_Word (Pkt_Addr);
        BP          : DWord_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?EXEC");
        case XRFNC is
            when XFSTS =>
                RAM.Write_Word (Pkt_Addr + XFP1, PID);
                BP := RAM.Read_Dword(Pkt_Addr + XFP2);
                if BP /= 0 then
                    RAM.Write_String_BA (BP, "CON10"); -- TODO always claims to be @CON10
                end if;
            when others =>
                raise AOSVS.Agent.Not_Yet_Implemented with "Function code:" & XRFNC'Image;
        end case;
        return true;
    end Sys_EXEC;

    function Sys_GDAY (CPU : in out CPU_T) return Boolean is
        Now : Ada.Calendar.Time := Ada.Calendar.Clock; 
    begin
        Loggers.Debug_Print (Sc_Log, "?GDAY");
        CPU.AC(0) := Dword_T(Ada.Calendar.Day(Now));
        CPU.AC(1) := Dword_T(Ada.Calendar.Month(Now));
        CPU.AC(2) := Dword_T(Ada.Calendar.Year(Now) - 1900); -- Should be -1900, but Y2K!
        return true;
    end Sys_GDAY;

    function Sys_GTMES (CPU : in out CPU_T; PID : in Word_T; TID : in Word_T) return Boolean is
        Pkt_Addr    : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        P_Greq      : Word_T      := RAM.Read_Word (Pkt_Addr + GREQ);
        P_Gnum      : Word_T      := RAM.Read_Word (Pkt_Addr + GNUM);
        P_Gres      : Dword_T;
        Err         : Word_T;
        Res_US      : Unbounded_String;
        Num_Args    : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?GTMES");
        Err := 0;
        case P_Greq is
        when GMES =>
            Loggers.Debug_Print (Sc_Log, "------ Req. Type: ?GMES");
            Res_US := AOSVS.Agent.Actions.Get_PR_Name (PID);
            Num_Args := Word_T(AOSVS.Agent.Actions.Get_Num_Args (PID));
            if Num_Args > 0 then
                for A in 1 .. Num_Args loop
                   Res_US := Res_US & " " &  AOSVS.Agent.Actions.Get_Nth_Arg(PID, A);
                end loop;
            end if;
            Res_US := Res_US & ASCII.NUL;
            CPU.AC(0) := Dword_T((Length(Res_US)+1)/2); -- word length
            CPU.AC(1) := Dword_T(GFCF);
            P_Gres := RAM.Read_Dword (Pkt_Addr + PARU_32.GRES);
            if P_Gres /= 16#ffff_ffff# then
                RAM.Write_String_BA (P_Gres, To_String(Res_US));
            end if;
            Loggers.Debug_Print (Sc_Log, "----- Returning: " & To_String(Res_US));

        when GCMD =>
            Loggers.Debug_Print (Sc_Log, "------ Req. Type: ?GCMD");
            -- Procs launched interactively do not return the program name part of the command...
            -- Res_US := AOSVS.Agent.Actions.Get_PR_Name (PID);
            Num_Args := Word_T(AOSVS.Agent.Actions.Get_Num_Args (PID));
            if Num_Args > 0 then
                for A in 1 .. Num_Args loop
                   if A > 1 then
                      Res_US := Res_US & " ";
                   end if;
                   Res_US :=  Res_US & AOSVS.Agent.Actions.Get_Nth_Arg(PID, A);
                end loop;
            end if;
            -- Res_US := Res_US & ASCII.NUL;
            CPU.AC(1) := Dword_T((Length(Res_US))); -- byte length
            P_Gres := RAM.Read_Dword (Pkt_Addr + PARU_32.GRES);
            if P_Gres /= 16#ffff_ffff# then
                RAM.Write_String_BA (P_Gres, To_String(Res_US));
            end if;
            Loggers.Debug_Print (Sc_Log, "----- Returning: " & To_String(Res_US));

        when GCNT =>
            Loggers.Debug_Print (Sc_Log, "------ Req. Type: ?GCNT");
            Num_Args := Word_T(AOSVS.Agent.Actions.Get_Num_Args (PID)) - 1;
            CPU.AC(0) := Dword_T(Num_Args);
            Loggers.Debug_Print (Sc_Log, "----- Returning Arg Count:" & Num_Args'Image);

        when GARG =>
            declare
               Arg_US  : Unbounded_String := AOSVS.Agent.Actions.Get_Nth_Arg(PID, P_Gnum);
               Arg_Int : Integer;
            begin
               Loggers.Debug_Print (Sc_Log, "------ Req. Type: ?GARG");
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

    function Sys_GTOD (CPU : in out CPU_T) return Boolean is
        Now : Ada.Calendar.Time := Ada.Calendar.Clock; 
    begin
        Loggers.Debug_Print (Sc_Log, "?GTOD");
        Loggers.Debug_Print (Debug_Log, "?GTOD");
        CPU.AC(0) := Dword_T(GNAT.Calendar.Second(Now));
        CPU.AC(1) := Dword_T(GNAT.Calendar.Minute(Now));
        CPU.AC(2) := Dword_T(GNAT.Calendar.Hour(Now));
        return true;
    end Sys_GTOD;

    function Sys_SINFO  (CPU : in out CPU_T) return Boolean is
        Pkt_Addr    : Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        P_SIRS      : Word_T := RAM.Read_Word (Pkt_Addr + SIRS);
    begin
        Loggers.Debug_Print (Sc_Log, "?SINFO");
        RAM.Write_Word (Pkt_Addr + SIRN, 16#07_49#); -- AOS/VS Version faked to 7.73
        RAM.Write_Dword (Pkt_Addr + SIMM, 255); -- max mem page - check this
        if RAM.Read_Dword (Pkt_Addr + SILN) /= 0 then
            RAM.Write_String_BA (RAM.Read_Dword (Pkt_Addr + SILN), "MASTERLDU");
        end if;
        if RAM.Read_Dword (Pkt_Addr + SIID) /= 0 then
            RAM.Write_String_BA (RAM.Read_Dword (Pkt_Addr + SIID), "VSEMUA");
        end if;
        if RAM.Read_Dword (Pkt_Addr + SIOS) /= 0 then
            RAM.Write_String_BA (RAM.Read_Dword (Pkt_Addr + SIOS), ":VSEMUA");
        end if;
        RAM.Write_Word(Pkt_Addr + SSIN, SAVS);
        return true;
    end Sys_SINFO;

end AOSVS.System;