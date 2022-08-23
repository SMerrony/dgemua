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


with Debug_Logs;    use Debug_Logs;
with Memory;        use Memory;

package body Resolver is

    function Resolve_8bit_Disp
       (CPU    : CPU_T; Indirect : Boolean; Mode : Mode_T;
        Disp15 : Integer_16) return Phys_Addr_T
    is
        Eff               : Phys_Addr_T;
        Ring              : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
        Ind_Addr          : Word_T;
        Indirection_Level : Integer     := 0;
    begin
        if Mode /= Absolute then
            -- relative mode, sign-extend to 32-bits
            Eff :=
               Integer_32_To_Phys
                  (Integer_32 (Disp15)); -- Disp15 is already sexted by decoder
        end if;
        case Mode is
            when Absolute =>
                Eff := Phys_Addr_T (Disp15) or Ring;
            when PC =>
                Eff := Eff + CPU.PC;
            when AC2 =>
                Eff := Eff + Phys_Addr_T (CPU.AC_I32(2));
            when AC3 =>
                Eff := Eff + Phys_Addr_T (CPU.AC_I32(3));
        end case;

        if Indirect then
            Eff      := Eff or Ring;
            Ind_Addr := RAM.Read_Word (Eff);
            while (Ind_Addr and 16#8000#) /= 0 loop
                Indirection_Level := Indirection_Level + 1;
                if Indirection_Level > 15 then
                    raise Indirection_Failure
                       with "Too many levels of indirection";
                end if;
                Ind_Addr := RAM.Read_Word (Phys_Addr_T (Ind_Addr) or Ring);
            end loop;
            Eff := Phys_Addr_T (Ind_Addr) or Ring;
        end if;

        if not CPU.ATU then
            -- constrain to 1st 32MB
            Eff := Eff and 16#01ff_ffff#;
        end if;

        return Eff;
    end Resolve_8bit_Disp;

    function Resolve_Eclipse_15bit_Disp (CPU        : CPU_T;
                                Indirect    : Boolean; 
                                Mode        : Mode_T;
                                Disp15      : Integer_16;
                                Disp_Offset : Integer_32) return Phys_Addr_T is
        Eff    : Phys_Addr_T;
        Ring   : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
        Ind_Addr : Word_T;
        Indirection_Level : Integer := 0;
    begin
        Loggers.Debug_Print (Debug_Log, "... Resolve_15 got - Indirect: "  & Indirect'Image &
                                        ", Mode: " & Mode'Image &
                                        ", Disp15:" & Disp15'Image &
                                        "., Disp Offset:" & Disp_Offset'Image);
        case Mode is
        when Absolute =>
            -- Just force to current ring
            Eff := (Phys_Addr_T(Disp15) and 16#0000_7fff#) or Ring;
        when PC =>
            Eff := Integer_32_To_Phys(Phys_To_Integer_32(CPU.PC) + Integer_32(Disp15) + Disp_Offset);
        when AC2 =>
            Eff := Integer_32_To_Phys(CPU.AC_I32(2) + Integer_32(Disp15)) or Ring;
        when AC3 =>
            Eff := Integer_32_To_Phys(CPU.AC_I32(3) + Integer_32(Disp15)) or Ring;
        end case;

        if Indirect then
            Loggers.Debug_Print (Debug_Log, "... Indirect addr resolving from : " & Dword_To_String (Dword_T(Eff), Octal, 11, true));

            Ind_Addr := RAM.Read_Word (Eff);
            while (Ind_Addr and 16#8000#) = 16#8000# loop
                Indirection_Level := Indirection_Level + 1;
                if Indirection_Level > 15 then
                    raise Indirection_Failure with "Too many levels of indirection";
                end if;
                Ind_Addr := RAM.Read_Word (Phys_Addr_T(Ind_Addr and 16#7fff#) or Ring);
                Loggers.Debug_Print (Debug_Log, "... Nested Indirect addr resolves to : " & Word_To_String (Ind_Addr, Octal, 11, true));
            end loop;
            Eff := Phys_Addr_T(Ind_Addr) or Ring;

            Loggers.Debug_Print (Debug_Log, "... Indirect addr resolves to    : " & Dword_To_String (Dword_T(Eff), Octal, 11, true));
        end if;

        if not CPU.ATU then
            -- constrain to 1st 32MB
            Loggers.Debug_Print (Debug_Log, "... ATU is OFF so constrained to to 1st 32MB");
            Eff := Eff and 16#01ff_ffff#;
        end if;

        return Eff;
    end Resolve_Eclipse_15bit_Disp;

    function Resolve_15bit_Disp (CPU        : CPU_T;
                                Indirect    : Boolean; 
                                Mode        : Mode_T;
                                Disp15      : Integer_16;
                                Disp_Offset : Integer_32) return Phys_Addr_T is
        Eff    : Phys_Addr_T;
        Ring   : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
        Ind_Addr : Dword_T;
        Indirection_Level : Integer := 0;
    begin
        Loggers.Debug_Print (Debug_Log, "... Resolve_15 got - Indirect: "  & Indirect'Image &
                                        ", Mode: " & Mode'Image &
                                        ", Disp15:" & Disp15'Image &
                                        "., Disp Offset:" & Disp_Offset'Image);
        case Mode is
        when Absolute =>
            Eff := (Phys_Addr_T(Disp15) and 16#0000_7fff#) or Ring;
        when PC =>
            Eff := Integer_32_To_Phys(Phys_To_Integer_32(CPU.PC) + Integer_32(Disp15) + Disp_Offset);
        when AC2 =>
            Eff := Integer_32_To_Phys(CPU.AC_I32(2) + Integer_32(Disp15)); -- or Ring;
        when AC3 =>
            Eff := Integer_32_To_Phys(CPU.AC_I32(3) + Integer_32(Disp15)); -- or Ring;
        end case;

        if Indirect then
            Loggers.Debug_Print (Debug_Log, "... Indirect addr resolving from : " & Dword_To_String (Dword_T(Eff), Octal, 11, true));
            Eff := Eff and 16#7fff_ffff#;
            Ind_Addr := RAM.Read_Dword (Eff);
            while (Ind_Addr and 16#8000_0000#) = 16#8000_0000# loop
                Indirection_Level := Indirection_Level + 1;
                if Indirection_Level > 15 then
                    raise Indirection_Failure with "Too many levels of indirection";
                end if;
                Ind_Addr := RAM.Read_Dword (Phys_Addr_T(Ind_Addr) and 16#7fff_ffff#);
                Loggers.Debug_Print (Debug_Log, "... Nested Indirect addr resolves to : " & Dword_To_String (Ind_Addr, Octal, 11, true));
            end loop;
            Eff := Phys_Addr_T(Ind_Addr); --  or Ring;

            Loggers.Debug_Print (Debug_Log, "... Indirect addr resolves to    : " & Dword_To_String (Dword_T(Eff), Octal, 11, true));
        end if;

        if not CPU.ATU then
            -- constrain to 1st 32MB
            Loggers.Debug_Print (Debug_Log, "... ATU is OFF so constrained to to 1st 32MB");
            Eff := Eff and 16#01ff_ffff#;
        end if;

        Loggers.Debug_Print (Debug_Log, "... Resolved to: " & Dword_To_String (Dword_T(Eff), Octal, 11, true));

        return Eff;
    end Resolve_15bit_Disp;

    function Resolve_31bit_Disp (CPU         : CPU_T;
                                 Indirect    : Boolean; 
                                 Mode        : Mode_T;
                                 Disp        : Integer_32;
                                 Disp_Offset : Integer_32) return Phys_Addr_T is
         Eff    : Phys_Addr_T;
         Ring   : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
         Ind_Addr : Dword_T;
         Indirection_Level : Integer := 0;
    begin

         case Mode is
            when Absolute =>
               -- Zero-extend to 28 bits
               Eff := Integer_32_To_Phys(Disp); --  or Ring;
            when PC =>
               Eff := Integer_32_To_Phys(Dword_To_Integer_32(Dword_T(CPU.PC)) + Disp + Disp_Offset);
            when AC2 =>
               Eff := Integer_32_To_Phys(CPU.AC_I32(2) + Disp);
            when AC3 =>
               Eff := Integer_32_To_Phys(CPU.AC_I32(3) + Disp);
         end case;

        --  --  debugging...
        --  if (Eff and 16#8000_0000#) = 16#8000_0000# then
        --      Loggers.Debug_Print (Debug_Log, "Mode: "& Mode'Image & " Disp: " & Disp'Image & "(10)");
        --      Loggers.Debug_Print (Debug_Log, "Mode: "& Mode'Image & " Disp: " & Dword_To_String (Dword_T(Eff), Hex, 8, true) & "(16)");
        --      raise Illegal_Address with "Illegal address during resolution: " & Dword_To_String (Dword_T(Eff), Hex, 8, true);
        --  end if;

         -- if Indirect or ((Eff and 16#8000_0000#) = 16#8000_0000#) then
         if Indirect then
            Eff := Eff and 16#7fff_ffff#;
            Eff := Eff or Ring;
            Ind_Addr := RAM.Read_Dword (Eff);
            while (Ind_Addr and 16#8000_0000#) /= 0 loop
               Indirection_Level := Indirection_Level + 1;
               if Indirection_Level > 15 then
                  raise Indirection_Failure with "Too many levels of indirection";
               end if;
               Ind_Addr := RAM.Read_Dword (Phys_Addr_T(Ind_Addr) and 16#7fff_ffff#);
            end loop;
            Eff := Phys_Addr_T(Ind_Addr) or Ring;
         end if;

         if not CPU.ATU then
            -- constrain to 1st 32MB
            Eff := Eff and 16#01ff_ffff#;
         end if;

         return Eff and 16#7fff_ffff#;
    end Resolve_31bit_Disp;

    function Resolve_32bit_Indirectable_Addr (ATU : Boolean; I_Addr : Dword_T) return Phys_Addr_T
    is
        Eff : Dword_T := I_Addr;
    begin
        while Test_DW_Bit (Eff, 0) loop
            Eff := RAM.Read_Dword (Phys_Addr_T (Eff and 16#7fff_ffff#));
        end loop;
        -- if not ATU then
        --     Eff := Eff and 16#01ff_ffff#;
        -- end if;
        return Phys_Addr_T (Eff);
    end Resolve_32bit_Indirectable_Addr;

    procedure Resolve_Eagle_Bit_Addr (CPU       : CPU_T; 
                                      Acd, Acs  : AC_ID; 
                                      Word_Addr : out Phys_Addr_T; 
                                      Bit_Num   : out Natural) is
    begin
        -- TODO handle segments and indirection
        if Acd = Acs then 
            Word_Addr := CPU.PC and 16#7000_0000#;
        else
            if Test_DW_Bit (CPU.AC(Acs), 0) then
                raise Not_Yet_Implemented with "Indirect 16-bit BIT pointers";
            end if;
            Word_Addr := Phys_Addr_T(CPU.AC(Acs));
        end if;
        Word_Addr := Word_Addr + Phys_Addr_T (Shift_Right (CPU.AC(Acd), 4));
        Bit_Num := Natural(CPU.AC(Acd) and 16#000f#);
    end Resolve_Eagle_Bit_Addr;

    procedure Resolve_Eclipse_Bit_Addr (CPU       : CPU_T; 
                                        Acd, Acs  : AC_ID; 
                                        Word_Addr : out Phys_Addr_T; 
                                        Bit_Num   : out Natural) is
    begin
        -- TODO handle segments and indirection
        if Acd = Acs then 
            Word_Addr := CPU.PC and 16#7000_0000#;
        else
            if Test_DW_Bit (CPU.AC(Acs), 0) then
                raise Not_Yet_Implemented with "Indirect 16-bit BIT pointers";
            end if;
            Word_Addr := Phys_Addr_T(CPU.AC(Acs)) and 16#0000_7fff#;
        end if;
        Word_Addr := Word_Addr + Phys_Addr_T (Shift_Right (CPU.AC(Acd), 4));
        Bit_Num := Natural(CPU.AC(Acd) and 16#000f#);
    end Resolve_Eclipse_Bit_Addr;

end Resolver;
