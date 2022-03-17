-- MIT License

-- Copyright ©2021,2022 Stephen Merrony

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

with Debug_Logs;    use Debug_Logs;
with Memory;        use Memory;

package body Resolver is

    function Resolve_8bit_Disp
       (CPU    : in CPU_T; Indirect : in Boolean; Mode : in Mode_T;
        Disp15 : in Integer_16) return Phys_Addr_T
    is
        Eff               : Phys_Addr_T;
        Ring              : Phys_Addr_T := CPU.PC and 16#7000_0000#;
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

    function Resolve_15bit_Disp (CPU        : in CPU_T;
                                Indirect    : in Boolean; 
                                Mode        : in Mode_T;
                                Disp15      : in Integer_16;
                                Disp_Offset : in Natural) return Phys_Addr_T is
        Eff    : Phys_Addr_T;
        Ring   : Phys_Addr_T := CPU.PC and 16#7000_0000#;
        Disp32 : Integer_32;
        Ind_Addr : Dword_T;
        Indirection_Level : Integer := 0;
    begin
        if Mode /= Absolute then
            -- relative mode, sign-extend to 32-bits
            Disp32 := Integer_32(Disp15); -- Disp15 is already sexted by decoder
        end if;
        case Mode is
        when Absolute =>
            -- Zero-extend to 28 bits, force to current ring
            Eff := (Phys_Addr_T(Disp15) and 16#0000_7fff#) or Ring;
        when PC =>
            Eff := Integer_32_To_Phys(Integer_32(CPU.PC) + Disp32 + Integer_32(Disp_Offset));
        when AC2 =>
            Eff := Phys_Addr_T(CPU.AC_I32(2) + Disp32) or Ring;
        when AC3 =>
            Eff := Phys_Addr_T(CPU.AC_I32(3) + Disp32) or Ring;   
        end case;

        if Indirect then
            Eff := Eff or Ring;
            Loggers.Debug_Print (Debug_Log, "... Indirect addr resolves from : " & Dword_To_String (Dword_T(Eff), Octal, 11, true));
            Ind_Addr := RAM.Read_Dword (Eff);
            while (Ind_Addr and 16#8000_0000#) /= 0 loop
                Indirection_Level := Indirection_Level + 1;
                if Indirection_Level > 15 then
                    raise Indirection_Failure with "Too many levels of indirection";
                end if;
                Ind_Addr := RAM.Read_Dword (Phys_Addr_T(Ind_Addr) and 16#7fff_ffff#);
                Loggers.Debug_Print (Debug_Log, "... Nested Indirect addr resolves to : " & Dword_To_String (Dword_T(Ind_Addr), Octal, 11, true));
            end loop;
            Eff := Phys_Addr_T(Ind_Addr) or Ring;
            Loggers.Debug_Print (Debug_Log, "... Indirect addr resolves to   : " & Dword_To_String (Dword_T(Eff), Octal, 11, true));
        end if;

        if not CPU.ATU then
            -- constrain to 1st 32MB
            Eff := Eff and 16#01ff_ffff#;
        end if;

        return Eff;
    end Resolve_15bit_Disp;

    function Resolve_31bit_Disp (CPU         : in CPU_T;
                                 Indirect    : in Boolean; 
                                 Mode        : in Mode_T;
                                 Disp        : in Integer_32;
                                 Disp_Offset : in Natural) return Phys_Addr_T is
         Eff    : Phys_Addr_T;
         Ring   : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         Ind_Addr : Dword_T;
         Indirection_Level : Integer := 0;
      begin
         case Mode is
            when Absolute =>
               -- Zero-extend to 28 bits
               Eff := Phys_Addr_T(Integer_32_To_Dword(Disp)); --  or Ring;
            when PC =>
               Eff := Phys_Addr_T(Integer_32_To_Dword(Dword_To_Integer_32(Dword_T(CPU.PC)) + Disp + Integer_32(Disp_Offset)));
            when AC2 =>
               Eff := Phys_Addr_T(Integer_32_To_Dword(CPU.AC_I32(2) + Disp));
            when AC3 =>
               Eff := Phys_Addr_T(Integer_32_To_Dword(CPU.AC_I32(3) + Disp));
         end case;

         if Indirect then
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

    function Resolve_32bit_Indirectable_Addr
       (ATU : in Boolean; I_Addr : in Dword_T) return Phys_Addr_T
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

    procedure Resolve_Eagle_Bit_Addr (CPU       : in CPU_T; 
                                      Acd, Acs  : in AC_ID; 
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

    procedure Resolve_Eclipse_Bit_Addr (CPU       : in CPU_T; 
                                        Acd, Acs  : in AC_ID; 
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
