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

with Ada.Text_IO; use Ada.Text_IO;

package body Processor.Nova_Op_P is 
   procedure Do_Nova_Op (I : Decoded_Instr_T; CPU : CPU_T) is
      Wide_Shifter           : Dword_T;
      Narrow_Shifter         : Word_T;
      Tmp_Acs, Tmp_Acd       : Word_T;
      Saved_Carry, Tmp_Carry : Boolean;
      PC_Inc                 : Phys_Addr_T;
   begin
      Tmp_Acs := CPU.AC_Wd(I.Acs);
      Tmp_Acd := CPU.AC_Wd(I.Acd);
      Saved_Carry := CPU.Carry;

      case I.Carry is
         when None => null;
         when Z => CPU.Carry := false;
         when O => CPU.Carry := true;
         when C => CPU.Carry := not CPU.Carry;
      end case;

      case I.Instruction is
         when I_ADC =>
            Wide_Shifter := Dword_T(Tmp_Acd) + Dword_T(not Tmp_Acs);
            Narrow_Shifter := DG_Types.Lower_Word (Wide_Shifter);
            if Wide_Shifter > 65535 then
               CPU.Carry := not CPU.Carry;
            end if;
         when I_ADD => -- unsigned
            Wide_Shifter := Dword_T(Tmp_Acd) + Dword_T(Tmp_Acs);
            Narrow_Shifter := DG_Types.Lower_Word (Wide_Shifter);
            if Wide_Shifter > 65535 then
               CPU.Carry := not CPU.Carry;
            end if;
         when I_AND =>
            Narrow_Shifter := Tmp_Acd and Tmp_Acs;
         when I_COM =>
            Narrow_Shifter := not Tmp_Acs;
         when I_INC =>
            Narrow_Shifter := Tmp_Acs + 1;
            if Tmp_Acs = 16#ffff# then
               CPU.Carry := not CPU.Carry;
            end if;
         when I_MOV =>
            Narrow_Shifter := Tmp_Acs;
         when I_NEG =>
            Narrow_Shifter := DG_Types.Integer_16_To_Word(- Word_To_Integer_16(Tmp_Acs));
            -- Narrow_Shifter := Word_T(-Integer_16(Tmp_Acs)); -- TODO Check this
            if Tmp_Acs = 0 then
               CPU.Carry := not CPU.Carry;
            end if;
         when I_SUB =>
            Narrow_Shifter := Tmp_Acd - Tmp_Acs;
            if Tmp_Acs <= Tmp_Acd then
               CPU.Carry := not CPU.Carry;
            end if;

         when others =>
            Put_Line ("ERROR: NOVA_OP instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: NOVA_OP instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;

      case I.Sh is
         when None => null;
         when L =>
            Tmp_Carry := CPU.Carry;
            CPU.Carry := Test_W_Bit (Narrow_Shifter, 0);
            Narrow_Shifter := Shift_Left (Narrow_Shifter, 1);
            if Tmp_Carry then
               Narrow_Shifter := Narrow_Shifter or 16#0001#;
            end if;
         when R =>
            Tmp_Carry := CPU.Carry;
            CPU.Carry := Test_W_Bit (Narrow_Shifter, 15);
            Narrow_Shifter := Shift_Right (Narrow_Shifter, 1);
            if Tmp_Carry then
               Narrow_Shifter := Narrow_Shifter or 16#8000#;
            end if;
         when S => 
            Narrow_Shifter := Swap_Bytes (Narrow_Shifter);
      end case;

      case I.Skip is
         when None => PC_Inc := 1;
         when SKP => PC_Inc := 2;
         when SZC => if not CPU.Carry then PC_Inc := 2; else PC_Inc := 1; end if;
         when SNC => if CPU.Carry then PC_Inc := 2; else PC_Inc := 1; end if;
         when SZR => if Narrow_Shifter = 0 then PC_Inc := 2; else PC_Inc := 1; end if;
         when SNR => if Narrow_Shifter /= 0 then PC_Inc := 2; else PC_Inc := 1; end if;
         when SEZ => if (not CPU.Carry) or (Narrow_Shifter = 0) then PC_Inc := 2; else PC_Inc := 1; end if;
         when SBN => if CPU.Carry and (Narrow_Shifter /= 0) then PC_Inc := 2; else PC_Inc := 1; end if;
      end case;

      if I.No_Load then
         -- don't load the result from the shifter, restore the Carry flag
         CPU.Carry := Saved_Carry;
      else
         CPU.AC(I.Acd) := Dword_T(Narrow_Shifter) and 16#0000_ffff#;
      end if;

      CPU.PC := CPU.PC + PC_Inc;
 
   end Do_Nova_Op;
end Processor.Nova_Op_P;