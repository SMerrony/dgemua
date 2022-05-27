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


with Ada.Text_IO; use Ada.Text_IO;

package body Processor.Eclipse_Op_P is 
   procedure Do_Eclipse_Op (I : Decoded_Instr_T; CPU : CPU_T) is
           Word : Word_T;
         Dword : Dword_T;
         S16   : Integer_16;
         Shift : Integer;
   begin
      case I.Instruction is

         when I_ADDI =>
            S16 := Word_To_Integer_16(CPU.AC_Wd(I.Ac));
            S16 := S16 + Word_To_Integer_16(I.Word_2);
            CPU.AC(I.Ac) := Dword_T(DG_Types.Integer_16_To_Word(S16)) and 16#0000_ffff#;

         when I_ADI =>
            Word := CPU.AC_Wd(I.Ac);
            Word := Word + Word_T(I.Imm_U16);
            CPU.AC(I.Ac) := Dword_T(Word);

         when I_ANDI =>
            Word := CPU.AC_Wd(I.Ac);
            CPU.AC(I.Ac) := Dword_T(Word and DG_Types.Integer_16_To_Word(I.Imm_S16)) and 16#0000_ffff#;

         when I_DHXL =>
            declare
               D_Plus_1 : AC_ID;
            begin
               if I.Ac = 3 then
                  D_Plus_1 := 0;
               else
                  D_Plus_1 := I.Ac + 1;
               end if;
               Dword := Dword_From_Two_Words (CPU.AC_Wd(I.Ac), CPU.AC_Wd(D_Plus_1));
               Dword := Shift_Left (Dword, Natural(I.Imm_U16) * 4);
               CPU.AC(I.Ac) := Dword_T(DG_Types.Upper_Word (Dword));
               CPU.AC(D_Plus_1) := Dword_T(DG_Types.Lower_Word (Dword));
            end;

         when I_DIVX =>
            declare
               Dividend_32 : constant Integer_32 := Integer_32(Word_To_Integer_16 (CPU.AC_Wd(1)));
               Divisor_32  : constant Integer_32 := Integer_32(Word_To_Integer_16 (CPU.AC_Wd(2)));
               Quotient_32 : Integer_32;
            begin
               CPU.Carry := false;
               if CPU.AC(2) = 0 then
                  CPU.Carry := true;
               else
                  if Test_DW_Bit (CPU.AC(1), 16) then
                     CPU.AC(0) := 16#0000_ffff#;
                  else
                     CPU.AC(0) := 0;
                  end if;
                  Quotient_32 := Dividend_32 / Divisor_32;
                  if (Quotient_32 < Min_Neg_S16) or (Quotient_32 > Max_Pos_S16) then
                     CPU.Carry := true;
                  else
                     CPU.AC_I32(0) := Dividend_32 mod Divisor_32;
                     CPU.AC_I32(1) := Quotient_32;
                  end if;
               end if;
            end;

         when I_DLSH =>
            declare
               D_Plus_1 : AC_ID;
            begin
               if I.Acd = 3 then
                  D_Plus_1 := 0;
               else
                  D_Plus_1 := I.Acd + 1;
               end if;
               Shift := Integer(Byte_To_Integer_8 (Get_Lower_Byte (CPU.AC_Wd(I.Acs))));
               Dword := Dword_From_Two_Words (CPU.AC_Wd(I.Acd), CPU.AC_Wd(D_Plus_1));
               if Shift /= 0 then
                  if (Shift < -31) or (Shift > 31) then
                     Dword := 0;
                  else
                     if Shift > 0 then
                        Dword := Shift_Left (Dword, Shift);
                     else
                        Dword := Shift_Right (Dword, Shift * (-1));
                     end if;
                  end if;
               end if;
               CPU.AC(I.Acd) := Dword_T(DG_Types.Upper_Word (Dword));
               CPU.AC(D_Plus_1) := Dword_T(DG_Types.Lower_Word (Dword));
            end;

         when I_FXTD =>
            Clear_W_Bit (CPU.PSR, PSR_OVK);
            Clear_W_Bit (CPU.PSR, PSR_OVR);

         when I_FXTE =>
            Set_W_Bit (CPU.PSR, PSR_OVK);
            Clear_W_Bit (CPU.PSR, PSR_OVR);

         when I_IOR =>
            Word := CPU.AC_Wd(I.Acd) or CPU.AC_Wd(I.Acs);
            CPU.AC(I.Acd) := Dword_T(Word);

         when I_IORI =>
            Word := CPU.AC_Wd(I.Ac) or I.Word_2;
            CPU.AC(I.Ac) := Dword_T(Word);

         when I_HXL =>
            Dword := Shift_Left (CPU.AC(I.Ac), Integer(I.Imm_U16) * 4);
            CPU.AC(I.Ac) := Dword and 16#0000_ffff#;
                        
         when I_HXR =>
            Dword := Shift_Right (CPU.AC(I.Ac) and 16#0000_ffff#, Integer(I.Imm_U16) * 4);
            CPU.AC(I.Ac) := Dword and 16#0000_ffff#;

         when I_LSH =>
            Word := CPU.AC_Wd(I.Acd);
            Shift := Integer(Byte_To_Integer_8 (Get_Lower_Byte (CPU.AC_Wd(I.Acs))));
            if Shift /= 0 then
               if (Shift < -15) or (Shift > 15) then
                  Word := 0;
               else
                  if Shift > 0 then
                     Word := Shift_Left (Word, Shift);
                  else
                     Word := Shift_Right (Word, Shift * (-1));
                  end if;
               end if;
            end if;
            CPU.AC(I.Acd) := Dword_T(Word);

         when I_SBI =>
            Word := CPU.AC_Wd(I.Ac);
            Word := Word - Word_T(I.Imm_U16);
            CPU.AC(I.Ac) := Dword_T(Word);
                  
         when I_XCH =>
            DWord := CPU.AC(I.Acs);
            CPU.AC(I.Acs) := CPU.AC(I.Acd) and 16#0000_ffff#;
            CPU.AC(I.Acd) := DWord and 16#0000_ffff#;

         when I_XCT=> -- funkiness ahead...
            CPU.XCT_Mode := true;
            CPU.XCT_Opcode := CPU.AC_Wd(I.Ac);
            return; -- PC NOT advanced

         when I_XORI =>
            CPU.AC(I.Ac) := DWord_T(Lower_Word(CPU.AC(I.Ac)) xor I.Word_2);

         when others =>
            Put_Line ("ERROR: ECLIPSE_OP instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: ECLIPSE_OP instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;

      CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);

   end Do_Eclipse_Op;

 end Processor.Eclipse_Op_P;