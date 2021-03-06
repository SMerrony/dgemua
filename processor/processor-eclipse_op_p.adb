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

with Ada.Text_IO; use Ada.Text_IO;

with Resolver;    use Resolver;

package body Processor.Eclipse_Op_P is 
   procedure Do_Eclipse_Op (I : in Decoded_Instr_T; CPU : in out CPU_T) is
           Word : Word_T;
         Dword : Dword_T;
         S16   : Integer_16;
         Shift : Integer;
   begin
      case I.Instruction is

         when I_ADDI =>
            S16 := Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Ac)));
            S16 := S16 + Word_To_Integer_16(I.Word_2);
            CPU.AC(I.Ac) := Dword_T(DG_Types.Integer_16_To_Word(S16)) and 16#0000_ffff#;

         when I_ADI =>
            Word := DG_Types.Lower_Word (CPU.AC(I.Ac));
            Word := Word + Word_T(I.Imm_U16);
            CPU.AC(I.Ac) := Dword_T(Word);

         when I_ANDI =>
            Word := DG_Types.Lower_Word (CPU.AC(I.Ac));
            CPU.AC(I.Ac) := Dword_T(Word and DG_Types.Integer_16_To_Word(I.Imm_S16)) and 16#0000_ffff#;

         when I_DHXL =>
            declare
               D_Plus_1 : AC_ID;
            begin
               if I.Acd = 3 then
                  D_Plus_1 := 0;
               else
                  D_Plus_1 := I.Acd + 1;
               end if;
               Dword := Dword_From_Two_Words (DG_Types.Lower_Word(CPU.AC(I.Acd)), DG_Types.Lower_Word(CPU.AC(D_Plus_1)));
               Dword := Shift_Left (Dword, Natural(I.Imm_U16) * 4);
               CPU.AC(I.Acd) := Dword_T(DG_Types.Upper_Word (Dword));
               CPU.AC(D_Plus_1) := Dword_T(DG_Types.Lower_Word (Dword));
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
               Shift := Integer(Byte_To_Integer_8 (Get_Lower_Byte (DG_Types.Lower_Word (CPU.AC(I.Acs)))));
               Dword := Dword_From_Two_Words (DG_Types.Lower_Word(CPU.AC(I.Acd)), DG_Types.Lower_Word(CPU.AC(D_Plus_1)));
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
            Word := DG_Types.Lower_Word (CPU.AC(I.Acd)) or DG_Types.Lower_Word (CPU.AC(I.Acs));
            CPU.AC(I.Acd) := Dword_T(Word);

         when I_IORI =>
            Word := DG_Types.Lower_Word (CPU.AC(I.Ac)) or I.Word_2;
            CPU.AC(I.Ac) := Dword_T(Word);

         when I_HXL =>
            Dword := Shift_Left (CPU.AC(I.Ac), Integer(I.Imm_U16) * 4);
            CPU.AC(I.Ac) := Dword and 16#0000_ffff#;
                        
         when I_HXR =>
            Dword := Shift_Right (CPU.AC(I.Ac), Integer(I.Imm_U16) * 4);
            CPU.AC(I.Ac) := Dword and 16#0000_ffff#;

         when I_LSH =>
            Word := DG_Types.Lower_Word (CPU.AC(I.Acd));
            Shift := Integer(Byte_To_Integer_8 (Get_Lower_Byte (DG_Types.Lower_Word (CPU.AC(I.Acs)))));
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
            Word := DG_Types.Lower_Word(CPU.AC(I.Ac));
            Word := Word - Word_T(I.Imm_U16);
            CPU.AC(I.Ac) := Dword_T(Word);
                  
         when I_XCH =>
            DWord := CPU.AC(I.Acs);
            CPU.AC(I.Acs) := CPU.AC(I.Acd) and 16#0000_ffff#;
            CPU.AC(I.Acd) := DWord and 16#0000_ffff#;

         when I_XCT=> -- funkiness ahead...
            CPU.XCT_Mode := true;
            CPU.XCT_Opcode := DG_Types.Lower_Word (CPU.AC(I.Ac));
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