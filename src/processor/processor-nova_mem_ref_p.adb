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
with Memory;      use Memory;
with Resolver;    use Resolver;

package body Processor.Nova_Mem_Ref_P is 
   procedure Do_Nova_Mem_Ref (I : Decoded_Instr_T; CPU : CPU_T) is
      Ring_Mask : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
      Addr : Phys_Addr_T;
      Word : Word_T;
   begin
      case I.Instruction is
         when I_DSZ =>
            Addr := (Resolve_8bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
            Word := RAM.Read_Word (Addr) - 1;
            RAM.Write_Word (Addr, Word);
            if Word = 0 then CPU.PC := CPU.PC + 1; end if;

         when I_ISZ =>
            Addr := (Resolve_8bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
            Word := RAM.Read_Word (Addr) + 1;
            RAM.Write_Word (Addr, Word);
            if Word = 0 then CPU.PC := CPU.PC + 1; end if;

         when I_LDA =>
            Addr := (Resolve_8bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
            Word := RAM.Read_Word (Addr);
            CPU.AC(I.Ac) := Dword_T(Word);

         when I_STA =>
            Addr := (Resolve_8bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
            Word := CPU.AC_Wd(I.Ac);
            RAM.Write_Word (Addr, Word);

         when others =>
            Put_Line ("ERROR: Nova_Mem_Ref instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: Nova_Mem_Ref instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      CPU.PC := CPU.PC + 1;
  
   end Do_Nova_Mem_Ref;
 end Processor.Nova_Mem_Ref_P;