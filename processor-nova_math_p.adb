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

package body Processor.Nova_Math_P is 
 procedure Do_Nova_Math (I : in Decoded_Instr_T; CPU : in out CPU_T) is
         DW : Dword_T;
      begin
         case I.Instruction is
            when I_DIV =>
               declare
                  UW, LW, Quot : Word_T;
               begin
                  UW := DG_Types.Lower_Word (CPU.AC(0));
                  LW := DG_Types.Lower_Word (CPU.AC(1));
                  DW := Dword_From_Two_Words (UW, LW);
                  Quot := DG_Types.Lower_Word (CPU.AC(2));
                  if (UW >= Quot) or (Quot = 0) then
                     CPU.Carry := true;
                  else
                     CPU.Carry := false;
                     CPU.AC(0) := (Dw mod Dword_T(Quot)) and 16#0000_ffff#;
                     CPU.AC(1) := (Dw / Dword_T(Quot)) and 16#0000_ffff#;
                  end if;
               end;     

            when I_MUL =>      
               DW := ((CPU.AC(1) and 16#0000_ffff#) *
                      (CPU.AC(2) and 16#0000_ffff#)) +
                      (CPU.AC(0) and 16#0000_ffff#);
               CPU.AC(0) := Dword_T(DG_Types.Upper_Word (DW));
               CPU.AC(1) := Dword_T(DG_Types.Lower_Word (DW));

            when others =>
               Put_Line ("ERROR: NOVA_MATH instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: NOVA_MATH instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + 1;
      end Do_Nova_Math;
 end Processor.Nova_Math_P;