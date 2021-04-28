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

package body Processor.Eagle_Decimal_P is 

   procedure Do_Eagle_Decimal (I : in Decoded_Instr_T; CPU : in out CPU_T) is
    begin
      case I.Word_2 is
         when 0 => raise Execution_Failure with "ERROR: WDMOV Not Yet Implemented";
         when 1 => -- WDCMP
            -- Short-circuit certain equality
            if (CPU.AC(0) = CPU.AC(1) and (CPU.AC(2) = CPU.AC(3))) then
               CPU.AC(1) := 0;
            else
               raise Execution_Failure with "ERROR: WDCMP not fully implemented";
            end if;
         when 2 => raise Execution_Failure with "ERROR: WDINC Not Yet Implemented";
         when 3 => raise Execution_Failure with "ERROR: WDDEC Not Yet Implemented";
         when others =>
            Put_Line ("ERROR: EAGLE_DECIMAL instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_DECIMAL instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
   end Do_Eagle_Decimal;

 end Processor.Eagle_Decimal_P;