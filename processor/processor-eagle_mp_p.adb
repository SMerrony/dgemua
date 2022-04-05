-- MIT License

-- Copyright ©2022 Stephen Merrony

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

with Debug_Logs;      use Debug_Logs;

package body Processor.Eagle_MP_P is

    procedure Do_Eagle_MP (I : in Decoded_Instr_T; CPU : in out CPU_T) is
    begin
        -- case I.Instruction is
            
        --     when I_JPID =>
        --         CPU.AC(0) := 16#ffff_ffff#;
         
        --     when others =>
        --         Put_Line ("ERROR: EAGLE_MP instruction " & To_String(I.Mnemonic) & 
        --                   " not yet implemented");
        --         raise Execution_Failure with "ERROR: EAGLE_MP instruction " & To_String(I.Mnemonic) & 
        --                                      " not yet implemented";
        -- end case;

        CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);

   end Do_Eagle_MP;

end Processor.Eagle_MP_P;
