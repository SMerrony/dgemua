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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body CPU.Decoder is

    -- Match_Instruction looks for a match for the opcode in the instruction set and returns
    -- the corresponding mnemonic.  It is used only by the decoderGenAllPossOpcodes() below when
    -- the emulator is initialising.
    procedure Match_Instruction (Opcode : in Word_T; Mnem : out Instr_Mnemonic_T; Found : out Boolean) is
        Instr_Char : Instr_Char_Rec;
        Tail       : Word_T;
    begin
        for I in Instr_Mnemonic_T range Instr_Mnemonic_T'Range loop
           Instr_Char := Instruction_Set(I);
           if (Opcode and Instr_Char.Mask) = Instr_Char.Bits then
              case I is
                 when I_ADC | I_ADD | I_AND | I_COM | I_INC | I_MOV | I_NEG | I_SUB =>
                    -- these instructions are not allowed to end in 1000(2) or 1001(2)
                    -- as those patterns are used for Eagle instructions
                    Tail := Opcode and 16#000f#;
                    if Tail /= 16#0008# and Tail /= 16#0009# then
                        Mnem := I;
                        Found := true;
                        return;
                    end if;
                 when others =>
                    Mnem := I;
                    Found := true;
                    return;                  
              end case;
           end if;
        end loop;
        Mnem := I_ZEX;
        Found := false;
        return;
    end Match_Instruction;

    procedure Generate_All_Possible_Opcodes is
       Mnem : Instr_Mnemonic_T; 
       Found : Boolean;
    begin
        for N in Opcode_Lookup_T'Range loop
           Match_Instruction (Word_T(N), Mnem, Found );
           if Found then
              Opcode_Lookup_Arr(N).Exists := true;
              Opcode_Lookup_Arr(N).Mnem := Mnem;
           else
              Opcode_Lookup_Arr(N).Exists := false;
           end if;
        end loop;
    end Generate_All_Possible_Opcodes;

end CPU.Decoder;