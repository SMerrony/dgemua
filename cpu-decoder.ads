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

-- BEWARE: DO NOT FALL INTO THE TRAP OF TRYING TO RESOLVE ADDRESSES HERE - DECODE ONLY!

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with CPU.Instructions; use CPU.Instructions;
with DG_Types;         use DG_Types;

package CPU.Decoder is

    type Ac_ID is new Integer range 0 .. 3;

    type Decoded_Instr_T is record
       Instruction   : Instr_Mnemonic_T;
       Mnemonic      : Unbounded_String;
       Format        : Instr_Format_T;
       Instr_Type    : Instr_Class_T;
       Instr_Len     : Positive;
       Disp_Offset   : Natural;
       Disassembly   : Unbounded_String;
       -- Instruction Parameters
       Mode          : Integer;
       Ind           : Boolean;
       Disp_15       : Word_T;          -- signed 15-bit displacement
       Disp_31       : Dword_T;         -- signed 31-bit displacement
       Arg_Count     : Integer;
       Ac, Acs, Acd  : Ac_ID;           -- single, src, dest ACs  
       Word_2        : Word_T;          -- 2nd word of instruction
       ABC           : Character;       -- A/B/C I/O 
       IO_Flag       : Character;
    end record;

    type Opcode_Rec is record
       Exists : Boolean;
       Mnem   : Instr_Mnemonic_T;
    end record;
    type Opcode_Lookup_T is array (0 .. 65535) of Opcode_Rec;

    Opcode_Lookup_Arr : Opcode_Lookup_T;

    procedure Match_Instruction (Opcode : in Word_T; Mnem : out Instr_Mnemonic_T; Found : out Boolean);
    procedure Generate_All_Possible_Opcodes;

end CPU.Decoder;