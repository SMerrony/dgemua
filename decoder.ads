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

with Interfaces; use Interfaces;

with CPU_Instructions; use CPU_Instructions;
with DG_Types;         use DG_Types;
with Memory;           use Memory;

package Decoder is

   type Carry_T is (None, Z, O, C);
   type IO_Flag_T is (None, S, C, P);
   type Mode_Num_T is new Word_T range 0 .. 3;
   type Mode_T is (Absolute, PC, AC2, AC3);
   type Shift_T is (None, L, R, S);
   type Skip_T is (None, SKP, SZC, SNC, SZR, SNR, SEZ, SBN);

   type Decoded_Instr_T is record
      Instruction : Instr_Mnemonic_T;
      Mnemonic    : Unbounded_String;
      Format      : Instr_Format_T;
      Instr_Type  : Instr_Class_T;
      Instr_Len   : Positive;
      Disp_Offset : Natural;
      Disassembly : Unbounded_String;
      -- Instruction Parameters
      Mode         : Mode_T;
      Ind          : Boolean;
      Disp_8       : Integer_8;       -- signed 8-bit displacement
      Disp_15      : Integer_16;      -- signed 15-bit displacement
      Disp_31      : Integer_32;      -- signed 31-bit displacement
      Imm_S16      : Integer_16;      -- signed 16-bit immediate
      Imm_U16      : Unsigned_16;
      Imm_U32      : Unsigned_32;
      Arg_Count    : Integer;
      Ac, Acs, Acd : AC_ID;           -- single, src, dest ACs
      Word_2       : Word_T;          -- 2nd word of instruction
      ABC          : Character;       -- A/B/C I/O
      IO_Flag      : IO_Flag_T;
      IO_Dev       : Natural;
      Sh           : Shift_T;
      Carry        : Carry_T;
      No_Load      : Boolean;
      Skip         : Skip_T;
      Bit_Number   : Natural;
   end record;

   type Opcode_Rec is record
      Exists : Boolean;
      Mnem   : Instr_Mnemonic_T;
   end record;
   type Opcode_Lookup_T is array (0 .. 65_535) of Opcode_Rec;

   Opcode_Lookup_Arr : Opcode_Lookup_T;

   procedure Match_Instruction
     (Opcode : in Word_T; Mnem : out Instr_Mnemonic_T; Found : out Boolean);
   procedure Generate_All_Possible_Opcodes;
   function Instruction_Decode
     (Opcode : in Word_T; PC : Phys_Addr_T; LEF_Mode : Boolean;
      IO_On  :    Boolean; ATU_On : Boolean; Disassemble : Boolean;
      Radix  : Number_Base_T)
      return Decoded_Instr_T;

   Decode_Failed : exception;

end Decoder;
