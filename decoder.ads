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

-- BEWARE: DO NOT FALL INTO THE TRAP OF TRYING TO RESOLVE ADDRESSES HERE - DECODE ONLY!

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

with CPU_Instructions; use CPU_Instructions;
with DG_Types;         use DG_Types;

package Decoder is

   type Carry_T is (None, Z, O, C);

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
      Disp_Offset : Integer_32;
      Disassembly : Unbounded_String;
      -- Instruction Parameters
      Mode         : Mode_T;
      Ind          : Boolean;
      Disp_8       : Integer_8;       -- signed 8-bit displacement
      Disp_15      : Integer_16;      -- signed 15-bit displacement
      Disp_31      : Integer_32;      -- signed 31-bit displacement
      Disp_32      : Unsigned_32;
      Imm_U16      : Unsigned_16;
      Imm_DW       : Dword_T;
      Arg_Count    : Integer;
      Ac, Acs, Acd : AC_ID;           -- single, src, dest ACs
      Word_1       : Word_T;          -- 1st word of instruction
      Word_2       : Word_T;          -- 2nd word of instruction
      Word_3       : Word_T;          -- 3rd word of instruction
      IO_Dir       : IO_Dir_T;
      IO_Reg       : IO_Reg_T;        -- A/B/C I/O
      IO_Flag      : IO_Flag_T;
      IO_Dev       : Dev_Num_T;
      IO_Test      : IO_Test_T;
      Sh           : Shift_T;
      Carry        : Carry_T;
      No_Load      : Boolean;
      Skip         : Skip_T;
      Bit_Number   : Natural;
      Low_Byte     : Boolean;
   end record;

   type Opcode_Rec is record
      Exists : Boolean;
      Mnem   : Instr_Mnemonic_T;
   end record;
   type Opcode_Lookup_T is array (0 .. 65_535) of Opcode_Rec;

   procedure Match_Instruction (Opcode : Word_T; 
                                Mnem : out Instr_Mnemonic_T; 
                                Found : out Boolean);
   function Generate_All_Possible_Opcodes return Opcode_Lookup_T;
   function Instruction_Decode (Opcode : Word_T; 
                                PC     : Phys_Addr_T;
                                LEF_Mode, IO_On, ATU_On : Boolean; 
                                Disassemble : Boolean;
      Radix  :    Number_Base_T) return Decoded_Instr_T;
   procedure Init;

   Decode_Failed : exception;

private
   Opcode_Lookup_Arr : Opcode_Lookup_T;

end Decoder;
