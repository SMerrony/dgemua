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
with Ada.Text_IO;           use Ada.Text_IO;

with CPU.Instructions; use CPU.Instructions;
with Debug_Logs;       use Debug_Logs;
with Memory;           use Memory;

package body CPU.Decoder is

   -- Match_Instruction looks for a match for the opcode in the instruction set and returns
   -- the corresponding mnemonic.  It is used only by the decoderGenAllPossOpcodes() below when
   -- the emulator is initialising.
   procedure Match_Instruction
     (Opcode : in Word_T; Mnem : out Instr_Mnemonic_T; Found : out Boolean)
   is
      Instr_Char : Instr_Char_Rec;
      Tail       : Word_T;
   begin
      for I in Instr_Mnemonic_T range Instr_Mnemonic_T'Range loop
         Instr_Char := Instruction_Set (I);
         if (Opcode and Instr_Char.Mask) = Instr_Char.Bits then
            case I is
               when I_ADC | I_ADD | I_AND | I_COM | I_INC | I_MOV | I_NEG |
                 I_SUB =>
            -- these instructions are not allowed to end in 1000(2) or 1001(2)
            -- as those patterns are used for Eagle instructions
                  Tail := Opcode and 16#000f#;
                  if Tail /= 16#0008# and Tail /= 16#0009# then
                     Mnem  := I;
                     Found := True;
                     return;
                  end if;
               when others =>
                  Mnem  := I;
                  Found := True;
                  return;
            end case;
         end if;
      end loop;
      Mnem  := I_ZEX;
      Found := False;
      return;
   end Match_Instruction;

   procedure Generate_All_Possible_Opcodes is
      Mnem  : Instr_Mnemonic_T;
      Found : Boolean;
   begin
      for N in Opcode_Lookup_T'Range loop
         Match_Instruction (Word_T (N), Mnem, Found);
         if Found then
            Opcode_Lookup_Arr (N).Exists := True;
            Opcode_Lookup_Arr (N).Mnem   := Mnem;
         else
            Opcode_Lookup_Arr (N).Exists := False;
         end if;
      end loop;
   end Generate_All_Possible_Opcodes;

-- Instruction_Lookup looks up an opcode in the opcode lookup table and returns
-- the corresponding mnemonic.  This needs to be as quick as possible
   function Instruction_Lookup
     (Opcode : in Word_T; LEF_Mode : Boolean) return Instr_Mnemonic_T
   is
   begin
      -- special case, if LEF mode is enabled then ALL I/O instructions are interpreted as LEF
      if LEF_Mode then
         -- check for I/O instruction
         if (Opcode and 2#1110_0000_0000_0000#) = 2#0110_0000_0000_0000# then
            return I_LEF;
         end if;
      end if;
      return Opcode_Lookup_Arr (Integer (Opcode)).Mnem;
   end Instruction_Lookup;

   function Char_Indirect (I : Boolean) return Character is
   begin
      if I then
         return '@';
      end if;
      return ' ';
   end Char_Indirect;

   function Decode_8bit_Disp (D8 : Byte_T; Mode : Mode_T) return Integer_16 is
   begin
      if Mode = Absolute then
         return Integer_16(D8);
      else
         return Integer_16(Integer_8(D8));
      end if;
   end Decode_8bit_Disp;

   function To_Mode (W : Mode_Num_T) return Mode_T is
   begin
      case W is
         when 0 => return Absolute;
         when 1 => return PC;
         when 2 => return AC2;
         when 3 => return AC3;
      end case;
   end To_Mode;

   function String_Mode (W : Mode_T) return String is
   begin
      case W is
         when Absolute => return "";
         when PC => return ",PC";
         when AC2 => return ",AC2";
         when AC3 => return ",AC3";
      end case;
   end String_Mode;

   function Instruction_Decode
     (Opcode : in Word_T; PC : Phys_Addr_T; LEF_Mode : Boolean;
      IO_On  :    Boolean; ATU_On : Boolean; Disassemble : Boolean)
      return Decoded_Instr_T
   is
      Decoded : Decoded_Instr_T;
      Instr   : Instr_Mnemonic_T;
   begin
      Decoded.Disassembly := To_Unbounded_String ("; Unknown instruction");
      Instr               := Instruction_Lookup (Opcode, LEF_Mode);

      Decoded.Instruction := Instr;
      Decoded.Mnemonic    := Instruction_Set (Instr).Mnemonic;
      Decoded.Disassembly := Instruction_Set (Instr).Mnemonic;
      Decoded.Format      := Instruction_Set (Instr).Instr_Fmt;
      Decoded.Instr_Type  := Instruction_Set (Instr).Instr_Class;
      Decoded.Instr_Len   := Instruction_Set (Instr).Instr_Len;
      Decoded.Disp_Offset := Instruction_Set (Instr).Disp_Offset;

      case Decoded.Format is  -- eg. DSZ, ISZ, JMP, JSR
         when NOVA_NOACC_EFF_ADDR_FMT =>
            -- Decoded.Acd := Ac_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Ind := Test_W_Bit(Opcode, 5);
            Decoded.Mode := To_Mode(Mode_Num_T(Get_W_Bits(Opcode, 6, 2)));
            Decoded.Disp_15 := Word_T(Decode_8bit_Disp(Get_Lower_Byte(Opcode), Decoded.Mode));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Char_Indirect(Decoded.Ind) &
                 Decoded.Disp_15'Image & "." & String_Mode(Decoded.Mode);
            end if;

         when others =>
            Loggers.Debug_Print
              (Debug_Log,
               "ERROR: Unhandled instruction format: " & Decoded.Format'Image &
               " for instruction: " & To_String (Decoded.Mnemonic));
      end case;

      return Decoded;
   end Instruction_Decode;

end CPU.Decoder;
