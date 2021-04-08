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

with CPU_Instructions; use CPU_Instructions;
with Debug_Logs;       use Debug_Logs;
with Devices;
with Devices.Bus;      
with Memory;           use Memory;

package body Decoder is

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
               when I_LEF =>
                  null;
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



   function Char_Carry (Cr : Carry_T) return Character is
   begin
      case Cr is
         when None => return ' ';
         when Z    => return 'Z';
         when O    => return 'O';
         when C    => return 'C';
      end case;
   end Char_Carry;

   function Char_Indirect (I : Boolean) return Character is
   begin
      if I then
         return '@';
      end if;
      return ' ';
   end Char_Indirect;

   function Char_IO_Flag (IO : IO_Flag_T) return Character is
   begin
      case IO is
         when None => return ' ';
         when S    => return 'S';
         when C    => return 'C';
         when P    => return 'P';
      end case;
   end Char_IO_Flag;

   function Char_No_Load (N : Boolean) return Character is
   begin
      if N then
         return '#';
      end if;
      return ' ';
   end Char_No_Load;

   function Char_Shift (Sh : Shift_T) return Character is
   begin
      case Sh is
         when None => return ' ';
         when L    => return 'L';
         when R    => return 'R';
         when S    => return 'S';
      end case;
   end Char_Shift;

   function Decode_2bit_Imm (I2 : Word_T) return Unsigned_16 is
   begin
      -- to expand range (by 1!) 1 is subtracted from operand by the Assembler...
      return Word_To_Unsigned_16(I2)+ 1;
   end Decode_2bit_Imm;

   function Decode_8bit_Disp (D8 : Byte_T; Mode : Mode_T) return Integer_16 is
   begin
      if Mode = Absolute then
         return Integer_16(D8);
      else
         return Integer_16(Byte_To_Integer_8(D8));
      end if;
   end Decode_8bit_Disp;

   function Decode_15bit_Disp (D15 : Word_T; Mode : Mode_T) return Integer_16 is
   begin
      if Mode = Absolute then
         return Integer_16 (D15 and 2#0111_1111_1111_1111#); -- zero extend
      end if;
      if Test_W_Bit (D15, 1) then
         return Word_To_Integer_16 (D15 or 2#1100_0000_0000_0000#);
      else
         return Integer_16 (D15 and 2#0011_1111_1111_1111#);
      end if;
   end Decode_15bit_Disp; -- TODO Test/verify this

   procedure Decode_16bit_Byte_Disp (D16 : in Word_T; Disp_16 : out Integer_16; Lo_Byte : out Boolean) is
   begin
      Lo_Byte := Test_W_Bit (D16, 15);
      Disp_16 := Integer_16(D16) / 2;
   end Decode_16bit_Byte_Disp;

   function Decode_Carry (Cr : Word_T) return Carry_T is
   begin
      case Cr is
         when 0 => return None;
         when 1 => return Z;
         when 2 => return O;
         when 3 => return C;
         when others => return None; -- TODO error handling
      end case;
   end Decode_Carry;

   function Decode_IO_Flag (W : Word_T) return IO_Flag_T is
   begin
      case W is
         when 0 => return None;
         when 1 => return S; -- Set
         when 2 => return C; -- Clear
         when 3 => return P; -- Pulse
         when others => return None; -- TODO error handling
      end case;
   end Decode_IO_Flag;

   function Decode_IO_Test (W : Word_T) return IO_Test_T is
   begin
      case W is
         when 0 => return BN;
         when 1 => return BZ; 
         when 2 => return DN; 
         when 3 => return DZ; 
         when others => raise Decode_Failed with "Unknown I/O Test";
      end case;
   end Decode_IO_Test;

   function Decode_Mode (W : Mode_Num_T) return Mode_T is
   begin
      case W is
         when 0 => return Absolute;
         when 1 => return PC;
         when 2 => return AC2;
         when 3 => return AC3;
      end case;
   end Decode_Mode;

   function Decode_Shift (Sh : Word_T) return Shift_T is
   begin
      case Sh is
         when 0 => return None;
         when 1 => return L;
         when 2 => return R;
         when 3 => return S;
         when others => return None; -- TODO error handling
      end case;
   end Decode_Shift;

   function Decode_Skip (Sk : Word_T) return Skip_T is
   begin
      case Sk is
         when 0 => return None;
         when 1 => return SKP;
         when 2 => return SZC;
         when 3 => return SNC;
         when 4 => return SZR;
         when 5 => return SNR;
         when 6 => return SEZ;
         when 7 => return SBN;
         when others => return None; -- TODO error handling
      end case;
   end Decode_Skip;

   function String_Mode (W : Mode_T) return String is
   begin
      case W is
         when Absolute => return "";
         when PC => return ",PC";
         when AC2 => return ",AC2";
         when AC3 => return ",AC3";
      end case;
   end String_Mode;

   function String_Skip (Sk : Skip_T) return String is
   begin
      if Sk = None then
         return "";
      end if;
      return Sk'Image;
   end String_Skip;

   function Instruction_Decode
     (Opcode : in Word_T; PC : Phys_Addr_T; LEF_Mode : Boolean;
      IO_On  :    Boolean; ATU_On : Boolean; Disassemble : Boolean; 
      Radix  : Number_Base_T)
      return Decoded_Instr_T
   is
      Decoded : Decoded_Instr_T;
      Instr   : Instr_Mnemonic_T;
      Tmp_8bit : Byte_T;
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

      case Decoded.Format is  

         when IO_TEST_DEV_FMT => -- eg. SKPn
            Decoded.IO_Test := Decode_IO_Test (Get_W_Bits (Opcode, 8, 2));
            Decoded.IO_Dev  := Dev_Num_T(Get_W_Bits (Opcode, 10, 6));
            if Disassemble then
               Decoded.Disassembly :=
                  Decoded.Disassembly & Decoded.IO_Test'Image & " " &
                  Devices.Bus.Actions.Get_Device_Name_Or_Number(Decoded.IO_Dev);
            end if;

         when IMM_ONEACC_FMT => -- eg. ADI, HXL, NADI, SBI, WADI, WLSI, WSBI
         	-- N.B. Immediate value is encoded by assembler to be one less than required
		      --      This is handled by decode2bitImm
            Decoded.Imm_U16 := Decode_2bit_Imm (Get_W_Bits (Opcode, 1, 2));
            Decoded.Ac := AC_ID(Get_W_Bits (Opcode, 3, 2));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Imm_U16'Image & Decoded.Ac'Image;
            end if;

         when NOACC_MODE_IND_2_WORD_X_FMT => -- eg. XJSR
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " &
                 Char_Indirect(Decoded.Ind) &
                  Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & String_Mode(Decoded.Mode) &
                 " [2-Word Instruction]";
            end if;

         when NOVA_DATA_IO_FMT => -- eg. DOA/B/C, DIA/B/C
            Decoded.Ac := AC_ID(Get_W_Bits (Opcode, 3, 2));
            if Decoded.Instruction = I_DIA or Decoded.Instruction = I_DIB or Decoded.Instruction = I_DIC then
               Decoded.IO_Dir := Data_In;
            else
               Decoded.IO_Dir := Data_Out;
            end if;
            case Decoded.Instruction is
               when I_DIA | I_DOA => Decoded.IO_Reg := A;
               when I_DIB | I_DOB => Decoded.IO_Reg := B;
               when I_DIC | I_DOC => Decoded.IO_Reg := C;
               when others => null;
            end case; 
            Decoded.IO_Flag := Decode_IO_Flag (Get_W_Bits (Opcode, 8, 2));
            Decoded.IO_Dev  := Dev_Num_T(Integer_16(Get_W_Bits (Opcode, 10, 6)));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & Char_IO_Flag (Decoded.IO_Flag) & " " &
                 Decoded.Ac'Image & "," & 
                 Devices.Bus.Actions.Get_Device_Name_Or_Number (Dev_Num_T(Decoded.IO_Dev));
            end if;

         when NOVA_NOACC_EFF_ADDR_FMT => -- eg. DSZ, ISZ, JMP, JSR
            Decoded.Ind     := Test_W_Bit(Opcode, 5);
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 6, 2)));
            Decoded.Disp_15 := Decode_8bit_Disp(Get_Lower_Byte(Opcode), Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Char_Indirect(Decoded.Ind) &
                 Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & String_Mode(Decoded.Mode);
            end if;

         when NOVA_ONEACC_EFF_ADDR_FMT => -- eg. LDA, STA
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Ind     := Test_W_Bit(Opcode, 5);
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 6, 2)));
            Decoded.Disp_15 := Decode_8bit_Disp(Get_Lower_Byte(Opcode), Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image & "," & Char_Indirect(Decoded.Ind) &
                 Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & String_Mode(Decoded.Mode);
            end if;

         when NOVA_TWOACC_MULT_OP_FMT => -- eg. ADC, ADD, AND, COM
            Decoded.Acs     := AC_ID(Get_W_Bits (Opcode, 1, 2));
            Decoded.Acd     := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Sh      := Decode_Shift (Get_W_Bits (Opcode, 8, 2));
            Decoded.Carry   := Decode_Carry (Get_W_Bits (Opcode, 10, 2));
            Decoded.No_Load := Test_W_Bit (Opcode, 12);
            Decoded.Skip    := Decode_Skip(Get_W_Bits(Opcode, 13, 3));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & Char_Carry(Decoded.Carry) & Char_Shift (Decoded.Sh) &
                 Char_No_Load (Decoded.No_Load) & " " &
                 Decoded.Acs'Image & "," & Decoded.Acd'Image & " " & String_Skip (Decoded.Skip);
            end if;

         when ONEACC_IMM_2_WORD_FMT => -- eg. IORI
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & 
                 Int_To_String (Integer(Decoded.Word_2), Radix, 8, false, true) & "," &
                 Decoded.Ac'Image & " [2-Word Instruction]";
            end if;

         when ONEACC_IMMDWD_3_WORD_FMT => -- eg. WANDI, WIORI, WLDAI
            Decoded.Ac     := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Imm_DW := Memory.RAM.Read_Dword (PC + 1);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Dword_To_String(Decoded.Imm_DW, Radix, 12, false ) & "," & 
                 Decoded.Ac'Image & " [3-Word Instruction]";
            end if;

         when ONEACC_IMMWD_2_WORD_FMT => -- eg. ADDI, NADDI, NLDAI, WASHI, WSEQI, WLSHI, WNADI
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Imm_S16 := Word_To_Integer_16(Decoded.Word_2);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Int_To_String (Integer(Decoded.Imm_S16), Radix, 8, false, true) & "," & 
                 Decoded.Ac'Image & " [2-Word Instruction]";
            end if;

         when ONEACC_MODE_2_WORD_X_B_FMT => -- eg. XLDB, XLEFB, XSTB
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 1, 2)));
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decode_16bit_Byte_Disp (D16 => Decoded.Word_2, Disp_16 => Decoded.Disp_15, Lo_Byte => Decoded.Low_Byte);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image & "," &
                 Char_Indirect(Decoded.Ind) & 
                 Int_To_String (Integer(Decoded.Disp_15 * 2), Radix, 8, false, true) & "+" &
                 Low_Byte_To_Char (Decoded.Low_Byte) &
                 String_Mode(Decoded.Mode) & " [2-Word Instruction]";
            end if;

         when ONEACC_MODE_IND_2_WORD_X_FMT => -- eg. XNLDA, XWMUL & many others
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 1, 2)));
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image & "," &
                 Char_Indirect(Decoded.Ind) & 
                 Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & String_Mode(Decoded.Mode) &
                 " [2-Word Instruction]";
            end if;

         when SPLIT_8BIT_DISP_FMT => -- eg. WBR, always a signed displacement
            Tmp_8bit := Byte_T(Memory.Get_W_Bits(Opcode, 1, 4));
            Tmp_8bit := Shift_Left (Tmp_8bit, 4);
            Tmp_8bit := Tmp_8bit or Byte_T(Memory.Get_W_Bits(Opcode, 6, 4));
            Decoded.Disp_8 := Integer_8(Decode_8bit_Disp(Tmp_8bit, Decoder.PC));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Disp_8'Image;
            end if;

         when TWOACC_1_WORD_FMT => -- eg. ANC, BTO, WSUB and MANY others
            Decoded.Acs     := AC_ID(Get_W_Bits (Opcode, 1, 2));
            Decoded.Acd     := AC_ID(Get_W_Bits (Opcode, 3, 2));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Acs'Image & "," & Decoded.Acd'Image;
            end if;

         when UNIQUE_1_WORD_FMT => -- nothing to do 
            null;

         when WSKB_FMT => -- eg. WSKBO/Z
            Tmp_8bit := Byte_T(Memory.Get_W_Bits(Opcode, 1, 3));
            Tmp_8bit := Shift_Left (Tmp_8bit, 2);
            Tmp_8bit := Tmp_8bit or Byte_T(Memory.Get_W_Bits(Opcode, 10, 2));
            Decoded.Bit_Number := Natural(Unsigned_8(Tmp_8bit));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Bit_Number'Image;
            end if;

         when others =>
            Loggers.Debug_Print
              (Debug_Log,
               "ERROR: Unhandled instruction format: " & Decoded.Format'Image &
               " for instruction: " & To_String (Decoded.Mnemonic));
            raise Decode_Failed with "Unhandled instruction format";
      end case;

      return Decoded;
   end Instruction_Decode;

end Decoder;
