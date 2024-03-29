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


with Debug_Logs;       use Debug_Logs;
with Devices;
with Devices.Bus;      
with Memory;           use Memory;

package body Decoder is

   -- Match_Instruction looks for a match for the opcode in the instruction set and returns
   -- the corresponding mnemonic.  It is used only by the decoderGenAllPossOpcodes() below when
   -- the emulator is initialising.
   procedure Match_Instruction (Opcode : Word_T; Mnem : out Instr_Mnemonic_T; Found : out Boolean)
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

   -- procedure Generate_All_Possible_Opcodes is
   --    Mnem  : Instr_Mnemonic_T;
   --    Found : Boolean;
   -- begin
   --    for N in Opcode_Lookup_T'Range loop
   --       Match_Instruction (Word_T (N), Mnem, Found);
   --       if Found then
   --          Opcode_Lookup_Arr (N).Exists := True;
   --          Opcode_Lookup_Arr (N).Mnem   := Mnem;
   --       else
   --          Opcode_Lookup_Arr (N).Exists := False;
   --       end if;
   --    end loop;
   -- end Generate_All_Possible_Opcodes;

   function  Generate_All_Possible_Opcodes return Opcode_Lookup_T is
      Lookup : Opcode_Lookup_T;
      Mnem  : Instr_Mnemonic_T;
      Found : Boolean;
   begin
      for N in Opcode_Lookup_T'Range loop
         Match_Instruction (Word_T (N), Mnem, Found);
         if Found then
            Lookup(N).Exists := True;
            Lookup(N).Mnem   := Mnem;
         else
            Lookup(N).Exists := False;
         end if;
      end loop;
      return Lookup;
   end Generate_All_Possible_Opcodes;

-- Instruction_Lookup looks up an opcode in the opcode lookup table and returns
-- the corresponding mnemonic.  This needs to be as quick as possible
   function Instruction_Lookup
     (Opcode : Word_T; LEF_Mode : Boolean) return Instr_Mnemonic_T is
     Mnem : Instr_Mnemonic_T;
   begin
      -- special case, if LEF mode is enabled then ALL I/O instructions are interpreted as LEF
      if LEF_Mode then
         -- check for I/O instruction
         if (Opcode and 2#1110_0000_0000_0000#) = 2#0110_0000_0000_0000# then
            return I_LEF;
         end if;
      end if;
      Mnem := Opcode_Lookup_Arr (Integer (Opcode)).Mnem;
      -- catch unimplemented Eagle instructions...
      if Mnem = I_ADC and (((Opcode and 2#0000_0000_0000_1111#) = 2#0000_0000_0000_1001#) or
                           ((Opcode and 2#0000_0000_0000_1111#) = 2#0000_0000_0000_1000#)) then
         raise Decode_Failed with "Unimplemented Eagle instruction: " & Word_To_String(Opcode, Binary, 16, true);
      end if;
      return Mnem;
   end Instruction_Lookup;

   procedure Init is
   begin
      Opcode_Lookup_Arr := Generate_All_Possible_Opcodes;
   end Init;

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
      -- on entry D15 is held in a Word_T (which is an Unsigned_16)
      Disp : Integer_16;
   begin
      if Mode = Absolute then
         -- in this case, the displacement is handled as an unsigned 15-bit value
         -- so we can just zero-extend it
         Disp := Word_To_Integer_16 (D15); -- zero extend
      else
         -- we are in one of the relative moded, the displacement must be
         -- treated as a 15-bit SIGNED number
         if (D15 and 16#4000#) = 16#4000# then -- check the 2nd bit
            Disp := Word_To_Integer_16 (D15 or 16#8000#); -- sign extend 
         else
            Disp := Word_To_Integer_16 (D15); -- zero extended
         end if;
      end if;
      return Disp;
   end Decode_15bit_Disp; 

   procedure Decode_16bit_Byte_Disp (D16 : Word_T; Disp_16 : out Integer_16; Lo_Byte : out Boolean) is
      Neg : constant Boolean := Test_W_Bit (D16, 0);
   begin
      Lo_Byte := Test_W_Bit (D16, 15);
      if Neg then
         Disp_16 := Word_To_Integer_16(16#8000# or Shift_Right (D16, 1));
      else 
         Disp_16 := Word_To_Integer_16(Shift_Right (D16, 1));
      end if;
   end Decode_16bit_Byte_Disp;

   function Decode_31bit_Disp (W_1, W_2 : Word_T; Mode : Mode_T) return Integer_32 is
      DW   : Dword_T;
      Disp : Integer_32;
   begin
      DW := Dword_From_Two_Words (W_1, W_2);
      if Mode /= Absolute then
         if (W_1 and 16#4000#) /= 0 then -- sign extend
            DW := DW or 16#8000_0000#;
         end if;
      end if;
      Disp :=  Dword_To_Integer_32(DW);
      return Disp;
   end Decode_31bit_Disp;

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
     (Opcode : Word_T; PC : Phys_Addr_T; LEF_Mode : Boolean;
      IO_On, ATU_On, Disassemble : Boolean; 
      Radix  : Number_Base_T)
      return Decoded_Instr_T
   is
      Decoded : Decoded_Instr_T;
      Instr   : Instr_Mnemonic_T;
      Tmp_8bit : Byte_T;
   begin
      Decoded.Disassembly := To_Unbounded_String ("; Unknown instruction");
      Instr               := Instruction_Lookup (Opcode, LEF_Mode);
      Decoded.Word_1      := Opcode;
      Decoded.Instruction := Instr;
      Decoded.Mnemonic    := Instruction_Set (Instr).Mnemonic;
      -- Decoded.Disassembly := To_Unbounded_String (Word_To_String (WD => Opcode, Base => Binary, Width => 16, Zero_Pad => true)) &
      --                        " " & Instruction_Set (Instr).Mnemonic;
      Decoded.Disassembly := Instruction_Set (Instr).Mnemonic;                             
      Decoded.Format      := Instruction_Set (Instr).Instr_Fmt;
      Decoded.Instr_Type  := Instruction_Set (Instr).Instr_Class;
      Decoded.Instr_Len   := Instruction_Set (Instr).Instr_Len;
      Decoded.Disp_Offset := Instruction_Set (Instr).Disp_Offset;

      Decoded.Ind := false;

      case Decoded.Format is  

         when IMM_MODE_2_WORD_FMT => -- eg. XNADI, XNSBI, XNSUB, XWADI, XWSBI
            Decoded.Imm_U16 := Decode_2bit_Imm (Get_W_Bits (Opcode, 1, 2));
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2 and 16#7fff#, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " &
                 Char_Indirect(Decoded.Ind) &
                  Int_To_String (Integer(Decoded.Imm_U16), Radix, 8, false, true) & "," &
                  Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & 
                  String_Mode(Decoded.Mode) & " [2-Word Instruction]";
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

         when IO_FLAGS_DEV_FMT => -- eg. NIO
            Decoded.IO_Flag := Decode_IO_Flag (Get_W_Bits (Opcode, 8, 2));
            Decoded.IO_Dev  := Dev_Num_T(Get_W_Bits (Opcode, 10, 6));
            if Disassemble then
               Decoded.Disassembly :=
                  Decoded.Disassembly & Decoded.IO_Flag'Image & " " &
                  -- Devices.Bus.Actions.Get_Device_Name_Or_Number(Decoded.IO_Dev);
                  Int_To_String (Int => Integer(Decoded.IO_Dev), Base => Radix, Width => 4, Zero_Pad => False, Truncate => True);
            end if;

         when IO_TEST_DEV_FMT => -- eg. SKPn
            Decoded.IO_Test := Decode_IO_Test (Get_W_Bits (Opcode, 8, 2));
            Decoded.IO_Dev  := Dev_Num_T(Get_W_Bits (Opcode, 10, 6));
            if Disassemble then
               Decoded.Disassembly :=
                  Decoded.Disassembly & Decoded.IO_Test'Image & " " &
                  -- Devices.Bus.Actions.Get_Device_Name_Or_Number(Decoded.IO_Dev);
                  Int_To_String (Int => Integer(Decoded.IO_Dev), Base => Radix, Width => 4, Zero_Pad => False, Truncate => True);
            end if;

         when LNDO_4_WORD_FMT => -- also LWDO
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 1, 2));
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Word_3  := Memory.RAM.Read_Word (PC + 2);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_31 := Decode_31bit_Disp (Decoded.Word_2 and 16#7fff#, Decoded.Word_3, Decoded.Mode);
            Decoded.Imm_U16 := Unsigned_16(Memory.RAM.Read_Word (PC + 3));
            if Disassemble then
               Decoded.Disassembly :=
                  Decoded.Disassembly & " " & Decoded.Ac'Image & "," & 
                  Int_To_String (Integer(Decoded.Imm_U16), Radix, 8, false, true) & 
                  Char_Indirect(Decoded.Ind) &
                  Int_To_String (Integer(Decoded.Disp_31), Radix, 12, false, true) &
                  String_Mode(Decoded.Mode) & " [4-Word Instruction]";
            end if;
                  
         when MULTI_PROC_2_WORD_FMT => -- The multiprocessor commands, THESE ARE DIFFERENT...
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            case Decoded.Word_2 is
               when 0 =>
                  Decoded.Instruction := I_JPSTOP;
                  Decoded.Mnemonic := To_Unbounded_String("JPSTOP");            
               when 1 =>
                  Decoded.Instruction := I_JPSTART;
                  Decoded.Mnemonic := To_Unbounded_String("JPSTART");
               when 2 =>
                  Decoded.Instruction := I_JPSTATUS;
                  Decoded.Mnemonic := To_Unbounded_String("JPSTATUS");                              
               when 3 =>
                  Decoded.Instruction := I_JPID;
                  Decoded.Mnemonic := To_Unbounded_String("JPID");
               when 4 =>
                  Decoded.Instruction := I_JPLCS;
                  Decoded.Mnemonic := To_Unbounded_String("JPLCS");                 
               when 5 =>
                  Decoded.Instruction := I_IMODE;
                  Decoded.Mnemonic := To_Unbounded_String("IMODE");
               when 7 =>
                  Decoded.Instruction := I_CINTR;
                  Decoded.Mnemonic := To_Unbounded_String("CINTR");
               when 8 =>
                  Decoded.Instruction := I_JPFLUSH;
                  Decoded.Mnemonic := To_Unbounded_String("JPFLUSH");   
               when 9 =>
                  Decoded.Instruction := I_JPLOAD;
                  Decoded.Mnemonic := To_Unbounded_String("JPLOAD");                              
               when 10 =>
                  Decoded.Instruction := I_JPFLOAD;
                  Decoded.Mnemonic := To_Unbounded_String("JPFLOAD");
               when others =>
                  raise Decode_Failed with "unknown multiprocessor instruction #" & 
                     Word_To_String (WD => Decoded.Word_2, Base => Hex, Width => 4, Zero_Pad => True);
            end case;
            Decoded.Disassembly := Decoded.Mnemonic;
            if Disassemble then
               Decoded.Disassembly :=
                  Decoded.Disassembly & " x" &
                  Word_To_String (WD => Decoded.Word_2, Base => Hex, Width => 4, Zero_Pad => true) &
                  " [2-Word Instruction] *** MULTIPROCESSOR ***";
            end if;

         when NOACC_MODE_2_WORD_FMT => -- eg. XPEFB
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decode_16bit_Byte_Disp (Decoded.Word_2, Decoded.Disp_15, Decoded.Low_Byte)  ;
            if Disassemble then
               Decoded.Disassembly :=
                  Decoded.Disassembly & " " &
                  Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) &
                  String_Mode(Decoded.Mode) & " " &
                  Low_Byte_To_Char (Decoded.Low_Byte) &" [2-Word Instruction]";
            end if;

         when NOACC_MODE_3_WORD_FMT => -- eg. LPEFB
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Disp_32 := Unsigned_32(Memory.RAM.Read_Dword (PC + 1));
            if Disassemble then
               Decoded.Disassembly :=
                  Decoded.Disassembly & " " &
                  Int_To_String (Unsigned_32_To_Integer(Decoded.Disp_32), Radix, 11, false, true) &
                  "," & String_Mode(Decoded.Mode) & " [3-Word Instruction]";
            end if;

         when NOACC_MODE_IMM_IND_3_WORD_FMT => -- eg. LNADI, LNSBI
            Decoded.Imm_U16 := Decode_2bit_Imm (Get_W_Bits (Opcode, 1, 2));
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Word_3  := Memory.RAM.Read_Word (PC + 2);
            Decoded.Disp_31 := Decode_31bit_Disp (Decoded.Word_2 and 16#7fff#, Decoded.Word_3, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & 
                 Int_To_String (Integer(Decoded.Imm_U16), Radix, 8, false, true) & " " &
                 Char_Indirect(Decoded.Ind) &
                 Int_To_String (Integer(Decoded.Disp_31), Radix, 12, false, true) & 
                 String_Mode(Decoded.Mode) & " [3-Word Instruction]";
            end if;

         when NOACC_MODE_IND_2_WORD_E_FMT => -- eg. EJSR, PSHJ
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 6, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2 and 16#7fff#, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " &
                 Char_Indirect(Decoded.Ind) &
                  Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & String_Mode(Decoded.Mode) &
                 " [2-Word Instruction]";
            end if;

         when NOACC_MODE_IND_2_WORD_X_FMT => -- eg. XJSR
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2 and 16#7fff#, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " &
                 Char_Indirect(Decoded.Ind) &
                  Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & String_Mode(Decoded.Mode) &
                 " [2-Word Instruction]";
            end if;

         when NOACC_MODE_IND_3_WORD_FMT => -- eg. LJMP/LJSR, LNISZ, LNDSZ, LWDS
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Word_3  := Memory.RAM.Read_Word (PC + 2);
            Decoded.Disp_31 := Decode_31bit_Disp (Decoded.Word_2 and 16#7fff#, Decoded.Word_3, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Char_Indirect(Decoded.Ind) &
                 Int_To_String (Integer(Decoded.Disp_31), Radix, 12, false, true) & String_Mode(Decoded.Mode);
            end if;

         when NOACC_MODE_IND_4_WORD_FMT => -- eg. LCALL
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_31 := Decode_31bit_Disp (Decoded.Word_2 and 16#7fff#, Memory.RAM.Read_Word (PC + 2), Decoded.Mode);
            Decoded.Arg_Count := Integer(Word_To_Integer_16(Memory.RAM.Read_Word (PC + 3)));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Char_Indirect(Decoded.Ind) &
                 Int_To_String (Integer(Decoded.Disp_31), Radix, 12, false, true) & 
                 String_Mode(Decoded.Mode) & "," & Decoded.Arg_Count'Image &
                 " [4-Word Instruction]";
            end if;

         when NOACC_MODE_IND_3_WORD_XCALL_FMT => -- Unique to XCALL?
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Word_3  := Memory.RAM.Read_Word (PC + 2);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2 and 16#7fff#, Decoded.Mode);
            Decoded.Arg_Count := Integer(Decoded.Word_3);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " &
                 Char_Indirect(Decoded.Ind) &
                  Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & 
                  String_Mode(Decoded.Mode) & "," &
                  Decoded.Arg_Count'Image & " [3-Word Instruction]";
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
            Decoded.IO_Dev  := Dev_Num_T(Word_To_Unsigned_16(Get_W_Bits (Opcode, 10, 6)));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & Char_IO_Flag (Decoded.IO_Flag) & " " &
                 Decoded.Ac'Image & "," & 
                 Devices.Bus.Actions.Get_Device_Name_Or_Number (Decoded.IO_Dev);
                 -- Int_To_String (Int => Integer(Decoded.IO_Dev), Base => Radix, Width => 4, Zero_Pad => False, Truncate => True);
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

         when ONEACC_1_WORD_FMT => -- eg. CVWN, HLV, LDAFP
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image;
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

         when ONEACC_IMM_3_WORD_FMT => -- eg. WADDI, WSANA, WUGTI, WXORI
             Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
             Decoded.Imm_DW  := RAM.Read_Dword (PC + 1);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & 
                 Dword_To_String(Decoded.Imm_DW, Radix, 11, false ) & "," &
                 Decoded.Ac'Image & " [3-Word Instruction]";
            end if;

         when ONEACC_IMMWD_2_WORD_FMT => -- eg. ADDI, NADDI, NLDAI, WASHI, WSEQI, WLSHI, WNADI
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Word_To_String (Decoded.Word_2, Radix, 8, false) & "," & 
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
                 Int_To_String (Integer(Decoded.Disp_15 * 2), Radix, 8, false, true) & "+" &
                 Low_Byte_To_Char (Decoded.Low_Byte) &
                 String_Mode(Decoded.Mode) & " [2-Word Instruction]";
            end if;
         
         when ONEACC_MODE_3_WORD_B_FMT => -- eg. LLDB, LLEFB
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 1, 2)));
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);   
            Decoded.Word_3  := Memory.RAM.Read_Word (PC + 2);
            Decoded.Disp_31 := Dword_To_Integer_32 (Dword_From_Two_Words (Decoded.Word_2,Decoded.Word_3)); --  FIXME - 32 not 31 !
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image & "," &
                 Int_To_String (Integer(Decoded.Disp_31), Radix, 11, false, true) &
                 String_Mode(Decoded.Mode) & " [3-Word Instruction]";
            end if;

         when ONEACC_MODE_IND_2_WORD_E_FMT => -- eg. DSPA, ELDA, ELDB, ELEF, ESTA
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 6, 2)));
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2 and 16#7fff#, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image & "," &
                 Char_Indirect(Decoded.Ind) & 
                 Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & String_Mode(Decoded.Mode) &
                 " [2-Word Instruction]";
            end if;

         when ONEACC_MODE_IND_2_WORD_X_FMT => -- eg. XNLDA, XWMUL & many others
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 1, 2)));
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2 and 16#7fff#, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image & "," &
                 Char_Indirect(Decoded.Ind) & 
                 Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) & 
                 -- Decoded.Disp_15'Image & "." &
                 String_Mode(Decoded.Mode) &
                 " [2-Word Instruction]";
            end if;

         when ONEACC_MODE_IND_3_WORD_FMT => -- eg. LLEF, LNADD/SUB LNDIV, LNLDA/STA, LNMUL, LWLDA/LWSTA, LNLDA
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 1, 2)));
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Word_3  := Memory.RAM.Read_Word (PC + 2);
            Decoded.Disp_31 := Decode_31bit_Disp (Decoded.Word_2 and 16#7fff#, Decoded.Word_3, Decoded.Mode);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " &Decoded.Ac'Image & "," &
                 Char_Indirect(Decoded.Ind) & 
                 Int_To_String (Integer(Decoded.Disp_31), Radix, 11, false, true) & String_Mode(Decoded.Mode) &
                 " [3-Word Instruction]";
            end if;

         when SPLIT_8BIT_DISP_FMT => -- eg. WBR, always a signed displacement
            Tmp_8bit := Byte_T(Get_W_Bits(Opcode, 1, 4));
            Tmp_8bit := Shift_Left (Tmp_8bit, 4);
            Tmp_8bit := Tmp_8bit or Byte_T(Get_W_Bits(Opcode, 6, 4));
            Decoded.Disp_8 := Integer_8(Decode_8bit_Disp(Tmp_8bit, Decoder.PC));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Disp_8'Image;
            end if;

         when THREE_WORD_DO_FMT => -- eg. XNDO
            Decoded.Mode    := Decode_Mode(Mode_Num_T(Get_W_Bits(Opcode, 3, 2)));
            Decoded.Ac      := AC_ID(Get_W_Bits (Opcode, 1, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Ind     := Test_W_Bit(Decoded.Word_2, 0);
            Decoded.Disp_15 := Decode_15bit_Disp(Decoded.Word_2 and 16#7fff#, Decoded.Mode);
            Decoded.Word_3  := Memory.RAM.Read_Word (PC + 2);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Ac'Image & "," &
                 Int_To_String (Integer(Decoded.Word_3), Radix, 11, false, true) & " " &
                 Char_Indirect(Decoded.Ind) & 
                 Int_To_String (Integer(Decoded.Disp_15), Radix, 8, false, true) &
                 String_Mode(Decoded.Mode) & " [3-Word Instruction]";
            end if;

         when TWOACC_1_WORD_FMT => -- eg. ANC, BTO, WSUB and MANY others
            Decoded.Acs     := AC_ID(Get_W_Bits (Opcode, 1, 2));
            Decoded.Acd     := AC_ID(Get_W_Bits (Opcode, 3, 2));
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Decoded.Acs'Image & "," & Decoded.Acd'Image;
            end if;

         when TWOACC_IMM_2_WORD_FMT => -- eg. CIOI
            Decoded.Acs     := AC_ID(Get_W_Bits (Opcode, 1, 2));
            Decoded.Acd     := AC_ID(Get_W_Bits (Opcode, 3, 2));
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & 
                 Dword_To_String (Dword_T(Decoded.Word_2), Radix, 11) & "," &
                 Decoded.Acs'Image & "," & Decoded.Acd'Image;
            end if;

         when UNIQUE_1_WORD_FMT => -- nothing to do 
            null;

         when UNIQUE_2_WORD_FMT =>
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            Decoded.Imm_U16 := Unsigned_16(Decoded.Word_2);
            if Disassemble then
               Decoded.Disassembly :=
                 Decoded.Disassembly & " " & Word_To_String(Decoded.Word_2, Octal, 7) & " [2-Word Instruction]";
            end if;

         when WIDE_DEC_SPECIAL_FMT => -- Funky - following word defines OpCode...
            Decoded.Word_2  := Memory.RAM.Read_Word (PC + 1);
            if Disassemble then
               case Decoded.Word_2 is
                  when 0 => Decoded.Disassembly := To_Unbounded_String ("WDMOV");
                  when 1 => Decoded.Disassembly := To_Unbounded_String ("WDCMP");
                  when 2 => Decoded.Disassembly := To_Unbounded_String ("WDINC");
                  when 3 => Decoded.Disassembly := To_Unbounded_String ("WDDEC");
                  when others => raise Decode_Failed with "Unknown WDxxx instruction";
               end case;
            end if;

         when WSKB_FMT => -- eg. WSKBO/Z
            Tmp_8bit := Byte_T(Get_W_Bits(Opcode, 1, 3));
            Tmp_8bit := Shift_Left (Tmp_8bit, 2);
            Tmp_8bit := Tmp_8bit or Byte_T(Get_W_Bits(Opcode, 10, 2));
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
            raise Decode_Failed with "Unhandled instruction format: " & Decoded.Format'Image &
               " for: " & To_String(Decoded.Mnemonic);
      end case;

      return Decoded;
   end Instruction_Decode;

end Decoder;
