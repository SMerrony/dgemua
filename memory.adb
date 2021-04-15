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

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Debug_Logs; use Debug_Logs;

package body Memory is

   protected body RAM is

      procedure Init (Debug_Logging : in Boolean) is
      begin
         Is_Logging  := Debug_Logging;
         ATU_Enabled := False;
         -- TODO BMC_DCH_Init
         for W in RAM'Range loop
            RAM (W) := 0;
         end loop;
         BMC_DCH.Init (Debug_Logging);
         Put_Line
           ("INFO: Initialised " & Integer'Image (RAM'Length) &
            " words of main memory");
      end Init;

      function Read_Byte
        (Word_Addr : in Phys_Addr_T; Low_Byte : in Boolean) return Byte_T
      is
         W : Word_T;
      begin
         W := RAM (Integer (Word_Addr));
         if not Low_Byte then
            W := Shift_Right (W, 8);
         end if;
         return Byte_T (W and 16#00ff#);
      end Read_Byte;

      procedure Write_Byte (Word_Addr : in Phys_Addr_T; Low_Byte : in Boolean; Byt : in Byte_T) is
         Wd : Word_T := Read_Word(Word_Addr);
      begin
         if Low_Byte then
            Wd := (Wd and 16#ff00#) or Word_T(Byt);
         else
            Wd := Shift_Left(Word_T(Byt), 8) or (Wd and 16#00ff#);
         end if;
         Write_Word(Word_Addr, Wd);
      end Write_Byte;

      function  Read_Byte_Eclipse_BA (Segment : in Phys_Addr_T; BA_16 : in Word_T) return Byte_T is
         Low_Byte : Boolean := Test_W_Bit(BA_16, 15);
         Addr : Phys_Addr_T;
      begin
         Addr := Shift_Right(Phys_Addr_T(BA_16), 1) or Segment;
         return Read_Byte(Addr, Low_Byte);
      end Read_Byte_Eclipse_BA;

      function Read_Dword (Word_Addr : in Phys_Addr_T) return Dword_T is
      begin
         return
           Dword_From_Two_Words
             (RAM (Integer (Word_Addr)), RAM (Integer (Word_Addr) + 1));
      end Read_Dword;

      procedure Write_Dword (Word_Addr : in Phys_Addr_T; Datum : Dword_T) is
      begin
         Write_Word (Word_Addr, Upper_Word (Datum));
         Write_Word (Word_Addr + 1, Lower_Word (Datum));
      end Write_Dword;

      function Read_Word (Word_Addr : in Phys_Addr_T) return Word_T is
      begin
         return RAM (Integer (Word_Addr));
      end Read_Word;

      procedure Write_Word (Word_Addr : in Phys_Addr_T; Datum : Word_T) is
      begin
         -- -- DEBUGGING
         -- if Word_Addr = 50 then
         --    Put_Line ("Writing to Location ")
         -- end if;
         RAM (Integer (Word_Addr)) := Datum;
      end Write_Word;

   end RAM;

   protected body Narrow_Stack is
      procedure Push (Segment : in Phys_Addr_T; Datum : in Word_T) is
         New_NSP : Word_T := RAM.Read_Word (NSP_Loc or Segment) + 1;
      begin
         RAM.Write_Word (NSP_Loc or Segment, New_NSP);
         RAM.Write_Word (Phys_Addr_T (New_NSP) or Segment, Datum);
      end Push;

      function Pop (Segment : in Phys_Addr_T) return Word_T is
         Old_NSP : Word_T := RAM.Read_Word (NSP_Loc or Segment);
         Datum   : Word_T := RAM.Read_Word (Phys_Addr_T (Old_NSP) or Segment);
      begin
         RAM.Write_Word (NSP_Loc or Segment, Old_NSP - 1);
         return Datum;
      end Pop;
   end Narrow_Stack;

   protected body BMC_DCH is

      procedure Init (Debug_logging : in Boolean) is
      begin
         Is_Logging := Debug_logging;
         Reset;
      end Init;

      procedure Reset is
      begin
         for R in Registers'Range loop
            Registers (R) := 0;
         end loop;
         Registers (IO_Chan_Def_Reg)    := IOC_CDR_1;
         Registers (IO_Chan_Status_Reg) := IOC_SR_1A or IOC_SR_1B;
         Registers (IO_Chan_Mask_Reg)   :=
           IOC_MR_MK1 or IOC_MR_MK2 or IOC_MR_MK3 or IOC_MR_MK4 or
           IOC_MR_MK5 or IOC_MR_MK6;
         Put_Line ("INFO: BMC_DCH Registers Reset");
      end Reset;

      function Read_Reg (Reg : in Integer) return Word_T is
      begin
         return Registers (Reg);
      end Read_Reg;

      -- Write_Reg populates a given 16-bit register with the supplied data
      -- N.B. Addressed by REGISTER not slot
      procedure Write_Reg (Reg : in Integer; Datum : in Word_T) is
      begin
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Write_Reg with Register: " & Reg'Image & " Datum:" &
               Datum'Image);
         end if;
         if Reg = IO_Chan_Def_Reg then
            -- certain bits in the new data cause IOCDR bits to be flipped rather than set
            for B in 0 .. 15 loop
               case B is
                  when 3 | 4 | 7 | 8 | 14 =>
                     if Test_W_Bit (Datum, B) then
                        Flip_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     end if;
                  when others =>
                     if Test_W_Bit (Datum, B) then
                        Set_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     else
                        Clear_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     end if;
               end case;
            end loop;
         else
            Registers (Reg) := Datum;
         end if;
      end Write_Reg;

      -- Write_Slot populates a whole SLOT (pair of registers) with the supplied doubleword
      -- N.B. Addressed by SLOT not register
      procedure Write_Slot (Slot : in Integer; Datum : in Dword_T) is
      begin
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Write_Slot with Slot: " & Slot'Image & " Datum:" &
               Datum'Image);
         end if;
         Registers (Slot * 2)       := Upper_Word (Datum);
         Registers ((Slot * 2) + 1) := Lower_Word (Datum);
      end Write_Slot;

      function Get_DCH_Mode return Boolean is
      begin
         return Test_W_Bit (Registers (IO_Chan_Def_Reg), 14);
      end Get_DCH_Mode;

      function Resolve_BMC_Mapped_Addr (M_Addr : in Phys_Addr_T) return Phys_Addr_T is
         Slot           : Integer := Integer(Shift_Right(M_Addr, 10));
         P_Addr, P_Page : Phys_Addr_T;
      begin
         -- N.B. at some point between 1980 and 1987 the lower 5 bits of the odd word were
	      -- prepended to the even word to extend the mappable space 
         P_Page := Shift_Left (Phys_Addr_T(Registers(Slot * 2)) and 16#0000_001f#, 16) +
                   Shift_Left (Phys_Addr_T(Registers((Slot * 2) + 1)), 10);
         P_Addr := (M_Addr and 16#0000_03ff#) or P_Page;
         return P_Addr;
      end Resolve_BMC_Mapped_Addr;

      function Resolve_DCH_Mapped_Addr
        (M_Addr : in Phys_Addr_T) return Phys_Addr_T
      is
         P_Addr, P_Page : Phys_Addr_T;
         Slot           : Integer;
         Offset         : Phys_Addr_T;
      begin
         -- the slot is up to 9 bits long
         Slot :=
           Integer (Shift_Right (M_Addr, 10) and 16#001f#) + First_DCH_Slot;
         if (Slot < First_DCH_Slot) or
           (Slot > (First_DCH_Slot + Num_DCH_Slots))
         then
            raise Invalid_DCH_Slot;
         end if;
         Offset := M_Addr and 16#0000_03ff#;
         -- N.B. at some point between 1980 and 1987 the lower 5 bits of the odd word were
         -- prepended to the even word to extend the mappable space
         P_Page :=
           Shift_Left
             (Phys_Addr_T (Registers (Slot * 2) and 16#0000_001f#), 16) or
           Phys_Addr_T (Registers ((Slot * 2) + 1));
         P_Addr := Shift_Left (P_Page, 10) or Offset;
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Resolve_DCH_Mapped_Addr got: " & M_Addr'Image &
               " Returning: " & P_Addr'Image);
         end if;
         return P_Addr;
      end Resolve_DCH_Mapped_Addr;

      procedure Write_Word_DCH_Chan
        (Unmapped : in out Phys_Addr_T; Datum : in Word_T)
      is
         P_Addr : Phys_Addr_T;
      begin
         if Get_DCH_Mode then
            P_Addr := Resolve_DCH_Mapped_Addr (Unmapped);
         else
            P_Addr := Unmapped;
         end if;
         RAM.Write_Word (P_Addr, Datum);
         -- auto-increment the supplied address
         Unmapped := Unmapped + 1;
      end Write_Word_DCH_Chan;

      function Decode_BMC_Addr (Unmapped : in Phys_Addr_T) return BMC_Addr_T is
         In_Addr : Phys_Addr_T := Shift_Left(Unmapped, 10);
         Res : BMC_Addr_T;
      begin
         Res.Is_Logical := Test_DW_Bit (Dword_T(In_Addr), 0);
         if Res.Is_Logical then
            Res.TT   := Natural(Get_DW_Bits(Dword_T(In_Addr), 2, 5));
            Res.TTR  := Natural(Get_DW_Bits(Dword_T(In_Addr), 7, 5));
            Res.P_Low := Unmapped and 16#0000_3fff#;
         else
            Res.Bk  := Natural(Get_DW_Bits(Dword_T(In_Addr), 1, 3));
            Res.XCA := Natural(Get_DW_Bits(Dword_T(In_Addr), 4, 3));
            Res.CA  := Unmapped and 16#0000_7fff#;
         end if;
         return Res;
      end Decode_BMC_Addr;

      procedure Read_Word_BMC_16 (Unmapped : in out Word_T; Datum : out Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : BMC_Addr_T := Decode_BMC_Addr (Phys_Addr_T(Unmapped));
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Phys_Addr_T(Unmapped)); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         Datum := RAM.Read_Word (P_Addr);
         Unmapped := Unmapped + 1;
      end Read_Word_BMC_16;

      procedure Write_Word_BMC_16 (Unmapped : in out Word_T; Datum : in Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : BMC_Addr_T := Decode_BMC_Addr (Phys_Addr_T(Unmapped));
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Phys_Addr_T(Unmapped)); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         RAM.Write_Word (P_Addr, Datum);
         Unmapped := Unmapped + 1;
      end Write_Word_BMC_16;

   end BMC_DCH;

   procedure Clear_W_Bit (Word : in out Word_T; Bit_Num : in Integer) is
   begin
      Word := Word and not Shift_Left (1, 15 - Bit_Num);
   end Clear_W_Bit;

   -- Flip_W_Bit flips a single bit in a Word using DG numbering
   procedure Flip_W_Bit (Word : in out Word_T; Bit_Num : in Integer) is
   begin
      Word := Word xor Shift_Left (1, 15 - Bit_Num);
   end Flip_W_Bit;

   procedure Set_W_Bit (Word : in out Word_T; Bit_Num : in Integer) is
   begin
      Word := Word or Shift_Left (1, 15 - Bit_Num);
   end Set_W_Bit;

   -- Does Word have bit <n> set?
   function Test_W_Bit (Word : in Word_T; Bit_Num : in Integer) return Boolean
   is
   begin
      return (Word and Shift_Left (1, 15 - Bit_Num)) /= 0;
   end Test_W_Bit;

   -- Does Dword have bit <n> set?
   function Test_DW_Bit (DW : in Dword_T; Bit_Num : in Integer) return Boolean
   is
   begin
      return (DW and Shift_Left (1, 31 - Bit_Num)) /= 0;
   end Test_DW_Bit;

   -- Get_W_Bits - in the DG world, the first (leftmost) bit is numbered zero...
   -- extract nbits from value starting at leftBit
   function Get_W_Bits
     (Word : in Word_T; First_Bit, Num_Bits : Natural) return Word_T
   is
      Mask : Word_T := Shift_Left (1, Num_Bits) - 1;
   begin
      if First_Bit >= 16 then
         return 0;
      end if;
      return Shift_Right (Word, 16 - (First_Bit + Num_Bits)) and Mask;
   end Get_W_Bits;

   function Get_DW_Bits (Dword : in Dword_T; First_Bit, Num_Bits : Natural) return Dword_T is
      Mask : Dword_T := Shift_Left (1, Num_Bits) - 1;
   begin
      if First_Bit >= 32 then
         return 0;
      end if;
      return Shift_Right (Dword, 32 - (First_Bit + Num_Bits)) and Mask;
   end Get_DW_Bits;

   -- return DG lower (RH) byte of a Word
   function Get_Lower_Byte (Word : in Word_T) return Byte_T is
   begin
      return Byte_T (Word and 16#00ff#);
   end Get_Lower_Byte;

   -- return DG Upper (LH) byte of a Word
   function Get_Upper_Byte (Word : in Word_T) return Byte_T is
   begin
      return Byte_T (Shift_Right (Word and 16#ff00#, 8));
   end Get_Upper_Byte;

   function Swap_Bytes (Word : in Word_T) return Word_T is
   begin
      return
        Shift_Right (Word and 16#ff00#, 8) or
        Shift_Left (Word and 16#00ff#, 8);
   end Swap_Bytes;

   function Lower_Word (DW : in Dword_T) return Word_T is
   begin
      return Word_T (DW and 16#0000_ffff#);
   end Lower_Word;
   pragma Inline (Lower_Word);

   function Upper_Word (DW : in Dword_T) return Word_T is
   begin
      return Word_T (Shift_Right (DW and 16#ffff_0000#, 16));
   end Upper_Word;

   procedure Get_Bytes_From_Word
     (Word : in Word_T; Low_Byte, High_Byte : out Byte_T)
   is
   begin
      Low_Byte  := Byte_T (Word and 16#00ff#);
      High_Byte := Byte_T (Shift_Right (Word and 16#ff00#, 8));
   end Get_Bytes_From_Word;

   function Word_From_Bytes (Lo, Hi : in Byte_T) return Word_T is
   begin
      return Shift_Left (Word_T (Lo), 8) or Word_T (Hi);
   end Word_From_Bytes;

   function Dword_From_Two_Words (Word_1, Word_2 : in Word_T) return Dword_T is
   begin
      return Shift_Left (Dword_T (Word_1), 16) or Dword_T (Word_2);
   end Dword_From_Two_Words;

   function Boolean_To_YN (B : Boolean) return Character is
   begin
      if B then
         return 'Y';
      else
         return 'N';
      end if;
   end Boolean_To_YN;

   function Low_Byte_To_Char (LB : in Boolean) return Character is
   begin
      if LB then
         return 'L';
      else
         return 'H';
      end if;
   end Low_Byte_To_Char;

   function String_To_Integer
     (Str : in String; Base : in Number_Base_T) return Integer
   is
      Neg       : Boolean := False;
      Res       : Integer := 0;
      Num_Start : Integer := Str'First;
      Hex_I     : Integer;
      Hexes     : String  := "0123456789ABCDEF";
      Val       : Integer;
   begin
      if Str (Str'First) = '-' then
         Neg       := True;
         Num_Start := Num_Start + 1;
      elsif Str (Str'First) = '+' then
         Num_Start := Num_Start + 1;
      end if;
      for C in Num_Start .. Str'Last loop
         Val := Character'Pos (Str (C)) - 48;
         case Base is
            when Octal =>
               Res := (Res * 8) + Val;
            when Decimal =>
               Res := (Res * 10) + Val;
            when Hex =>
               --Hex_I := Ada.Strings.Fixed.Index ( "0123456789ABCDEF", String(Str(C)) ) - 1 ;
               Hex_I :=
                 Ada.Strings.Fixed.Index ("0123456789ABCDEF", Str (C)'Image) -
                 1;
               Res := (Res * 16) + Integer (Hex_I);
            when others =>
               null;
         end case;
      end loop;
      return Res;
   end String_To_Integer;

   -- String_To_Dword tries to convert an unsigned string in the given base
   function String_To_Dword
     (Str : in String; Base : in Number_Base_T) return Dword_T
   is
      Res   : Dword_T := 0;
      Hex_I : Integer;
   begin
      for C of Str loop
         case Base is
            when Octal =>
               Res := (Res * 8) + Dword_T'Value ((1 => C));
            when Decimal =>
               Res := (Res * 10) + Dword_T'Value ((1 => C));
            when Hex =>
               Hex_I :=
                 Ada.Strings.Fixed.Index ("0123456789ABCDEF", C'Image) - 1;
               Res := (Res * 16) + Dword_T (Hex_I);
            when others =>
               null;
         end case;
      end loop;
      return Res;
   end String_To_Dword;

   function Sext_Word_To_Dword (Wd : in Word_T) return Dword_T is
      Dw : Dword_T;
   begin
      Dw := Dword_T (Wd);
      if (Wd and 16#8000#) /= 0 then
         Dw := Dw or 16#ffff_0000#;
      end if;
      return Dw;
   end Sext_Word_To_Dword;

   -- Convert an (unsigned) Double-Word to a String
   function Dword_To_String
     (DW       : in Dword_T; Base : in Number_Base_T; Width : in Integer;
      Zero_Pad : in Boolean := False) return String
   is
      Res       : String (1 .. Width);
      Tmp_DW    : Dword_T          := DW;
      Bas_DW    : Dword_T;
      Remainder : Integer;
      Binaries  : String (1 .. 2)  := "01";
      Octals    : String (1 .. 8)  := "01234567";
      Decimals  : String (1 .. 10) := "0123456789";
      Hexes     : String (1 .. 16) := "0123456789ABCDEF";
      Col       : Integer          := Width;
   begin
      if Zero_Pad then
         for C in Res'Range loop
            Res (C) := '0';
         end loop;
      else
         for C in Res'Range loop
            Res (C) := ' ';
         end loop;
      end if;
      case Base is
         when Binary =>
            Bas_DW := 2;
         when Octal =>
            Bas_DW := 8;
         when Decimal =>
            Bas_DW := 10;
         when Hex =>
            Bas_DW := 16;
      end case;
      loop
         Remainder := Integer (Tmp_DW mod Bas_DW);
         Tmp_DW    := Tmp_DW / Bas_DW;
         case Base is
            when Binary =>
               Res (Col) := Binaries (Remainder + 1);
            when Octal =>
               Res (Col) := Octals (Remainder + 1);
            when Decimal =>
               Res (Col) := Decimals (Remainder + 1);
            when Hex =>
               Res (Col) := Hexes (Remainder + 1);
            when others =>
               null;
         end case;
         Col := Col - 1;
         exit when Tmp_DW = 0 or Col = 0;
      end loop;
      return Res;
   end Dword_To_String;

   function Int_To_String
     (Int      : in Integer; Base : in Number_Base_T; Width : in Integer;
      Zero_Pad : in Boolean := False; Truncate : in Boolean := False)
      return String
   is
      Res       : String (1 .. Width);
      Tmp_Int   : Integer          := Int;
      Bas_Int   : Integer;
      Remainder : Integer;
      Octals    : String (1 .. 8)  := "01234567";
      Decimals  : String (1 .. 10) := "0123456789";
      Hexes     : String (1 .. 16) := "0123456789ABCDEF";
      Col       : Integer          := Width;
      Negative  : Boolean          := Int < 0;
   begin
      if Zero_Pad then
         for C in Res'Range loop
            Res (C) := '0';
         end loop;
      else
         for C in Res'Range loop
            Res (C) := ' ';
         end loop;
      end if;
      case Base is
         when Binary =>
            Bas_Int := 2;
         when Octal =>
            Bas_Int := 8;
         when Decimal =>
            Bas_Int := 10;
         when Hex =>
            Bas_Int := 16;
      end case;
      loop
         Remainder := Integer (Tmp_Int mod Bas_Int);
         Tmp_Int   := Tmp_Int / Bas_Int;
         case Base is
            when Octal =>
               Res (Col) := Octals (Remainder + 1);
            when Decimal =>
               Res (Col) := Decimals (Remainder + 1);
            when Hex =>
               Res (Col) := Hexes (Remainder + 1);
            when others =>
               null;
         end case;
         Col := Col - 1;
         exit when Tmp_Int = 0 or Col = 0;
      end loop;
      if Negative then
         Res (Col) := '-';
      end if;
      if Truncate then
         return Trim (Res, Both);
      end if;
      return Res;
   end Int_To_String;

   -- Convert an (unsigned) Byte to a String
   function Byte_To_String
     (Byt      : in Byte_T; Base : in Number_Base_T; Width : in Integer;
      Zero_Pad : in Boolean := False) return String
   is
      Res       : String (1 .. Width);
      Tmp_Byt   : Byte_T           := Byt;
      Bas_Byt   : Byte_T;
      Remainder : Integer;
      Octals    : String (1 .. 8)  := "01234567";
      Decimals  : String (1 .. 10) := "0123456789";
      Hexes     : String (1 .. 16) := "0123456789ABCDEF";
      Col       : Integer          := Width;
   begin
      if Zero_Pad then
         for C in Res'Range loop
            Res (C) := '0';
         end loop;
      else
         for C in Res'Range loop
            Res (C) := ' ';
         end loop;
      end if;
      case Base is
         when Binary =>
            Bas_Byt := 2;
         when Octal =>
            Bas_Byt := 8;
         when Decimal =>
            Bas_Byt := 10;
         when Hex =>
            Bas_Byt := 16;
      end case;
      loop
         Remainder := Integer (Tmp_Byt mod Bas_Byt);
         Tmp_Byt   := Tmp_Byt / Bas_Byt;
         case Base is
            when Octal =>
               Res (Col) := Octals (Remainder + 1);
            when Decimal =>
               Res (Col) := Decimals (Remainder + 1);
            when Hex =>
               Res (Col) := Hexes (Remainder + 1);
            when others =>
               null;
         end case;
         Col := Col - 1;
         exit when Tmp_Byt = 0 or Col = 0;
      end loop;
      return Res;
   end Byte_To_String;

end Memory;
