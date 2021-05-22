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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body DG_Types is

   procedure Clear_W_Bit (Word : in out Word_T; Bit_Num : in Integer) is
   begin
      Word := Word and not Shift_Left (1, 15 - Bit_Num);
   end Clear_W_Bit;

   procedure Clear_QW_Bit (QW : in out Qword_T; Bit_Num : in Integer) is
   begin
      QW := QW and not Shift_Left (1, 63 - Bit_Num);
   end Clear_QW_Bit;

   -- Flip_W_Bit flips a single bit in a Word using DG numbering
   procedure Flip_W_Bit (Word : in out Word_T; Bit_Num : in Integer) is
   begin
      Word := Word xor Shift_Left (1, 15 - Bit_Num);
   end Flip_W_Bit;

   procedure Set_W_Bit (Word : in out Word_T; Bit_Num : in Integer) is
   begin
      Word := Word or Shift_Left (1, 15 - Bit_Num);
   end Set_W_Bit;

   procedure Set_QW_Bit (QW : in out Qword_T; Bit_Num : in Integer) is
   begin
      QW := QW or Shift_Left (1, 63 - Bit_Num);
   end Set_QW_Bit;

   -- Does Word have bit <n> set?
   function Test_W_Bit (Word : in Word_T; Bit_Num : in Integer) return Boolean
   is ((Word and Shift_Left (1, 15 - Bit_Num)) /= 0);

   -- Does Dword have bit <n> set?
   function Test_DW_Bit (DW : in Dword_T; Bit_Num : in Integer) return Boolean
   is ((DW and Shift_Left (1, 31 - Bit_Num)) /= 0);

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
      (Byte_T (Word and 16#00ff#));

   -- return DG Upper (LH) byte of a Word
   function Get_Upper_Byte (Word : in Word_T) return Byte_T is
      (Byte_T (Shift_Right (Word and 16#ff00#, 8)));

   function Swap_Bytes (Word : in Word_T) return Word_T is
      (Shift_Right (Word and 16#ff00#, 8) or
       Shift_Left (Word and 16#00ff#, 8));

   function Lower_Word (DW : in Dword_T) return Word_T is
      (Word_T (DW and 16#0000_ffff#));

   function Lower_Dword ( QW : in Qword_T) return Dword_T is
      (Dword_T(QW and 16#ffff_ffff#));

   function Upper_Word (DW : in Dword_T) return Word_T is
      (Word_T (Shift_Right (DW and 16#ffff_0000#, 16)));

   function Upper_Dword (QW : in Qword_T) return Dword_T is
      (Dword_T (Shift_Right (QW, 32)));

   procedure Get_Bytes_From_Word
     (Word : in Word_T; Low_Byte, High_Byte : out Byte_T)
   is
   begin
      Low_Byte  := Byte_T (Word and 16#00ff#);
      High_Byte := Byte_T (Shift_Right (Word and 16#ff00#, 8));
   end Get_Bytes_From_Word;

   function Word_From_Bytes (Lo, Hi : in Byte_T) return Word_T is
      (Shift_Left (Word_T (Lo), 8) or Word_T (Hi));

   function Dword_From_Two_Words (Word_1, Word_2 : in Word_T) return Dword_T is
      (Shift_Left (Dword_T (Word_1), 16) or Dword_T (Word_2));

   function Qword_From_Two_Dwords (Dword_1, Dword_2 : in Dword_T) return Qword_T is
      (Shift_Left (Qword_T(Dword_1), 32) or Qword_T(Dword_2));

   function Boolean_To_YN (B : Boolean) return Character is
      (if B then 'Y' else 'N');

   function Low_Byte_To_Char (LB : in Boolean) return Character is
      (if LB then 'L' else 'H');

   function Byte_Arr_To_Unbounded (B_Arr : in Byte_Arr_T) return Unbounded_String is
      Res : Unbounded_String;
   begin
      for C in B_Arr'Range loop
         Res := Res & Byte_To_Char(B_Arr(C));
      end loop;
      return Res;
   end Byte_Arr_To_Unbounded;

   procedure Get_Data_Sensitive_Portion (B_Arr     : in Byte_Arr_T;
                                         Max_Len   : in Integer;
                                         Last_Byte : out Integer;
                                         Too_Long  : out Boolean) is
   begin
      Too_Long := true;
      for B in B_Arr'Range loop
         if (B_Arr(B) = 0) then -- or (B_Arr(B) = 10) or(B_Arr(B) = 12) or(B_Arr(B) = 13) then
            if B > Max_Len then
               exit;
            end if;
            Last_Byte := B - 1;
            Too_Long := false;
            exit;
         end if;
      end loop;

   end Get_Data_Sensitive_Portion;

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

   procedure Decode_Dec_Data_Type (DTI          : in Dword_T; 
                                   Scale_Factor : out Integer_8;
                                   Dec_Type     : out Natural;
                                   Size         : out Natural) is
   begin
      Scale_Factor := Integer_8(Get_DW_Bits(DTI, 0, 8));
      Dec_Type     := Natural(Unsigned_8(Get_DW_Bits(DTI, 24, 3)));
      Size         := Natural(Unsigned_8(Get_DW_Bits(DTI, 27, 5)));
      if Dec_Type /= Packed_Dec then
         Size := Size + 1;
      end if;
   end Decode_Dec_Data_Type;

   -- Floating-Point Conversions...

   function DG_Double_To_Long_Float (DG_Dbl : in Double_Overlay) return Long_Float is
      LF : Long_Float := 0.0;
      Exp_I : Integer := Integer(DG_Dbl.Double_Phys.Exponent) - 64;
      Mant_LF : Long_Float := Long_Float(DG_Dbl.Double_Phys.Mantissa);
   begin
      if DG_Dbl.Double_Phys.Mantissa /= 0 then
         LF := Mant_LF * Long_Float(2 ** (-24 + (4 * Exp_I)));
         if DG_Dbl.Double_Phys.Sign then
            LF := LF * (-1.0);
         end if;
      end if;
      return LF;
   end DG_Double_To_Long_Float;

   function Long_Float_To_DG_Double (LF : in Long_Float) return Qword_T is
      Working_Float : Long_Float := LF;
      DG_Dbl : Double_Overlay;
      Shifts : Integer := 0;
   begin
      DG_Dbl.Double_QW := 0; -- zero everything, fine for zero-val return
      if LF /= 0.0 then
         if LF < 0.0 then
            DG_Dbl.Double_Phys.Sign := true;
            Working_Float := Working_Float * (-1.0);
         end if;
         if Working_Float > 0.0625 then
            while Working_Float >= 1.0 loop
               Working_Float := Working_Float / 16.0; -- lossy, should be a shift
               Shifts := Shifts + 1;
            end loop;
         else
            while Working_Float > 0.0625 loop
               Shifts := Shifts - 1;
               Working_Float := Working_Float * 16.0; -- see above
            end loop;
         end if;
         DG_Dbl.Double_Phys.Exponent := Double_Exp(Shifts + 64);
         DG_Dbl.Double_Phys.Mantissa := Double_Mant(Long_Float'Fraction(Working_Float));
      end if;
      return DG_Dbl.Double_QW;
   end Long_Float_To_DG_Double;

end DG_Types;