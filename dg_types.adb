-- MIT License

-- Copyright (c)2021,2022 Stephen Merrony

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

with Memory; use Memory;

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

    -- Does Qword have bit <n> set?
   function Test_QW_Bit (QW : in Qword_T; Bit_Num : in Integer) return Boolean
   is ((QW and Shift_Left (1, 63 - Bit_Num)) /= 0);

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
                                         DS_Bytes  : out Integer) is
      Ix : Integer := B_Arr'First;
   begin
      DS_Bytes := 0;
      loop
         -- delimiter?
         exit when B_Arr(Ix) = 0;
         DS_Bytes := DS_Bytes + 1;
         Ix := Ix + 1;
         exit when Ix = B_Arr'Last;
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

   function String_To_Dword (Str : in String; Base : in Number_Base_T) return Dword_T is
      -- Attempt to convert Str to a Dword, may raise CONSTRAINT_ERROR
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

   -- Convert an (unsigned) Word to a String
   function Word_To_String
     (WD       : in Word_T; Base : in Number_Base_T; Width : in Integer;
      Zero_Pad : in Boolean := False) return String
   is
      Res       : String (1 .. Width);
      Tmp_WD    : Word_T           := WD;
      Bas_WD    : Word_T;
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
            Bas_WD := 2;
         when Octal =>
            Bas_WD := 8;
         when Decimal =>
            Bas_WD := 10;
         when Hex =>
            Bas_WD := 16;
      end case;
      loop
         Remainder := Integer (Tmp_WD mod Bas_WD);
         Tmp_WD    := Tmp_WD / Bas_WD;
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
         exit when Tmp_WD = 0 or Col = 0;
      end loop;
      return Res;
   end Word_To_String;

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
      if Negative then
         Tmp_Int := -Tmp_Int;
      end if;
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

   -- Decimal (Commericial) routines...

   procedure Decode_Dec_Data_Type (DTI          : in Dword_T; 
                                   Scale_Factor : out Integer_8;
                                   Dec_Type     : out Natural;
                                   Size         : out Natural) is
   begin
      Scale_Factor := Integer_8(Get_DW_Bits(DTI, 0, 8));
      Dec_Type     := Natural(Unsigned_8(Get_DW_Bits(DTI, 24, 3)));
      Size         := Natural(Unsigned_8(Get_DW_Bits(DTI, 27, 5)));
      if Dec_Type = Packed_Dec then
         if Size mod 2 = 0 then
            Size := Size + 1;
         end if;
      else
         Size := Size + 1;
      end if;
      
   end Decode_Dec_Data_Type;

   function Read_Decimal (BA : in Dword_T; Size : in Natural) return Unbounded_String is
      B_Arr : Byte_Arr_T := RAM.Read_Bytes_BA (BA, Size);
      Res_US : Unbounded_String;
   begin
      for C in 0 .. Size - 1 loop
         Res_US := Res_US & Byte_To_Char(B_Arr(C));
      end loop;
      Res_US := Trim(Res_US, Both);
      return Res_US;
   end Read_Decimal;

   -- Floating-Point Conversions...

-- #define IBM32_SIGN ((npy_uint32)0x80000000U)
-- #define IBM32_EXPT ((npy_uint32)0x7f000000U)
-- #define IBM32_FRAC ((npy_uint32)0x00ffffffU)
-- #define IBM32_TOP  ((npy_uint32)0x00f00000U)
-- #define BITCOUNT_MAGIC ((npy_uint32)0x000055afU)
-- #define TIES_TO_EVEN_RSHIFT3  ((npy_uint64)0x000000000000000bU)

   function DG_Double_To_Long_Float (DG_Dbl : in Double_Overlay) return Long_Float is
   -- decode a DG Double (64-bit) float - which was the same as IBM's format
   -- baSed on https://github.com/enthought/ibm2ieee/blob/main/ibm2ieee/_ibm2ieee.c
      IBM : Unsigned_64 := Unsigned_64(DG_Dbl.Double_QW);
      IBM_Expt, IEEE_Expt, Leading_Zeros : Integer;
      IBM_Frac, Top_Digit                : Unsigned_64;
      IEEE_Sign, IEEE_Frac, Round_Up     : Unsigned_64;
      U_64  : Unsigned_64;
      IEEE  : IEEE_Float_64;
      for IEEE'Address use U_64'Address;
   begin
      IEEE_Sign := IBM and 16#8000_0000_0000_0000#;
      IBM_Frac  := IBM and 16#00ff_ffff_ffff_ffff#;

      if IBM_Frac = 0 then
         return 0.0;
      end if;

      -- reduce shift by 2 to get binary exp from hex
      IBM_Expt := Integer(Shift_Right((IBM and 16#7f00_0000_0000_0000#), 54));

      -- normalise mantissa, then count leading zeroes
      Top_Digit := Unsigned_64(IBM and 16#00f0_0000_0000_0000#);
      while Top_Digit = 0 loop
         IBM_Frac := Shift_Left(IBM_Frac, 4);
         IBM_Expt := IBM_Expt - 4;
         Top_Digit := IBM_Frac and 16#00f0_0000_0000_0000#;
      end loop;
      Leading_Zeros := Integer(Shift_Right(Unsigned_64(16#0000_55af#), Integer(Shift_Right (Top_Digit, 51))) and 16#03#);

      IBM_Frac := Shift_Left(IBM_Frac, Leading_Zeros);
      IEEE_Expt := IBM_Expt + 765 - Leading_Zeros;

      Round_Up := (if (IBM_Frac and 16#000000000000000b#) /= 0 then 1 else 0); 
      IEEE_Frac := Shift_Right(Shift_Right(IBM_Frac,2) + Round_Up, 1);

      U_64 := IEEE_Sign + Shift_Left(Unsigned_64(IEEE_Expt), 52) + IEEE_Frac;

      return Long_Float(IEEE);
   end DG_Double_To_Long_Float;

   function DG_Single_To_Long_Float (Single : in Dword_T) return Long_Float is
      DG_Dbl : Double_Overlay;
   begin
      DG_Dbl.Double_QW := Shift_Left(Qword_T(Single), 32);
      return DG_Double_To_Long_Float(DG_Dbl);
   end DG_Single_To_Long_Float;

   function Long_Float_To_DG_Double (LF : in Long_Float) return Qword_T is
      IEEE : IEEE_Float_64 := IEEE_Float_64(LF);
      U_64 : Unsigned_64;
      for U_64'Address use IEEE'Address;
      IBM  : Unsigned_64 := 0;
      IBM_Sign, IBM_Expt, IBM_Frac : Unsigned_64;
      IEEE_Expt_U16 : Unsigned_16;
      IEEE_Expt_I : Integer;
      IEEE_Frac : Unsigned_64;
    begin
      if LF = 0.0 then
         return 0;
      end if;
      IBM_Sign := U_64 and 16#8000_0000_0000_0000#;
      IEEE_Expt_U16 := Unsigned_16(Shift_Right((U_64 and 16#7ff0_0000_0000_0000#), 52));
      IEEE_Expt_I := Integer(IEEE_Expt_U16);
      IEEE_Frac := Shift_Right((U_64 and 16#000f_ffff_ffff_ffff#), 1) or 16#0008_0000_0000_0000#;

      -- IEEE_Expt_I := IEEE_Expt_I + 130;
      -- Shift_Amt := Integer(Unsigned_32((- IEEE_Expt_I)) and Unsigned_32(3));
      -- IEEE_Frac := Shift_Right(IEEE_Frac, Shift_Amt);
      -- IEEE_Expt_I := Integer(Shift_Right(Unsigned_32(IEEE_Expt_I + 3), 2));

      -- while IEEE_Frac < 16#1000_0000_0000_0000# loop
      --    IEEE_Expt_I := IEEE_Expt_I - 1;
      --    IEEE_Frac := Shift_Left(IEEE_Frac, 4);
      -- end loop;

      IEEE_Expt_I := IEEE_Expt_I - 1022; -- subtract excess 217 and add 1

      if (IEEE_Expt_I mod 4) /= 0 then
         IEEE_Frac := Shift_Right(IEEE_Frac, (4 - (IEEE_Expt_I mod 4)));
         IEEE_Expt_I := IEEE_Expt_I + 4;
      end if;

      IEEE_Expt_I := IEEE_Expt_I / 4; -- convert to base 16
      IEEE_Expt_I := IEEE_Expt_I + 64; -- excess 64

      --IBM_Expt := Unsigned_64(Unsigned_16(IEEE_Expt_I) and 16#007f#);
      IBM_Expt := Integer_To_Unsigned_64(IEEE_Expt_I) and 16#007f#;
      IBM_Frac := Shift_Left(IEEE_Frac, 4);
      IBM := IBM_Sign or Shift_Left(IBM_Expt, 56) or IBM_Frac;

      return Qword_T(IBM);
   end Long_Float_To_DG_Double;

   function Long_Float_To_DG_Single (LF : in Long_Float) return Dword_T is
      QW : Qword_T := Long_Float_To_DG_Double (LF);
   begin
      return Upper_Dword (QW);
   end Long_Float_To_DG_Single;

end DG_Types;