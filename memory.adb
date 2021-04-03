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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
      function  Read_Dword (Word_Addr : in Phys_Addr_T) return Dword_T is
      begin
         return Dword_From_Two_Words (RAM(Integer(Word_Addr)), RAM(Integer(Word_Addr) + 1));
      end Read_Dword;

      function Read_Word (Word_Addr : in Phys_Addr_T) return Word_T is
      begin
         return RAM (Integer (Word_Addr));
      end Read_Word;

      procedure Write_Word (Word_Addr : in Phys_Addr_T; Datum : Word_T) is
      begin
         RAM (Integer (Word_Addr)) := Datum;
      end Write_Word;

   end RAM;

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
         Put_Line ("INFO: BMC_DCH Registers initialised");
      end Reset;

   end BMC_DCH;

   procedure Clear_W_Bit (Word : in out Word_T; Bit_Num : in Integer) is
   begin
      Word := Word and not Shift_Left (1, 15 - Bit_Num);
   end Clear_W_Bit;

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
   function Get_W_Bits (Word : in Word_T; First_Bit, Num_Bits : Natural) return Word_T is
      Mask : Word_T := Shift_Left (1, Num_Bits) - 1;
   begin
      if First_Bit >= 16 then return 0; end if;
      return Shift_Right( Word, 16 - (First_Bit + Num_Bits)) and Mask;
   end Get_W_Bits;

   -- return DG lower (RH) byte of a Word
   function Get_Lower_Byte (Word : in Word_T) return Byte_T is
   begin
      return Byte_T (Word and 16#00ff#);
   end Get_Lower_Byte;

   -- return DG Upper (LH) byte of a Word
   function Get_Upper_Byte (Word : in Word_T) return Byte_T is
   begin
      return Byte_T (Shift_Right(Word and 16#ff00#, 8));
   end Get_Upper_Byte;
   
   function Swap_Bytes (Word : In Word_T) return Word_T is
   begin
      return Shift_Right(Word and 16#ff00#, 8) or Shift_Left(Word and 16#00ff#, 8);
   end Swap_Bytes;

   function Lower_Word( DW : in Dword_T) return Word_T is
   begin
      return Word_T(DW and 16#0000_ffff#);
   end Lower_Word;

   procedure Get_Bytes_From_Word (Word : in Word_T; Low_Byte, High_Byte : out Byte_T) is
   begin
      Low_Byte := Byte_T (Word and 16#00ff#);
      High_Byte := Byte_T (Shift_Right(Word and 16#ff00#, 8));
   end Get_Bytes_From_Word;

   function Dword_From_Two_Words (Word_1, Word_2 : in Word_T) return Dword_T is
   begin
      return Shift_Left (Dword_T(Word_1), 16) or Dword_T(Word_2);
   end;

   function Boolean_To_YN (B : Boolean) return Character is
   begin
      if B then
         return 'Y';
      else
         return 'N';
      end if;
   end Boolean_To_YN;

   function String_To_Integer(Str : in String; Base : in Number_Base_T) return Integer is
      Neg : Boolean := false;
      Res : Integer := 0;
      Num_Start : Integer := Str'First;
      Hex_I : Integer;
      Hexes : String := "0123456789ABCDEF";
      Val : Integer;
   begin
      if Str(Str'First) = '-' then
         Neg := true;
         Num_Start := Num_Start + 1;
      elsif Str(Str'First) = '+' then
         Num_Start := Num_Start + 1;
      end if;
      for C in Num_Start .. Str'Last loop
         Val := Character'Pos(Str(C)) - 48;
         case Base is
            when Octal =>
               Res := (Res * 8) + Val;
            when Decimal =>
               Res := (Res * 10) + Val;
            when Hex =>
               --Hex_I := Ada.Strings.Fixed.Index ( "0123456789ABCDEF", String(Str(C)) ) - 1 ;
               Hex_I := Ada.Strings.Fixed.Index ( "0123456789ABCDEF", Str(C)'Image ) - 1 ;
               Res := (Res * 16) + Integer (Hex_I);
            when others =>
               null;
         end case; 
      end loop;
      return Res;
   end String_To_Integer;

   -- String_To_Dword tries to convert an unsigned string in the given base
   function String_To_Dword (Str : in String; Base : in Number_Base_T) return Dword_T
   is
      Res : Dword_T := 0;
      Hex_I : Integer;
   begin
      for C of Str loop
         case Base is
            when Octal =>
               Res := (Res * 8) + Dword_T'Value((1 => C));
            when Decimal =>
               Res := (Res * 10) + Dword_T'Value((1 => C));
            when Hex =>
               Hex_I := Ada.Strings.Fixed.Index ( "0123456789ABCDEF", C'Image ) - 1 ;
               Res := (Res * 16) + Dword_T (Hex_I);
            when others =>
               null;
         end case;  
      end loop;
      return Res;
   end String_To_Dword;


   -- Convert an (unsigned) Double-Word to a String
   function Dword_To_String
     (DW       : in Dword_T; Base : in Number_Base_T; Width : in Integer;
      Zero_Pad : in Boolean := False) return String
   is
      Res       : String (1 .. Width);
      Tmp_DW    : Dword_T          := DW;
      Bas_DW    : Dword_T;
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
         when Binary => Bas_DW := 2;
         when Octal  => Bas_DW := 8;
         when Decimal  => Bas_DW := 10;
         when Hex  => Bas_DW := 16;
      end case;
      loop
         Remainder := Integer (Tmp_DW mod Bas_DW);
         Tmp_DW    := Tmp_DW / Bas_DW;
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
         exit when Tmp_DW = 0 or Col = 0;
      end loop;
      return Res;
   end Dword_To_String;

   function Int_To_String
       (Int       : in Integer; 
        Base : in Number_Base_T; 
        Width : in Integer;
        Zero_Pad : in Boolean := False) 
        return String is
      Res       : String (1 .. Width);
      Tmp_Int   : Integer := Int;
      Bas_Int   : Integer;
      Remainder : Integer;
      Octals    : String (1 .. 8)  := "01234567";
      Decimals  : String (1 .. 10) := "0123456789";
      Hexes     : String (1 .. 16) := "0123456789ABCDEF";
      Col       : Integer          := Width;
      Negative  : Boolean := Int < 0;
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
         when Binary => Bas_Int := 2;
         when Octal  => Bas_Int := 8;
         when Decimal  => Bas_Int := 10;
         when Hex  => Bas_Int := 16;
      end case;
      loop
         Remainder := Integer (Tmp_Int mod Bas_Int);
         Tmp_Int    := Tmp_Int / Bas_Int;
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
      return Res;
   end Int_To_String;

   -- Convert an (unsigned) Byte to a String
   function Byte_To_String
     (Byt       : in Byte_T; Base : in Number_Base_T; Width : in Integer;
      Zero_Pad : in Boolean := False) return String
   is
      Res       : String (1 .. Width);
      Tmp_Byt    : Byte_T          := Byt;
      Bas_Byt    : Byte_T;
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
         when Binary => Bas_Byt := 2;
         when Octal  => Bas_Byt := 8;
         when Decimal  => Bas_Byt := 10;
         when Hex  => Bas_Byt := 16;
      end case;
      loop
         Remainder := Integer (Tmp_Byt mod Bas_Byt);
         Tmp_Byt    := Tmp_Byt / Bas_Byt;
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
