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

   -- return DG lower (RH) byte of a Word
   function Get_Lower_Byte (Word : in Word_T) return Byte_T is
   begin
      return Byte_T (Word and 16#00ff#);
   end Get_Lower_Byte;

   function Boolean_To_YN (B : Boolean) return Character is
   begin
      if B then
         return 'Y';
      else
         return 'N';
      end if;
   end Boolean_To_YN;

   -- Convert an (unsigned) Double-Word to a String
   function Dword_To_String
     (DW       : in Dword_T; Base : in Integer; Width : in Integer;
      Zero_Pad : in Boolean := False) return String
   is
      Res       : String (1 .. Width);
      Tmp_DW    : Dword_T          := DW;
      Bas_DW    : Dword_T          := Dword_T (Base);
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
      loop
         Remainder := Integer (Tmp_DW mod Bas_DW);
         Tmp_DW    := Tmp_DW / Bas_DW;
         case Base is
            when 8 =>
               Res (Col) := Octals (Remainder + 1);
            when 10 =>
               Res (Col) := Decimals (Remainder + 1);
            when 16 =>
               Res (Col) := Hexes (Remainder + 1);
            when others =>
               null;
         end case;
         Col := Col - 1;
         exit when Tmp_DW = 0 or Col = 0;
      end loop;
      return Res;
   end Dword_To_String;

end Memory;
