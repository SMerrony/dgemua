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
with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

package DG_Types is

    type Byte_T   is new Unsigned_8;
    type Word_T   is new Unsigned_16;
    type Dword_T  is new Unsigned_32;
    type Qword_T  is new Unsigned_64;

    type Phys_Addr_T is new Unsigned_32;

    type Byte_Arr_T is array (Natural range <>) of Byte_T;

    Devices_Max : constant Integer := 63;

    type AC_ID is new Integer range 0 .. 3;
    type Dev_Num_T is new Integer range 0 .. Devices_Max;
    type IO_Dir_T  is (Data_In, Data_Out);
    type IO_Reg_T  is (A, B, C, N);
    type IO_Flag_T is (None, S, C, P);
    type IO_Test_T is (BN, BZ, DN, DZ);  

    type AC_Circle_T is array (0..7) of AC_ID;
    AC_Circle : constant AC_Circle_T := (0,1,2,3,0,1,2,3);

    type Number_Base_T is (Binary, Octal, Decimal, Hex);

    type Double_Exp  is mod 2 ** 7;
    type Double_Mant is mod 2 ** 56;
    type Physical_Double is record
       --- Physical_Double is the DG-internal representation of F.P. Doubles
       --- It must be preprocessed before use, the exp and mant must be adjusted.
       Sign     : Boolean;
       Exponent : Double_Exp;
       Mantissa : Double_Mant;       
    end record;
    for Physical_Double use record
       Sign     at 0 range  0 .. 0;
       Exponent at 0 range  1 .. 7;
       Mantissa at 0 range  8 .. 63;
    end record;

    type Q_or_P is (QW, Phy);
    type Double_Overlay(Rep : Q_or_P := QW) is record
        case Rep is
            when QW =>
                Double_QW : Qword_T;
            when Phy =>
                Double_Phys : Physical_Double;
        end case;
    end record with Unchecked_Union;
   

    Dasher_NL          : constant Character := Character'Val(8#12#);
    Dasher_Erase_EOL   : constant Character := Character'Val(8#13#);
    Dasher_Erase_Page  : constant Character := Character'Val(8#14#);
    Dasher_Write_Window_Addr : constant Character := Character'Val(8#20#); -- followed by col, row
    Dasher_Underline   : constant Character := Character'Val(8#24#);
    Dasher_Normal      : constant Character := Character'Val(8#25#);
    Dasher_Cursor_Left : constant Character := Character'Val(8#31#);
    Dasher_Dim_On      : constant Character := Character'Val(8#34#);
    Dasher_Dim_Off     : constant Character := Character'Val(8#35#);
    Dasher_Delete      : constant Character := Character'Val(8#177#);

    Max_Pos_S16 : constant Integer_32 :=  (2 ** 15) - 1;
    Min_Neg_S16 : constant Integer_32 := -(Max_Pos_S16 + 1);
    Max_Pos_S32 : constant Integer_64 :=  (2 ** 31) - 1;
    Min_Neg_S32 : constant Integer_64 := -(Max_Pos_S32 + 1);

    -- Decimal ('Commercial') data types
    Unpacked_Dec_TSC : constant Natural := 0;
    Unpacked_Dec_LSC : constant Natural := 1;
    Unpacked_Dec_TS  : constant Natural := 2;
    Unpacked_Dec_LS  : constant Natural := 3; -- <sign><zeroes><int>
    Unpacked_Dec_U   : constant Natural := 4; -- <zeroes><int>
    Packed_Dec       : constant Natural := 5;
    Twos_Comp_Dec    : constant Natural := 6;
    FP_Dec           : constant Natural := 7;

    Not_Yet_Implemented : Exception;

    -- boolean routines
    function Boolean_To_YN (B : in Boolean) return Character;

    -- bit routines
    procedure Clear_W_Bit (Word : in out Word_T; Bit_Num : in Integer);
    procedure Flip_W_Bit  (Word : in out Word_T; Bit_Num : in Integer);
    procedure Set_W_Bit   (Word : in out Word_T; Bit_Num : in Integer);
    function  Test_W_Bit  (Word : in Word_T; Bit_Num : in Integer) return Boolean with Inline;
    function  Get_W_Bits  (Word : in Word_T; First_Bit, Num_Bits : Natural) return Word_T;
    function  Get_DW_Bits (Dword : in Dword_T; First_Bit, Num_Bits : Natural) return Dword_T;
    function  Test_DW_Bit (DW: in Dword_T; Bit_Num : in Integer) return Boolean;
    procedure Clear_QW_Bit (QW : in out Qword_T; Bit_Num : in Integer);
    procedure Set_QW_Bit  (QW : in out Qword_T; Bit_Num : in Integer);
    function  Test_QW_Bit (QW: in Qword_T; Bit_Num : in Integer) return Boolean;

    -- byte routines
    function Get_Lower_Byte (Word : in Word_T) return Byte_T;
    function Get_Upper_Byte (Word : in Word_T) return Byte_T;
    function Swap_Bytes (Word : In Word_T) return Word_T;
    procedure Get_Bytes_From_Word (Word : in Word_T; Low_Byte, High_Byte : out Byte_T);
    function Word_From_Bytes( Lo, Hi : in Byte_T) return Word_T;
    function Byte_To_String
       (Byt      : in Byte_T; 
        Base     : in Number_Base_T; 
        Width    : in Integer;
        Zero_Pad : in Boolean := False) 
        return String;  
    function Low_Byte_To_Char (LB : in Boolean) return Character;
    function Byte_Arr_To_Unbounded (B_Arr : in Byte_Arr_T) return Unbounded_String;
    procedure Get_Data_Sensitive_Portion (B_Arr     : in Byte_Arr_T;
                                          Max_Len   : in Integer;
                                          Last_Byte : out Integer;
                                          Too_Long  : out Boolean);

    -- Dword routines
    function Lower_Word( DW : in Dword_T) return Word_T with Inline;
    function Upper_Word( DW : in Dword_T) return Word_T;
    function Dword_From_Two_Words (Word_1, Word_2 : in Word_T) return Dword_T;
    function Dword_To_String
       (DW       : in Dword_T; 
        Base     : in Number_Base_T; 
        Width    : in Integer;
        Zero_Pad : in Boolean := False) 
        return String;
    function String_To_Dword (Str : in String; Base : in Number_Base_T) return Dword_T;
    function Sext_Word_To_Dword(Wd : in Word_T) return Dword_T;

    -- Qword routines
    function Lower_Dword ( QW : in Qword_T) return Dword_T;
    function Upper_Dword ( QW : in Qword_T) return Dword_T;
    function Qword_From_Two_Dwords (Dword_1, Dword_2 : in Dword_T) return Qword_T;


    -- string/integer routines
    function Int_To_String
       (Int      : in Integer; 
        Base     : in Number_Base_T; 
        Width    : in Integer;
        Zero_Pad : in Boolean := false;
        Truncate : in Boolean := false) 
        return String;
    function String_To_Integer(Str : in String; Base : in Number_Base_T) return Integer;

    -- decimal routines
    procedure Decode_Dec_Data_Type (DTI          : in Dword_T; 
                                    Scale_Factor : out Integer_8;
                                    Dec_Type     : out Natural;
                                    Size         : out Natural);
    function Read_Decimal (BA : in Dword_T; Size : in Natural) return Unbounded_String;

    -- floating point routines
    function DG_Double_To_Long_Float (DG_Dbl : in Double_Overlay) return Long_Float;    
    function DG_Single_To_Long_Float (Single : in Dword_T) return Long_Float;
    function Long_Float_To_DG_Double (LF : in Long_Float) return Qword_T;
    function Long_Float_To_DG_Single (LF : in Long_Float) return Dword_T;
                  
    -- unchecked conversions
    function Byte_To_Integer_8   is new Ada.Unchecked_Conversion(Byte_T, Integer_8);
    function Char_To_Byte        is new Ada.Unchecked_Conversion(Character, Byte_T);
    function Byte_To_Char        is new Ada.Unchecked_Conversion(Byte_T, Character);
    function Dword_To_Integer_32 is new Ada.Unchecked_Conversion(Dword_T, Integer_32);
    function Dword_To_Integer    is new Ada.Unchecked_Conversion(Dword_T, Integer);
    function Integer_32_To_Dword is new Ada.Unchecked_Conversion(Integer_32, Dword_T);
    function Integer_32_To_Phys  is new Ada.Unchecked_Conversion(Integer_32, Phys_Addr_T);
    function Word_To_Integer_16  is new Ada.Unchecked_Conversion(Word_T, Integer_16);
    function Integer_16_To_Word  is new Ada.Unchecked_Conversion(Integer_16, Word_T);
    function Word_To_Unsigned_16 is new Ada.Unchecked_Conversion(Word_T, Unsigned_16);
    function Integer_64_To_Unsigned_64 is new Ada.Unchecked_Conversion(Integer_64, Unsigned_64);
    function Unsigned_32_To_Integer is new Ada.Unchecked_Conversion(Unsigned_32, Integer);
    function Integer_To_Unsigned_64 is new Ada.Unchecked_Conversion(Integer, Unsigned_64);
end DG_Types;