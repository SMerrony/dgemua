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
    type Word_Arr_T is array (Natural range <>) of Word_T;

    Devices_Max      : constant Integer := 63;
    Disk_Block_Bytes : constant Integer := 512;
    Disk_Block_Words : constant Integer := 256; -- 512 bytes

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
   
    Dasher_Null             : constant Character := Character'Val(0);
    Dasher_Print_Form       : constant Character := Character'Val(1);
    Dasher_Rev_Off          : constant Character := Character'Val(2); -- from D210 onwards
    Dasher_Blink_Enable     : constant Character := Character'Val(3); -- for the whole screen
    Dasher_Blink_Disable    : constant Character := Character'Val(4); -- for the whole screen
    Dasher_Read_Window_Addr : constant Character := Character'Val(5); -- requires response...
    Dasher_Ack              : constant Character := Character'Val(6); -- sent to host to indicatelocal print is complete
    Dasher_Bell             : constant Character := Character'Val(7);
    Dasher_Home             : constant Character := Character'Val(8); -- window home
    Dasher_Tab              : constant Character := Character'Val(9);
    Dasher_NL               : constant Character := Character'Val(10);
    Dasher_Erase_EOL        : constant Character := Character'Val(11);
    Dasher_Erase_Page       : constant Character := Character'Val(12);
    Dasher_FF               : constant Character := Dasher_Erase_Page;
    Dasher_CR               : constant Character := Character'Val(13);
    Dasher_Blink_On         : constant Character := Character'Val(14);
    Dasher_Blink_Off        : constant Character := Character'Val(15);
    Dasher_Write_Window_Addr: constant Character := Character'Val(16); -- followed by col then row
    Dasher_Print_Screen     : constant Character := Character'Val(17);
    Dasher_Roll_Enable      : constant Character := Character'Val(18);
    Dasher_Roll_Disable     : constant Character := Character'Val(19);
    Dasher_Underline        : constant Character := Character'Val(20);
    Dasher_Normal           : constant Character := Character'Val(21); -- cancels Underline
    Dasher_Rev_On           : constant Character := Character'Val(22); -- from D210 onwards
    Dasher_Cursor_Up        : constant Character := Character'Val(23);
    Dasher_Cursor_Right     : constant Character := Character'Val(24);
    Dasher_Cursor_Left      : constant Character := Character'Val(25);
    Dasher_Cursor_Down      : constant Character := Character'Val(26);
    Dasher_Escape           : constant Character := Character'Val(27);
    Dasher_Dim_On           : constant Character := Character'Val(28);
    Dasher_Dim_Off          : constant Character := Character'Val(29);
    Dasher_Command          : constant Character := Character'Val(30);

    Dasher_Space            : constant Character := Character'Val(32);
    
    Dasher_F15              : constant Character := Character'Val(112);
    Dasher_F1               : constant Character := Character'Val(113);
    Dasher_F2               : constant Character := Character'Val(114);
    Dasher_F3               : constant Character := Character'Val(115);
    Dasher_F4               : constant Character := Character'Val(116);
    Dasher_F5               : constant Character := Character'Val(117);
    Dasher_F6               : constant Character := Character'Val(118);
    Dasher_F7               : constant Character := Character'Val(119);
    Dasher_F8               : constant Character := Character'Val(120);
    Dasher_F9               : constant Character := Character'Val(121);
    Dasher_F10              : constant Character := Character'Val(122);
    Dasher_F11              : constant Character := Character'Val(123);
    Dasher_F12              : constant Character := Character'Val(124);  
    Dasher_F13              : constant Character := Character'Val(125);
    Dasher_F14              : constant Character := Character'Val(126);

    Dasher_C1               : constant Character := Character'Val(92);
    Dasher_C2               : constant Character := Character'Val(93);
    Dasher_C3               : constant Character := Character'Val(94);
    Dasher_C4               : constant Character := Character'Val(95);
    Dasher_Shift_C1         : constant Character := Character'Val(88);
    Dasher_Shift_C2         : constant Character := Character'Val(89);
    Dasher_Shift_C3         : constant Character := Character'Val(90);
    Dasher_Shift_C4         : constant Character := Character'Val(91); 
                
    Dasher_Delete           : constant Character := Character'Val(127);

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
    Packed_Dec       : constant Natural := 5; -- <BCD int + Sign>
    Twos_Comp_Dec    : constant Natural := 6;
    FP_Dec           : constant Natural := 7;

    Interrupted,
    Not_Yet_Implemented : exception;

    -- boolean routines
    function Boolean_To_YN (B : Boolean) return Character;

    -- bit routines
    procedure Clear_W_Bit (Word : in out Word_T; Bit_Num : Integer);
    procedure Flip_W_Bit  (Word : in out Word_T; Bit_Num : Integer);
    procedure Set_W_Bit   (Word : in out Word_T; Bit_Num : Integer);
    function  Test_W_Bit  (Word : Word_T; Bit_Num : Integer) return Boolean with Inline;
    function  Get_W_Bits  (Word : Word_T; First_Bit, Num_Bits : Natural) return Word_T;
    function  Get_DW_Bits (Dword : Dword_T; First_Bit, Num_Bits : Natural) return Dword_T;
    function  Test_DW_Bit (DW: Dword_T; Bit_Num : Integer) return Boolean;
    procedure Clear_QW_Bit (QW : in out Qword_T; Bit_Num : Integer);
    procedure Set_QW_Bit  (QW : in out Qword_T; Bit_Num : Integer);
    function  Test_QW_Bit (QW: Qword_T; Bit_Num : Integer) return Boolean;

    -- byte routines
    function Get_Lower_Byte (Word : Word_T) return Byte_T;
    function Get_Upper_Byte (Word : Word_T) return Byte_T;
    function Swap_Bytes (Word : Word_T) return Word_T;
    procedure Get_Bytes_From_Word (Word : Word_T; Low_Byte, High_Byte : out Byte_T);
    function Word_From_Bytes( Lo, Hi : Byte_T) return Word_T;
    function Byte_To_String
       (Byt      : Byte_T; 
        Base     : Number_Base_T; 
        Width    : Integer;
        Zero_Pad : Boolean := False) 
        return String;  
    function Low_Byte_To_Char (LB : Boolean) return Character;
    function Byte_Arr_To_Unbounded (B_Arr : Byte_Arr_T) return Unbounded_String;
    procedure Get_Data_Sensitive_Portion (B_Arr     : Byte_Arr_T;
                                          Max_Len   : Integer;
                                          DS_Bytes  : out Integer);
    function Word_To_String
       (WD       : Word_T; 
        Base     : Number_Base_T; 
        Width    : Integer;
        Zero_Pad : Boolean := False) 
        return String;
    -- Dword routines
    function Lower_Word( DW : Dword_T) return Word_T with Inline;
    function Upper_Word( DW : Dword_T) return Word_T;
    function Dword_From_Two_Words (Word_1, Word_2 : Word_T) return Dword_T;
    function Dword_To_String
       (DW       : Dword_T; 
        Base     : Number_Base_T; 
        Width    : Integer;
        Zero_Pad : Boolean := False) 
        return String;
    function String_To_Dword (Str : String; Base : Number_Base_T) return Dword_T;
    -- Attempt to convert Str to a Dword, may raise CONSTRAINT_ERROR

    function Sext_Word_To_Dword(Wd : Word_T) return Dword_T;

    -- Qword routines
    function Lower_Dword ( QW : Qword_T) return Dword_T;
    function Upper_Dword ( QW : Qword_T) return Dword_T;
    function Qword_From_Two_Dwords (Dword_1, Dword_2 : Dword_T) return Qword_T;
    -- Dword_1 is the most-significant (left-most) DW


    -- string/integer routines
    function Int_To_String
       (Int      : Integer; 
        Base     : Number_Base_T; 
        Width    : Integer;
        Zero_Pad : Boolean := false;
        Truncate : Boolean := false) 
        return String;
    function String_To_Integer(Str : String; Base : Number_Base_T) return Integer;

    -- decimal routines
    procedure Decode_Dec_Data_Type (DTI          : Dword_T; 
                                    Scale_Factor : out Integer_8;
                                    Dec_Type     : out Natural;
                                    Size         : out Natural);
    function Read_Decimal (BA : Dword_T; Size : Natural) return Unbounded_String;

    -- floating point routines
    function DG_Double_To_Long_Float (DG_Dbl : Double_Overlay) return Long_Float;    
    function DG_Single_To_Long_Float (Single : Dword_T) return Long_Float;
    function Long_Float_To_DG_Double (LF : Long_Float) return Qword_T;
    function Long_Float_To_DG_Single (LF : Long_Float) return Dword_T;
                  
    -- unchecked conversions
    function Byte_To_Integer_8   is new Ada.Unchecked_Conversion(Byte_T, Integer_8);
    function Char_To_Byte        is new Ada.Unchecked_Conversion(Character, Byte_T);
    function Byte_To_Char        is new Ada.Unchecked_Conversion(Byte_T, Character);
    function Dword_To_Integer_32 is new Ada.Unchecked_Conversion(Dword_T, Integer_32);
    function Phys_To_Integer_32  is new Ada.Unchecked_Conversion(Phys_Addr_T, Integer_32);
    function Dword_To_Integer    is new Ada.Unchecked_Conversion(Dword_T, Integer);
    function Integer_32_To_Dword is new Ada.Unchecked_Conversion(Integer_32, Dword_T);
    function Integer_32_To_Phys  is new Ada.Unchecked_Conversion(Integer_32, Phys_Addr_T);
    function Word_To_Integer_16  is new Ada.Unchecked_Conversion(Word_T, Integer_16);
    function Integer_16_To_Word  is new Ada.Unchecked_Conversion(Integer_16, Word_T);
    function Word_To_Unsigned_16 is new Ada.Unchecked_Conversion(Word_T, Unsigned_16);
    function Integer_64_To_Unsigned_64 is new Ada.Unchecked_Conversion(Integer_64, Unsigned_64);
    function Integer_64_To_Integer_32  is new Ada.Unchecked_Conversion(Integer_64, Integer_32); 
    function Unsigned_32_To_Integer is new Ada.Unchecked_Conversion(Unsigned_32, Integer);
    function Integer_To_Unsigned_64 is new Ada.Unchecked_Conversion(Integer, Unsigned_64);
end DG_Types;
