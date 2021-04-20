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

with Interfaces; use Interfaces;

package DG_Types is

    type Byte_T   is new Unsigned_8;
    type Word_T   is new Unsigned_16;
    type Dword_T  is new Unsigned_32;
    type Qword_T  is new Unsigned_64;

    type Phys_Addr_T is new Unsigned_32;

    Devices_Max : constant Integer := 63;

    type AC_ID is new Integer range 0 .. 3;
    type Dev_Num_T is new Integer range 0 .. Devices_Max;
    type IO_Dir_T  is (Data_In, Data_Out);
    type IO_Reg_T  is (A, B, C, N);
    type IO_Flag_T is (None, S, C, P);
    type IO_Test_T is (BN, BZ, DN, DZ);  

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

    Not_Yet_Implemented : Exception;

end DG_Types;