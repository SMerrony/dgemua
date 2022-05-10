-- MIT License

-- Copyright (c) 2021,2022 Stephen Merrony

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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with DG_Types; use DG_Types;

package Simh_Tapes is

    -- tape image markers
    Mtr_Tmk     : constant Dword_T := 0;
    Mtr_EOM     : constant Dword_T := 16#ffff_ffff#;
    Mtr_Gap     : constant Dword_T := 16#ffff_fffe#;
    Mtr_Max_Len : constant Dword_T := 16#00ff_ffff#;
    Mtr_Erf     : constant Dword_T := 16#8000_0000#;

    -- status codes
    type Mt_Stat is
       (OK, Tmk, Unatt, IOerr, InvRec, InvFmt, BOT, EOM, RecErr, WrOnly);

    type Mt_Rec is array (Natural range <>) of Byte_T;

    procedure Read_Meta_Data    (Img_Stream : Stream_Access; Meta_Data : out Dword_T);
    procedure Write_Meta_Data   (Img_Stream : Stream_Access; Meta_Data : Dword_T);
    procedure Read_Record_Data  (Img_Stream : Stream_Access; Num_Bytes : Natural; Rec : out Mt_Rec);
    procedure Write_Record_Data (Img_Stream : Stream_Access; Rec : Mt_Rec);
    procedure Rewind            (Img_File   : in out File_Type);
    function  Space_Forward     (Img_Stream : Stream_Access; Num_Recs : Integer) return Mt_Stat;
    function  Scan_Image        (Img_Filename : String) return String; 
end Simh_Tapes;
