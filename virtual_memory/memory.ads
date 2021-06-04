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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Interfaces; use Interfaces;

with DG_Types;   use DG_Types;

package Memory is

    Words_Per_Page : constant Natural := 1024;
    Ring_7_Page_0  : constant Natural := 16#001c_0000#;

    -- Physical stuff that has to exist...
    Mem_Size_Words : constant Integer     := 8_388_608;
    -- MemSizeLCPID is the code returned by the LCPID to indicate the size of RAM in half megabytes
	Mem_Size_LCPID : constant Dword_T := 63;
	-- MemSizeNCLID is the code returned by NCLID to indicate size of RAM in 32Kb increments
	Mem_Size_NCLID : constant Word_T := Word_T(((Mem_Size_Words * 2) / (32 * 1024)) - 1);

    type Memory_Region is array (Natural range <>) of Word_T;
    type Page_T is array (0 .. Words_Per_Page - 1) of Word_T;
    type Page_Arr_T is array (Natural range <>) of Page_T;

    -- Page 0 special locations for stacks
    WSFH_Loc : constant Phys_Addr_T := 8#14#;
    WFP_Loc : constant Phys_Addr_T := 8#20#;
    WSP_Loc : constant Phys_Addr_T := 8#22#;
    WSL_Loc : constant Phys_Addr_T := 8#24#;
    WSB_Loc : constant Phys_Addr_T := 8#26#;
    NSP_Loc : constant Phys_Addr_T := 8#40#;
    NFP_Loc : constant Phys_Addr_T := 8#41#;
    NSL_Loc : constant Phys_Addr_T := 8#42#;
    NSF_Loc : constant Phys_Addr_T := 8#43#;

    -- Wide Stack Fault codes 
    WSF_Overflow        : constant Dword_T := 0;
    WSF_Pending         : constant Dword_T := 1;
    WSF_Too_Many_Args   : constant Dword_T := 2;
    WSF_Underflow       : constant Dword_T := 3;
    WSF_Return_Overflow : constant Dword_T := 4;

    function NaturalHash (K : Natural) return Hash_Type is (Hash_Type (K));

    package VRAM_Map is new Ada.Containers.Hashed_Maps (
        Key_Type        => Natural,
        Hash            => NaturalHash,
        Equivalent_Keys => "=",
        Element_Type    => Page_T );

    protected RAM is
        procedure Init (Debug_Logging : in Boolean);
        procedure Map_Page (Page : in Natural; Is_Shared : in Boolean);
        function  Page_Mapped (Page : in Natural) return Boolean;
        function  Get_Last_Unshared_Page return Dword_T;
        function  Get_First_Shared_Page return Dword_T;
        function  Get_Num_Shared_Pages return Dword_T;
        
        procedure Map_Range (Start_Addr : in Phys_Addr_T;
                             Region     : in Memory_Region;
                             Is_Shared  : in Boolean);
        procedure Map_Shared_Pages (Start_Addr : in Phys_Addr_T; Pages : in Page_Arr_T);

        -- function Address_Mapped (Addr : in Phys_Addr_T) return Boolean;
        function  Read_Word   (Word_Addr : in Phys_Addr_T) return Word_T with Inline;
        function  Read_Dword  (Word_Addr : in Phys_Addr_T) return Dword_T;
        function  Read_Qword  (Word_Addr : in Phys_Addr_T) return Qword_T;
        procedure Write_Word  (Word_Addr : in Phys_Addr_T; Datum : Word_T);
        procedure Write_Dword (Word_Addr : in Phys_Addr_T; Datum : Dword_T);
        procedure Write_Qword (Word_Addr : in Phys_Addr_T; Datum : Qword_T);

        function  Read_Byte (Word_Addr : in Phys_Addr_T; Low_Byte : in Boolean) return Byte_T;
        function  Read_Byte_BA (BA : in Dword_T) return Byte_T;
        procedure Write_Byte (Word_Addr : in Phys_Addr_T; Low_Byte : in Boolean; Byt : in Byte_T);
        procedure Write_Byte_BA (BA : in Dword_T; Datum : in Byte_T);
        procedure Copy_Byte_BA (Src, Dest : in Dword_T);
        function  Read_Byte_Eclipse_BA (Segment : in Phys_Addr_T; BA_16 : in Word_T) return Byte_T;
        procedure Write_Byte_Eclipse_BA (Segment : in Phys_Addr_T; BA_16 : in Word_T; Datum : in Byte_T);
        function  Read_Bytes_BA (BA : in Dword_T; Num : in Natural) return Byte_Arr_T;

        -- specific support for VS/Emua...
        function  Read_String_BA (BA : in Dword_T) return String;
        procedure Write_String_BA (BA : in Dword_T; Str : in String);
    private
        VRAM : VRAM_Map.Map;
        Logging : Boolean;
        First_Shared_Page,
        Last_Unshared_Page,
        Num_Shared_Pages : Natural;
        
    end RAM;

    protected Narrow_Stack is
        procedure Push (Segment : in Phys_Addr_T; Datum : in Word_T);
        function  Pop  (Segment : in Phys_Addr_T) return Word_T;
    end Narrow_Stack;

    Page_Already_Mapped,
    Read_From_Unmapped_Page,
    Write_To_Unmapped_Page : exception;

end Memory;