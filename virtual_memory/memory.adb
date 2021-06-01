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
with Ada.Text_IO;

with Debug_Logs; use Debug_Logs;


package body Memory is

    protected body RAM is

        procedure Init (Debug_Logging : in Boolean) is
        begin
            Logging  := Debug_Logging;
            -- always need to map user page 0
            Map_Page (Ring_7_Page_0, false);
            First_Shared_Page := (2 ** 31) - 1;
        end Init;

        procedure Map_Page (Page : in Natural; Is_Shared : in Boolean) is
            New_Page : Page_T := (others => 0);
        begin
            if Page_Mapped(Page) then
               raise Page_Already_Mapped with Page'Image;
            else
               VRAM.Include(Page, New_Page);
                if Is_Shared then
                    Num_Shared_Pages := Num_Shared_Pages  + 1;
                    if Page < First_Shared_Page then
                        First_Shared_Page := Page;
                    end if;
                else
                    Last_Unshared_Page := Page;
                end if;
            end if;
            Ada.Text_IO.Put_Line ("Memory: Mapped " &
                                  (if Is_Shared then "Shared" else "Unshared") &
                                  " page " & Int_To_String(Page, Hex, 8));
        end Map_Page;

        function Page_Mapped (Page : in Natural) return Boolean is (VRAM.Contains(Page));
        
        function Address_Mapped (Addr : in Phys_Addr_T) return Boolean is
            (VRAM.Contains(Natural(Shift_Right(Addr, 10))));

        procedure Map_Range (Start_Addr : in Phys_Addr_T;
                             Region     : in Memory_Region;
                             Is_Shared  : in Boolean) is
            Loc : Phys_Addr_T;
        begin
            for Offset in Region'Range loop
                Loc := Start_Addr + Phys_Addr_T(Offset - Region'First);
                -- if we have hit a page boundary then check it's mapped
                if (Loc and 16#03ff#) = 0 then
                    if not Address_Mapped(Loc) then
                        Map_Page(Natural(Shift_Right(Loc, 10)), Is_Shared);
                        Ada.Text_IO.Put_Line ("Memory: Mapped " &
                                         (if Is_Shared then "Shared" else "Unshared") &
                                         " page " & Int_To_String(Natural(Shift_Right(Loc, 10)), Hex, 8) &
                                         " for " & Dword_To_String(Dword_T(Loc), Hex, 8));
                    end if;
                end if;
                Write_Word(Loc, Region(Offset));
            end loop;
        end Map_Range;

        function  Get_Last_Unshared_Page return Dword_T is
            (Dword_T(Last_Unshared_Page));

        function  Get_First_Shared_Page return Dword_T is
            (Dword_T(First_Shared_Page));

        function  Get_Num_Shared_Pages return Dword_T is
            (Dword_T(Num_Shared_Pages));

        function Read_Word (Word_Addr : in Phys_Addr_T) return Word_T is
            Page : Natural := Natural(Shift_Right(Word_Addr, 10));
        begin
            if not Page_Mapped(Page) then
                raise Read_From_Unmapped_Page with Int_To_String(Page, Hex, 8) &
                    " for address: " & Dword_To_String(Dword_T(Word_Addr), Hex, 8);
            end if;
            return VRAM(Page)(Integer(Word_Addr and 16#03ff#));
        end Read_Word;

        function Read_Dword (Word_Addr : in Phys_Addr_T) return Dword_T is
            Page : Natural := Natural(Shift_Right(Word_Addr, 10));
            Hi_WD : Word_T := Read_Word(Word_Addr);
            Lo_WD : Word_T := Read_Word(Word_Addr + 1);
        begin
            return Dword_From_Two_Words(Hi_WD, Lo_WD);
        end Read_Dword;

        function Read_Qword  (Word_Addr : in Phys_Addr_T) return Qword_T is
            DW_L, DW_R : Dword_T;
        begin
            DW_L := Read_Dword (Word_Addr);
            DW_R := Read_Dword (Word_Addr + 2);
            return Shift_Left(Qword_T(DW_L), 32) or Qword_T(DW_R);
        end Read_Qword;

        procedure Write_Word (Word_Addr : in Phys_Addr_T; Datum : Word_T) is
            Page : Natural := Natural(Shift_Right(Word_Addr, 10));
        begin
            if not Page_Mapped(Page) then
                raise Write_To_Unmapped_Page with Page'Image;
            end if;
            VRAM(Page)(Integer(Word_Addr and 16#03ff#)) := Datum;
        end Write_Word;

        procedure Write_Dword (Word_Addr : in Phys_Addr_T; Datum : Dword_T) is
        begin
            Write_Word(Word_Addr, Upper_Word(Datum));
            Write_Word(Word_Addr + 1, Lower_Word(Datum));
        end Write_Dword;

        procedure Write_Qword (Word_Addr : in Phys_Addr_T; Datum : Qword_T) is
        begin
            Write_Dword(Word_Addr, Dword_T(Shift_Right(Datum, 32)));
            Write_Dword(Word_Addr + 2, Dword_T(Datum and 16#0000_ffff#));
        end Write_Qword;

        function Read_Byte (Word_Addr : in Phys_Addr_T; Low_Byte : in Boolean) return Byte_T
        is
            W : Word_T;
        begin
            W := Read_Word (Word_Addr);
            if not Low_Byte then
                W := Shift_Right (W, 8);
            end if;
            return Byte_T (W and 16#00ff#);
        end Read_Byte;

        function Read_Byte_BA (BA : in Dword_T) return Byte_T is
            LB : Boolean := Test_DW_Bit (BA, 31);
        begin
            return Read_Byte (Phys_Addr_T(Shift_Right(BA, 1)), LB);
        end Read_Byte_BA;

        function  Read_Bytes_BA (BA : in Dword_T; Num : in Natural) return Byte_Arr_T is
            Bytes : Byte_Arr_T (0 .. Num-1);
        begin
            for B in 0 .. Num - 1 loop
               Bytes(B) := Read_Byte_BA (BA + Dword_T(B));
            end loop;
            return Bytes;
        end Read_Bytes_BA;

        function Read_String_BA (BA : in Dword_T) return String is
            Is_Low_Byte    : Boolean := Test_DW_Bit (BA, 31);
            Byte, Low_Byte : Byte_T;
            Offset         : Dword_T := 0;
            U_Str          : Unbounded_String;
        begin
            loop
               Byte := Read_Byte_BA (BA + Offset);
               exit when Byte = 0;
               U_Str := U_Str & Byte_To_Char(Byte);
               Offset := Offset + 1;               
            end loop;
            return To_String(U_Str);
        end Read_String_BA;

        procedure Write_String_BA (BA : in Dword_T; Str : in String) is
            Offset         : Dword_T := 0;
        begin
            for C in Str'Range loop
               Write_Byte_BA (BA + Offset, Byte_T(Character'Pos(Str(C))));
               Offset := Offset + 1;
            end loop;
            Write_Byte_BA (BA + Offset, 0);
        end Write_String_BA;


        function  Read_Byte_Eclipse_BA (Segment : in Phys_Addr_T; BA_16 : in Word_T) return Byte_T is
            Low_Byte : Boolean := Test_W_Bit(BA_16, 15);
            Addr : Phys_Addr_T;
        begin
            Addr := Shift_Right(Phys_Addr_T(BA_16), 1) or Segment;
            return Read_Byte(Addr, Low_Byte);
        end Read_Byte_Eclipse_BA;

        procedure Write_Byte_Eclipse_BA (Segment : in Phys_Addr_T; BA_16 : in Word_T; Datum : in Byte_T) is
            Low_Byte : Boolean := Test_W_Bit(BA_16, 15);
            Addr : Phys_Addr_T;
        begin
            Addr := Shift_Right(Phys_Addr_T(BA_16), 1) or Segment;
            Write_Byte (Addr, Low_Byte, Datum);
        end Write_Byte_Eclipse_BA;

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

        procedure Write_Byte_BA (BA : in Dword_T; Datum : in Byte_T) is
            LB : Boolean := Test_DW_Bit (BA, 31);
        begin
            Write_Byte (Phys_Addr_T(Shift_Right(BA, 1)), LB, Datum);
        end Write_Byte_BA;

        procedure Copy_Byte_BA (Src, Dest : in Dword_T) is
            Src_LB  : Boolean := Test_DW_Bit (Src, 31);
            Dest_LB : Boolean := Test_DW_Bit (Dest, 31);
            Byt     : Byte_T;
        begin
            Byt := Read_Byte (Phys_Addr_T(Shift_Right(Src, 1)), Src_LB);
            Write_Byte (Phys_Addr_T(Shift_Right(Dest, 1)), Dest_LB, Byt);
        end Copy_Byte_BA;

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

end Memory;