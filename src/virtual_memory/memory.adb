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

        procedure Init (Debug_Logging : Boolean) is
        begin
            Logging  := Debug_Logging;
            -- always need to map user page 0
            Map_Page (Ring_7_Page_0, false);
            First_Shared_Page := (2 ** 31) - 1;
            Num_Shared_Pages := 0;
            Num_Unshared_Pages := 0;
        end Init;

        procedure Map_Page (Page : Natural; Is_Shared : Boolean) is
            New_Page : constant Page_T := (others => 0);
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
                    Num_Unshared_Pages := Num_Unshared_Pages  + 1;
                    Last_Unshared_Page := Page;
                end if;
            end if;
            Ada.Text_IO.Put_Line ("Memory: Mapped " &
                                  (if Is_Shared then "Shared" else "Unshared") &
                                  " page " & Int_To_String(Page, Hex, 8));
        end Map_Page;

        function Page_Mapped (Page : Natural) return Boolean is (VRAM.Contains(Page));
        
        function Address_Mapped (Addr : Phys_Addr_T) return Boolean is
            (VRAM.Contains(Natural(Shift_Right(Addr, 10))));

        procedure Map_Range (Start_Addr : Phys_Addr_T;
                             Region     : Memory_Region;
                             Is_Shared  : Boolean) is
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

        procedure Map_Shared_Pages (Start_Addr : Phys_Addr_T; Pages : Page_Arr_T) is
            Page_Start : Phys_Addr_T;
        begin
            for P in Pages'Range loop
                Page_Start := Start_Addr + Phys_Addr_T(P * Words_Per_Page);
               if not Address_Mapped (Page_Start) then
                  Map_Page(Natural(Shift_Right(Page_Start,10)), true);
               end if;
               for W in 0 .. Words_Per_Page - 1 loop
                  Write_Word(Page_Start + Phys_Addr_T(W), Pages(P)(W));
               end loop; 
            end loop;
        end Map_Shared_Pages;

        function  Get_Last_Unshared_Page return Dword_T is
            (Dword_T(Last_Unshared_Page));

        function  Get_First_Shared_Page return Dword_T is
            (Dword_T(First_Shared_Page));

        function  Get_Num_Shared_Pages return Dword_T is
            (Dword_T(Num_Shared_Pages));

        function  Get_Num_Unshared_Pages return Dword_T is
            (Dword_T(Num_Unshared_Pages));    

        function Read_Word (Word_Addr : Phys_Addr_T) return Word_T is
            pragma Suppress (Index_Check);
            Page : constant Natural := Natural(Shift_Right(Word_Addr, 10));
        begin
            if not Page_Mapped(Page) then
                raise Read_From_Unmapped_Page with Int_To_String(Page, Hex, 8) &
                    " Unmapped Read for address: " & Dword_To_String(Dword_T(Word_Addr), Octal, 11);
            end if;
            return VRAM(Page)(Integer(Word_Addr and 16#03ff#));
        end Read_Word;

        function Read_Dword (Word_Addr : Phys_Addr_T) return Dword_T is
            -- Page : Natural := Natural(Shift_Right(Word_Addr, 10));
            Hi_WD : constant Word_T := Read_Word(Word_Addr);
            Lo_WD : constant Word_T := Read_Word(Word_Addr + 1);
        begin
            return Dword_From_Two_Words(Hi_WD, Lo_WD);
        end Read_Dword;

        function Read_Qword  (Word_Addr : Phys_Addr_T) return Qword_T is
            DW_L, DW_R : Dword_T;
        begin
            DW_L := Read_Dword (Word_Addr);
            DW_R := Read_Dword (Word_Addr + 2);
            return Shift_Left(Qword_T(DW_L), 32) or Qword_T(DW_R);
        end Read_Qword;

        procedure Write_Word (Word_Addr : Phys_Addr_T; Datum : Word_T) is
            Page : constant Natural := Natural(Shift_Right(Word_Addr, 10));
        begin
            if not Page_Mapped(Page) then
                raise Write_To_Unmapped_Page with Page'Image;
            end if;
            VRAM(Page)(Integer(Word_Addr and 16#03ff#)) := Datum;
        end Write_Word;

        procedure Write_Dword (Word_Addr : Phys_Addr_T; Datum : Dword_T) is
        begin
            Write_Word(Word_Addr, Upper_Word(Datum));
            Write_Word(Word_Addr + 1, Lower_Word(Datum));
        end Write_Dword;

        procedure Write_Qword (Word_Addr : Phys_Addr_T; Datum : Qword_T) is
        begin
            Write_Dword(Word_Addr, Dword_T(Shift_Right(Datum, 32)));
            Write_Dword(Word_Addr + 2, Dword_T(Datum and 16#0000_ffff#));
        end Write_Qword;

        function Read_Byte (Word_Addr : Phys_Addr_T; Low_Byte : Boolean) return Byte_T
        is
            W : Word_T;
        begin
            W := Read_Word (Word_Addr);
            if not Low_Byte then
                W := Shift_Right (W, 8);
            end if;
            return Byte_T (W and 16#00ff#);
        end Read_Byte;

        function Read_Byte_BA (BA : Dword_T) return Byte_T is
            LB : constant Boolean := Test_DW_Bit (BA, 31);
        begin
            return Read_Byte (Phys_Addr_T(Shift_Right(BA, 1)), LB);
        end Read_Byte_BA;

        function  Read_Bytes_BA (BA : Dword_T; Num : Natural) return Byte_Arr_T is
            Bytes : Byte_Arr_T (0 .. Num-1);
        begin
            for B in Bytes'Range loop
               Bytes(B) := Read_Byte_BA (BA + Dword_T(B));
            end loop;
            return Bytes;
        end Read_Bytes_BA;

        function Read_String_BA (BA : Dword_T; Keep_NUL : Boolean) return String is
            Byte           : Byte_T;
            Offset         : Dword_T := 0;
            U_Str          : Unbounded_String;
        begin
            loop
               Byte := Read_Byte_BA (BA + Offset);
               exit when Byte = 0;
               U_Str := U_Str & Byte_To_Char(Byte);
               Offset := Offset + 1;               
            end loop;
            if Keep_NUL then
                return To_String(U_Str) & ASCII.NUL;
            else
                return To_String(U_Str);
            end if;
        end Read_String_BA;

        procedure Write_String_BA (BA : Dword_T; Str : String) is
        -- Write an AOS/VS "String" into memory, appending a NUL character
            Offset         : Dword_T := 0;
        begin
            for C in Str'Range loop
               Write_Byte_BA (BA + Offset, Byte_T(Character'Pos(Str(C))));
               Offset := Offset + 1;
            end loop;
            Write_Byte_BA (BA + Offset, 0);
        end Write_String_BA;


        function  Read_Byte_Eclipse_BA (Segment : Phys_Addr_T; BA_16 : Word_T) return Byte_T is
            Low_Byte : constant Boolean := Test_W_Bit(BA_16, 15);
            Addr : Phys_Addr_T;
        begin
            Addr := Shift_Right(Phys_Addr_T(BA_16), 1) or Segment;
            return Read_Byte(Addr, Low_Byte);
        end Read_Byte_Eclipse_BA;

        procedure Write_Byte_Eclipse_BA (Segment : Phys_Addr_T; BA_16 : Word_T; Datum : Byte_T) is
            Low_Byte : constant Boolean := Test_W_Bit(BA_16, 15);
            Addr : Phys_Addr_T;
        begin
            Addr := Shift_Right(Phys_Addr_T(BA_16), 1) or Segment;
            Write_Byte (Addr, Low_Byte, Datum);
        end Write_Byte_Eclipse_BA;

        procedure Write_Byte (Word_Addr : Phys_Addr_T; Low_Byte : Boolean; Byt : Byte_T) is
            Wd : Word_T := Read_Word(Word_Addr);
        begin
            if Low_Byte then
                Wd := (Wd and 16#ff00#) or Word_T(Byt);
            else
                Wd := Shift_Left(Word_T(Byt), 8) or (Wd and 16#00ff#);
            end if;
            Write_Word(Word_Addr, Wd);
        end Write_Byte;

        procedure Write_Byte_BA (BA : Dword_T; Datum : Byte_T) is
            LB : constant Boolean := Test_DW_Bit (BA, 31);
        begin
            Write_Byte (Phys_Addr_T(Shift_Right(BA, 1)), LB, Datum);
        end Write_Byte_BA;

        procedure Copy_Byte_BA (Src, Dest : Dword_T) is
            Src_LB  : constant Boolean := Test_DW_Bit (Src, 31);
            Dest_LB : constant Boolean := Test_DW_Bit (Dest, 31);
            Byt     : Byte_T;
        begin
            Byt := Read_Byte (Phys_Addr_T(Shift_Right(Src, 1)), Src_LB);
            Write_Byte (Phys_Addr_T(Shift_Right(Dest, 1)), Dest_LB, Byt);
        end Copy_Byte_BA;

    end RAM;

    protected body Narrow_Stack is
      procedure Push (Segment : Phys_Addr_T; Datum : Word_T) is
         New_NSP : constant Word_T := RAM.Read_Word (NSP_Loc or Segment) + 1;
      begin
         RAM.Write_Word (NSP_Loc or Segment, New_NSP);
         RAM.Write_Word (Phys_Addr_T (New_NSP) or Segment, Datum);
         -- if CPU.Debug_Logging then
            Loggers.Debug_Print (Debug_Log, "... Pushed " & Word_To_String (Datum, Octal, 7) &
                                           " to: " & Dword_To_String (Dword_T(Phys_Addr_T (New_NSP) or Segment), Octal, 11));
         -- end if;
      end Push;

      function Pop (Segment : Phys_Addr_T) return Word_T is
         Old_NSP : constant Word_T := RAM.Read_Word (NSP_Loc or Segment);
         Datum   : constant Word_T := RAM.Read_Word (Phys_Addr_T (Old_NSP) or Segment);
      begin
         RAM.Write_Word (NSP_Loc or Segment, Old_NSP - 1);
         -- if CPU.Debug_Logging then
            Loggers.Debug_Print (Debug_Log, "... Popped " & Word_To_String (Datum, Octal, 7) &
                                           " from: " & Dword_To_String (Dword_T(Phys_Addr_T (Old_NSP) or Segment), Octal, 11));
         -- end if;
         return Datum;
      end Pop;
   end Narrow_Stack;

end Memory;