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
-- with Ada.Strings;           use Ada.Strings;
-- with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
-- with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Debug_Logs;      use Debug_Logs;
with Memory_Channels; use Memory_Channels;

package body Memory is

   protected body RAM is

      procedure Init (Debug_Logging : in Boolean) is
      begin
         Is_Logging  := Debug_Logging;
         ATU_Enabled := False;
         for W in RAM'Range loop
            RAM (W) := 0;
         end loop;
         BMC_DCH.Init (Debug_Logging);
         Put_Line
           ("INFO: Initialised " & Integer'Image (RAM'Length) &
            " words of main memory");
      end Init;

      function Read_Byte
        (Word_Addr : in Phys_Addr_T; Low_Byte : in Boolean) return Byte_T
      is
         W : Word_T;
      begin
         W := RAM (Word_Addr); -- *** DIRECT ACCESS TO RAM ***
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
         Bytes : Byte_Arr_T (0 .. Num);
      begin
         for B in 0 .. Num - 1 loop
            Bytes(B) := Read_Byte_BA (BA + Dword_T(B));
         end loop;
         return Bytes;
      end Read_Bytes_BA;

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

      function Read_Dword (Word_Addr : in Phys_Addr_T) return Dword_T is
         (Dword_From_Two_Words(RAM (Word_Addr), RAM (Word_Addr + 1))); -- *** Direct RAM Access ***

      procedure Write_Dword (Word_Addr : in Phys_Addr_T; Datum : Dword_T) is
      begin
         Write_Word (Word_Addr, Upper_Word (Datum));
         Write_Word (Word_Addr + 1, DG_Types.Lower_Word (Datum));
      end Write_Dword;

      function Read_Word (Word_Addr : in Phys_Addr_T) return Word_T is
         (RAM (Word_Addr));

      function Read_Qword  (Word_Addr : in Phys_Addr_T) return Qword_T is
            DW_L, DW_R : Dword_T;
        begin
            DW_L := Read_Dword (Word_Addr);
            DW_R := Read_Dword (Word_Addr + 2);
            return Shift_Left(Qword_T(DW_L), 32) or Qword_T(DW_R);
        end Read_Qword;   

      procedure Write_Word (Word_Addr : in Phys_Addr_T; Datum : Word_T) is
      -- FOR THE MOMENT _ALL_ MEMORY WRITES ARE VIA THIS PROC
      begin
         -- -- DEBUGGING
         -- if Word_Addr = 2 then
         --    Put_Line ("Writing to Location 2, VALUE: " & Datum'Image);
         -- end if;
         -- if Word_Addr = 0 then
         --    Put_Line ("Writing to Location 0, VALUE: " & Datum'Image);
         -- end if;
         RAM (Word_Addr) := Datum;
      end Write_Word;

      procedure Write_Qword (Word_Addr : in Phys_Addr_T; Datum : Qword_T) is
        begin
            Write_Dword(Word_Addr, Dword_T(Shift_Right(Datum, 32)));
            Write_Dword(Word_Addr + 2, Dword_T(Datum and 16#0000_ffff#));
        end Write_Qword;
   end RAM;

   protected body Narrow_Stack is
      procedure Push (Segment : in Phys_Addr_T; Datum : in Word_T) is
         New_NSP : Word_T := RAM.Read_Word (NSP_Loc or Segment) + 1;
      begin
         RAM.Write_Word (NSP_Loc or Segment, New_NSP);
         RAM.Write_Word (Phys_Addr_T (New_NSP) or Segment, Datum);
         -- if CPU.Debug_Logging then
            Loggers.Debug_Print (Debug_Log, "... Pushed " & Word_To_String (Datum, Octal, 11) &
                                            " to: " & Dword_To_String (Dword_T(New_NSP) or Dword_T(Segment), Octal, 11));
         -- end if;
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
