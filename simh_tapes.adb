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

-- TODO Error Handling

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Simh_Tapes is

   function Reverse_Dword_Bytes (In_Dw : in Dword_T) return Dword_T is
   begin
      return  Shift_Left (In_Dw and 16#0000_00ff#, 24) or
        Shift_Left (In_Dw and 16#0000_ff00#, 8) or
        Shift_Right (In_Dw and 16#00ff_0000#, 8) or
        Shift_Right (In_Dw and 16#ff00_0000#, 24);
   end Reverse_Dword_Bytes;

   -- Read_Meta_Data reads a four byte (one doubleword) header, trailer, or other metadata record
   -- from the supplied tape image file
   procedure Read_Meta_Data
     (Img_Stream : in out Stream_Access; Meta_Data : out Dword_T)
   is
      Tmp_Dw : Dword_T;
   begin
      Dword_T'Read (Img_Stream, Tmp_Dw);
      -- Meta_Data := Reverse_Dword_Bytes (Tmp_Dw);
      Meta_Data := Tmp_Dw;
   end Read_Meta_Data;

   -- Write_Meta_Data writes a 4-byte header/trailer or other metadata
   procedure Write_Meta_Data (Img_Stream : in out Stream_Access; Meta_Data : in Dword_T) is
      -- Tmp_Dw : Dword_T;
   begin
      -- Tmp_Dw := Reverse_Dword_Bytes (Meta_Data);
      -- Dword_T'Write (Img_Stream, Tmp_Dw);
       Dword_T'Write (Img_Stream, Meta_Data);
   end Write_Meta_Data;

   -- Read_Record_Data attempts to read a data record from SimH tape image, fails if wrong number of bytes read
   -- N.B. does not read the header and trailer
   procedure Read_Record_Data  (Img_Stream : in out Stream_Access; Num_Bytes : in Natural; Rec : out Mt_Rec) is
      Tmp_Rec : Mt_Rec (1..Num_Bytes);
      Out_Rec_Ix : Integer := Rec'First;
   begin
      for C in 1 .. Num_Bytes loop
         Byte_T'Read (Img_Stream, Tmp_Rec(C));
         Rec(Out_Rec_Ix) := Tmp_Rec(C);
         Out_Rec_Ix := Out_Rec_Ix + 1;
      end loop;
   end Read_Record_Data;

   -- Write_Record_Data writes the actual data - not the header/trailer
   procedure Write_Record_Data (Img_Stream : in out Stream_Access; Rec : in Mt_Rec) is
   begin
      for C in Rec'Range loop
         Byte_T'Write( Img_Stream, Rec(C));
      end loop;
   end Write_Record_Data;

   procedure Rewind (Img_File : in out File_Type) is
   begin
      Set_Index (Img_File, 1);
   end Rewind;

   -- internal function
   function Space_Forward_1_Rec (Img_Stream : in out Stream_Access) return Mt_Stat is
      Hdr, Trailer : Dword_T;
   begin
      Read_Meta_Data (Img_Stream , Hdr);
      if Hdr = Mtr_Tmk then
         return Tmk;
      end if;
      -- read and discard 1 record
      declare
         Rec : Mt_Rec(1..Natural(Hdr));
      begin
          Read_Record_Data (Img_Stream , Natural(Hdr), Rec);
      end;
      -- check trailer
      Read_Meta_Data (Img_Stream , Trailer);
      if Hdr /= Trailer then
         return InvRec;
      end if;
      return OK;
   end Space_Forward_1_Rec;

   -- SpaceFwd advances the virtual tape by the specified amount (N.B. 0 means 1 whole file)
   function Space_Forward (Img_Stream : in out Stream_Access; Num_Recs : in Integer) return Mt_Stat is
      Simh_Stat    : Mt_Stat := IOerr;
      Done         : Boolean := false;
      Hdr, Trailer : Dword_T;
      Rec_Cnt      : Integer := Num_Recs;
   begin
      if Num_Recs = 0 then
         -- one whole file
         while not Done loop
            Read_Meta_Data (Img_Stream , Hdr);
            if Hdr = Mtr_Tmk then
               Simh_Stat := OK;
               Done := true;
            else
               -- read and discard 1 record
               declare
                  Rec : Mt_Rec(1..Natural(Hdr));
               begin
                  Read_Record_Data (Img_Stream , Natural(Hdr), Rec);
               end;
               -- check trailer
               Read_Meta_Data (Img_Stream , Trailer);
               if Hdr /= Trailer then
                  return InvRec;
               end if;
            end if;
         end loop;
      else
         -- otherwise word count is a negative number and we space fwd that many records
         while Rec_Cnt /= 0 loop
            Rec_Cnt := Rec_Cnt + 1;
            Simh_Stat := Space_Forward_1_Rec (Img_Stream);
            if Simh_Stat /= OK then
               return Simh_Stat;
            end if;
         end loop;
      end if;
      return Simh_Stat;
   end Space_Forward;

   -- Scan_Image - attempt to read a whole tape image ensuring headers, record sizes, and trailers match
   -- TODO if csv is true then output is in CSV format
   function Scan_Image (Img_Filename : in  String) return String is
      Result : Unbounded_String;
      Img_File : File_Type;
      Img_Stream : Stream_Access;
      Hdr, Trailer : Dword_T;
      File_Count : Integer := -1;
      File_Size, Mark_Count, Record_Num  : Integer := 0;
      Dummy_Rec : Mt_Rec(1..32768);
   begin
      Open (File => Img_File, Mode => In_File, Name => Img_Filename);
      Img_Stream := stream(Img_File);
Record_Loop :
      loop
         Read_Meta_Data (Img_Stream , Hdr);
         case Hdr is
            when Mtr_Tmk =>
               if File_Size > 0 then
                  File_Count := File_Count + 1;
                  Result := Result & Dasher_NL & "File " & Integer'Image(File_Count) &
                            " : " & Integer'Image(File_Size) & " bytes in " &
                            Integer'Image(Record_Num) & " blocks";
                  File_Size := 0;
                  Record_Num := 0;
               end if;
               Mark_Count := Mark_Count + 1;
               if Mark_Count = 3 then
                  Result := Result & Dasher_NL & "Triple Mark (old End-of-Tape indicator)";
                  exit Record_Loop;
               end if;

            when Mtr_EOM =>
               Result := Result & Dasher_NL & "End of Medium";
               exit Record_Loop;

            when Mtr_Gap =>
               Result := Result & Dasher_NL & "Erase Gap";
               Mark_Count := 0;

            when others =>
               Record_Num := Record_Num + 1;
               Mark_Count := 0;
               Read_Record_Data (Img_Stream, Natural(Hdr), Dummy_Rec);
               Read_Meta_Data (Img_Stream , Trailer);
               if Hdr = Trailer then
                  File_Size := File_Size + Integer(Hdr);
               else
                  Result := Result & Dasher_NL & "Non-matching trailer found.";
               end if;
            end case;
      end loop Record_Loop;

      Close (Img_File);
      return To_String(Result);
   end Scan_Image;

end Simh_Tapes;
