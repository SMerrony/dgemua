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

with Debug_Logs; use Debug_Logs;

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

      function Read_Byte
        (Word_Addr : in Phys_Addr_T; Low_Byte : in Boolean) return Byte_T
      is
         W : Word_T;
      begin
         W := RAM (Word_Addr);
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
      begin
         return
           Dword_From_Two_Words
             (RAM (Word_Addr), RAM (Word_Addr + 1));
      end Read_Dword;

      procedure Write_Dword (Word_Addr : in Phys_Addr_T; Datum : Dword_T) is
      begin
         Write_Word (Word_Addr, Upper_Word (Datum));
         Write_Word (Word_Addr + 1, DG_Types.Lower_Word (Datum));
      end Write_Dword;

      function Read_Word (Word_Addr : in Phys_Addr_T) return Word_T is
      begin
         -- return RAM (Integer (Word_Addr));
         return RAM (Word_Addr);
      end Read_Word;

      procedure Write_Word (Word_Addr : in Phys_Addr_T; Datum : Word_T) is
      begin
         -- -- DEBUGGING
         -- if Word_Addr = 50 then
         --    Put_Line ("Writing to Location ")
         -- end if;
         -- RAM (Integer (Word_Addr)) := Datum;
         RAM (Word_Addr) := Datum;
      end Write_Word;

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
         Put_Line ("INFO: BMC_DCH Registers Reset");
      end Reset;

      function Read_Reg (Reg : in Integer) return Word_T is
      begin
         return Registers (Reg);
      end Read_Reg;

      -- Write_Reg populates a given 16-bit register with the supplied data
      -- N.B. Addressed by REGISTER not slot
      procedure Write_Reg (Reg : in Integer; Datum : in Word_T) is
      begin
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Write_Reg with Register: " & Reg'Image & " Datum:" &
               Datum'Image);
         end if;
         if Reg = IO_Chan_Def_Reg then
            -- certain bits in the new data cause IOCDR bits to be flipped rather than set
            for B in 0 .. 15 loop
               case B is
                  when 3 | 4 | 7 | 8 | 14 =>
                     if Test_W_Bit (Datum, B) then
                        Flip_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     end if;
                  when others =>
                     if Test_W_Bit (Datum, B) then
                        Set_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     else
                        Clear_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     end if;
               end case;
            end loop;
         else
            Registers (Reg) := Datum;
         end if;
      end Write_Reg;

      -- Write_Slot populates a whole SLOT (pair of registers) with the supplied doubleword
      -- N.B. Addressed by SLOT not register
      procedure Write_Slot (Slot : in Integer; Datum : in Dword_T) is
      begin
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Write_Slot with Slot: " & Slot'Image & " Datum:" &
               Datum'Image);
         end if;
         Registers (Slot * 2)       := Upper_Word (Datum);
         Registers ((Slot * 2) + 1) := DG_Types.Lower_Word (Datum);
      end Write_Slot;

      function Get_DCH_Mode return Boolean is
      begin
         return Test_W_Bit (Registers (IO_Chan_Def_Reg), 14);
      end Get_DCH_Mode;

      function Resolve_BMC_Mapped_Addr (M_Addr : in Phys_Addr_T) return Phys_Addr_T is
         Slot           : Integer := Integer(Shift_Right(M_Addr, 10));
         P_Addr, P_Page : Phys_Addr_T;
      begin
         -- N.B. at some point between 1980 and 1987 the lower 5 bits of the odd word were
	      -- prepended to the even word to extend the mappable space 
         P_Page := Shift_Left (Phys_Addr_T(Registers(Slot * 2)) and 16#0000_001f#, 16) +
                   Shift_Left (Phys_Addr_T(Registers((Slot * 2) + 1)), 10);
         P_Addr := (M_Addr and 16#0000_03ff#) or P_Page;
         return P_Addr;
      end Resolve_BMC_Mapped_Addr;

      function Resolve_DCH_Mapped_Addr
        (M_Addr : in Phys_Addr_T) return Phys_Addr_T
      is
         P_Addr, P_Page : Phys_Addr_T;
         Slot           : Integer;
         Offset         : Phys_Addr_T;
      begin
         -- the slot is up to 9 bits long
         Slot :=
           Integer (Shift_Right (M_Addr, 10) and 16#001f#) + First_DCH_Slot;
         if (Slot < First_DCH_Slot) or
           (Slot > (First_DCH_Slot + Num_DCH_Slots))
         then
            raise Invalid_DCH_Slot;
         end if;
         Offset := M_Addr and 16#0000_03ff#;
         -- N.B. at some point between 1980 and 1987 the lower 5 bits of the odd word were
         -- prepended to the even word to extend the mappable space
         P_Page :=
           Shift_Left
             (Phys_Addr_T (Registers (Slot * 2) and 16#0000_001f#), 16) or
           Phys_Addr_T (Registers ((Slot * 2) + 1));
         P_Addr := Shift_Left (P_Page, 10) or Offset;
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Resolve_DCH_Mapped_Addr got: " & M_Addr'Image &
               " Returning: " & P_Addr'Image);
         end if;
         return P_Addr;
      end Resolve_DCH_Mapped_Addr;

      procedure Write_Word_DCH_Chan
        (Unmapped : in out Phys_Addr_T; Datum : in Word_T)
      is
         P_Addr : Phys_Addr_T;
      begin
         if Get_DCH_Mode then
            P_Addr := Resolve_DCH_Mapped_Addr (Unmapped);
         else
            P_Addr := Unmapped;
         end if;
         RAM.Write_Word (P_Addr, Datum);
         -- auto-increment the supplied address
         Unmapped := Unmapped + 1;
      end Write_Word_DCH_Chan;

      function Decode_BMC_Addr (Unmapped : in Phys_Addr_T) return BMC_Addr_T is
         In_Addr : Phys_Addr_T := Shift_Left(Unmapped, 10);
         Res : BMC_Addr_T;
      begin
         Res.Is_Logical := Test_DW_Bit (Dword_T(In_Addr), 0);
         if Res.Is_Logical then
            Res.TT   := Natural(Get_DW_Bits(Dword_T(In_Addr), 2, 5));
            Res.TTR  := Natural(Get_DW_Bits(Dword_T(In_Addr), 7, 5));
            Res.P_Low := Unmapped and 16#0000_3fff#;
         else
            Res.Bk  := Natural(Get_DW_Bits(Dword_T(In_Addr), 1, 3));
            Res.XCA := Natural(Get_DW_Bits(Dword_T(In_Addr), 4, 3));
            Res.CA  := Unmapped and 16#0000_7fff#;
         end if;
         return Res;
      end Decode_BMC_Addr;

      procedure Read_Word_BMC_16 (Unmapped : in out Word_T; Datum : out Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : BMC_Addr_T := Decode_BMC_Addr (Phys_Addr_T(Unmapped));
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Phys_Addr_T(Unmapped)); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         Datum := RAM.Read_Word (P_Addr);
         Unmapped := Unmapped + 1;
      end Read_Word_BMC_16;

      procedure Write_Word_BMC_16 (Unmapped : in out Word_T; Datum : in Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : BMC_Addr_T := Decode_BMC_Addr (Phys_Addr_T(Unmapped));
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Phys_Addr_T(Unmapped)); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         RAM.Write_Word (P_Addr, Datum);
         Unmapped := Unmapped + 1;
      end Write_Word_BMC_16;

   end BMC_DCH;

end Memory;
