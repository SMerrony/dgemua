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

with Ada.Text_IO; use Ada.Text_IO;

with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with Resolver;    use Resolver;

package body Processor.Eagle_Mem_Ref_P is 

   procedure Do_Eagle_Mem_Ref (I : Decoded_Instr_T; CPU : CPU_T) is
      Addr : Phys_Addr_T;
      Word : Word_T;
      S64,
      S64_Mem,
      S64_Ac : Integer_64;
      I32  : Integer_32;
      I16_Ac, I16_Mem : Integer_16;
      Low_Byte: Boolean;

      procedure Set_OVR (New_OVR : Boolean) is
      begin
         if New_OVR then
               Set_W_Bit(CPU.PSR, 1);
         else
               Clear_W_Bit(CPU.PSR, 1);
         end if;
      end Set_OVR;

      function WA_from_BA (W1, W2 : Word_T) return Integer_32 is
         Neg : constant Boolean := (W1 and 16#8000#) = 16#8000#;
         I32 : Integer_32;
      begin
         if Neg then
            I32 := Dword_To_Integer_32 (16#8000_0000# or Shift_Right (Dword_From_Two_Words (W1,W2), 1));
         else 
            I32 := Dword_To_Integer_32 (Shift_Right (Dword_From_Two_Words (W1,W2), 1));
         end if;
         return I32;
      end WA_from_BA;

   begin
      case I.Instruction is

         when I_LLDB =>
            I32 := WA_from_BA(I.Word_2,I.Word_3);
            Addr := Resolve_31bit_Disp (CPU, false, I.Mode, I32, I.Disp_Offset);
            Low_Byte := (I.Disp_31 mod 2 = 1);
            CPU.AC(I.Ac) := Dword_T(RAM.Read_Byte(Addr, Low_Byte));

         when I_LLEF =>
            CPU.AC(I.Ac) := Dword_T(Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset));

         when I_LLEFB =>
            I32 := WA_from_BA(I.Word_2,I.Word_3);
            Addr := Shift_Left(Resolve_31bit_Disp (CPU, false, I.Mode, I32, I.Disp_Offset), 1);
            if I.Disp_31 mod 2 = 1 then
               Addr := Addr or 1;
            end if;
            CPU.AC(I.Ac) := Dword_T(Addr);

         when I_LNADI =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            Word := RAM.Read_Word (Addr);
            Word := Word + Word_T(I.Imm_U16);
            RAM.Write_Word (Addr, Word);

         when I_LNADD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            I32 := Integer_32(Word_To_Integer_16(RAM.Read_Word(Addr))) + 
                   Integer_32(Word_To_Integer_16(CPU.AC_Wd(I.Ac)));
            CPU.Carry := (I32 > Max_Pos_S16) or (I32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC_I32(I.Ac) := I32;

         when I_LNLDA =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            CPU.AC(I.Ac) := Sext_Word_To_Dword (RAM.Read_Word(Addr));

         when I_LNSBI =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            Word := RAM.Read_Word (Addr);
            Word := Word - Word_T(I.Imm_U16);
            RAM.Write_Word (Addr, Word);   

         when I_LNSTA =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            RAM.Write_Word (Addr, CPU.AC_Wd(I.Ac));

         when I_LSTB =>
            I32 := WA_from_BA(I.Word_2,I.Word_3);
            Addr := Resolve_31bit_Disp (CPU, false, I.Mode,I32, I.Disp_Offset);
            Low_Byte :=(I.Disp_31 mod 2 = 1);
            RAM.Write_Byte(Addr, Low_Byte, Byte_T(CPU.AC(I.Ac)));

         when I_LWADD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            S64 := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr))) + Integer_64(CPU.AC_I32(I.Ac));
            if S64 < Min_Neg_S32 or S64 > Max_Pos_S32 then
               Set_OVR (true);
            end if;
            CPU.AC(I.Ac) := Lower_Dword(Qword_T(Integer_64_To_Unsigned_64(S64)));

         when I_LWADI =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            S64 := Integer_64(Dword_To_Integer_32 (RAM.Read_Dword(Addr))) + Integer_64(I.Imm_U16);
            if S64 < Min_Neg_S32 or S64 > Max_Pos_S32 then
               Set_OVR (true);
            end if;
            RAM.Write_Dword (Addr, Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#));

         when I_LWLDA =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            CPU.AC(I.Ac) := RAM.Read_Dword (Addr);

         when I_LWMUL =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            S64 := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr))) * Integer_64(CPU.AC_I32(I.Ac));
            if S64 < Min_Neg_S32 or S64 > Max_Pos_S32 then
               Set_OVR (true);
            end if;
            CPU.AC(I.Ac) := Lower_Dword(Qword_T(Integer_64_To_Unsigned_64(S64)));

         when I_LWSTA =>
            Loggers.Debug_Print (Debug_Log, "... Word 2: " & Word_To_String (I.Word_2, Hex, 4, true));
            Loggers.Debug_Print (Debug_Log, "... Word 3: " & Word_To_String (I.Word_3, Hex, 4, true));
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            RAM.Write_Dword (Addr, CPU.AC(I.Ac));

         when I_LWSUB =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            I32 := Dword_To_Integer_32(CPU.AC(I.Ac)) - Dword_To_Integer_32(RAM.Read_Dword(Addr));
            CPU.AC_I32(I.Ac) := I32;

         when I_WBLM =>
            -- AC0 - unused, AC1 - no. wds to move (if neg then descending order), AC2 - src, AC3 - dest
            while CPU.AC(1) /= 0 loop
               RAM.Write_Word(Phys_Addr_T(CPU.AC(3)), 
                                       RAM.Read_Word (Phys_Addr_T(CPU.AC(2))));
               if Test_DW_Bit (CPU.AC(1), 0) then
                  CPU.AC(1) := CPU.AC(1) + 1;
                  CPU.AC(2) := CPU.AC(2) - 1;
                  CPU.AC(3) := CPU.AC(3) - 1;
               else
                  CPU.AC(1) := CPU.AC(1) - 1;
                  CPU.AC(2) := CPU.AC(2) + 1;
                  CPU.AC(3) := CPU.AC(3) + 1;
               end if;
            end loop;

         when I_WBTO | I_WBTZ =>
            declare
               Offset  : constant Phys_Addr_T := Phys_Addr_T(Shift_Right(CPU.AC(I.Acd), 4));
               Bit_Num : constant Integer := Integer(CPU.AC(I.Acd) and 16#0000_000f#);
            begin
               if I.Acs = I.Acd then
                  Addr := CPU.PC and 16#7000_0000#;
               else
                  Addr := Resolve_32bit_Indirectable_Addr(CPU.ATU, CPU.AC(I.Acs));
               end if;
               Word := RAM.Read_Word (Addr + Offset);
               if I.Instruction = I_WBTO then
                  Set_W_Bit (Word, Bit_Num);
               else
                  Clear_W_Bit (Word, Bit_Num);
               end if;
               RAM.Write_Word (Addr + Offset, Word);
            end;  

         when I_WCMP =>
            declare
               Str1_Dir, Str2_Dir : Integer_32;
               Str1_Char, Str2_Char : Byte_T;
               function Get_Dir(AC : Dword_T) return Integer_32 is
               begin
                  if AC = 0 then return 0; end if;
                  if Test_DW_Bit (AC, 0) then
                     return -1;
                  else
                     return 1;
                  end if;
               end Get_Dir;
            begin
               Str1_Dir := Get_Dir(CPU.AC(1));
               Str2_Dir := Get_Dir(CPU.AC(0));
               if (Str1_Dir = 0) and (Str2_Dir = 0) then
                  Loggers.Debug_Print (Debug_Log, "WARNING: WCMP called with 2 zero lengths not doing anything");
               else
                  while (CPU.AC(1) /= 0) and (CPU.AC(0) /= 0) loop
                  Loggers.Debug_Print (Debug_Log, "... AC0:" & CPU.AC(0)'Image & " AC1:" & CPU.AC(1)'Image);
                     -- read the two bytes to compare, substitute with a space if one string has run out
                     if CPU.AC(1) /= 0 then
                        Str1_Char := RAM.Read_Byte_BA (CPU.AC(3));
                     else
                        Str1_Char := 32;
                     end if;
                     if CPU.AC(0) /= 0 then
                        Str2_Char := RAM.Read_Byte_BA (CPU.AC(2));
                     else
                        Str2_Char := 32;
                     end if;
                     Loggers.Debug_Print (Debug_Log, "... Comparing " & Str1_Char'Image & " and " & Str2_Char'Image);
                     -- compare
                     if Str1_Char < Str2_Char then
                        CPU.AC(1) := 16#ffff_ffff#;
                        exit;
                     end if;
                     if Str1_Char > Str2_Char then
                        CPU.AC(1) := 1;
                        exit;
                     end if;
                     -- they were equal, so adjust remaining lengths, move pointers, and loop round
                     if Str2_Dir < 0 then
                        CPU.AC(2) := CPU.AC(2) - 1;
                        if CPU.AC(0) /= 0 then
                           CPU.AC(0) := CPU.AC(0) + 1;
                        end if;
                     else
                        CPU.AC(2) := CPU.AC(2) + 1;
                        if CPU.AC(0) /= 0 then
                           CPU.AC(0) := CPU.AC(0) - 1;
                        end if;
                     end if;
                     if Str1_Dir < 0 then
                        CPU.AC(3) := CPU.AC(3) - 1;
                        if CPU.AC(1) /= 0 then
                           CPU.AC(1) := CPU.AC(1) + 1;
                        end if;                      
                     else
                        CPU.AC(3) := CPU.AC(3) + 1;
                        if CPU.AC(1) /= 0 then
                           CPU.AC(1) := CPU.AC(1) - 1;
                        end if;    
                     end if;
                  end loop;
               end if;
            end;

         when I_WCMV =>
            declare
               Dest_Ascend, Src_Ascend : Boolean;
               Dest_Cnt, Src_Cnt : Integer_32;
               Blank_Fill        : Boolean := false;
            begin
               Dest_Cnt := CPU.AC_I32(0);
               if Dest_Cnt = 0 then
                  Loggers.Debug_Print (Debug_Log, "WARNING: WCMV called with AC0 = 0, not moving anything");
                  CPU.Carry := false;
               else
                  Dest_Ascend := Dest_Cnt > 0;
                  Src_Cnt := CPU.AC_I32(1);
                  if Src_Cnt = 0 then
                     Blank_Fill := true;
                     Loggers.Debug_Print (Debug_Log, "... Filling with blanks");
                  end if;
                  Src_Ascend := Src_Cnt > 0;
                  Loggers.Debug_Print (Debug_Log, "... Source Count:" & Src_Cnt'Image & "., Dest. Count:" & Dest_Cnt'Image);
                  CPU.Carry := (abs Src_Cnt) > (abs Dest_Cnt);
                  -- move Src_Cnt bytes
                  loop
                     if Blank_Fill then
                        RAM.Write_Byte_BA (CPU.AC(2), 32);
                     else
                        Loggers.Debug_Print (Debug_Log, "... Copy from: " & Dword_To_String (CPU.AC(3),Octal,11,true) & 
                                                         " to: " & Dword_To_String (CPU.AC(2),Octal,11,true) &
                                                         " remaining Src:" & Src_Cnt'Image & "., Dest:" & Dest_Cnt'Image &
                                                         " character: " & Character'Val(RAM.Read_Byte_BA (CPU.AC(3))));
                        RAM.Copy_Byte_BA(CPU.AC(3),CPU.AC(2));
                     end if;
                     if Src_Ascend then
                        CPU.AC(3) := CPU.AC(3) + 1;
                        Src_Cnt := Src_Cnt - 1;
                     else
                        CPU.AC(3) := CPU.AC(3) - 1;
                        Src_Cnt := Src_Cnt + 1;
                     end if;
                     if Dest_Ascend then
                        CPU.AC(2) := CPU.AC(2) + 1;
                        Dest_Cnt := Dest_Cnt - 1;
                     else
                        CPU.AC(2) := CPU.AC(2) - 1;
                        Dest_Cnt := Dest_Cnt + 1;
                     end if;
                     exit when (Src_Cnt = 0) or (Dest_Cnt = 0);
                  end loop;
                  -- now fill any excess bytes with ASCII spaces
                  while Dest_Cnt /= 0 loop
                     RAM.Write_Byte_BA(CPU.AC(2), 32);
                     if Dest_Ascend then
                        CPU.AC(2) := CPU.AC(2) + 1;
                        Dest_Cnt := Dest_Cnt - 1;
                     else
                        CPU.AC(2) := CPU.AC(2) - 1;
                        Dest_Cnt := Dest_Cnt + 1;
                     end if;
                  end loop;
                  CPU.AC(0) := 0;
                  CPU.AC_I32(1) :=Src_Cnt;
               end if;                  
            end;

         when I_WCST =>
            declare
                  Delim_Tab_Addr : Phys_Addr_T;
                  type Delim_Tab_T is array (Byte_T range 0 .. 255) of Boolean;
                  Delim_Tab : Delim_Tab_T;
                  Wd        : Word_T;
                  Src_Len   : constant Integer_32 := CPU.AC_I32(1);
                  Char_Ix   : Integer_32 := 0;
                  Ascending : constant Boolean := (Src_Len > 0);
                  Char_Val  : Byte_T;
            begin
               if CPU.AC(1) = 0 then
                  Loggers.Debug_Print (Debug_Log, "WARNING: WCST called with AC1 = 0, not scanning anything");
               else
                  Delim_Tab_Addr := Resolve_32bit_Indirectable_Addr (CPU.ATU, CPU.AC(0));
                  CPU.AC(0) := Dword_T(Delim_Tab_Addr);

                  -- load the table which is 256 bits stored as 16 words
                  for T_Ix in 0 .. 15 loop
                     Wd := RAM.Read_Word (Delim_Tab_Addr + Phys_Addr_T(T_Ix));
                     for Bit in 0 .. 15 loop
                        Delim_Tab(Byte_T((T_Ix * 16) + Bit)) := Test_W_Bit (Wd, Bit);
                     end loop;
                     Loggers.Debug_Print (Debug_Log, "... Delim. Tab. " & Word_To_String(Wd, Binary, 16, true));
                  end loop;  

                  if Ascending then
                     while Char_Ix < Src_Len loop
                        Char_Val := RAM.Read_Byte_BA(CPU.AC(3) + Dword_T(Char_Ix));
                        Char_Ix := Char_Ix + 1;
                        exit when Delim_Tab(Char_Val);
                     end loop;
                     CPU.AC(1) := Integer_32_To_Dword(Char_Ix);
                     CPU.AC(3) := CPU.AC(3) + Dword_T(Char_Ix);
                  else
                     raise Not_Yet_Implemented;
                  end if;


               end if;
            end;

         when I_WCTR =>
            declare
               Trans_Tab_Addr : Phys_Addr_T;
               type Trans_Tab_T is array (0 .. 255) of Byte_T;
               Trans_Tab : Trans_Tab_T;
               Src_Byte, Trans_Byte, Str2_Byte, Trans2_Byte : Byte_T;
            begin
               if CPU.AC(1) = 0 then
                  Loggers.Debug_Print (Debug_Log, "WARNING: WCTR called with AC1 = 0, not translating anything");
               else
                  Trans_Tab_Addr := Resolve_32bit_Indirectable_Addr (CPU.ATU, CPU.AC(0));
                  Trans_Tab_Addr := Phys_Addr_T(RAM.Read_Dword (Trans_Tab_Addr));
                  for C in 0 .. 255 loop
                     Trans_Tab(C) := RAM.Read_Byte_BA (Dword_T(Trans_Tab_Addr) + Dword_T(C));
                  end loop;
                  while CPU.AC(1) /= 0 loop
                     Src_Byte := RAM.Read_Byte_BA (CPU.AC(3));
                     CPU.AC(3) := CPU.AC(3) + 1;
                     Trans_Byte := Trans_Tab(Integer(Src_Byte));
                     if CPU.AC_I32(1) < 0 then
                        -- translate-and-move mode
                        -- Loggers.Debug_Print (Debug_Log, "... Translate-and-Move mode");
                        RAM.Write_Byte_BA(CPU.AC(2), Trans_Byte);
                        CPU.AC(2) := CPU.AC(2) + 1;
                        CPU.AC(1) := CPU.AC(1) + 1;
                     else
                        -- translate-and-compare mode
                        -- Loggers.Debug_Print (Debug_Log, "... Translate-and-Compare mode");
                        Str2_Byte := RAM.Read_Byte_BA (CPU.AC(2));
                        CPU.AC(2) := CPU.AC(2) + 1;
                        Trans2_Byte := Trans_Tab(Integer(Str2_Byte));
                        if Src_Byte < Trans2_Byte then
                           CPU.AC(1) := 16#ffff_ffff#;
                           exit;
                        elsif Src_Byte > Trans2_Byte then
                           CPU.AC(1) := CPU.AC(1) + 1;
                           exit;
                        end if;
                        CPU.AC(1) := CPU.AC(1) - 1;
                     end if;
                  end loop;
               end if;
            end;

         when I_WLDB =>
            CPU.AC(I.Acd) := Dword_T(RAM.Read_Byte_BA(CPU.AC(I.Acs)));

         when I_WSTB =>
            RAM.Write_Byte_BA (CPU.AC(I.Acs), Byte_T(CPU.AC(I.Acd) and 16#00ff#));

         when I_XLDB =>
            Addr := Resolve_15bit_Disp (CPU, false, I.Mode, I.Disp_15, I.Disp_Offset); -- TODO 'Long' resolve???
            CPU.AC(I.Ac) := Dword_T(RAM.Read_Byte(Addr, I.Low_Byte));

         when I_XLEF =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            Loggers.Debug_Print (Debug_Log, "... Addr resolved to: " &  Dword_To_String(Dword_T(Addr), Octal, 11));
            Loggers.Debug_Print (Debug_Log, "... from Disp_15: " & Int_To_String( Integer(I.Disp_15), Octal, 11) &
                                            " Offset: " &  Int_To_String( Integer(I.Disp_Offset), Octal, 11));
            CPU.AC(I.Ac) := Dword_T(Addr);

         when I_XLEFB =>
            Addr := Resolve_15bit_Disp (CPU, false, I.Mode, I.Disp_15, I.Disp_Offset);
            Addr := Shift_Left (Addr, 1);
            if I.Low_Byte then
               Addr := Addr + 1;
            end if;
            CPU.AC(I.Ac) := Dword_T(Addr); -- FIXME constrain to Ring? or in Resolve?

         when I_XNADD =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            I16_Mem := Word_To_Integer_16(RAM.Read_Word(Addr));
            I16_Ac  := Word_To_Integer_16(CPU.AC_Wd(I.Ac));
            I16_Ac := I16_Ac + I16_Mem;
            I32 := Integer_32(I16_Ac) + Integer_32(I16_Mem);
            if (I32 > Max_Pos_S16) or (I32 < Min_Neg_S16) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            CPU.AC_I32(I.Ac) := Integer_32(I16_Ac);

         when I_XNADI =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            I32 := Integer_32(Word_To_Integer_16(RAM.Read_Word(Addr))) + Integer_32(I.Imm_U16);
            if (I32 > Max_Pos_S16) or (I32 < Min_Neg_S16) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            RAM.Write_Word (Addr, Integer_16_To_Word(Integer_16(I32)));

         when I_XNLDA =>
            Loggers.Debug_Print (Debug_Log, "... Opcode 2: " & Word_To_String (WD => I.Word_2, Base => Binary, Width => 16, Zero_Pad => True));
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            -- TESTING...
            Addr := Addr or (CPU.PC and 16#7000_0000#);
            -- END TESTING
            I16_Mem := Word_To_Integer_16 (RAM.Read_Word (Addr));
            CPU.AC_I32(I.Ac) := Integer_32(I16_Mem);

         when I_XNMUL =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            I16_Mem := Word_To_Integer_16(RAM.Read_Word(Addr));
            I16_Ac  := Word_To_Integer_16(CPU.AC_Wd(I.Ac));
            I16_Ac := I16_Ac * I16_Mem;
            I32 := Integer_32(I16_Ac) * Integer_32(I16_Mem);
            if (I32 > Max_Pos_S16) or (I32 < Min_Neg_S16) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            CPU.AC_I32(I.Ac) := Integer_32(I16_Ac);

         when I_XNSBI =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            I32 := Integer_32(Word_To_Integer_16(RAM.Read_Word(Addr))) - Integer_32(I.Imm_U16);
            if (I32 > Max_Pos_S16) or (I32 < Min_Neg_S16) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            RAM.Write_Word (Addr, Word_T(I32));

         when I_XNSTA =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            RAM.Write_Word (Addr, CPU.AC_Wd(I.Ac));

         when I_XSTB =>
            Addr := Resolve_15bit_Disp (CPU, false, I.Mode, I.Disp_15, I.Disp_Offset); -- TODO 'Long' resolve???
            RAM.Write_Byte (Word_Addr => Addr, 
                              Low_Byte => I.Low_Byte, 
                              Byt => Byte_T(CPU.AC(I.Ac) and 16#0000_00ff#));

         when I_XNSUB =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            I16_Mem := Word_To_Integer_16(RAM.Read_Word(Addr));
            I16_Ac  := Word_To_Integer_16(CPU.AC_Wd(I.Ac));
            I16_Ac := I16_Ac - I16_Mem;
            I32 := Integer_32(I16_Ac) - Integer_32(I16_Mem);
            if (I32 > Max_Pos_S16) or (I32 < Min_Neg_S16) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            CPU.AC_I32(I.Ac) := Integer_32(I16_Ac);

         when I_XWADD =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            S64_Mem := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr)));
            S64_Ac  := Integer_64(CPU.AC_I32(I.Ac));
            S64 := S64_Ac + S64_Mem;
            if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            CPU.Ac(I.Ac) := Dword_T(Integer_64_To_Unsigned_64(S64)and 16#0000_0000_ffff_ffff#);

         when I_XWADI =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            S64 := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr))) + Integer_64(I.Imm_U16);
            if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            RAM.Write_Dword (Addr, Dword_T(Integer_64_To_Unsigned_64(S64)));

         when I_XWDIV =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            S64_Mem := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr)));
            S64_Ac  := Integer_64(CPU.AC_I32(I.Ac));
            Set_OVR (false);
            if S64_Mem = 0 then
               Set_OVR (true);
            end if;
            S64 := S64_Ac / S64_Mem;
            if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
               Set_OVR (true);
            else
               -- N.B. the masking below is required to prevent possible overflow...
               CPU.Ac(I.Ac) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            end if;

         when I_XWLDA =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            CPU.AC(I.Ac) := RAM.Read_Dword (Addr);

         when I_XWMUL =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            S64_Mem := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr)));
            Set_OVR (false);
            S64_Ac  := Integer_64(CPU.AC_I32(I.Ac));
            S64 := S64_Ac * S64_Mem;
            if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
               Set_OVR (true);
            else
               -- N.B. the masking below is required to prevent possible overflow...
               CPU.Ac(I.Ac) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            end if;
            
         when I_XWSBI =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            S64 := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr))) - Integer_64(I.Imm_U16);
            if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
               CPU.Carry := true;
               Set_OVR (true);
            end if;
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            RAM.Write_Dword (Addr, Dword_T(Integer_64_To_Unsigned_64(S64)));

         when I_XWSTA =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            RAM.Write_Dword (Addr, CPU.AC(I.Ac));

         when I_XWSUB =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            S64_Mem := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr)));
            S64_Ac  := Integer_64(CPU.AC_I32(I.Ac));
            S64 := S64_Ac - S64_Mem;
            if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
               CPU.Carry := true;
               Set_OVR (true);
            else
               CPU.Ac(I.Ac) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            end if;

         when others =>
            Put_Line ("ERROR: EAGLE_MEMREF instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_MEMREF instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);

   end Do_Eagle_Mem_Ref;

 end Processor.Eagle_Mem_Ref_P;