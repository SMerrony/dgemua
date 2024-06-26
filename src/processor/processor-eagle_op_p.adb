-- Copyright ©2021,2022,2024 Stephen Merrony
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

package body Processor.Eagle_Op_P is 

   procedure Do_Eagle_Op (I : Decoded_Instr_T; CPU : CPU_T) is

      Acd_S16, S16          : Integer_16;
      Acd_S32, Acs_S32, S32 : Integer_32;
      S64                   : Integer_64;
      Shift                 : Integer;
            
      procedure Set_OVR (New_OVR : Boolean) is
      begin
        if New_OVR then
            Set_W_Bit(CPU.PSR, PSR_OVR);
        else
            Clear_W_Bit(CPU.PSR, PSR_OVR);
        end if;
      end Set_OVR;
   begin
      case I.Instruction is

         when I_CRYTC =>
            CPU.Carry := not CPU.Carry;

         when I_CRYTO =>
            CPU.Carry := true;

         when I_CRYTZ =>
            CPU.Carry := false;

         when I_CVWN =>
            if ((CPU.AC(I.Ac) and 16#ffff_0000#) = 0) or ((CPU.AC(I.Ac) and 16#ffff_0000#) = 16#ffff_0000#) then
               Set_OVR (true);
            else
               Set_OVR (false);
            end if;
            CPU.AC(I.Ac) := CPU.AC(I.Ac) and 16#0000_ffff#;
            if Test_DW_Bit (CPU.AC(I.Ac), 16) then
               CPU.AC(I.Ac) := CPU.AC(I.Ac) or 16#ffff_0000#;
            end if;
         
         when I_NADD =>
            S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acd)))) + 
                     Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs))));
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC_I32(I.Acd) := S32;

         when I_NADDI =>
            Acd_S16 := DG_Types.Word_To_Integer_16(Lower_Word(CPU.AC(I.Ac)));
            S16     := Word_To_Integer_16(I.Word_2);
            S32     := Integer_32(Acd_S16) + Integer_32(S16);
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC_I32(I.Ac) := S32; 

         when I_NADI =>
            S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Ac)))) +
                     Integer_32(I.Imm_U16);
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC_I32(I.Ac) := S32; 

         when I_NDIV =>
            Acd_S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acd))));
            Acs_S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs))));
            if Acs_S32 = 0 then
               CPU.Carry := true;
               Set_OVR (true);
            else
               S32 := Acd_S32 / Acs_S32;
               CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
               Set_OVR (CPU.Carry);
               CPU.AC_I32(I.Acd) := S32;   
            end if;

         when I_NLDAI =>
            CPU.AC(I.Ac) := Sext_Word_To_Dword(I.Word_2);

         when I_NMUL =>
            S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acd)))) * 
                     Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs))));
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC_I32(I.Acd) := S32; 

         when I_NNEG =>
            S32 := -Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs))));
            Set_OVR (CPU.AC(I.Acs) = 8#100000#);
            CPU.AC_I32(I.Acd) := S32; 

         when I_NSBI =>
            S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Ac)))) -
                     Integer_32(I.Imm_U16);
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC(I.Ac) := Integer_32_To_Dword(S32); 

         when I_NSUB =>
            S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acd)))) - 
                     Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs))));
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC_I32(I.Acd) := S32;

         when I_SEX =>
            CPU.AC(I.Acd) := CPU.AC(I.Acs) and 16#0000_ffff#;
            if Test_DW_Bit (CPU.AC(I.Acd), 16) then
                CPU.AC(I.Acd) := CPU.AC(I.Acd) or 16#ffff_0000#;
            end if;

         when I_SPSR =>
            CPU.PSR := Lower_Word (CPU.AC(0));

         when I_SSPT =>  -- NO-OP - see p.8-5 of MV/10000 Sys Func Chars 
            Loggers.Debug_Print(Debug_Log, "INFO: SSPT is a No-Op on this VM, continuing...");

         when I_WADC =>
            Acd_S32 := CPU.AC_I32(I.Acd);
            Acs_S32 := Dword_To_Integer_32(not CPU.AC(I.Acs));
            S64 := Integer_64(Acd_S32) + Integer_64(Acs_S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC(I.Acd) := Dword_T(S64);

         when I_WADD =>
            Acd_S32 := CPU.AC_I32(I.Acd);
            Acs_S32 := CPU.AC_I32(I.Acs);
            S64 := Integer_64(Acd_S32) + Integer_64(Acs_S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC(I.Acd) := Dword_T(S64);

         when I_WADDI =>
            S32 := Dword_To_Integer_32(I.Imm_DW);
            S64 := Integer_64(CPU.AC_I32(I.Ac)) + Integer_64(S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR(CPU.Carry);
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC(I.Ac) := Dword_T(S64);

         when I_WADI =>
            S32 := Integer_32(Integer_16(I.Imm_U16));
            S64 := Integer_64(CPU.AC_I32(I.Ac)) + Integer_64(S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR(CPU.Carry);
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC(I.Ac) := Dword_T(S64);

         when I_WANC => -- fnarr, fnarr
            CPU.AC(I.Acd) := CPU.AC(I.Acd) and (not CPU.AC(I.Acs));

         when I_WAND =>
            CPU.AC(I.Acd) := CPU.AC(I.Acd) and CPU.AC(I.Acs);

         when I_WANDI =>
            CPU.AC(I.Ac) := CPU.AC(I.Ac) and I.Imm_DW;

         when I_WASHI => -- "EAGLE"!
            Shift := Integer(Integer_32(Word_To_Integer_16(I.Word_2)));
            if (Shift /= 0) and (CPU.AC(I.Ac) /= 0) then
               S32 := CPU.AC_I32(I.Ac);
               if Shift < 0 then
                  Shift := Shift * (-1);
                  S32 := S32 / (2 ** Shift);
               else
                  S32 := S32 * (2 ** Shift);
               end if;
               CPU.AC_I32(I.Ac) := S32;
            end if;

         when I_WCOM =>
            CPU.AC(I.Acd) := not CPU.AC(I.Acs);

         when I_WDIV =>
            if CPU.AC(I.Acs) /= 0 then
               Acd_S32 := CPU.AC_I32(I.Acd);
               Acs_S32 := CPU.AC_I32(I.Acs);
               S64 := Integer_64(Acd_S32) / Integer_64(Acs_S32);
               if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
                  Set_OVR(true);
               else
                  CPU.AC_I32(I.Acd) := Integer_32(S64); -- Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
                  Set_OVR(false);
               end if;
            else
               Set_OVR (true);
            end if;

         when I_WDIVS =>
            declare
               Divd : Integer_64;
            begin
               if CPU.AC(2) = 0 then
                  Set_OVR (true);
               else
                  S64 := Integer_64(Qword_From_Two_Dwords(CPU.AC(0), CPU.AC(1)));
                  S32 := CPU.AC_I32(2);  
                  Divd := S64 / Integer_64(S32);
                  if (Divd < -2147483648) or (Divd > 2147483647) then
                        Set_OVR (true);
                  else
                     CPU.AC(0) := Dword_T (S64 mod Integer_64 (S32));
                     CPU.AC(1) := Lower_Dword (Integer_64_To_Qword (Divd));
                     Set_OVR(false);
                  end if;
               end if;
            end;

         when I_WHLV =>
            S32 := CPU.AC_I32(I.Ac) / 2; --  This should be correct, Ada rounds down
            CPU.AC_I32(I.Ac) := S32;

         when I_WINC =>
            CPU.Carry := CPU.AC(I.Acs) = 16#ffff_ffff#; -- TODO handle overflow flag
            CPU.AC(I.Acd) := CPU.AC(I.Acs) + 1;

         when I_WIOR =>
            CPU.AC(I.Acd) :=  CPU.AC(I.Acd) or CPU.AC(I.Acs);

         when I_WIORI =>
            CPU.AC(I.Ac) := CPU.AC(I.Ac) or I.Imm_DW;

         when I_WLDAI =>
            CPU.AC(I.Ac) := I.Imm_DW;

         when I_WLSH =>
            Shift := Integer(Byte_To_Integer_8(Byte_T(CPU.AC(I.Acs) and 16#00ff#)));
            if Shift < 0 then -- shift right
               CPU.AC(I.Acd) := Shift_Right (CPU.AC(I.Acd), -Shift);
            elsif Shift > 0 then -- shift left
               CPU.AC(I.Acd) := Shift_Left (CPU.AC(I.Acd), Shift);
            end if;

         when I_WLSHI =>
            Shift := Integer(Byte_To_Integer_8(Byte_T(I.Word_2 and 16#00ff#)));
            if Shift < 0 then -- shift right
               CPU.AC(I.Ac) := Shift_Right (CPU.AC(I.Ac), -Shift);
            elsif Shift > 0 then -- shift left
               CPU.AC(I.Ac) := Shift_Left (CPU.AC(I.Ac), Shift);
            end if;

         when I_WLSI =>
            CPU.AC(I.Ac) := Shift_Left (CPU.AC(I.Ac), Integer(I.Imm_U16));

         when I_WMOV =>
            CPU.AC(I.Acd) := CPU.AC(I.Acs);

         when I_WMOVR =>
            CPU.AC(I.Ac) := Shift_Right(CPU.AC(I.Ac), 1) and 16#7fff_ffff#;

         when I_WMUL =>
            Acd_S32 := CPU.AC_I32(I.Acd);
            Acs_S32 := CPU.AC_I32(I.Acs);
            S64 := Integer_64(Acd_S32) * Integer_64(Acs_S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            --  CPU.AC_I32(I.Acd) := Integer_64_To_Integer_32(S64); -- Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC (I.Acd) := Lower_Dword (Integer_64_To_Qword (S64));

         when I_WMULS =>
            Acd_S32 := CPU.AC_I32(1);
            Acs_S32 := CPU.AC_I32(2);
            S64 := Integer_64(Acd_S32) * Integer_64(Acs_S32) + Integer_64(CPU.AC_I32(0));
            CPU.AC(0) := Upper_Dword(Qword_T(Integer_64_To_Unsigned_64(S64)));
            CPU.AC(1) := Lower_Dword(Qword_T(Integer_64_To_Unsigned_64(S64)));

         when I_WNADI =>
            Acd_S32 := CPU.AC_I32(I.Ac);
            S64 := Integer_64(Acd_S32) + Integer_64(Word_To_Integer_16(I.Word_2));
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            CPU.AC_I32(I.Ac) := Integer_32(S64); -- Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            

         when I_WNEG =>
            CPU.Carry := CPU.AC(I.Acs) = 16#8000_0000#; -- TODO Error in PoP?
            Set_OVR(CPU.Carry);
            S32 := (- CPU.AC_I32(I.Acs));
            CPU.AC_I32(I.Acd) := S32;

         when I_WSBI =>
            S32 := Integer_32(Integer_16(I.Imm_U16));
            S64 := Integer_64(CPU.AC_I32(I.Ac)) - Integer_64(S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR(CPU.Carry);
            CPU.AC_I32(I.Ac) := Integer_32(S64); -- Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);

         when I_WSUB =>
            Acd_S32 := CPU.AC_I32(I.Acd);
            Acs_S32 := CPU.AC_I32(I.Acs);
            S64 := Integer_64(Acd_S32) - Integer_64(Acs_S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            CPU.AC(I.Acd) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);

         when I_WXCH =>
            declare
               DW : Dword_T;
            begin
               DW := CPU.AC(I.Acs);
               CPU.AC(I.Acs) := CPU.AC(I.Acd);
               CPU.AC(I.Acd) := DW;
            end;

         when I_WXOR =>
            CPU.AC(I.Acd) :=  CPU.AC(I.Acd) xor CPU.AC(I.Acs);

         when I_WXORI =>
            CPU.AC(I.Ac) := CPU.AC(I.Ac) xor I.Imm_DW;

         when I_ZEX =>
            CPU.AC(I.Acd) := 0 or Dword_T(DG_Types.Lower_Word(CPU.AC(I.Acs)));

         when others =>
            Put_Line ("ERROR: EAGLE_Op instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_Op instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);


   end Do_Eagle_Op;

 end Processor.Eagle_Op_P;