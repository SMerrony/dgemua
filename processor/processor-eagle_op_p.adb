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

with Ada.Text_IO; use Ada.Text_IO;

with Debug_Logs;  use Debug_Logs;
with Resolver;    use Resolver;

package body Processor.Eagle_Op_P is 

   procedure Do_Eagle_Op (I : in Decoded_Instr_T; CPU : in out CPU_T) is
 
      Acd_S32, Acs_S32, S32: Integer_32;
      S64   : Integer_64;
      -- Word  : Word_T;
      Shift : Integer;
            
      procedure Set_OVR (New_OVR : in Boolean) is
      begin
        if New_OVR then
            Set_W_Bit(CPU.PSR, 1);
        else
            Clear_W_Bit(CPU.PSR, 1);
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
            CPU.AC(I.Acd) := Integer_32_To_Dword(S32);

         when I_NADI =>
            S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Ac)))) +
                     Integer_32(I.Imm_U16);
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC(I.Ac) := Integer_32_To_Dword(S32); 

         when I_NLDAI =>
            CPU.AC(I.Ac) := Sext_Word_To_Dword(I.Word_2);

         when I_NMUL =>
            S32 := Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acd)))) * 
                     Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs))));
            CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
            Set_OVR (CPU.Carry);
            CPU.AC(I.Acd) := Integer_32_To_Dword(S32); 

         when I_NNEG =>
            S32 := -Integer_32(Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs))));
            Set_OVR (CPU.AC(I.Acs) = 8#100000#);
            CPU.AC(I.Acd) := Integer_32_To_Dword(S32); 

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
            CPU.AC(I.Acd) := Integer_32_To_Dword(S32);

         when I_SEX =>
            CPU.AC(I.Acd) := CPU.AC(I.Acs) and 16#0000_ffff#;
            if Test_DW_Bit (CPU.AC(I.Acd), 16) then
                CPU.AC(I.Acd) := CPU.AC(I.Acd) or 16#ffff_0000#;
            end if;

         when I_SSPT =>  -- NO-OP - see p.8-5 of MV/10000 Sys Func Chars 
            Loggers.Debug_Print(Debug_Log, "INFO: SSPT is a No-Op on this VM, continuing...");

         when I_WADC =>
            Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
            Acs_S32 := Dword_To_Integer_32(not CPU.AC(I.Acs));
            S64 := Integer_64(Acd_S32) + Integer_64(Acs_S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC(I.Acd) := Dword_T(S64);

         when I_WADD =>
            Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
            Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
            S64 := Integer_64(Acd_S32) + Integer_64(Acs_S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC(I.Acd) := Dword_T(S64);

         when I_WADDI =>
            S32 := Dword_To_Integer_32(I.Imm_DW);
            S64 := Integer_64(Dword_To_Integer_32(CPU.AC(I.Ac))) + Integer_64(S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR(CPU.Carry);
            S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
            CPU.AC(I.Ac) := Dword_T(S64);

         when I_WADI =>
            S32 := Integer_32(Integer_16(I.Imm_U16));
            S64 := Integer_64(Dword_To_Integer_32(CPU.AC(I.Ac))) + Integer_64(S32);
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

         when I_WCOM =>
            CPU.AC(I.Acd) := not CPU.AC(I.Acs);

         when I_WDIV =>
            if CPU.AC(I.Acs) /= 0 then
               Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
               Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
               S64 := Integer_64(Acd_S32) / Integer_64(Acs_S32);
               if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
                  Set_OVR(true);
               else
                  CPU.AC(I.Acd) := Dword_T(S64);
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
                  S32 := Dword_To_Integer_32(CPU.AC(2));  
                  Divd := S64 / Integer_64(S32);
                  if (Divd < -2147483648) or (Divd > 2147483647) then
                        Set_OVR (true);
                  else
                     CPU.AC(0) := Dword_T(S64 mod Integer_64(S32));
                     CPU.AC(1) := Dword_T(Divd);
                     Set_OVR(false);
                  end if;
               end if;
            end;

         when I_WHLV =>
            S32 := Dword_To_Integer_32(CPU.AC(I.Ac)) / 2;
            CPU.AC(I.Ac) := Integer_32_To_Dword(S32);

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
            CPU.AC(I.Ac) := Shift_Right(CPU.AC(I.Ac), 1);

         when I_WMUL =>
            Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
            Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
            S64 := Integer_64(Acd_S32) * Integer_64(Acs_S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            CPU.AC(I.Acd) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);

         when I_WMULS =>
            Acd_S32 := Dword_To_Integer_32(CPU.AC(1));
            Acs_S32 := Dword_To_Integer_32(CPU.AC(2));
            S64 := Integer_64(Acd_S32) * Integer_64(Acs_S32) + Integer_64(Dword_To_Integer_32(CPU.AC(0)));
            CPU.AC(0) := Upper_Dword(Qword_T(Integer_64_To_Unsigned_64(S64)));
            CPU.AC(1) := Lower_Dword(Qword_T(Integer_64_To_Unsigned_64(S64)));

         when I_WNADI =>
            Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Ac));
            S64 := Integer_64(Acd_S32) + Integer_64(Word_To_Integer_16(I.Word_2));
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR (CPU.Carry);
            CPU.AC(I.Ac) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);

         when I_WNEG =>
            CPU.Carry := CPU.AC(I.Acs) = 16#8000_0000#; -- TODO Error in PoP?
            Set_OVR(CPU.Carry);
            CPU.AC(I.Acd) := (not CPU.AC(I.Acs)) + 1;

         when I_WSBI =>
            S32 := Integer_32(Integer_16(I.Imm_U16));
            S64 := Integer_64(Dword_To_Integer_32(CPU.AC(I.Ac))) - Integer_64(S32);
            CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
            Set_OVR(CPU.Carry);
            CPU.AC(I.Ac) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);

         when I_WSUB =>
            Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
            Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
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