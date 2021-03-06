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

package body Processor.Eagle_PC_P is 

   procedure Do_Eagle_PC (I : in Decoded_Instr_T; CPU : in out CPU_T) is
         Addr : Phys_Addr_T;
         Word : Word_T;
         DW   : Dword_T;
         Skip : Boolean;
         S32_S, S32_D : Integer_32;
         Bit_Num : Natural;
         
         procedure WS_Push (Datum : in Dword_T) is
         begin
            CPU.WSP := CPU.WSP + 2;
            RAM.Write_Dword (CPU.WSP, Datum);
         end WS_Push;

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

         when I_DSZTS | I_ISZTS =>
            declare
               S32 : Integer_32 := Dword_To_Integer_32(RAM.Read_Dword(CPU.WSP));
            begin
               if I.Instruction = I_DSZTS then
                  S32 := S32 - 1;
               else 
                  S32 := S32 + 1;
               end if;
               RAM.Write_Dword (CPU.WSP, Integer_32_To_Dword(S32));
               Loggers.Debug_Print(Debug_Log, "... @WSP now: " & Dword_To_String (RAM.Read_Dword(CPU.WSP), Octal, 11, true));
               Set_OVR (false);
               if S32 = 0 then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
               end if;  
            end;

         when I_LDSP =>
            declare
               Hi, Lo, Offset : Integer_32;
               Val            : Integer_32 := Dword_To_Integer_32(CPU.AC(I.Ac));
               Table_Addr     : Phys_Addr_T;
               Table_Ix       : Phys_Addr_T;
            begin
               Table_Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               Hi := Dword_To_Integer_32(RAM.Read_Dword(Table_Addr - 2));
               Lo := Dword_To_Integer_32(RAM.Read_Dword(Table_Addr - 4));
               if Val < Lo or Val > Hi then
                  CPU.PC := CPU.PC + 3;
               else
                  Offset := 2 * (Val - Lo);
                  Table_Ix := Table_Addr + Phys_Addr_T(Offset);
                  DW := RAM.Read_Dword (Table_Ix);
                  if Test_DW_Bit (DW, 4) then
                     -- sign-extend from 28-bits
                     DW := DW or 16#ffff_0000#;
                  end if;
                  if DW = 16#ffff_ffff# then
                     CPU.PC := CPU.PC + 3;
                  else
                     S32_D := Integer_32(Table_Ix) + Dword_To_Integer_32(DW);
                     CPU.PC := Phys_Addr_T(S32_D);
                  end if;
               end if;
            end;

         when I_LJMP =>
            CPU.PC := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);

         when I_LJSR =>
            CPU.AC(3) := Dword_T(CPU.PC) + 3;
            CPU.PC := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);

         when I_LNDSZ =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            Word := RAM.Read_Word(Addr) - 1;
            RAM.Write_Word (Addr, Word);
            if Word = 0 then
               CPU.PC := CPU.PC + 4;
            else
               CPU.PC := CPU.PC + 3;
            end if;
                        
         when I_LNISZ =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            Word := RAM.Read_Word(Addr) + 1;
            RAM.Write_Word (Addr, Word);
            if Word = 0 then
               CPU.PC := CPU.PC + 4;
            else
               CPU.PC := CPU.PC + 3;
            end if;  

         when I_LPSHJ =>
            WS_Push (Dword_T(CPU.PC) + 3);
            CPU.PC := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);

         when I_LWDO => 
            declare
               Count : Integer_32 := Integer_32(CPU.AC(I.Ac));
               Mem_Var_Addr : Phys_Addr_T := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               Mem_Var : Integer_32 := Dword_To_Integer_32(RAM.Read_Dword (Mem_Var_Addr)) + 1;
            begin
               RAM.Write_Dword (Mem_Var_Addr, Integer_32_To_Dword(Mem_Var));
               CPU.AC(I.Ac) := Integer_32_To_Dword(Mem_Var);
               if Mem_Var > Count then
                  -- loop ends
                  CPU.PC := CPU.PC + Phys_Addr_T(I.Imm_U16) + 1;
               else
                  CPU.PC := CPU.PC + 4;
               end if;
            end;

         when I_LWDSZ =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DW := RAM.Read_Dword(Addr) - 1;
            RAM.Write_Dword(Addr, DW);
            if DW = 0 then
               CPU.PC := CPU.PC + 4;
            else
               CPU.PC := CPU.PC + 3;
            end if;

         when I_LWISZ =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DW := RAM.Read_Dword(Addr) + 1;
            RAM.Write_Dword(Addr, DW);
            if DW = 0 then
               CPU.PC := CPU.PC + 4;
            else
               CPU.PC := CPU.PC + 3;
            end if;

         when I_NSALA =>
            Word := not DG_Types.Lower_Word (CPU.AC(I.Ac));
            if (Word and I.Word_2) = 0 then
               CPU.PC := CPU.PC + 3;
            else
               CPU.PC := CPU.PC + 2;
            end if;

         when I_NSANA =>
            Word := DG_Types.Lower_Word (CPU.AC(I.Ac));
            if (Word and I.Word_2) = 0 then
               CPU.PC := CPU.PC + 3;
            else
               CPU.PC := CPU.PC + 2;
            end if;

         when I_WBR =>
            CPU.PC := Phys_Addr_T(Integer_32(CPU.PC) + Integer_32(I.Disp_8));

         when I_WSEQ | I_WSNE =>
            if I.Acs = I.Acd then
               DW := 0;
            else
               DW := CPU.AC(I.Acd);
            end if;
            if I.Instruction = I_WSEQ then
               Skip := CPU.Ac(I.Acs) = DW;
            else
               Skip := CPU.Ac(I.Acs) /= DW;
            end if;
            if Skip then CPU.PC := CPU.PC + 2; else CPU.PC := CPU.PC + 1; end if;

         when I_WCLM =>
            declare
               Hi, Lo : Integer_32;
               Val    : Integer_32 := Dword_To_Integer_32(CPU.AC(I.Acs));
            begin
               if I.Acs /= I.Acd then
                  Lo := Dword_To_Integer_32(RAM.Read_Dword (Phys_Addr_T(CPU.AC(I.Acd))));
                  Hi := Dword_To_Integer_32(RAM.Read_Dword (Phys_Addr_T(CPU.AC(I.Acd)+2)));
                  if (Val >= Lo) and (Val <= Hi) then
                     CPU.PC := CPU.PC + 2;
                  else
                     CPU.PC := CPU.PC + 1;
                  end if;
               else
                  Lo := Dword_To_Integer_32(RAM.Read_Dword (Phys_Addr_T(CPU.PC + 1)));
                  Hi := Dword_To_Integer_32(RAM.Read_Dword (Phys_Addr_T(CPU.PC + 3)));
                  if (Val >= Lo) and (Val <= Hi) then
                     CPU.PC := CPU.PC + 6;
                  else
                     CPU.PC := CPU.PC + 5;
                  end if;
               end if;
            end;

         when I_WMESS =>
            DW := RAM.Read_Dword(Phys_Addr_T(CPU.AC(2)));
            if ((DW xor CPU.AC(0)) and CPU.AC(3)) = 0 then
               RAM.Write_Dword(Phys_Addr_T(CPU.AC(2)), CPU.AC(1));
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
            end if;
            CPU.AC(1) := DW;


         when I_WSEQI | I_WSGTI | I_WSLEI | I_WSNEI =>
            if I.Instruction = I_WSEQI then
               Skip := CPU.AC(I.Ac) = Sext_Word_To_Dword (I.Word_2);
            elsif I.Instruction = I_WSGTI then
               Skip := Dword_To_Integer_32(CPU.AC(I.Ac)) >= Dword_To_Integer_32(Sext_Word_To_Dword (I.Word_2));
            elsif I.Instruction = I_WSLEI then
               Skip := Dword_To_Integer_32(CPU.AC(I.Ac)) <= Dword_To_Integer_32(Sext_Word_To_Dword (I.Word_2));
            else
               Skip := CPU.AC(I.Ac) /= Sext_Word_To_Dword (I.Word_2);
            end if;
            if Skip then
               CPU.PC := CPU.PC + 3;
            else
               CPU.PC := CPU.PC + 2;
            end if;
                        
         when I_WSGE | I_WSGT | I_WSLE | I_WSLT =>
            if I.Acs = I.Acd then
               S32_D := 0;
            else
               S32_D := Dword_To_Integer_32(CPU.AC(I.Acd));
            end if;
            S32_S := Dword_To_Integer_32(CPU.AC(I.Acs));
            if I.Instruction = I_WSGE then
               Skip := S32_S >= S32_D;
            elsif I.Instruction = I_WSGT then
               Skip := S32_S > S32_D;
            elsif I.Instruction = I_WSLE then
               Skip := S32_S <= S32_D;
            elsif I.Instruction = I_WSLT then
               Skip := S32_S < S32_D; 
            end if;
            if Skip then
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
            end if;

         when I_WSKBO =>
            if Test_DW_Bit (CPU.AC(0), I.Bit_Number) then
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
         end if;

         when I_WSKBZ =>
            if not Test_DW_Bit (CPU.AC(0), I.Bit_Number) then
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
         end if;

         when I_WSNB =>
            Resolve_Eagle_Bit_Addr (CPU, I.Acd , I.Acs, Addr, Bit_Num);
            if Test_W_Bit (RAM.Read_Word(Addr), Bit_Num) then
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
            end if;

         when I_WSZB =>
            Resolve_Eagle_Bit_Addr (CPU, I.Acd , I.Acs, Addr, Bit_Num);
            if not Test_W_Bit (RAM.Read_Word(Addr), Bit_Num) then
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
            end if;

         when I_WSZBO =>
            Resolve_Eagle_Bit_Addr (CPU, I.Acd , I.Acs, Addr, Bit_Num);
            Word := RAM.Read_Word(Addr);
            if not Test_W_Bit (Word, Bit_Num) then
               Set_W_Bit (Word, Bit_Num);
               RAM.Write_Word (Addr, Word);
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
            end if;

         when I_WUSGE =>
            if I.Acs = I.Acd then
               CPU.PC := CPU.PC + 2;
            else
               if CPU.AC(I.Acs) >= CPU.AC(I.Acd) then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
               end if;
            end if;

         when I_WUSGT =>
            if I.Acs = I.Acd then
               if CPU.AC(I.Acs) > 0 then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
               end if;
            else
               if CPU.AC(I.Acs) > CPU.AC(I.Acd) then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
               end if;
            end if;
         
         when I_WUGTI =>
            if CPU.AC(I.Ac) > I.Imm_DW then
               CPU.PC := CPU.PC + 4;
            else
               CPU.PC := CPU.PC + 3;
            end if;

         when I_XCALL => -- FIXME - XCALL only handling trivial case
            CPU.AC(3) := Dword_T(CPU.PC) + 3;
            if I.Arg_Count >= 0 then
               DW := Shift_Left(Dword_T(CPU.PSR), 16);
               DW := DW or (Dword_T(I.Arg_Count) and 16#0000_ffff#);
            else
               DW := Dword_T(I.Arg_Count) and 16#0000_7fff#;
            end if;
            WS_Push (DW);
            CPU.PC := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);

         when I_XJMP =>
            CPU.PC := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) or (CPU.PC and 16#7000_0000#);

         when I_XJSR =>
            CPU.AC(3) := Dword_T(CPU.PC + 2);
            CPU.PC := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) or (CPU.PC and 16#7000_0000#);

         when I_XNDO =>
            declare
               Loop_Var_Addr    : Phys_Addr_T;
               Loop_Var, Ac_Var : Integer_32;
            begin
               Loop_Var_Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Word := RAM.Read_Word (Loop_Var_Addr) + 1 ;
               Loop_Var := Integer_32(Word_To_Integer_16(Word)); 
               RAM.Write_Word(Loop_Var_Addr, Word);
               Ac_Var := Dword_To_Integer_32(CPU.AC(I.Ac));
               CPU.AC(I.Ac) := Integer_32_To_Dword(Loop_Var);
               if Loop_Var > Ac_Var then 
                  -- loop ends
                  CPU.PC := CPU.PC + 1 + Phys_Addr_T(I.Word_3);
               else
                  CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
               end if;
            end;

         when I_XNDSZ =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            Word := RAM.Read_Word (Addr) - 1;
            RAM.Write_Word (Addr, Word);
            if Word = 0 then 
               CPU.PC := CPU.PC + 3;
            else
               CPU.PC := CPU.PC + 2;
            end if;
                        
         when I_XNISZ =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            Word := RAM.Read_Word (Addr) + 1;
            RAM.Write_Word (Addr, Word);
            if Word = 0 then 
               CPU.PC := CPU.PC + 3;
            else
               CPU.PC := CPU.PC + 2;
            end if;
                             
         when I_XWDO =>
            declare
               Loop_Var_Addr    : Phys_Addr_T;
               Loop_Var, Ac_Var : Integer_32;
            begin
               Loop_Var_Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               DW := RAM.Read_Dword (Loop_Var_Addr) + 1 ;
               Loop_Var := Dword_To_Integer_32(DW); 
               RAM.Write_Dword(Loop_Var_Addr, DW);
               Ac_Var := Dword_To_Integer_32(CPU.AC(I.Ac));
               CPU.AC(I.Ac) := Integer_32_To_Dword(Loop_Var);
               if Loop_Var > Ac_Var then 
                  -- loop ends
                  CPU.PC := CPU.PC + 1 + Phys_Addr_T(I.Word_3);
               else
                  CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
               end if;
            end; 

         when I_XWDSZ =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DW   := RAM.Read_Dword (Addr) - 1;
            RAM.Write_Dword (Addr, DW);
            if DW = 0 then 
               CPU.PC := CPU.PC + 3;
            else
               CPU.PC := CPU.PC + 2;
            end if;
                        
         when I_XWISZ =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DW   := RAM.Read_Dword (Addr) + 1;
            RAM.Write_Dword (Addr, DW);
            if DW = 0 then 
               CPU.PC := CPU.PC + 3;
            else
               CPU.PC := CPU.PC + 2;
            end if;  

         when others =>
            Put_Line ("ERROR: EAGLE_PC instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_PC instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;

   end Do_Eagle_PC;

 end Processor.Eagle_PC_P;