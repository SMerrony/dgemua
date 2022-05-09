-- MIT License

-- Copyright ©2021,2022 Stephen Merrony

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
with Memory;      use Memory;
with Resolver;    use Resolver;

package body Processor.Eclipse_Mem_Ref_P is 

   procedure Do_Eclipse_Mem_Ref (I : Decoded_Instr_T; CPU : CPU_T) is
      Addr : Phys_Addr_T;
      Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
      Bit_Num : Natural;
      Word : Word_T;
   begin
      case I.Instruction is

         when I_BLM => -- AC0 - unused, AC1 - no. wds to move, AC2 - src, AC3 - dest
            declare
               Num_Wds :  Word_T := CPU.AC_Wd(1);
               Src, Dest : Phys_Addr_T;
            begin
               if (Num_Wds = 0)  or (Num_Wds > 32768) then
                  Loggers.Debug_Print (Debug_Log, "WARNING: BLM called with AC1 out of bounds, No-Op");
               else
                  Src  := Ring or Phys_Addr_T(CPU.AC_Wd(2));
                  Dest := Ring or Phys_Addr_T(CPU.AC_Wd(3));
                  if CPU.Debug_Logging then
                     Loggers.Debug_Print (Debug_Log, "BLM moving" & Num_Wds'Image & " words from" &
                        Src'Image & " to" & Dest'Image);
                  end if;
                  while Num_Wds /= 0 loop
                     RAM.Write_Word (Dest, RAM.Read_Word(Src));
                     Num_Wds := Num_Wds - 1;
                     Src := Src + 1;
                     Dest := Dest + 1;
                  end loop;
                  CPU.AC(1) := 0;
                  CPU.AC(2) := Dword_T(Src); -- TODO verify this
                  CPU.AC(3) := Dword_T(Dest);
               end if;
            end;

         when I_BTO | I_BTZ =>
            Resolve_Eclipse_Bit_Addr (CPU, I.Acd, I.Acs, Addr, Bit_Num);
            Addr := Addr or Ring;
            Word := RAM.Read_Word (Addr);
            if I.Instruction = I_BTO then
               Set_W_Bit (Word, Bit_Num);
            else
               Clear_W_Bit (Word, Bit_Num);
            end if;
            RAM.Write_Word (Addr, Word);

         when I_CMP =>
            declare
               Byte_1, Byte_2 : Byte_T;
               Res            : Dword_T := 0;
               Str_1_BP, Str_2_BP : Word_T;
               Str_1_Len : Integer_16 := Word_To_Integer_16(CPU.AC_Wd(1));
               Str_2_Len : Integer_16 := Word_To_Integer_16(CPU.AC_Wd(0));
            begin
               if (Str_1_Len = 0) and (Str_2_Len = 0) then
                  CPU.AC(1) := 0;
               else
                  Str_1_BP := CPU.AC_Wd(3);
                  Str_2_BP := CPU.AC_Wd(2);
                  loop
                     if Str_1_Len = 0 then 
                        Byte_1 := 32; 
                     else 
                        Byte_1 := RAM.Read_Byte_Eclipse_BA(Ring,Str_1_BP); 
                     end if;
                     if Str_2_Len = 0 then 
                        Byte_2 := 32; 
                     else 
                        Byte_2 := RAM.Read_Byte_Eclipse_BA(Ring,Str_2_BP); 
                     end if;     
                     if Byte_1 > Byte_2 then
                        Res := 1;
                        exit;
                     end if;
                     if Byte_1 < Byte_2 then
                        Res := 16#ffff_ffff#;
                        exit;
                     end if;
                     if Str_1_Len > 0 then
                        Str_1_BP  := Str_1_BP + 1;
                        Str_1_Len := Str_1_Len - 1;
                     end if;
                     if Str_1_Len < 0 then
                        Str_1_BP  := Str_1_BP - 1;
                        Str_1_Len := Str_1_Len + 1;
                     end if;
                     if Str_2_Len > 0 then
                        Str_2_BP  := Str_2_BP + 1;
                        Str_2_Len := Str_2_Len - 1;
                     end if;
                     if Str_2_Len < 0 then
                        Str_2_BP  := Str_2_BP - 1;
                        Str_2_Len := Str_2_Len + 1;
                     end if;
                     exit when (Str_1_Len = 0) and (Str_2_Len = 0);
                  end loop;
                  CPU.AC(0) := Dword_T(Str_2_Len);
                  CPU.AC(1) := Res;
                  CPU.AC(2) := Dword_T(Str_2_BP);
                  CPU.AC(3) := Dword_T(Str_1_BP);                    
               end if;
            end;
            
         when I_CMV =>
            declare
               Dest_Ascend, Src_Ascend : Boolean;
               Dest_Cnt, Src_Cnt : Integer_16;
            begin
               Dest_Cnt := Word_To_Integer_16(CPU.AC_Wd(0));
               if Dest_Cnt = 0 then
                  Loggers.Debug_Print (Debug_Log, "WARNING: CMV called with AC0 = 0, not moving anything");
                  CPU.Carry := false;
               else
                  Dest_Ascend := Dest_Cnt > 0;
                  Src_Cnt := Word_To_Integer_16(CPU.AC_Wd(1));
                  Src_Ascend := Src_Cnt > 0;
                  CPU.Carry := (Abs Src_Cnt) > (Abs Dest_Cnt);
                  -- move Src_Cnt bytes
                  loop
                     RAM.Write_Byte_Eclipse_BA(Ring, CPU.AC_Wd(2), 
                                                RAM.Read_Byte_Eclipse_BA(Ring, CPU.AC_Wd(3)));
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
                     RAM.Write_Byte_Eclipse_BA(Ring, CPU.AC_Wd(2), 32);
                     if Dest_Ascend then
                        CPU.AC(2) := CPU.AC(2) + 1;
                        Dest_Cnt := Dest_Cnt - 1;
                     else
                        CPU.AC(2) := CPU.AC(2) - 1;
                        Dest_Cnt := Dest_Cnt + 1;
                     end if;
                  end loop;
                  CPU.AC(0) := 0;
                  CPU.AC(1) := Dword_T(Src_Cnt);
               end if;                  
            end;
         when I_ELDA =>
            Addr := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
            CPU.AC(I.Ac) := Dword_T(RAM.Read_Word(Addr));

         when I_ELEF =>
            Addr := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
            CPU.AC(I.Ac) := Dword_T(Addr);

         when I_ESTA =>
            Addr := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
            RAM.Write_Word (Addr, CPU.AC_Wd(I.Ac));

         when I_LDB =>
            CPU.AC(I.Acd) := Dword_T (RAM.Read_Byte_Eclipse_BA (Ring, CPU.AC_Wd(I.Acs)));

         when I_LEF =>
            Addr := Resolve_8bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15);
            Addr := (Addr and 16#0000_7fff#) or Ring;
            CPU.AC(I.Ac) := Dword_T(Addr);

         when I_STB =>
            declare
               Low_Byte : Boolean := Test_DW_Bit(CPU.AC(I.Acs), 31);
            begin
               Addr := Shift_Right (Phys_Addr_T(CPU.AC_Wd(I.Acs)), 1);
               Addr := (Addr and 16#7fff#) or Ring;
               RAM.Write_Byte(Addr, Low_Byte, Byte_T(CPU.AC(I.Acd)));
            end;


         when others =>
            Put_Line ("ERROR: ECLIPSE_MEMREF instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: ECLIPSE_MEMREF instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);


   end Do_Eclipse_Mem_Ref;

 end Processor.Eclipse_Mem_Ref_P;