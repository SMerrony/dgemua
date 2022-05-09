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

package body Processor.Eclipse_PC_P is 

   procedure Do_Eclipse_PC (I : Decoded_Instr_T; CPU : CPU_T) is
      Addr : Phys_Addr_T;
      Ring : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
      Word : Word_T;
      Bit_Num : Natural;
   begin
      case I.Instruction is

         when I_CLM => 
            declare
               Acs, L, H : Integer_16;
               Incr : Phys_Addr_T;
            begin
               Acs := Word_To_Integer_16(DG_Types.Lower_Word(CPU.AC(I.Acs)));
               if I.Acs = I.Acd then
                  L := Word_To_Integer_16(RAM.Read_Word(CPU.PC + 1));
                  H := Word_To_Integer_16(RAM.Read_Word(CPU.PC + 2));
                  if (Acs < L) or (Acs > H) then
                     Incr := 3;
                  else
                     Incr := 4;
                  end if;
               else
                  L := Word_To_Integer_16(RAM.Read_Word (Phys_Addr_T (CPU.AC(I.Acd)) or Ring));
                  H := Word_To_Integer_16(RAM.Read_Word (Phys_Addr_T (CPU.AC(I.Acd) + 1) or Ring));
                  if (Acs < L) or (Acs > H) then
                     Incr := 1;
                  else
                     Incr := 2;
                  end if;
               end if;
               if CPU.Debug_Logging then
                  Loggers.Debug_Print (Debug_Log, "CLM Compared " & Acs'Image &
                  " with limits " & L'Image & " and " & H'Image &
                  ", moving PC by " & Incr'Image);
               end if;
               CPU.PC := ((CPU.PC + Incr) and 16#7fff#) or Ring;
            end;

         when I_DSPA =>
            declare
               Table_Start, Offset, Table_Entry,
               Low_Limit, High_Limit : Phys_Addr_T;
            begin
               Table_Start := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               Offset     := Phys_Addr_T(CPU.AC(I.Ac));
               Low_Limit  := Phys_Addr_T(RAM.Read_Word(Table_Start - 2));
               High_Limit := Phys_Addr_T(RAM.Read_Word(Table_Start - 1));
               if CPU.Debug_Logging then
                  Loggers.Debug_Print (Debug_Log, 
                  "DSPA called with table at " & Table_Start'Image &
                  ", offset of " & Offset'Image &
                  ", Low: " & Low_Limit'Image & ", High: " & High_Limit'Image);
               end if;
               if (Offset < Low_Limit) or (Offset > High_Limit) then
                  raise Out_Of_Bounds with "in DSPA";
               end if;
               Table_Entry := Table_Start - Low_Limit + Offset;
               Addr := Phys_Addr_T(RAM.Read_Word(Table_Entry));
               if Addr = 16#ffff_ffff# then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := (Addr and 16#0000_ffff#) or Ring;
               end if;
            end;

         when I_EJMP =>
            Addr := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
            CPU.PC := Addr;

         when I_EJSR =>
            Addr := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
            CPU.AC(3) := Dword_T(CPU.PC) + 2;
            CPU.PC := Addr;

         when I_FNS =>
            CPU.PC := CPU.PC + 1;

         when I_SGT =>
            declare
               Acd : constant Integer_16 := Integer_16(CPU.AC_Wd(I.Acd));
               Acs : constant Integer_16 := Integer_16(CPU.AC_Wd(I.Acs));
            begin
               if Acs > Acd then 
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
               end if;
            end;

         when I_SNB =>
            Resolve_Eclipse_Bit_Addr (CPU, I.Acd , I.Acs, Addr, Bit_Num);
            Addr := Addr or Ring;
            Word := RAM.Read_Word (Addr);
            if Test_W_Bit (Word, Bit_Num) then
               CPU.PC := CPU.PC + 2;
            else
               CPU.PC := CPU.PC + 1;
            end if;


         when others =>
            Put_Line ("ERROR: ECLIPSE_PC instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: ECLIPSE_PC instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;

   end Do_Eclipse_PC;

 end Processor.Eclipse_PC_P;