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

package body Processor.Eclipse_FPU_P is 

   procedure Debug_FPACs (CPU : in CPU_T) is
   begin
      Loggers.Debug_Print (Debug_Log, "FPAC0: " & CPU.FPAC(0)'Image & 
                                     " FPAC1: " & CPU.FPAC(1)'Image & 
                                     " FPAC2: " & CPU.FPAC(2)'Image & 
                                     " FPAC3: " & CPU.FPAC(3)'Image);
      -- Ada.Text_IO.Put_Line("FPAC0: " & CPU.FPAC(0)'Image & 
      --                                " FPAC1: " & CPU.FPAC(1)'Image & 
      --                                " FPAC2: " & CPU.FPAC(2)'Image & 
      --                                " FPAC3: " & CPU.FPAC(3)'Image);
   end Debug_FPACs;   

   procedure Do_Eclipse_FPU (I : in Decoded_Instr_T; CPU : in out CPU_T) is
      QW : Qword_T;
      --LF : Long_Float;
      DG_Dbl : Double_Overlay;
   begin
      Debug_FPACs (CPU);
      case I.Instruction is

         when I_FAD =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) + CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));

         when I_FCLE =>
            CPU.FPSR := 0; -- TODO verify - PoP contradicts itself

         when I_FCMP =>
            if CPU.FPAC(I.Acs) = CPU.FPAC(I.Acd) then
               Set_N (CPU, false);
               Set_Z (CPU, true);
            elsif CPU.FPAC(I.Acs) > CPU.FPAC(I.Acd) then
               Set_N (CPU, true);
               Set_Z (CPU, false);
            else 
               Set_N (CPU, false);
               Set_Z (CPU, false);   
            end if;

         when I_FDD =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) / CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));

         when I_FSGT =>
            if (not Get_Z(CPU)) and (not Get_N(CPU)) then
               CPU.PC := CPU.PC + 1;
            end if;

         when I_FINT =>
            CPU.FPAC(I.Ac) := Long_Float'Truncation(CPU.FPAC(I.Ac));
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_FMD =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) * CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));   

         when I_FSEQ =>
            if Get_Z(CPU) then
               CPU.PC := CPU.PC + 1;
            end if;

         when I_FSGE =>
            if not Get_N(CPU) then
               CPU.PC := CPU.PC + 1;
            end if;

         when I_FSLT =>
            if Get_N(CPU) then
               CPU.PC := CPU.PC + 1;
            end if;

         when I_FTD =>
            Clear_QW_Bit (CPU.FPSR, FPSR_Te);

         when I_FTE =>
            Set_QW_Bit (CPU.FPSR, FPSR_Te);

         when I_FRDS =>
            QW := Long_Float_To_DG_Double(CPU.FPAC(I.Acs));
            if Test_QW_Bit (CPU.FPSR, FPSR_Rnd) then
               -- FIXME - should round not truncate
               DG_Dbl.Double_QW := QW and 16#ffff_ffff_0000_0000#;
               CPU.FPAC(I.Acd) := DG_Double_To_Long_Float(DG_Dbl);
            else
               DG_Dbl.Double_QW := QW and 16#ffff_ffff_0000_0000#;
               CPU.FPAC(I.Acd) := DG_Double_To_Long_Float(DG_Dbl);
            end if;
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));
                  
         when I_FSD =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) - CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));

         when I_FSNE =>
            if Get_Z(CPU) then   
               CPU.PC := CPU.PC + 1;
            end if;


         when others =>
            Put_Line ("ERROR: ECLIPSE_FPU instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: ECLIPSE_FPU instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      Debug_FPACs (CPU);
      CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
   end Do_Eclipse_FPU;

 end Processor.Eclipse_FPU_P;