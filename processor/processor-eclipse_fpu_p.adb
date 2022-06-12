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

package body Processor.Eclipse_FPU_P is 

   procedure Debug_FPACs (CPU : CPU_T) is
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

   procedure Do_Eclipse_FPU (I : Decoded_Instr_T; CPU : CPU_T) is
      QW : Qword_T;
      --LF : Long_Float;
      DG_Dbl : Double_Overlay;
   begin
      Debug_FPACs (CPU);
      case I.Instruction is

         when I_FAB =>
            CPU.FPAC(I.Ac) := abs (CPU.FPAC(I.Ac));
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_FAD =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) + CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));

         when I_FAS =>
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

         when I_FINT =>
            CPU.FPAC(I.Ac) := Long_Float'Truncation(CPU.FPAC(I.Ac));
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_FLAS =>
            declare
               I32 : Integer_32;
            begin
               I32 := Integer_32 (Word_To_Integer_16 (CPU.AC_Wd(I.Acs)));
               CPU.FPAC(I.Acd) := Long_Float(I32);
               Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
               Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));
            end;

         when I_FMD =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) * CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));   

         when I_FMOV =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));      

         when I_FMS =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) * CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));

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
         
         when I_FSGT =>
            if (not Get_Z(CPU)) and (not Get_N(CPU)) then
               CPU.PC := CPU.PC + 1;
            end if;

         when I_FSNE =>
            if Get_Z(CPU) then   
               CPU.PC := CPU.PC + 1;
            end if;

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

         when I_FSS =>
            CPU.FPAC(I.Acd) := CPU.FPAC(I.Acd) - CPU.FPAC(I.Acs);
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Acd) = 0.0));   

         when I_FTD =>
            Clear_QW_Bit (CPU.FPSR, FPSR_Te);

         when I_FTE =>
            Set_QW_Bit (CPU.FPSR, FPSR_Te);

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