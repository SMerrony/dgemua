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

package body Processor.Eagle_Decimal_P is 

   procedure Do_Eagle_Decimal (I : Decoded_Instr_T; CPU : CPU_T) is
    begin
      case I.Word_2 is
         when 0 => raise Execution_Failure with "ERROR: WDMOV Not Yet Implemented";
         when 1 => -- WDCMP
            -- Short-circuit certain equality
            if (CPU.AC(0) = CPU.AC(1)) and (CPU.AC(2) = CPU.AC(3)) then
               CPU.AC(1) := 0;
            else
               declare
                  SF_1, SF_2     : Integer_8;
                  Type_1, Type_2 : Natural;
                  Size_1, Size_2 : Natural;
                  Dec_US_1, Dec_US_2 : Unbounded_String;
               begin
                  Decode_Dec_Data_Type (CPU.AC(0), SF_1, Type_1, Size_1);
                  Decode_Dec_Data_Type (CPU.AC(1), SF_2, Type_2, Size_2);
                  Dec_US_1 := Read_Decimal(CPU.AC(2), Size_1);
                  Dec_US_2 := Read_Decimal(CPU.AC(3), Size_2);
                  Loggers.Debug_Print (Debug_Log, "... Arg 1 Type: " & Type_1'Image & " String: " & To_String(Dec_US_1) & " Length:" & Size_1'Image);
                  Loggers.Debug_Print (Debug_Log, "... Arg 2 Type: " & Type_2'Image & " String: " & To_String(Dec_US_2) & " Length:" & Size_2'Image);
                  if Type_1 = 3 and Type_2 = 3 then
                     if Integer'Value(To_String(Dec_US_1)) = Integer'Value(To_String(Dec_US_2)) then
                        CPU.AC(1) := 0;
                     elsif Integer'Value(To_String(Dec_US_1)) > Integer'Value(To_String(Dec_US_2)) then
                        CPU.AC(1) := 1;
                     else
                        CPU.AC_I32(1) := -1;
                     end if;
                     CPU.Carry := false;
                  else
                     raise Execution_Failure with "ERROR: WDCMP not fully implemented";
                  end if;
               end;
            end if;
         when 2 => raise Execution_Failure with "ERROR: WDINC Not Yet Implemented";
         when 3 => raise Execution_Failure with "ERROR: WDDEC Not Yet Implemented";
         when others =>
            Put_Line ("ERROR: EAGLE_DECIMAL instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_DECIMAL instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
   end Do_Eagle_Decimal;

 end Processor.Eagle_Decimal_P;