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

with Debug_Logs;      use Debug_Logs;
with Memory_Channels; use Memory_Channels;
with Resolver;        use Resolver;

package body Processor.Eagle_FPU_P is 

   procedure Do_Eagle_FPU (I : in Decoded_Instr_T; CPU : in out CPU_T) is
      Unconverted  : Long_Float;
      Scale_Factor : Integer_8;
      Dec_Type     : Natural;
      SSize         : Natural;
   begin
      case I.Instruction is

         when I_WFLAD =>
            CPU.FPAC(I.Acd) := Long_Float(Dword_To_Integer_32(CPU.AC(I.Acs)));
            Set_Z (CPU, (CPU.AC(I.Acs) = 0));
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));

         when I_WSTI =>
            CPU.AC(2) := CPU.AC(3);
            Unconverted := CPU.FPAC(I.Ac);
            Decode_Dec_Data_Type (CPU.AC(1),Scale_Factor, Dec_Type, SSize);
            if Scale_Factor /= 0 then
               raise Not_Yet_Implemented with "Non-Zero Decimal Scale factors";
            end if;
            case Dec_Type is
               when Unpacked_Dec_LS =>
                  declare
                     Converted : String(1 .. SSize);
                     Int_Val   : Integer := Integer(Unconverted);
                     Str_Val   : String  := Int_Val'Image;
                     Src_Ix    : Integer := Str_Val'Last;
                     Dest_Ix   : Integer := SSize;
                  begin
                     Converted(1) := (if Int_Val < 0 then '-' else '+');
                     for D in 2 .. SSize loop
                        Converted(D) := '0';
                     end loop;
                     loop
                        Converted(Dest_Ix) := Str_Val(Src_Ix);
                        Dest_Ix := Dest_Ix - 1;
                        Src_Ix := Src_Ix - 1;
                        exit when ((Int_Val < 0) and (Src_Ix = 1)) or ((Int_Val >= 0) and (Src_Ix = 0));
                     end loop;
                     for C in 1 .. SSize loop
                        RAM.Write_Byte_BA(CPU.AC(3), Char_To_Byte(Converted(C)));
                        CPU.AC(3) := CPU.AC(3) + 1;
                     end loop;
                     
                  end;
               when others =>
                  raise Not_Yet_Implemented with "Decimal data type: " & Dec_Type'Image;
            end case;


         when others =>
            Put_Line ("ERROR: EAGLE_FPU instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_FPU instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
   end Do_Eagle_FPU;

 end Processor.Eagle_FPU_P;