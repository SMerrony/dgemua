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
with Resolver;        use Resolver;

package body Processor.Eagle_FPU_P is 

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

   function Floor(X : in Long_Float) return Integer_32 is
      Answer : Integer_32 := Integer_32(X);
   begin
      if Long_Float(Answer) > X then
         Answer := Answer - 1;
      end if;
      return Answer;
   end Floor;


   procedure Do_Eagle_FPU (I : in Decoded_Instr_T; CPU : in out CPU_T) is
      Scale_Factor : Integer_8;
      Dec_Type     : Natural;
      SSize        : Natural;
      Addr         : Phys_Addr_T;
      DW           : Dword_T;
      QW           : Qword_T;
      DG_Dbl       : Double_Overlay;
   begin
      Debug_FPACs (CPU);
      case I.Instruction is

        when I_LFAMD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword (Addr);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) + DG_Double_To_Long_Float(DG_Dbl);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_LFLDD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword (Addr);
            CPU.FPAC(I.Ac) := DG_Double_To_Long_Float(DG_Dbl);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_LFLDS =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            CPU.FPAC(I.Ac) := DG_Single_To_Long_Float(RAM.Read_Dword(Addr));
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_LFDMD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword (Addr);
            -- TODO handle zero divisor
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) / DG_Double_To_Long_Float(DG_Dbl);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_LFMMD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword (Addr);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) * DG_Double_To_Long_Float(DG_Dbl);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));   

         when I_LFMMS =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) * DG_Single_To_Long_Float(RAM.Read_Dword(Addr));
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

        when I_LFSMD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword (Addr);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) - DG_Double_To_Long_Float(DG_Dbl);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_LFSTD =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            QW := Long_Float_To_DG_Double (CPU.FPAC(I.Ac));
            RAM.Write_Qword(Addr, QW);

          when I_LFSTS =>
            Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
            DW := Long_Float_To_DG_Single (CPU.FPAC(I.Ac));
            RAM.Write_Dword(Addr, DW);    

         when I_WFFAD =>
            -- Acs and Acd are the 'other way around' with this instruction
            CPU.AC(I.Acs) := Dword_T(Floor(CPU.FPAC(I.Acd)));

         when I_WFLAD =>
            CPU.FPAC(I.Acd) := Long_Float(Dword_To_Integer_32(CPU.AC(I.Acs)));
            Set_Z (CPU, (CPU.AC(I.Acs) = 0));
            Set_N (CPU, (CPU.FPAC(I.Acd) < 0.0));

         when I_WLDI =>
            declare
               SF       : Integer_8;
               Dec_Type : Natural;
               Size     : Natural;
               Dec_US   : Unbounded_String;
               CI       : Integer;
            begin
               Decode_Dec_Data_Type(CPU.AC(1), SF, Dec_Type, Size);
               CPU.AC(2) := CPU.AC(3);
               Dec_US := Read_Decimal(CPU.AC(3), Size);
               case Dec_Type is
                  when Unpacked_Dec_U =>
                     CI := Integer'Value(To_String(Dec_US));
                     CPU.FPAC(I.Ac) := Long_Float(CI);
                  when others =>
                     raise Not_Yet_Implemented with "Packed Data-Types in WLDI";
               end case;
               Set_Z (CPU, (CPU.AC(I.Ac) = 0));
               Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            end;

         when I_WSTI =>
            CPU.AC(2) := CPU.AC(3);
            Decode_Dec_Data_Type (CPU.AC(1),Scale_Factor, Dec_Type, SSize);
            if Scale_Factor /= 0 then
               raise Not_Yet_Implemented with "Non-Zero Decimal Scale factors is WSTI";
            end if;
            case Dec_Type is

               when Packed_Dec =>
                  declare
                     type Nibble is mod 2 ** 4;
                     type Nibble_Arr_T is array (0 .. SSize) of Nibble;
                     Nibble_Arr : Nibble_Arr_T := (others => 0);
                     Int_Val    : Integer := Integer(CPU.FPAC(I.Ac));
                     Byte       : Byte_T;
                  begin
                     -- trailing sign
                     Nibble_Arr(SSize) := (if Int_Val < 0 then 16#D# else 16#C#);
                     -- digits in reverse order
                     for D in reverse 0 .. SSize - 1 loop
                        Nibble_Arr(D) := Nibble(Int_Val mod 10);
                        Int_Val := Int_Val / 10;
                     end loop;
                     for B in 0 ..SSize / 2 loop
                        Byte := Shift_Left(Byte_T(Nibble_Arr(2 * B)), 4) or Byte_T(Nibble_Arr((2 * B) + 1));
                        RAM.Write_Byte_BA(CPU.AC(3), Byte);
                        Loggers.Debug_Print (Debug_Log, "... BCD stored: " & Byte_To_String(Byte, Hex, 2, true));
                        CPU.AC(3) := CPU.AC(3) + 1;
                     end loop;
                  end;
               when Unpacked_Dec_LS =>
                  declare
                     Converted : String(1 .. SSize);
                     Int_Val   : Integer := Integer(CPU.FPAC(I.Ac));
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
                     Loggers.Debug_Print (Debug_Log, "... UDecLS stored: " & Converted);
                  end;
                                 
               when Unpacked_Dec_U =>
                  declare
                     Converted : String(1 .. SSize);
                     Int_Val   : Integer := Integer(CPU.FPAC(I.Ac));
                     Str_Val   : String  := Int_Val'Image;
                     Src_Ix    : Integer := Str_Val'Last;
                     Dest_Ix   : Integer := SSize;
                  begin
                     for D in 1 .. SSize loop
                        Converted(D) := '0';
                     end loop;
                     loop
                        Converted(Dest_Ix) := Str_Val(Src_Ix);
                        Dest_Ix := Dest_Ix - 1;
                        Src_Ix := Src_Ix - 1;
                        exit when (Src_Ix = 1) or (Dest_Ix = 0);
                     end loop;
                     for C in 1 .. SSize loop
                        RAM.Write_Byte_BA(CPU.AC(3), Char_To_Byte(Converted(C)));
                        CPU.AC(3) := CPU.AC(3) + 1;
                     end loop;
                     Loggers.Debug_Print (Debug_Log, "... UDecUS stored: " & Converted);
                  end;
               when others =>
                  raise Not_Yet_Implemented with "Decimal data type: " & Dec_Type'Image;
            end case;

         when I_XFAMD =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword(Addr);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) + DG_Double_To_Long_Float(DG_Dbl);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_XFLDD =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword(Addr);
            CPU.FPAC(I.Ac) := DG_Double_To_Long_Float(DG_Dbl);
            Set_Z(CPU, (CPU.FPAC(I.Ac) = 0.0));
            Set_N(CPU, (CPU.FPAC(I.Ac) < 0.0));

         when I_XFDMS =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DW := RAM.Read_Dword(Addr);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) / DG_Single_To_Long_Float(DW);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));  

         when I_XFLDS =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DW := RAM.Read_Dword(Addr);
            CPU.FPAC(I.Ac) := DG_Single_To_Long_Float(DW);
            Set_Z(CPU, (CPU.FPAC(I.Ac) = 0.0));
            Set_N(CPU, (CPU.FPAC(I.Ac) < 0.0));

         when I_XFMMD =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DG_Dbl.Double_QW := RAM.Read_Qword(Addr);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) * DG_Double_To_Long_Float(DG_Dbl);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));

         when I_XFMMS =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DW := RAM.Read_Dword(Addr);
            CPU.FPAC(I.Ac) := CPU.FPAC(I.Ac) * DG_Single_To_Long_Float(DW);
            Set_N (CPU, (CPU.FPAC(I.Ac) < 0.0));
            Set_Z (CPU, (CPU.FPAC(I.Ac) = 0.0));   

         when I_XFSTD =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DG_Dbl.Double_QW := Long_Float_To_DG_Double(CPU.FPAC(I.Ac));
            RAM.Write_Qword (Addr, DG_Dbl.Double_QW);

         when I_XFSTS =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            DG_Dbl.Double_QW := Long_Float_To_DG_Double(CPU.FPAC(I.Ac));
            RAM.Write_Dword (Addr, Upper_Dword(DG_Dbl.Double_QW));

         when others =>
            Put_Line ("ERROR: EAGLE_FPU instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_FPU instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      Debug_FPACs (CPU);
      CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
   end Do_Eagle_FPU;

 end Processor.Eagle_FPU_P;