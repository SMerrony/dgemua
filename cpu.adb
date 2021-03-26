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

with CPU.Decoder;
with Memory;

package body CPU is

   protected body Actions is

      procedure Init  is
      begin
         Reset;
         Decoder.Generate_All_Possible_Opcodes;
         -- TODO Start stats sender
      end Init;

      procedure Reset  is
      begin
         CPU.PC := 0;
         for A in 0 .. 3 loop
            CPU.AC(A)   := 0;
            CPU.FPAC(A) := 0.0;
         end loop;
         CPU.PSR := 0;
         CPU.Carry := false;
         CPU.ATU   := false;
         CPU.ION   := false;
         CPU.PF_Flag := false;
         Set_OVR (false);
         CPU.Instruction_Count := 0;
         Put_Line ("INFO: CPU reset");
      end Reset;

      procedure Set_OVR (New_OVR : Boolean) is
      begin
        if New_OVR then
            Memory.Set_W_Bit(CPU.PSR, 1);
        else
            Memory.Clear_W_Bit(CPU.PSR, 1);
        end if;
      end Set_OVR;

   end Actions;

end CPU;