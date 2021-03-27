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
with Status_Monitor;

package body CPU is

   protected body Actions is

      procedure Init  is
      begin
         Reset;
         Decoder.Generate_All_Possible_Opcodes;
         Status_Sender.Start;
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

      procedure Set_OVR (New_OVR : in Boolean) is
      begin
        if New_OVR then
            Memory.Set_W_Bit(CPU.PSR, 1);
        else
            Memory.Clear_W_Bit(CPU.PSR, 1);
        end if;
      end Set_OVR;

      function Get_Status return CPU_Monitor_Rec is
         Stats : CPU_Monitor_Rec;
      begin
         Stats.PC := CPU.PC;
         Stats.AC := CPU.AC;
         Stats.Carry := CPU.Carry;
         Stats.ATU   := CPU.ATU;
         Stats.ION   := CPU.ION;
         Stats.Instruction_Count := CPU.Instruction_Count;
         return Stats;
      end Get_Status;

   end Actions;

   task body Status_Sender is 
      Stats : CPU_Monitor_Rec;
   begin
      accept Start do
         Put_line ("INFO: CPU Status Sender started");
      end Start;
      loop
         Stats := Actions.Get_Status;
         select 
            Status_Monitor.Monitor.CPU_Update (Stats);
            Put_Line ("INFO: CPU Status sender sent update");
         else
            delay 5.0;
            Put_Line ("INFO: CPU Status sender waiting 5s");
         end select;
         delay 0.333;
      end loop;
   end Status_Sender;

end CPU;