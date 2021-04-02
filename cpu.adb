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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Decoder;               use Decoder;
with Debug_Logs;            use Debug_Logs;
with Memory;                use Memory;
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

      -- Boot sets up the CPU for booting, it is NOT started
      procedure Boot (Dev : Devices.Dev_Num_T; PC : Phys_Addr_T) is
      begin
         CPU.SR := 16#8000# or Word_T(Dev);
         CPU.AC(0) := Dword_T(Dev);
         CPU.PC := PC;
      end Boot;

      -- Prepare_For_Running should be called prior to a normal run
      procedure Prepare_For_Running is
      begin
         CPU.Instruction_Count := 0;
         CPU.SCP_IO := false;
      end Prepare_For_Running;

      function  Execute (Instr : Decoded_Instr_T) return Boolean is
         OK : Boolean;
      begin
         case Instr.Instr_Type is

            when others =>
               Put_Line ("ERROR: Unimplemented instruction type in Execute function " & 
                         Instr.Instr_Type'Image);
               OK := false;
         end case;
         -- CPU.Instruction_Count := CPU.Instruction_Count + 1;
         return OK;
      end Execute;

      function  Single_Step (Radix : in Number_Base_T) return String is
         This_Op : Word_T;
         Instr   : Decoded_Instr_T;
         Segment : Integer;
      begin
         This_Op := Memory.RAM.Read_Word(CPU.PC);
         Segment := Integer(Shift_Right(CPU.PC, 29));
         Instr := Instruction_Decode (Opcode => This_Op, 
                           PC => CPU.PC, 
                           LEF_Mode => CPU.SBR(Segment).LEF, 
                           IO_On => CPU.SBR(Segment).IO, 
                           ATU_On => CPU.ATU, 
                           Disassemble => true, 
                           Radix => Radix);
         if not Execute (Instr) then
            raise Execution_Failure with " *** ERROR: Could not execute instruction ***";
         end if;         
         return To_String(Instr.Disassembly);
      end Single_Step;

      function  Run (Disassemble : in Boolean; Radix : in Number_Base_T) return Instr_Count_T is
         I_Counts : Instr_Count_T := (others => 0);
         This_Op : Word_T;
         Instr   : Decoded_Instr_T;
         Segment : Integer;

         begin
         Run_Loop:
            loop
               -- FETCH
               This_Op := Memory.RAM.Read_Word(CPU.PC);

               -- DECODE
               Segment := Integer(Shift_Right(CPU.PC, 29));
               Instr := Instruction_Decode (Opcode => This_Op, 
                                          PC => CPU.PC, 
                                          LEF_Mode => CPU.SBR(Segment).LEF, 
                                          IO_On => CPU.SBR(Segment).IO, 
                                          ATU_On => CPU.ATU, 
                                          Disassemble => Disassemble, 
                                          Radix => Radix);

               -- Instruction Counting
               I_Counts(Instr.Instruction) := I_Counts(Instr.Instruction) + 1;

               if CPU.Debug_Logging then
                  Loggers.Debug_Print (Debug_Log, Get_Compact_Status(Radix) & "  " & To_String(Instr.Disassembly));
               end if;

               -- EXECUTE
               if not Execute (Instr) then
                  exit Run_Loop;
               end if;

               -- INTERRUPT?

               -- BREAKPOINT?

               -- Console Interrupt?
   
            end loop Run_Loop;

         return I_Counts;
      end Run;

      function Disassemble_Range (Low_Addr, High_Addr : Phys_Addr_T; Radix : Number_Base_T) return String is
         Skip_Decode : Integer := 0;
         Tmp_Dis : Unbounded_String;
         Word    : Word_T;
         Byte_1, Byte_2 : Byte_T;
         Instr   : Decoded_Instr_T;
      begin
         if Low_Addr > High_Addr then
            return " *** Invalid address range for disassembly ***";
         end if;
         for Addr in Low_Addr .. High_Addr loop
            Word := RAM.Read_Word (Addr);
            Get_Bytes_From_Word (Word, Byte_2, Byte_1);
            Tmp_Dis := Tmp_Dis & Dasher_NL & Dword_To_String (Dword_T(Addr), Octal, 12, true) & ": " &
                       Byte_To_String (Byte_1, Hex, 2, true) & " " & Byte_To_String (Byte_2, Hex, 2, true) & " " &
                       Dword_To_String(Dword_T(Word), Octal, 6, true) & " '";
            if Byte_1 >= 32 and Byte_1 <= 126 then
               Tmp_Dis := Tmp_Dis & Character'Val (Byte_1);
            else
               Tmp_Dis := Tmp_Dis & " ";
            end if;
            if Byte_2 >= 32 and Byte_2 <= 126 then
               Tmp_Dis := Tmp_Dis & Character'Val (Byte_2);
            else
               Tmp_Dis := Tmp_Dis & " ";
            end if;
            Tmp_Dis := Tmp_Dis & "' ";
            if Skip_Decode = 0 then
               Instr := Instruction_Decode (Word, Addr, false, false, false, true, Radix);
               Tmp_Dis := Tmp_Dis & Instr.Disassembly;
               if Instr.Instr_Len > 1 then
                  Skip_Decode := Instr.Instr_Len - 1;
               end if;
            else
               Skip_Decode := Skip_Decode - 1;
            end if;
         end loop;
         return To_String (Tmp_Dis);
      end Disassemble_Range;

      procedure Set_OVR (New_OVR : in Boolean) is
      begin
        if New_OVR then
            Set_W_Bit(CPU.PSR, 1);
        else
            Clear_W_Bit(CPU.PSR, 1);
        end if;
      end Set_OVR;

      procedure Set_SCP_IO (SCP_IO : in Boolean) is
      begin
         CPU.SCP_IO := SCP_IO;
      end Set_SCP_IO;

      function Get_Compact_Status (Radix : Number_Base_T) return String is
      begin
         return "AC0=" & Dword_To_String (CPU.AC(0), Radix, 12, true) &
                " AC1=" & Dword_To_String (CPU.AC(1), Radix, 12, true) &
                " AC2=" & Dword_To_String (CPU.AC(2), Radix, 12, true) &
                " AC3=" & Dword_To_String (CPU.AC(3), Radix, 12, true) &
                " C:" & Boolean_To_YN (CPU.Carry) &
                " I:" & Boolean_To_YN (CPU.ION) &
                " PC=" & Dword_To_String (Dword_T(CPU.PC), Radix, 12, true);
      end Get_Compact_Status;

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
         Status_Monitor.Monitor.CPU_Update (Stats);
         delay 0.333;
      end loop;
   end Status_Sender;

end CPU;