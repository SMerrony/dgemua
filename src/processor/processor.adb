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

with Ada.Text_IO;           use Ada.Text_IO;

with Debug_Logs;            use Debug_Logs;
with Devices;               -- use Devices;
with Devices.Bus;           -- use Devices.Bus;
with Devices.Console;
with Memory;                use Memory;      
with Status_Monitor;

with Processor.Eagle_Decimal_P;
with Processor.Eagle_FPU_P;
with Processor.Eagle_IO_P;
with Processor.Eagle_Mem_Ref_P;
with Processor.Eagle_MP_P;
with Processor.Eagle_Op_P;
with Processor.Eagle_PC_P;
with Processor.Eagle_Stack_P;
with Processor.Eclipse_FPU_P;
with Processor.Eclipse_Mem_Ref_P;
with Processor.Eclipse_Op_P;
with Processor.Eclipse_PC_P;
with Processor.Eclipse_Stack_P;
with Processor.Nova_IO_P; 
with Processor.Nova_Math_P; 
with Processor.Nova_Mem_Ref_P; 
with Processor.Nova_Op_P; 
with Processor.Nova_PC_P; 

package body Processor is

   function Make return CPU_T is
      CPU : constant CPU_T := new CPU_Rec;
   begin
      Reset (CPU);
      return CPU;
   end Make;
   
   procedure Reset (CPU : CPU_T) is
   begin
      CPU.PC := 0;
      for A in AC_ID loop
         CPU.AC(A)   := 0;
         CPU.FPAC(A) := 0.0;
      end loop;
      CPU.PSR := 0;
      CPU.Carry := false;
      CPU.ATU   := false;
      CPU.ION   := false;
      CPU.PF_Flag := false;
      Set_OVR (CPU, false);
      CPU.Instruction_Count := 0;
      Put_Line ("INFO: CPU reset");
   end Reset;

   -- Boot sets up the CPU to boot, it is NOT started
   procedure Boot (CPU : CPU_T; Dev : Dev_Num_T; PC : Phys_Addr_T) is
   begin
      CPU.SR := 16#8000# or Word_T(Dev);
      CPU.AC(0) := Dword_T(Dev); 
      case Dev is
         when Devices.DSKP | Devices.DPF =>
            CPU.AC(0) := Shift_Left (CPU.AC(0), 1);
         when others =>
            null;
      end case;
      CPU.PC := PC;
   end Boot;

   -- Prepare_For_Running should be called prior to a normal run
   procedure Prepare_For_Running (CPU : CPU_T) is
   begin
      CPU.Instruction_Count := 0;
      CPU.XCT_Mode := false;
   end Prepare_For_Running;

   procedure Set_Debug_Logging (CPU : CPU_T; OnOff : Boolean) is
   begin
      CPU.Debug_Logging := OnOff;
   end Set_Debug_Logging;

   procedure Execute (CPU : CPU_T; Instr : Decoded_Instr_T) is
   begin
      case Instr.Instr_Type is
         when EAGLE_DECIMAL  => Processor.Eagle_Decimal_P.Do_Eagle_Decimal(Instr, CPU);
         when EAGLE_MEMREF   => Processor.Eagle_Mem_Ref_P.Do_Eagle_Mem_Ref(Instr, CPU);
         when EAGLE_FPU      => Processor.Eagle_FPU_P.Do_Eagle_FPU(Instr, CPU);         
         when EAGLE_IO       => Processor.Eagle_IO_P.Do_Eagle_IO(Instr, CPU);
         when EAGLE_MP       => Processor.Eagle_MP_P.Do_Eagle_MP (Instr, CPU);
         when EAGLE_OP       => Processor.Eagle_Op_P.Do_Eagle_Op(Instr, CPU);
         when EAGLE_PC       => Processor.Eagle_PC_P.Do_Eagle_PC(Instr, CPU);
         when EAGLE_STACK    => Processor.Eagle_Stack_P.Do_Eagle_Stack(Instr, CPU);
         when ECLIPSE_FPU    => Processor.Eclipse_FPU_P.Do_Eclipse_FPU(Instr, CPU);
         when ECLIPSE_MEMREF => Processor.Eclipse_Mem_Ref_P.Do_Eclipse_Mem_Ref(Instr, CPU); 
         when ECLIPSE_OP     => Processor.Eclipse_Op_P.Do_Eclipse_Op(Instr, CPU); 
         when ECLIPSE_PC     => Processor.Eclipse_PC_P.Do_Eclipse_PC(Instr, CPU);
         when ECLIPSE_STACK  => Processor.Eclipse_Stack_P.Do_Eclipse_Stack(Instr, CPU);
         when NOVA_IO        => Processor.Nova_IO_P.Do_Nova_IO(Instr, CPU);  
         when NOVA_MATH      => Processor.Nova_Math_P.Do_Nova_Math(Instr, CPU);  
         when NOVA_MEMREF    => Processor.Nova_Mem_Ref_P.Do_Nova_Mem_Ref(Instr, CPU);
         when NOVA_OP        => Processor.Nova_Op_P.Do_Nova_Op(Instr, CPU);
         when NOVA_PC        => Processor.Nova_PC_P.Do_Nova_PC(Instr, CPU);

         -- when others =>
         --    Put_Line ("ERROR: Unimplemented instruction type in Execute function " & 
         --                Instr.Instr_Type'Image & " for instruction " &
         --                To_String(Instr.Mnemonic));
         --    raise Execution_Failure with "ERROR: Unimplemented instruction type in Execute function " & 
         --                Instr.Instr_Type'Image & " for instruction " &
         --                To_String(Instr.Mnemonic);      
      end case;
      CPU.Instruction_Count := CPU.Instruction_Count + 1;

   end Execute;

   procedure Single_Step (CPU : CPU_T; Radix : Number_Base_T; Disass : out Unbounded_String) is
      This_Op : Word_T;
      Instr   : Decoded_Instr_T;
      Segment : Integer;

   begin
      This_Op := RAM.Read_Word(CPU.PC);
      Segment := Integer(Shift_Right(CPU.PC, 29));
      Instr := Instruction_Decode (Opcode => This_Op, 
                        PC => CPU.PC, 
                        LEF_Mode => CPU.SBR(Segment).LEF, 
                        IO_On => CPU.SBR(Segment).IO, 
                        ATU_On => CPU.ATU, 
                        Disassemble => true, 
                        Radix => Radix);
      Execute (CPU, Instr);      
      Disass := Instr.Disassembly;
   end Single_Step;

   function Get_Compact_Status (CPU : CPU_T; Radix : Number_Base_T) return String is
   begin
      return   "PC=" & Dword_To_String (Dword_T(CPU.PC), Radix, 11, true) &
               " " & Dword_To_String (CPU.AC(0), Radix, 11, true) &
               " " & Dword_To_String (CPU.AC(1), Radix, 11, true) &
               " " & Dword_To_String (CPU.AC(2), Radix, 11, true) &
               " " & Dword_To_String (CPU.AC(3), Radix, 11, true) &
               " C:" & (if CPU.Carry then "1" else "0") &
               " I:" & (if CPU.ION then "1" else "0");
   end Get_Compact_Status;

   function Get_ATU (CPU : CPU_T) return Boolean is (CPU.ATU);

   function  Get_Instruction_Count (CPU : CPU_T) return Unsigned_64 is (CPU.Instruction_Count);

   function Get_IO (CPU : CPU_T; Seg : Natural) return Boolean is (CPU.SBR(Seg).IO);

   function Get_ION (CPU : CPU_T) return Boolean is (CPU.ION);
 
   function Get_LEF (CPU : CPU_T; Seg : Natural) return Boolean is (CPU.SBR(Seg).LEF);

   procedure Set_PC (CPU : CPU_T; PC : Phys_Addr_T) is
   begin
      CPU.PC := PC;
   end Set_PC;

   function Get_PC (CPU : CPU_T) return Phys_Addr_T is (CPU.PC);

   function Get_Status (CPU : CPU_T) return CPU_Monitor_Rec is
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

   function Get_XCT_Mode (CPU : CPU_T) return Boolean is (CPU.XCT_Mode);

   procedure Set_XCT_Mode (CPU : CPU_T; YN : Boolean) is
   begin
      CPU.XCT_Mode := YN;
   end Set_XCT_Mode;

   function Get_XCT_Opcode (CPU : CPU_T) return Word_T is (CPU.XCT_Opcode);

   procedure Set_Ac (CPU : CPU_T; AC : AC_ID; Datum : Dword_T) is
   begin
      CPU.AC(AC) := Datum;
   end Set_Ac;

   function Get_N (CPU : CPU_T) return Boolean is
      (Test_QW_Bit (CPU.FPSR, FPSR_N));

   procedure Set_N (CPU : CPU_T; N : Boolean) is
   begin
      if N then
         Set_QW_Bit (CPU.FPSR, FPSR_N);
      else
         Clear_QW_Bit (CPU.FPSR, FPSR_N);
      end if;
   end Set_N;

   function Get_Z (CPU : CPU_T) return Boolean is
      (Test_QW_Bit (CPU.FPSR, FPSR_Z));

   procedure Set_Z (CPU : CPU_T; Z : Boolean) is
   begin
      if Z then
         Set_QW_Bit (CPU.FPSR, FPSR_Z);
      else
         Clear_QW_Bit (CPU.FPSR, FPSR_Z);
      end if;
   end Set_Z;

   procedure Set_OVK (CPU : CPU_T; New_OVK : Boolean) is
   begin
      if New_OVK then
         Set_W_Bit(CPU.PSR, PSR_OVK);
      else
         Clear_W_Bit(CPU.PSR, PSR_OVK);
      end if;
   end Set_OVK;

   procedure Set_OVR (CPU : CPU_T; New_OVR : Boolean) is
      begin
      if New_OVR then
         Set_W_Bit(CPU.PSR, PSR_OVR);
      else
         Clear_W_Bit(CPU.PSR, PSR_OVR);
      end if;
   end Set_OVR;  

   procedure Run (CPU : CPU_T;
                  Disassemble : Boolean; 
                  Radix : Number_Base_T; 
                  Breakpoints : BP_Sets.Set;
                  I_Counts : out Instr_Count_T) is
         use Ada.Containers;
         This_Op : Word_T;
         Instr   : Decoded_Instr_T;
         Segment : Integer;
         PC      : Phys_Addr_T;
         XCT     : Boolean;
         Any_Breakpoints : constant Boolean := Breakpoints.Length /= 0;
      begin
         I_Counts := (others => 0);
      Run_Loop:
         loop
            PC := Get_PC (CPU);

            -- BREAKPOINT?
            if Any_Breakpoints then
               if Breakpoints.Contains (PC) then
                  Devices.Console.SCP_Handler.Set_SCP_IO (true);
                  Devices.Console.TTOut.Put_String (" *** BREAKpoint hit ***");
               end if;
            end if;

            -- FETCH
            XCT := Get_XCT_Mode (CPU);
            if XCT then
               This_Op := Get_XCT_Opcode (CPU);
            else
               This_Op := RAM.Read_Word(PC);
            end if;

            -- DECODE
            Segment := Integer(Shift_Right(PC, 29));
            Instr := Instruction_Decode (Opcode => This_Op, 
                                       PC => PC, 
                                       LEF_Mode => Get_LEF(CPU, Segment), 
                                       IO_On => Get_IO(CPU, Segment), 
                                       ATU_On => Get_ATU (CPU), 
                                       Disassemble => Disassemble, 
                                       Radix => Radix);

            -- Instruction Counting
            I_Counts(Instr.Instruction) := I_Counts(Instr.Instruction) + 1;

            if Disassemble then
               Loggers.Debug_Print (Debug_Log, Get_Compact_Status(CPU, Radix) & "  " & To_String(Instr.Disassembly));
            end if;

            -- EXECUTE
            Execute (CPU, Instr);

            -- INTERRUPT?
            if Get_ION (CPU) and Devices.Bus.States.Get_IRQ then
               raise Not_Yet_Implemented with "INTERRUPTs not yet handled";
            end if;

            -- XCT
            if XCT then
               Set_XCT_Mode(CPU, false);
            end if;

            -- Console Interrupt?
            if Devices.Console.SCP_IO then 
               Devices.Console.TTOut.Put_String (" *** Console ESCape ***");
               exit Run_Loop;
            end if;

         end loop Run_Loop;

   end Run;

   procedure VRun (CPU : CPU_T;
                   Disassemble : Boolean; 
                   Radix : Number_Base_T;
                   I_Counts : in out Instr_Count_T; 
                   Syscall_Trap : out Boolean) is
      This_Op : Word_T;
      Instr   : Decoded_Instr_T;
      Segment : Integer;
      PC      : Phys_Addr_T := Get_PC (CPU);
      XCT     : Boolean;
   begin
      loop
         -- FETCH
         XCT := Get_XCT_Mode (CPU);
         if XCT then
            This_Op := Get_XCT_Opcode (CPU);
         else
            This_Op := RAM.Read_Word(PC);
         end if;

         -- DECODE
         Segment := Integer(Shift_Right(PC, 29));
         Instr := Instruction_Decode (Opcode => This_Op, 
                                    PC => PC, 
                                    LEF_Mode => true, 
                                    IO_On => Get_IO(CPU, Segment), 
                                    ATU_On => Get_ATU (CPU), 
                                    Disassemble => Disassemble, 
                                    Radix => Radix);

         -- Instruction Counting
         I_Counts(Instr.Instruction) := I_Counts(Instr.Instruction) + 1;

         if Disassemble then
            Loggers.Debug_Print (Debug_Log, Get_Compact_Status(CPU, Radix) & "  " & To_String(Instr.Disassembly));
            -- Ada.Text_IO.Put_Line(Get_Compact_Status(CPU, Radix) & "  " & To_String(Instr.Disassembly));
         end if;

         -- EXECUTE
         Execute (CPU, Instr);

         -- XCT
         if XCT then
            Set_XCT_Mode(CPU, false);
         end if;

         -- System Call?
         PC := Get_PC (CPU);

         if PC = 16#3000_0000# then -- 06000000000
            Syscall_Trap := true;
            exit;
         else
            Syscall_Trap := false;
         end if;

         exit when PC = 8#16000000000#;

      end loop;
   end VRun;

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

   task body Status_Sender is 
      SS_CPU   : CPU_T;
      Stats : CPU_Monitor_Rec;
   begin
      accept Start (CPU : CPU_T) do
         SS_CPU := CPU;
         Put_line ("INFO: CPU Status Sender started");
      end Start;
      loop
         Stats := Get_Status (SS_CPU);
         -- Stats := SS_CPU.Get_Status;
         Status_Monitor.Monitor.CPU_Update (Stats);
         delay Status_Period_S;
      end loop;
   end Status_Sender;

end Processor;
