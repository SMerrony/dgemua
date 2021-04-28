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

with CPU_Instructions;      use CPU_Instructions;
with Debug_Logs;            use Debug_Logs;
with Decoder;               use Decoder;
with Devices;               use Devices;
with Devices.Bus;           use Devices.Bus;
with Devices.Console;
with DG_Floats;             use DG_Floats;
with DG_Types;              use DG_Types;
with Memory;                use Memory;
with Resolver;              use Resolver;
with Status_Monitor;

with Processor.Eagle_Decimal_P;
with Processor.Eagle_IO_P;
with Processor.Eagle_Mem_Ref_P;
with Processor.Eagle_Op_P;
with Processor.Eagle_PC_P;
with Processor.Eagle_Stack_P;
with Processor.Eclipse_Mem_Ref_P;
with Processor.Eclipse_Op_P;
with Processor.Nova_IO_P; 
with Processor.Nova_Math_P; 
with Processor.Nova_Mem_Ref_P; 
with Processor.Nova_Op_P; 
with Processor.Nova_PC_P; 

package body Processor is

   procedure Init  is
   begin
      Actions.Reset;
      Decoder.Generate_All_Possible_Opcodes;
      Status_Sender.Start;
   end Init;

   protected body Actions is
   
     procedure Set_OVK (New_OVK : in Boolean) is
      begin
        if New_OVK then
            Set_W_Bit(CPU.PSR, 0);
        else
            Clear_W_Bit(CPU.PSR, 0);
        end if;
      end Set_OVK;

      procedure Set_OVR (New_OVR : in Boolean) is
      begin
        if New_OVR then
            Set_W_Bit(CPU.PSR, 1);
        else
            Clear_W_Bit(CPU.PSR, 1);
        end if;
      end Set_OVR;

      procedure Reset  is
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
         Set_OVR (false);
         CPU.Instruction_Count := 0;
         Put_Line ("INFO: CPU reset");
      end Reset;

      -- Boot sets up the CPU to boot, it is NOT started
      procedure Boot (Dev : Dev_Num_T; PC : Phys_Addr_T) is
      begin
         CPU.SR := 16#8000# or Word_T(Dev);
         CPU.AC(0) := Dword_T(Dev); 
         CPU.PC := PC;
      end Boot;

      -- Prepare_For_Running should be called prior to a normal run
      procedure Prepare_For_Running is
      begin
         CPU.Instruction_Count := 0;
         CPU.XCT_Mode := false;
      end Prepare_For_Running;

      procedure Set_Debug_Logging (OnOff : in Boolean) is
      begin
         CPU.Debug_Logging := OnOff;
      end Set_Debug_Logging;

      procedure Eclipse_FPU (I : in Decoded_Instr_T) is
      begin
         case I.Instruction is

            when I_FCLE =>
               CPU.FPSR := 0; -- TODO verify - PoP contradicts itself

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
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eclipse_FPU;

      procedure Eclipse_PC (I : in Decoded_Instr_T) is
         Addr : Phys_Addr_T;
         Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
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
                     L := Word_To_Integer_16(RAM.Read_Word (Phys_Addr_T (DG_Types.Lower_Word (CPU.AC(I.Acd))) or Ring));
                     H := Word_To_Integer_16(RAM.Read_Word (Phys_Addr_T (DG_Types.Lower_Word (CPU.AC(I.Acd)) + 1) or Ring));
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
                  Offset     := Phys_Addr_T(DG_Types.Lower_Word(CPU.AC(I.Ac)));
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
               CPU.AC(3) := Dword_T(CPU.PC) + 2;
               Addr := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               CPU.PC := Addr;

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
      end Eclipse_PC;

      procedure Eclipse_Stack (I : in Decoded_Instr_T) is
         Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         First, Last, This_Ac : Natural;
         Addr : Phys_Addr_T;
      begin
         case I.Instruction is
            when I_POP =>
               First := Natural(I.Acs);
               Last  := Natural(I.Acd);
               if Last > First then First := First + 4; end if;
               This_Ac := First;
               loop
                  if CPU.Debug_Logging then
                     Loggers.Debug_Print (Debug_Log, "POP popping AC" & This_AC'Image);
                  end if;
                  CPU.AC(AC_Circle(This_AC)) := Dword_T(Narrow_Stack.Pop (Ring));
                  exit when This_Ac = Last;
                  This_Ac := This_Ac -1;
               end loop;

            when I_POPJ =>
               Addr := Phys_Addr_T(Narrow_Stack.Pop(Ring));
               CPU.PC := (Addr and 16#7fff#) or Ring;
               return; -- because PC has been set

            when I_PSH =>
               First := Natural(I.Acs);
               Last  := Natural(I.Acd);
               if Last < First then Last := Last + 4; end if;
               for This_AC in First .. Last loop
                  if CPU.Debug_Logging then
                     Loggers.Debug_Print (Debug_Log, "PSH pushing AC" & This_AC'Image);
                  end if;
                  Narrow_Stack.Push (Ring, DG_Types.Lower_Word(CPU.AC(AC_Circle(This_AC))));
               end loop;

            when I_PSHJ =>
               Narrow_Stack.Push (Ring, DG_Types.Lower_Word(DWord_T(CPU.PC)) + 2);
               Addr := (Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               CPU.PC := Addr;
               return; -- because PC has been set

            when I_RTN => -- complement of I_SAVE below
               declare
                  NFP_Sav, Popped_Wd : Word_T;
               begin
                  NFP_Sav := RAM.Read_Word (NFP_Loc or Ring);
                  RAM.Write_Word (NSP_Loc or Ring, NFP_Sav);
                  Popped_Wd := Narrow_Stack.Pop (Ring);             -- 5
                  CPU.Carry := Test_W_Bit (Popped_Wd, 0);
                  CPU.PC    := Phys_Addr_T(Popped_Wd and 16#7fff#) or Ring;
                  CPU.AC(3) := Dword_T(Narrow_Stack.Pop(Ring));     -- 4
                  CPU.AC(2) := Dword_T(Narrow_Stack.Pop(Ring));     -- 3
                  CPU.AC(1) := Dword_T(Narrow_Stack.Pop(Ring));     -- 2
                  CPU.AC(0) := Dword_T(Narrow_Stack.Pop(Ring));     -- 1
                  RAM.Write_Word (NFP_Loc or Ring, DG_Types.Lower_Word(CPU.AC(3)));
                  return; -- because PC has been set
               end;

            when I_SAVE =>
               declare
                  NFP_Sav, NSP_Sav, Word : Word_T;
               begin
                  NFP_Sav := RAM.Read_Word (NFP_Loc or Ring);
                  NSP_Sav := RAM.Read_Word (NSP_Loc or Ring);
                  Narrow_Stack.Push(Ring, DG_Types.Lower_Word(CPU.AC(0))); -- 1
                  Narrow_Stack.Push(Ring, DG_Types.Lower_Word(CPU.AC(1))); -- 2
                  Narrow_Stack.Push(Ring, DG_Types.Lower_Word(CPU.AC(2))); -- 3 
                  Narrow_Stack.Push(Ring, NFP_Sav);               -- 4
                  Word := DG_Types.Lower_Word(CPU.AC(3));
                  if CPU.Carry then
                     Word := Word or 16#8000#;
                  else
                     Word := Word and 16#7fff#;
                  end if;
                  Narrow_Stack.Push(Ring, Word);                  -- 5
                  RAM.Write_Word (NSP_Loc or Ring, NSP_Sav + 5 + Word_T(I.Imm_U16));
                  RAM.Write_Word (NFP_Loc or Ring, NSP_Sav + 5);
                  CPU.AC(3) := Dword_T(NSP_Sav) + 5;
               end;

            when others =>
               Put_Line ("ERROR: Eclipse_Stack instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
               raise Execution_Failure with "ERROR: Eclipse_Stack instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented"; 
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eclipse_Stack;

      procedure Execute (Instr : in Decoded_Instr_T) is
      begin
         case Instr.Instr_Type is
            when EAGLE_DECIMAL  => Processor.Eagle_Decimal_P.Do_Eagle_Decimal(Instr, CPU);
            when EAGLE_MEMREF   => Processor.Eagle_Mem_Ref_P.Do_Eagle_Mem_Ref(Instr, CPU);
            when EAGLE_IO       => Processor.Eagle_IO_P.Do_Eagle_IO(Instr, CPU);
            when EAGLE_OP       => Processor.Eagle_Op_P.Do_Eagle_Op(Instr, CPU);
            when EAGLE_PC       => Processor.Eagle_PC_P.Do_Eagle_PC(Instr, CPU);
            when EAGLE_STACK    => Processor.Eagle_Stack_P.Do_Eagle_Stack(Instr, CPU);
            when ECLIPSE_FPU    => Eclipse_FPU(Instr);
            when ECLIPSE_MEMREF => Processor.Eclipse_Mem_Ref_P.Do_Eclipse_Mem_Ref(Instr, CPU); 
            when ECLIPSE_OP     => Processor.Eclipse_Op_P.Do_Eclipse_Op(Instr, CPU); 
            when ECLIPSE_PC     => Eclipse_PC(Instr);
            when ECLIPSE_STACK  => Eclipse_Stack(Instr);
            when NOVA_IO        => Processor.Nova_IO_P.Do_Nova_IO(Instr, CPU);  
            when NOVA_MATH      => Processor.Nova_Math_P.Do_Nova_Math(Instr, CPU);  
            when NOVA_MEMREF    => Processor.Nova_Mem_Ref_P.Do_Nova_Mem_Ref(Instr, CPU);
            when NOVA_OP        => Processor.Nova_Op_P.Do_Nova_Op(Instr, CPU);
            when NOVA_PC        => Processor.Nova_PC_P.Do_Nova_PC(Instr, CPU);

            when others =>
               Put_Line ("ERROR: Unimplemented instruction type in Execute function " & 
                         Instr.Instr_Type'Image & " for instuction " &
                         To_String(Instr.Mnemonic));
               raise Execution_Failure with "ERROR: Unimplemented instruction type in Execute function " & 
                         Instr.Instr_Type'Image & " for instuction " &
                         To_String(Instr.Mnemonic);      
         end case;
         CPU.Instruction_Count := CPU.Instruction_Count + 1;

      end Execute;

      procedure Single_Step (Radix : in Number_Base_T; Disass : out Unbounded_String) is
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
         Execute (Instr);      
         Disass := Instr.Disassembly;
      end Single_Step;



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

 
      function Get_Compact_Status (Radix : Number_Base_T) return String is
      begin
         return "AC0=" & Dword_To_String (CPU.AC(0), Radix, 11, true) &
                " AC1=" & Dword_To_String (CPU.AC(1), Radix, 11, true) &
                " AC2=" & Dword_To_String (CPU.AC(2), Radix, 11, true) &
                " AC3=" & Dword_To_String (CPU.AC(3), Radix, 11, true) &
                " C:" & Boolean_To_YN (CPU.Carry) &
                " I:" & Boolean_To_YN (CPU.ION) &
                " PC=" & Dword_To_String (Dword_T(CPU.PC), Radix, 11, true);
      end Get_Compact_Status;

      function  Get_ATU return Boolean is
      begin
         return CPU.ATU;
      end Get_ATU;

      function  Get_Instruction_Count return Unsigned_64 is
      begin
         return CPU.Instruction_Count;
      end Get_Instruction_Count;

      function Get_IO (Seg : in Natural) return Boolean is
      begin
         return CPU.SBR(Seg).IO;
      end Get_IO;

      function Get_LEF (Seg : in Natural) return Boolean is
      begin
         return CPU.SBR(Seg).LEF;
      end Get_LEF;

      function  Get_PC return Phys_Addr_T is
      begin
         return CPU.PC;
      end Get_PC;

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

      function Get_XCT_Mode return Boolean is
      begin
         return CPU.XCT_Mode;
      end Get_XCT_Mode;

      procedure Set_XCT_Mode (YN : in Boolean) is
      begin
         CPU.XCT_Mode := YN;
      end Set_XCT_Mode;

      function Get_XCT_Opcode return Word_T is
      begin
         return CPU.XCT_Opcode;
      end Get_XCT_Opcode;

   end Actions;

   procedure Run (Disassemble : in Boolean; 
                  Radix : in Number_Base_T; 
                  Breakpoints : in BP_Sets.Set;
                  I_Counts : out Instr_Count_T) is
         use Ada.Containers;
         This_Op : Word_T;
         Instr   : Decoded_Instr_T;
         Segment : Integer;
         PC      : Phys_Addr_T;
         XCT     : Boolean;
         Any_Breakpoints : Boolean := Breakpoints.Length /= 0;
      begin
      Run_Loop:
         loop
            PC := Actions.Get_PC;

            -- FETCH
            XCT := Actions.Get_XCT_Mode;
            if XCT then
               This_Op := Actions.Get_XCT_Opcode;
            else
               This_Op := RAM.Read_Word(PC);
            end if;

            -- DECODE
            Segment := Integer(Shift_Right(PC, 29));
            Instr := Instruction_Decode (Opcode => This_Op, 
                                       PC => PC, 
                                       LEF_Mode => Actions.Get_LEF(Segment), 
                                       IO_On => Actions.Get_IO(Segment), 
                                       ATU_On => Actions.Get_ATU, 
                                       Disassemble => Disassemble, 
                                       Radix => Radix);

            -- Instruction Counting
            I_Counts(Instr.Instruction) := I_Counts(Instr.Instruction) + 1;

            if Disassemble then
               Loggers.Debug_Print (Debug_Log, Actions.Get_Compact_Status(Radix) & "  " & To_String(Instr.Disassembly));
            end if;

            -- EXECUTE
            Actions.Execute (Instr);

            -- INTERRUPT?

            -- XCT
            if XCT then
               Actions.Set_XCT_Mode(false);
            end if;

            -- BREAKPOINT?
            if Any_Breakpoints then
               if Breakpoints.Contains (PC) then
                  Devices.Console.SCP_Handler.Set_SCP_IO (true);
                  Devices.Console.TTOut.Put_String (" *** BREAKpoint hit ***");
               end if;
            end if;

            -- Console Interrupt?
            if Devices.Console.SCP_IO then 
               Devices.Console.TTOut.Put_String (" *** Console ESCape ***");
               exit Run_Loop;
            end if;

         end loop Run_Loop;

   end Run;

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

end Processor;
