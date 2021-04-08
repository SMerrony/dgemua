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
with Decoder;               use Decoder;
with Devices;               use Devices;
with Devices.Bus;           use Devices.Bus;
with Debug_Logs;            use Debug_Logs;
with Memory;                use Memory;
with Status_Monitor;

package body CPU is

   procedure Init  is
   begin
      Actions.Reset;
      Decoder.Generate_All_Possible_Opcodes;
      Status_Sender.Start;
   end Init;

   protected body Actions is

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
      end Prepare_For_Running;

      function Resolve_8bit_Disp (Indirect    : in Boolean; 
                                  Mode        : in Mode_T;
                                  Disp15      : in Integer_16) return Phys_Addr_T is
         Eff    : Phys_Addr_T;
         Ring   : Phys_Addr_T := CPU.PC and 16#0700_0000#;
         Disp32 : Integer_32;
         Ind_Addr : Word_T;
         Indirection_Level : Integer := 0;
      begin
         if Mode /= Absolute then
            -- relative mode, sign-extend to 32-bits
            Disp32 := Integer_32(Disp15); -- Disp15 is already sexted by decoder
         end if;
         case Mode is
            when Absolute =>
               Eff := Phys_Addr_T(Disp15) or Ring;
            when PC =>
               Eff := Eff + CPU.PC;
            when AC2 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(2)) + Disp32) or Ring;
            when AC3 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(3)) + Disp32) or Ring;   
         end case;

         if Indirect then
            Eff := Eff or Ring;
            Ind_Addr := Memory.RAM.Read_Word (Eff);
            while (Ind_Addr and 16#8000#) /= 0 loop
               Indirection_Level := Indirection_Level + 1;
               if Indirection_Level > 15 then
                  raise Indirection_Failure with "Too many levels of indirection";
               end if;
               Ind_Addr := Memory.RAM.Read_Word (Phys_Addr_T(Ind_Addr) or Ring);
            end loop;
            Eff := Phys_Addr_T(Ind_Addr) or Ring;
         end if;

         if not CPU.ATU then
            -- constrain to 1st 32MB
            Eff := Eff and 16#01ff_ffff#;
         end if;

         return Eff;
      end Resolve_8bit_Disp;

      function Resolve_15bit_Disp (Indirect    : in Boolean; 
                                   Mode        : in Mode_T;
                                   Disp15      : in Integer_16;
                                   Disp_Offset : in Natural) return Phys_Addr_T is
         Eff    : Phys_Addr_T;
         Ring   : Phys_Addr_T := CPU.PC and 16#0700_0000#;
         Disp32 : Integer_32;
         Ind_Addr : Dword_T;
         Indirection_Level : Integer := 0;
      begin
         if Mode /= Absolute then
            -- relative mode, sign-extend to 32-bits
            Disp32 := Integer_32(Disp15); -- Disp15 is already sexted by decoder
         end if;
         case Mode is
            when Absolute =>
               -- Zero-extend to 28 bits, force to current ring
               Eff := (Phys_Addr_T(Disp15) and 16#0000_7fff#) or Ring;
            when PC =>
               Eff := Phys_Addr_T(Integer_32(CPU.PC) + Disp32 + Integer_32(Disp_Offset));
            when AC2 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(2)) + Disp32) or Ring;
            when AC3 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(3)) + Disp32) or Ring;   
         end case;

         if Indirect then
            Eff := Eff or Ring;
            Ind_Addr := Memory.RAM.Read_Dword (Eff);
            while (Ind_Addr and 16#8000_0000#) /= 0 loop
               Indirection_Level := Indirection_Level + 1;
               if Indirection_Level > 15 then
                  raise Indirection_Failure with "Too many levels of indirection";
               end if;
               Ind_Addr := Memory.RAM.Read_Dword (Phys_Addr_T(Ind_Addr) and 16#7fff_ffff#);
            end loop;
            Eff := Phys_Addr_T(Ind_Addr) or Ring;
         end if;

         if not CPU.ATU then
            -- constrain to 1st 32MB
            Eff := Eff and 16#01ff_ffff#;
         end if;

         return Eff;
      end Resolve_15bit_Disp;

      procedure Eagle_Mem_Ref (I : in Decoded_Instr_T) is
         Addr : Phys_Addr_T;
         Word : Word_T;
      begin
         case I.Instruction is
            when I_XLDB =>
               Addr := Resolve_15bit_Disp (false, I.Mode, I.Disp_15, I.Disp_Offset); -- TODO 'Long' resolve???
               CPU.AC(I.Ac) := Dword_T(Memory.RAM.Read_Byte(Addr, I.Low_Byte));

            when I_XNLDA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Word := Memory.RAM.Read_Word (Addr);
               CPU.AC(I.Ac) := Dword_T(Word);
               if Test_W_Bit (Word, 0) then
                  CPU.AC(I.Ac) := CPU.AC(I.Ac) or 16#ffff_0000#;
               end if;

            when I_XNSTA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Memory.RAM.Write_Word (Addr, Memory.Lower_Word(CPU.AC(I.Ac)));

            when I_XWLDA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               CPU.AC(I.Ac) := Memory.RAM.Read_Dword (Addr);

            when I_XWSTA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Memory.RAM.Write_Dword (Addr, CPU.AC(I.Ac));

            when others =>
               Put_Line ("ERROR: EAGLE_MEMREF instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_MEMREF instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eagle_Mem_Ref;

      procedure Eagle_IO (I : in Decoded_Instr_T) is
         -- Addr : Phys_Addr_T;
         -- Word : Word_T;
      begin
         case I.Instruction is
            when I_PRTSEL =>
               -- only handle the query mode, setting is a no-op on this 'single-channel' machine
               if Lower_Word (CPU.AC(0)) = 16#ffff# then
                  -- return default I/O channel if -1 passed in
                  CPU.AC(0) := 0;
               end if;

            when others =>
               Put_Line ("ERROR: EAGLE_IO instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_IO instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eagle_IO;

      procedure Eagle_Op (I : in Decoded_Instr_T) is
         Acd_S32, Acs_S32 : Integer_32;
         S64 : Integer_64;
      begin
         case I.Instruction is

            when I_CRYTC =>
               CPU.Carry := not CPU.Carry;

            when I_CRYTO =>
               CPU.Carry := true;

            when I_CRYTZ =>
               CPU.Carry := false;

            when I_NLDAI =>
               CPU.AC(I.Ac) := Dword_T(Integer_32(Word_To_Integer_16(I.Word_2)));

            when I_WADD =>
               Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
               Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
               S64 := Integer_64(Acd_S32) + Integer_64(Acs_S32);
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Acd) := Dword_T(S64);

            when I_WAND =>
               CPU.AC(I.Acd) := CPU.AC(I.Acd) and CPU.AC(I.Acs);

            when I_WINC =>
               CPU.Carry := CPU.AC(I.Acs) = 16#ffff_ffff#; -- TODO handle overflow flag
               CPU.AC(I.Acd) := CPU.AC(I.Acs) + 1;

            when I_WLDAI =>
               CPU.AC(I.Ac) := I.Imm_DW;

            when I_WSUB =>
               Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
               Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
               S64 := Integer_64(Acd_S32) - Integer_64(Acs_S32);
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Acd) := Dword_T(S64);

            when others =>
               Put_Line ("ERROR: EAGLE_Op instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_Op instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eagle_Op;

      procedure Eagle_PC (I : in Decoded_Instr_T) is
         DW : Dword_T;
         Skip: Boolean;
      begin
         case I.Instruction is

            when I_WBR =>
               CPU.PC := Phys_Addr_T(Integer_32(CPU.PC) + Integer_32(I.Disp_8));

            when I_WSEQ | I_WSNE =>
               if I.Acs = I.Acd then
                  DW := 0;
               else
                  DW := CPU.AC(I.Acd);
               end if;
               if I.Instruction = I_WSEQ then
                  Skip := CPU.Ac(I.Acs) = DW;
               else
                  Skip := CPU.Ac(I.Acs) /= DW;
               end if;
               if Skip then CPU.PC := CPU.PC + 2; else CPU.PC := CPU.PC + 1; end if;


            when I_XJMP =>
               CPU.PC := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) or (CPU.PC and 16#7000_0000#);

            when I_XJSR =>
               CPU.AC(3) := Dword_T(CPU.PC + 2);
               CPU.PC := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) or (CPU.PC and 16#7000_0000#);

            when others =>
               Put_Line ("ERROR: EAGLE_PC instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_PC instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
      end Eagle_PC;

      procedure Eclipse_Mem_Ref (I : in Decoded_Instr_T) is
         Addr : Phys_Addr_T;
         Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
      begin
         case I.Instruction is
            when I_LEF =>
               Addr := Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15);
               Addr := (Addr and 16#0000_7fff#) or Ring;

            when others =>
               Put_Line ("ERROR: ECLIPSE_MEMREF instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: ECLIPSE_MEMREF instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eclipse_Mem_Ref;

      procedure Eclipse_Op (I : in Decoded_Instr_T) is
         Word : Word_T;
      begin
         case I.Instruction is

            when I_ANDI =>
               Word := Lower_Word (CPU.AC(I.Ac));
               CPU.AC(I.Ac) := Dword_T(Word and Word_T(I.Imm_S16)) and 16#0000_ffff#;

            when others =>
               Put_Line ("ERROR: ECLIPSE_OP instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: ECLIPSE_OP instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eclipse_Op;


      procedure Nova_IO (I : in Decoded_Instr_T) is
         Seg_Num : Integer := Integer(Shift_Right(CPU.PC, 28) and 16#07#);
         Datum : Word_T;
         Busy, Done : Boolean;
      begin
         if CPU.ATU and CPU.SBR(Seg_Num).Lef then
               raise Execution_Failure with "LEF not yet implemented";
         end if;
         case I.Instruction is
            when I_DIA | I_DIB | I_DIC | I_DOA | I_DOB | I_DOC =>

               -- catch CPU I/O instructions
               if I.IO_Dev = Devices.CPU then
                  raise Execution_Failure with "CPU I/O not yet implemented";
               end if;

               if Bus.Actions.Is_Connected(I.IO_Dev) and Bus.Actions.Is_IO_Dev(I.IO_Dev) then
                  if I.IO_Dir = Data_In then
                        Devices.Bus.Actions.Data_In(I.IO_Dev, I.IO_Reg, I.IO_Flag, Datum);
                        CPU.AC(I.Ac) := Dword_T(Datum);
                     else
                        Datum := Lower_Word (CPU.AC(I.Ac));
                        Devices.Bus.Actions.Data_Out(I.IO_Dev, Datum, I.IO_Reg, I.IO_Flag);
                  end if;
               else
                  Loggers.Debug_Print(Debug_Log, "WARNING: I/O Attempted to unattached or non-I/O capable device ");
                  return;
               end if;

            when I_SKP =>
               case I.IO_Dev is
                  when Devices.CPU =>
                     Busy := CPU.ION;
                     Done := CPU.PF_Flag;
                  when Dev_Num_T(8#12#) | Dev_Num_T(8#13#) => -- TODO ignore for now
                     CPU.PC := CPU.PC + 2;
                     return;
                  when others =>
                     Busy := Devices.Bus.States.Get_Busy(I.IO_Dev);
                     Done := Devices.Bus.States.Get_Done(I.IO_Dev);
               end case;
               case I.IO_Test is
                  when BN => if Busy then CPU.PC := CPU.PC + 1; end if;
                  when BZ => if not Busy then CPU.PC := CPU.PC + 1; end if;
                  when DN => if Done then CPU.PC := CPU.PC + 1; end if;
                  when DZ => if not Done then CPU.PC := CPU.PC + 1; end if;
               end case;
                          
            when others =>
               Put_Line ("ERROR: Nova_IO instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: Nova_IO instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + 1;
      end Nova_IO;

      procedure Nova_Op (I : in Decoded_Instr_T) is
         Wide_Shifter           : Dword_T;
         Narrow_Shifter         : Word_T;
         Tmp_Acs, Tmp_Acd       : Word_T;
         Saved_Carry, Tmp_Carry : Boolean;
         PC_Inc                 : Phys_Addr_T;
      begin
         Tmp_Acs := Lower_Word (CPU.AC(I.Acs));
         Tmp_Acd := Lower_Word (CPU.AC(I.Acd));
         Saved_Carry := CPU.Carry;

         case I.Carry is
            when None => null;
            when Z => CPU.Carry := false;
            when O => CPU.Carry := true;
            when C => CPU.Carry := not CPU.Carry;
         end case;

         case I.Instruction is
            when I_ADC =>
               Wide_Shifter := Dword_T(Tmp_Acd) + Dword_T(not Tmp_Acs);
               Narrow_Shifter := Lower_Word (Wide_Shifter);
               if Wide_Shifter > 65535 then
                  CPU.Carry := not CPU.Carry;
               end if;
            when I_ADD => -- unsigned
               Wide_Shifter := Dword_T(Tmp_Acd) + Dword_T(Tmp_Acs);
               Narrow_Shifter := Lower_Word (Wide_Shifter);
               if Wide_Shifter > 65535 then
                  CPU.Carry := not CPU.Carry;
               end if;
            when I_AND =>
               Narrow_Shifter := Tmp_Acd and Tmp_Acs;
            when I_COM =>
               Narrow_Shifter := not Tmp_Acs;
            when I_INC =>
               Narrow_Shifter := Tmp_Acs + 1;
               if Tmp_Acs = 16#ffff# then
                  CPU.Carry := not CPU.Carry;
               end if;
            when I_MOV =>
               Narrow_Shifter := Tmp_Acs;
            when I_NEG =>
               Narrow_Shifter := Integer_16_To_Word(- Word_To_Integer_16(Tmp_Acs));
               -- Narrow_Shifter := Word_T(-Integer_16(Tmp_Acs)); -- TODO Check this
               if Tmp_Acs = 0 then
                  CPU.Carry := not CPU.Carry;
               end if;
            when I_SUB =>
               Narrow_Shifter := Tmp_Acd - Tmp_Acs;
               if Tmp_Acs <= Tmp_Acd then
                  CPU.Carry := not CPU.Carry;
               end if;

            when others =>
               Put_Line ("ERROR: NOVA_OP instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: NOVA_OP instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;

         case I.Sh is
            when None => null;
            when L =>
               Tmp_Carry := CPU.Carry;
               CPU.Carry := Test_W_Bit (Narrow_Shifter, 0);
               Narrow_Shifter := Shift_Left (Narrow_Shifter, 1);
               if Tmp_Carry then
                  Narrow_Shifter := Narrow_Shifter or 16#0001#;
               end if;
            when R =>
               Tmp_Carry := CPU.Carry;
               CPU.Carry := Test_W_Bit (Narrow_Shifter, 15);
               Narrow_Shifter := Shift_Right (Narrow_Shifter, 1);
               if Tmp_Carry then
                  Narrow_Shifter := Narrow_Shifter or 16#8000#;
               end if;
            when S => 
               Narrow_Shifter := Swap_Bytes (Narrow_Shifter);
         end case;

         case I.Skip is
            when None => PC_Inc := 1;
            when SKP => PC_Inc := 2;
            when SZC => if not CPU.Carry then PC_Inc := 2; else PC_Inc := 1; end if;
            when SNC => if CPU.Carry then PC_Inc := 2; else PC_Inc := 1; end if;
            when SZR => if Narrow_Shifter = 0 then PC_Inc := 2; else PC_Inc := 1; end if;
            when SNR => if Narrow_Shifter /= 0 then PC_Inc := 2; else PC_Inc := 1; end if;
            when SEZ => if (not CPU.Carry) or (Narrow_Shifter = 0) then PC_Inc := 2; else PC_Inc := 1; end if;
            when SBN => if CPU.Carry and (Narrow_Shifter /= 0) then PC_Inc := 2; else PC_Inc := 1; end if;
         end case;

         if I.No_Load then
            -- don't load the result from the shifter, restore the Carry flag
            CPU.Carry := Saved_Carry;
         else
            CPU.AC(I.Acd) := Dword_T(Narrow_Shifter) and 16#0000_ffff#;
         end if;

         CPU.PC := CPU.PC + PC_Inc;
      end Nova_Op;

      procedure Nova_Mem_Ref (I : in Decoded_Instr_T) is
         Ring_Mask : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         Addr : Phys_Addr_T;
         Word : Word_T;
      begin
         case I.Instruction is
            when I_LDA =>
               Addr := (Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
               Word := Memory.RAM.Read_Word (Addr);
               CPU.AC(I.Ac) := Dword_T(Word);

            when I_STA =>
               Addr := (Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
               Word := Lower_Word (CPU.AC(I.Ac));
               Memory.RAM.Write_Word (Addr, Word);

            when others =>
               Put_Line ("ERROR: Nova_Mem_Ref instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: Nova_Mem_Ref instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + 1;
      end Nova_Mem_Ref;

      procedure Nova_PC (I : in Decoded_Instr_T) is
         Ring_Mask : Phys_Addr_T := CPU.PC and 16#7000_0000#;
      begin
         case I.Instruction is
            when I_JMP =>
               CPU.PC := Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) or Ring_Mask;

            when others =>
               Put_Line ("ERROR: Nova_PC instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: Nova_PC instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
      end Nova_PC;
      
      procedure Execute (Instr : in Decoded_Instr_T) is
      begin
         case Instr.Instr_Type is

            when EAGLE_MEMREF   => Eagle_Mem_Ref(Instr);
            when EAGLE_IO       => Eagle_IO(Instr);
            when EAGLE_OP       => Eagle_Op(Instr);
            when EAGLE_PC       => Eagle_PC(Instr);
            when ECLIPSE_MEMREF => Eclipse_Mem_Ref(Instr);
            when ECLIPSE_OP     => Eclipse_Op(Instr);
            when NOVA_IO        => Nova_IO(Instr);  
            when NOVA_MEMREF    => Nova_Mem_Ref(Instr);
            when NOVA_OP        => Nova_Op(Instr);
            when NOVA_PC        => Nova_PC(Instr);

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
         This_Op := Memory.RAM.Read_Word(CPU.PC);
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

      procedure Set_OVR (New_OVR : in Boolean) is
      begin
        if New_OVR then
            Set_W_Bit(CPU.PSR, 1);
        else
            Clear_W_Bit(CPU.PSR, 1);
        end if;
      end Set_OVR;

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

   end Actions;

      procedure Run (Disassemble : in Boolean; Radix : in Number_Base_T; I_Counts : out Instr_Count_T) is
            This_Op : Word_T;
            Instr   : Decoded_Instr_T;
            Segment : Integer;
            PC      : Phys_Addr_T;
         begin
         Run_Loop:
            loop
               PC := Actions.Get_PC;

               -- FETCH
               This_Op := Memory.RAM.Read_Word(PC);

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

               -- BREAKPOINT?

               -- Console Interrupt?
   
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

end CPU;