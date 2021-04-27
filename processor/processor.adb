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

with Processor.Eagle_Mem_Ref_P;
with Processor.Eagle_Op_P;
with Processor.Eagle_PC_P;
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

      -- Wide Stack Helper subprograms...

      procedure WS_Pop (DW : out Dword_T) is
      begin
         DW := RAM.Read_Dword (CPU.WSP);
         CPU.WSP := CPU.WSP - 2;
      end WS_Pop;

      procedure WS_Pop_QW (QW : out Qword_T) is
         RHS, LHS : Dword_T;
      begin
         WS_Pop(RHS);
         WS_Pop(LHS);
         QW := Shift_Left(Qword_T(LHS), 32) or Qword_T(RHS);
      end WS_Pop_QW;


      procedure WS_Push (Datum : in Dword_T) is
      begin
         CPU.WSP := CPU.WSP + 2;
         RAM.Write_Dword (CPU.WSP, Datum);
      end WS_Push;

      procedure WS_Push_QW (Datum : in Qword_T) is
      begin
         CPU.WSP := CPU.WSP + 2;
         RAM.Write_Dword (CPU.WSP, Dword_T( Shift_Right (Datum, 32)));
         CPU.WSP := CPU.WSP + 2;
         RAM.Write_Dword (CPU.WSP, Dword_T(Datum));
      end WS_Push_QW;

      -- WSP_Check_Bounds does a pre-flight check to see if the intended change of WSP would cause a stack fault
      -- Is_Save must be set by WMSP, WSSVR, WSSVS, WSAVR & WSAVS
      procedure WSP_Check_Bounds (Delta_Words : in Integer; Is_Save : in Boolean;
                                  OK : out boolean; Primary_Fault, Secondary_Fault : out Dword_T) is
      begin
         OK := true;
         if Delta_Words > 0 then
            if CPU.WSP + Phys_Addr_T(Delta_Words) > CPU.WSL then
               OK := false;
               Secondary_Fault := WSF_Overflow;
               if Is_Save then
                  Primary_Fault := WSF_Pending;
               else
                  Primary_Fault := WSF_Overflow;
               end if;
            end if;
         else
            if CPU.WSP - Phys_Addr_T(abs Delta_Words) < CPU.WSB then
               OK := false;
               Secondary_Fault := WSF_Underflow;
               if Is_Save then
                  Primary_Fault := WSF_Pending;
               else
                  Primary_Fault := WSF_Underflow;
               end if;
            end if;
         end if;
      end WSP_Check_Bounds;

      procedure WSP_Handle_Fault (Ring : in Phys_Addr_T; I_Len : in Positive; Primary_Fault, Secondary_Fault : in Dword_T) is
         DW : Dword_T;
         WSFH_Addr : Phys_Addr_T;
      begin
         -- from pp.5-23 of PoP
         -- step 1
         if Primary_Fault = WSF_Overflow then
            CPU.WSP := CPU.WSL; -- Seems odd, should this be WSB???
         end if;
         -- step 2
         DW := Dword_T(CPU.PC);
         if Primary_Fault /= WSF_Pending then
            DW := DW + Dword_T(I_Len);
         end if;
         if CPU.Carry then
            DW := DW or 16#8000_0000#;
         end if;
         WS_Push (Dword_From_Two_Words(CPU.PSR, 0));
         WS_Push (CPU.AC(0));
         WS_Push (CPU.AC(1));
         WS_Push (CPU.AC(2));
         WS_Push (Dword_T(CPU.WFP));
         WS_Push (DW);
         -- step 3
         Clear_W_Bit (CPU.PSR, 0); -- OVK
         Clear_W_Bit (CPU.PSR, 1); -- OVR
         Clear_W_Bit (CPU.PSR, 2); -- IRES
         -- step 4
         CPU.WSP := CPU.WSP and 16#7fff_ffff#;
         -- step 5
         CPU.WSL := CPU.WSL or 16#8000_0000#;
         -- step 6
         RAM.Write_Dword (Ring and WFP_Loc, Dword_T(CPU.WFP));
         RAM.Write_Dword (Ring and WSP_Loc, Dword_T(CPU.WSP));
         RAM.Write_Dword (Ring and WSL_Loc, Dword_T(CPU.WSL));
         RAM.Write_Dword (Ring and WSB_Loc, Dword_T(CPU.WSB));
         -- step 7
         CPU.AC(0) := Dword_T(CPU.PC);
         -- step 8
         CPU.AC(1) := Primary_Fault;
         -- step 9
         WSFH_Addr := Phys_Addr_T(RAM.Read_Word(Ring or WSFH_Loc)) or Ring;
         Loggers.Debug_Print(Debug_Log, "Jumping to Wide Stack Fault Handler at " & 
                            Dword_To_String (Dword_T(WSFH_Addr), Octal, 11));
         CPU.PC := WSFH_Addr;
      end WSP_Handle_Fault;

      procedure Eagle_Decimal (I : in Decoded_Instr_T) Is
      begin
         case I.Word_2 is
            when 0 => raise Execution_Failure with "ERROR: WDMOV Not Yet Implemented";
            when 1 => -- WDCMP
               -- Short-circuit certain equality
               if (CPU.AC(0) = CPU.AC(1) and (CPU.AC(2) = CPU.AC(3))) then
                  CPU.AC(1) := 0;
               else
                  raise Execution_Failure with "ERROR: WDCMP not fully implemented";
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
      end Eagle_Decimal;

      procedure Eagle_IO (I : in Decoded_Instr_T) is
         -- Addr : Phys_Addr_T;
         Word : Word_T;
         Dwd : Dword_T;
      begin
         case I.Instruction is

            when I_CIO =>
               Word := DG_Types.Lower_Word (CPU.AC(I.Acs));
               declare
                  IO_Chan : Word_T := Get_W_Bits (Word, 1, 3);
                  Map_Reg_Addr : Integer := Integer(Word and 16#0fff#);
               begin
                  if IO_Chan /= 0 and IO_Chan /= 7 then
                     raise Unsupported_IO_Channel with "Attempt to use CIO on channel " & IO_Chan'Image;
                  end if;
                  if Test_W_Bit (Word, 0) then -- write command
                     BMC_DCH.Write_Reg (Map_Reg_Addr, DG_Types.Lower_Word(CPU.AC(I.Acd)));
                  else  -- read command
                     CPU.AC(I.Acd) := Dword_T(BMC_DCH.Read_Reg(Map_Reg_Addr));
                  end if;
               end;
               
            when I_CIOI =>
               -- TODO handle I/O channel
               declare
                  Map_Reg_Addr : Integer;
               begin
                  if I.Acs = I.Acd then
                     Word := I.Word_2;
                  else
                     Word := I.Word_2 or DG_Types.Lower_Word (CPU.AC(I.Acs));
                  end if;
                  Map_Reg_Addr := Integer(Word and 16#0fff#);
                  if Test_W_Bit (Word, 0) then -- write command
                     BMC_DCH.Write_Reg (Map_Reg_Addr, DG_Types.Lower_Word(CPU.AC(I.Acd)));
                  else  -- read command
                     CPU.AC(I.Acd) := Dword_T(BMC_DCH.Read_Reg(Map_Reg_Addr));
                  end if;
               end;

            when I_ECLID | I_LCPID => -- these appear to be identical...
               Dwd := Shift_Left (Dword_T(Model_No), 16);
               Dwd := Dwd or Shift_Left(Dword_T(Microcode_Rev), 8);
               Dwd := Dwd or (Mem_Size_LCPID and 16#0f#);
               CPU.AC(0) := Dwd;

            when I_NCLID =>
               CPU.AC(0) := Dword_T(Model_No) and 16#ffff#;
               CPU.AC(1) := Dword_T(Microcode_Rev) and 16#ffff#;
               CPU.AC(2) := Dword_T(Mem_Size_NCLID) and 16#ffff#;

            when I_PRTSEL =>
               -- only handle the query mode, setting is a no-op on this 'single-channel' machine
               if DG_Types.Lower_Word (CPU.AC(0)) = 16#ffff# then
                  -- return default I/O channel if -1 passed in
                  CPU.AC(0) := 0;
               end if;

            when I_WLMP =>
               if CPU.AC(0) = 0 then
                  Loggers.Debug_Print (Debug_Log, "WARNING: WLMP called with AC0 = 0, No-Op");
               else
                  while CPU.AC(1) /= 0 loop
                     Dwd := RAM.Read_Dword(Phys_Addr_T(CPU.AC(2)));
                     BMC_DCH.Write_Slot(Integer(CPU.AC(0) and 16#0000_07ff#), Dwd);
                     CPU.AC(2) := CPU.AC(2) + 2;
                     CPU.AC(0) := CPU.AC(0) + 1;
                     CPU.AC(1) := CPU.AC(1) - 1;
                  end loop;
               end if;

            when others =>
               Put_Line ("ERROR: EAGLE_IO instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_IO instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eagle_IO;

      procedure Eagle_Stack (I : in Decoded_Instr_T) is
         Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         OK   : Boolean;
         DW, Primary_Fault, Secondary_Fault : Dword_T;
         QW : Qword_T;
         Req_Space : Integer;
         First, Last, This_Ac : Natural;
         Addr : Phys_Addr_T;
      begin
         case I.Instruction is

            when I_LCALL => -- FIXME - LCALL only handling trivial case
               declare
                  OK : Boolean;
                  Primary_Fault, Secondary_Fault : Dword_T;
                  Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
                  PC_4 : Dword_T := Dword_T(CPU.PC) + 4;
               begin
                  if I.Arg_Count >= 0 then
                     DW := Dword_T(I.Arg_Count);
                  else
                     DW := RAM.Read_Dword (CPU.WSP) and 16#0000_ffff#;
                  end if;
                  DW := DW or Shift_Left(Dword_T(CPU.PSR), 16);
                  WSP_Check_Bounds (Delta_Words => 2, 
                                    Is_Save => false, 
                                    OK => OK, 
                                    Primary_Fault => Primary_Fault, 
                                    Secondary_Fault => Secondary_Fault);
                  if not OK then
                     Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WSAVR/S");
                     WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                     -- return;
                  end if;
                  WS_Push (DW);
                  CPU.PC := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset); 
                  CPU.AC(3) := PC_4;
                  return; -- we have set the PC
               end;

           when I_LDAFP =>
               CPU.AC(I.Ac) := Dword_T(CPU.WFP);
               Set_OVR (false);
            when I_LDASB =>
               CPU.AC(I.Ac) := Dword_T(CPU.WSB);
               Set_OVR (false); 
            when I_LDASL =>
               CPU.AC(I.Ac) := Dword_T(CPU.WSL);
               Set_OVR (false);
            when I_LDASP =>
               CPU.AC(I.Ac) := Dword_T(CPU.WSP);
               Set_OVR (false); 
            when I_LDATS =>
               CPU.AC(I.Ac) := RAM.Read_Dword (CPU.WSP);
               Set_OVR (false);

           when I_LPEF =>
                Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
                WS_Push (Dword_T(Addr));
                Set_OVR (false);

            when I_LPEFB =>
               WSP_Check_Bounds (Delta_Words => 2, 
                                 Is_Save => false, 
                                 OK => OK, 
                                 Primary_Fault => Primary_Fault, 
                                 Secondary_Fault => Secondary_Fault);
               if not OK then
                  Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by LPEFB");
                  WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                  return; -- We have set the PC
               end if;
               -- FIXME should the byte address have sign preserved?
               Addr := Resolve_31bit_Disp (CPU, false, I.Mode, Integer_32(Shift_Right(I.Disp_32,1)), I.Disp_Offset);
               Addr := Shift_Left(Addr, 1);
               if I.Disp_32 mod 2 = 1 then
                  Addr := Addr + 1;
               end if;
               WS_Push (Dword_T(Addr));
               Set_OVR (false);

           when I_STAFP =>
               -- TODO Segment handling here?
               CPU.WFP := Phys_Addr_T(CPU.AC(I.Ac));
               -- according the PoP does not write through to page zero...
               Set_OVR (false);
            
            when I_STASB =>
               CPU.WSB := Phys_Addr_T(CPU.AC(I.Ac));
               RAM.Write_Dword (Ring or WSB_Loc, CPU.AC(I.Ac));
               Set_OVR (false);

            when I_STASL =>
               CPU.WSL := Phys_Addr_T(CPU.AC(I.Ac));
               RAM.Write_Dword (Ring or WSL_Loc, CPU.AC(I.Ac));
               Set_OVR (false);

            when I_STASP =>
               -- TODO Segment handling here?
               CPU.WSP := Phys_Addr_T(CPU.AC(I.Ac));
               -- according the PoP does not write through to page zero...
               Set_OVR (false);

            when I_STATS =>
               RAM.Write_Dword (CPU.WSP, CPU.AC(I.Ac));
               Set_OVR (false);

            when I_WFPOP =>
               WS_Pop_QW (QW);   CPU.FPAC(3) := Long_Float(QW);
               WS_Pop_QW (QW);   CPU.FPAC(2) := Long_Float(QW);
               WS_Pop_QW (QW);   CPU.FPAC(1) := Long_Float(QW);
               WS_Pop_QW (QW);   CPU.FPAC(0) := Long_Float(QW);
               WS_Pop_QW (QW);
               -- CPU.FPSR := 0;
               CPU.FPSR := QW and 16#7ff0_0000_0000_0000#; -- copy bits 1..11
               if (QW and 16#7800_0000_0000_0000#)  /= 0 then
                  Set_QW_Bit (CPU.FPSR, 0);
                  CPU.FPSR := QW and 16#0000_000f_0000_0000#; -- copy bits 28..31
                  CPU.FPSR := QW and 16#0000_0000_7fff_ffff#; -- copy bits 33..63
               end if;

            when I_WFPSH =>
               WS_Push_QW (CPU.FPSR); -- TODO Is this right?
               WS_Push_QW (Qword_T(CPU.FPAC(0))); -- FIXME WRONG!
               WS_Push_QW (Qword_T(CPU.FPAC(1)));
               WS_Push_QW (Qword_T(CPU.FPAC(2)));
               WS_Push_QW (Qword_T(CPU.FPAC(3)));

            when I_WMSP =>
               declare
                  DeltaWds : Integer := Integer( Dword_To_Integer_32(CPU.AC(I.Ac))) * 2;
               begin
                  WSP_Check_Bounds (Delta_Words => DeltaWds, 
                                    Is_Save => false, 
                                    OK => OK, 
                                    Primary_Fault => Primary_Fault, 
                                    Secondary_Fault => Secondary_Fault);
                  if not OK then
                     Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WMSP");
                     WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                     return; -- We have set the PC
                  end if;
                  CPU.WSP := CPU.WSP + Phys_Addr_T(DeltaWds);
                  Set_OVR (false);
               end;


            when I_WPOP =>
               First := Natural(I.Acs);
               Last  := Natural(I.Acd);
               if Last > First then First := First + 4; end if;
               This_Ac := First;
               loop
                  if CPU.Debug_Logging then
                     Loggers.Debug_Print (Debug_Log, "POP popping AC" & This_AC'Image);
                  end if;
                  WS_Pop(CPU.AC(AC_Circle(This_AC)));
                  exit when This_Ac = Last;
                  This_Ac := This_Ac -1;
               end loop;

            when I_WPOPJ =>
               WS_Pop(DW);
               DW := (DW and 16#0fff_ffff#) or Dword_T(Ring);
               CPU.PC := Phys_Addr_T(DW);
               WSP_Check_Bounds (Delta_Words => 0, 
                                 Is_Save => false, 
                                 OK => OK, 
                                 Primary_Fault => Primary_Fault, 
                                 Secondary_Fault => Secondary_Fault);
               if not OK then
                  Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WPOPJ");
                  WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                  return; -- We have set the PC
               end if;
               return; -- we've set PC


            when I_WPSH =>
               First := Natural(I.Acs);
               Last := Natural(I.Acd);
               if Last < First then Last := Last + 4; end if;
               for This_AC in First .. Last loop
                  if CPU.Debug_Logging then
                     Loggers.Debug_Print (Debug_Log, "WPSH pushing AC" & This_AC'Image);
                  end if;   
                  WS_Push (CPU.AC(AC_Circle(This_AC)));
               end loop;
               Set_OVR (false);

            when I_WRTN => -- FIXME: WRTN incomplete, handle PSR and Rings
               CPU.WSP := CPU.WFP;
               WS_Pop (DW);
               CPU.Carry := Test_DW_Bit (DW, 0);
               CPU.PC := Phys_Addr_T(DW and 16#7fff_ffff#);
               WS_Pop (CPU.AC(3));
               CPU.WFP := Phys_Addr_T(CPU.AC(3));
               WS_Pop (CPU.AC(2));
               WS_Pop (CPU.AC(1));
               WS_Pop (CPU.AC(0));
               WS_Pop (DW);
               CPU.PSR := Upper_Word (DW);
               CPU.WSP := CPU.WSP - Phys_Addr_T (Shift_Left ((DW and 16#0000_7fff#), 1));
               return; -- We've set PC

            when I_WSAVR | I_WSAVS =>
               Req_Space := Integer(Word_To_Integer_16(I.Word_2));
               WSP_Check_Bounds (Delta_Words => (Req_Space * 2) + 12, 
                                 Is_Save => true, 
                                 OK => OK, 
                                 Primary_Fault => Primary_Fault, 
                                 Secondary_Fault => Secondary_Fault);
               if not OK then
                  Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WSAVR/S");
                  WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                  return; -- We have set the PC
               end if;
               DW := CPU.AC(3) and 16#7fff_ffff#;
               if CPU.Carry then
                  DW := DW or 16#8000_0000#;
               end if;
               WS_Push (CPU.AC(0));
               WS_Push (CPU.AC(1));
               WS_Push (CPU.AC(2));
               WS_Push (Dword_T(CPU.WFP));
               WS_Push (DW);
               CPU.WFP := CPU.WSP;
               CPU.AC(3) := Dword_T(CPU.WSP);
               if Req_Space > 0 then
                  CPU.WSP := CPU.WSP + Phys_Addr_T(Req_Space * 2);
               end if;
               if I.Instruction = I_WSAVR then
                  Set_OVK (false);
               else
                  Set_OVK (true);
               end if;

            when I_XPEF =>
               Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               WS_Push (Dword_T(Addr));

            when I_XPEFB =>
               Addr := Resolve_15bit_Disp (CPU, false, I.Mode, I.Disp_15, I.Disp_Offset);
               Addr := Shift_Left(Addr, 1);
               if I.Low_Byte then
                  Addr := Addr + 1;
               end if;
               WS_Push (Dword_T(Addr));
               Set_OVR (false);
               WSP_Check_Bounds (Delta_Words => 0, 
                                 Is_Save => false, 
                                 OK => OK, 
                                 Primary_Fault => Primary_Fault, 
                                 Secondary_Fault => Secondary_Fault);
               if not OK then
                  Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by LPEFB");
                  WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                  return; -- We have set the PC
               end if;

            when others =>
               Put_Line ("ERROR: EAGLE_STACK instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_STACK instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eagle_Stack;

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
            when EAGLE_DECIMAL  => Eagle_Decimal(Instr);
            when EAGLE_MEMREF   => Processor.Eagle_Mem_Ref_P.Do_Eagle_Mem_Ref(Instr, CPU);
            when EAGLE_IO       => Eagle_IO(Instr);
            when EAGLE_OP       => Processor.Eagle_Op_P.Do_Eagle_Op(Instr, CPU);
            when EAGLE_PC       => Processor.Eagle_PC_P.Do_Eagle_PC(Instr, CPU);
            when EAGLE_STACK    => Eagle_Stack(Instr);
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
