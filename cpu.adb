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

      procedure Set_Debug_Logging (OnOff : in Boolean) is
      begin
         CPU.Debug_Logging := OnOff;
      end Set_Debug_Logging;

      function Resolve_8bit_Disp (Indirect    : in Boolean; 
                                  Mode        : in Mode_T;
                                  Disp15      : in Integer_16) return Phys_Addr_T is
         Eff    : Phys_Addr_T;
         Ring   : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         Ind_Addr : Word_T;
         Indirection_Level : Integer := 0;
      begin
         if Mode /= Absolute then
            -- relative mode, sign-extend to 32-bits
            Eff := Integer_32_To_Phys(Integer_32(Disp15)); -- Disp15 is already sexted by decoder
         end if;
         case Mode is
            when Absolute =>
               Eff := Phys_Addr_T(Disp15) or Ring;
            when PC =>
               Eff := Eff + CPU.PC;
            when AC2 =>
               Eff := Eff + Phys_Addr_T(Integer_32(CPU.AC(2)));
            when AC3 =>
               Eff := Eff + Phys_Addr_T(Integer_32(CPU.AC(3)));
         end case;

         if Indirect then
            Eff := Eff or Ring;
            Ind_Addr := RAM.Read_Word (Eff);
            while (Ind_Addr and 16#8000#) /= 0 loop
               Indirection_Level := Indirection_Level + 1;
               if Indirection_Level > 15 then
                  raise Indirection_Failure with "Too many levels of indirection";
               end if;
               Ind_Addr := RAM.Read_Word (Phys_Addr_T(Ind_Addr) or Ring);
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
         Ring   : Phys_Addr_T := CPU.PC and 16#7000_0000#;
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
               Eff := Integer_32_To_Phys(Integer_32(CPU.PC) + Disp32 + Integer_32(Disp_Offset));
            when AC2 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(2)) + Disp32) or Ring;
            when AC3 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(3)) + Disp32) or Ring;   
         end case;

         if Indirect then
            Eff := Eff or Ring;
            Ind_Addr := RAM.Read_Dword (Eff);
            while (Ind_Addr and 16#8000_0000#) /= 0 loop
               Indirection_Level := Indirection_Level + 1;
               if Indirection_Level > 15 then
                  raise Indirection_Failure with "Too many levels of indirection";
               end if;
               Ind_Addr := RAM.Read_Dword (Phys_Addr_T(Ind_Addr) and 16#7fff_ffff#);
            end loop;
            Eff := Phys_Addr_T(Ind_Addr) or Ring;
         end if;

         if not CPU.ATU then
            -- constrain to 1st 32MB
            Eff := Eff and 16#01ff_ffff#;
         end if;

         return Eff;
      end Resolve_15bit_Disp;

      function Resolve_31bit_Disp (Indirect    : in Boolean; 
                                   Mode        : in Mode_T;
                                   Disp        : in Integer_32;
                                   Disp_Offset : in Natural) return Phys_Addr_T is
         Eff    : Phys_Addr_T;
         Ring   : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         Ind_Addr : Dword_T;
         Indirection_Level : Integer := 0;
      begin
         case Mode is
            when Absolute =>
               -- Zero-extend to 28 bits
               Eff := Phys_Addr_T(Disp); --  or Ring;
            when PC =>
               Eff := Phys_Addr_T(Integer_32(CPU.PC) + Disp + Integer_32(Disp_Offset));
            when AC2 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(2)) + Disp);
            when AC3 =>
               Eff := Phys_Addr_T(Integer_32(CPU.AC(3)) + Disp);
         end case;

         if Indirect then
            Eff := Eff or Ring;
            Ind_Addr := RAM.Read_Dword (Eff);
            while (Ind_Addr and 16#8000_0000#) /= 0 loop
               Indirection_Level := Indirection_Level + 1;
               if Indirection_Level > 15 then
                  raise Indirection_Failure with "Too many levels of indirection";
               end if;
               Ind_Addr := RAM.Read_Dword (Phys_Addr_T(Ind_Addr) and 16#7fff_ffff#);
            end loop;
            Eff := Phys_Addr_T(Ind_Addr) or Ring;
         end if;

         if not CPU.ATU then
            -- constrain to 1st 32MB
            Eff := Eff and 16#01ff_ffff#;
         end if;

         return Eff and 16#7fff_ffff#;
      end Resolve_31bit_Disp;

      procedure Resolve_Eclipse_Bit_Addr (Acd, Acs  : in AC_ID; 
                                          Word_Addr : out Phys_Addr_T; 
                                          Bit_Num   : out Natural) is
      begin
         -- TODO handle segments and indirection
         if Acd = Acs then 
            Word_Addr := 0;
         else
            if Test_DW_Bit (CPU.AC(Acd), 0) then
               raise Not_Yet_Implemented with "Indirect 16-bit BIT pointers";
            end if;
            Word_Addr := Phys_Addr_T(CPU.AC(Acs)) and 16#0000_7fff#;
         end if;
         Word_Addr := Word_Addr + Phys_Addr_T (Shift_Right (CPU.AC(Acd), 4));
         Bit_Num := Natural(CPU.AC(Acd) and 16#000f#);
      end Resolve_Eclipse_Bit_Addr;

      procedure Eagle_Mem_Ref (I : in Decoded_Instr_T) is
         Addr : Phys_Addr_T;
         Word : Word_T;
         S64  : Integer_64;
      begin
         case I.Instruction is

            when I_LLEF =>
               CPU.AC(I.Ac) := Dword_T(Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset));

            when I_LNLDA =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               CPU.AC(I.Ac) := Sext_Word_To_Dword (RAM.Read_Word(Addr));

            when I_LNSTA =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               RAM.Write_Word (Addr, Lower_Word(CPU.AC(I.Ac)));

            when I_LWLDA =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               CPU.AC(I.Ac) := RAM.Read_Dword (Addr);

            when I_LWSTA =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               RAM.Write_Dword (Addr, CPU.AC(I.Ac));

            when I_WBLM =>
               -- AC0 - unused, AC1 - no. wds to move (if neg then descending order), AC2 - src, AC3 - dest
               while CPU.AC(1) /= 0 loop
                  RAM.Write_Word(Phys_Addr_T(CPU.AC(3)), 
                                          RAM.Read_Word (Phys_Addr_T(CPU.AC(2))));
                  if Test_DW_Bit (CPU.AC(1), 0) then
                     CPU.AC(1) := CPU.AC(1) + 1;
                     CPU.AC(2) := CPU.AC(2) - 1;
                     CPU.AC(3) := CPU.AC(3) - 1;
                  else
                     CPU.AC(1) := CPU.AC(1) - 1;
                     CPU.AC(2) := CPU.AC(2) + 1;
                     CPU.AC(3) := CPU.AC(3) + 1;
                  end if;
               end loop;
  
            when I_XLDB =>
               Addr := Resolve_15bit_Disp (false, I.Mode, I.Disp_15, I.Disp_Offset); -- TODO 'Long' resolve???
               CPU.AC(I.Ac) := Dword_T(RAM.Read_Byte(Addr, I.Low_Byte));

            when I_XLEF =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               CPU.AC(I.Ac) := Dword_T(Addr);

            when I_XNLDA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Word := RAM.Read_Word (Addr);
               CPU.AC(I.Ac) := Dword_T(Word);
               if Test_W_Bit (Word, 0) then
                  CPU.AC(I.Ac) := CPU.AC(I.Ac) or 16#ffff_0000#;
               end if;

            when I_XNSTA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               RAM.Write_Word (Addr, Lower_Word(CPU.AC(I.Ac)));

            when I_XWADI =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               S64 := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr))) + Integer_64(I.Imm_U16);
               if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
                  CPU.Carry := true;
                  Set_OVR (true);
               end if;
               S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
               RAM.Write_Dword (Addr, Dword_T(S64));

            when I_XWLDA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               CPU.AC(I.Ac) := RAM.Read_Dword (Addr);

            when I_XWSBI =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               S64 := Integer_64(Dword_To_Integer_32(RAM.Read_Dword(Addr))) - Integer_64(I.Imm_U16);
               if (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32) then
                  CPU.Carry := true;
                  Set_OVR (true);
               end if;
               S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
               RAM.Write_Dword (Addr, Dword_T(S64));

            when I_XWSTA =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               RAM.Write_Dword (Addr, CPU.AC(I.Ac));

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
         Word : Word_T;
         Dwd : Dword_T;
      begin
         case I.Instruction is

            when I_CIO =>
               Word := Lower_Word (CPU.AC(I.Acs));
               declare
                  IO_Chan : Word_T := Get_W_Bits (Word, 1, 3);
                  Map_Reg_Addr : Integer := Integer(Word and 16#0fff#);
               begin
                  if IO_Chan /= 0 and IO_Chan /= 7 then
                     raise Unsupported_IO_Channel with "Attempt to use CIO on channel " & IO_Chan'Image;
                  end if;
                  if Test_W_Bit (Word, 0) then -- write command
                     BMC_DCH.Write_Reg (Map_Reg_Addr, Lower_Word(CPU.AC(I.Acd)));
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
                     Word := I.Word_2 or Lower_Word (CPU.AC(I.Acs));
                  end if;
                  Map_Reg_Addr := Integer(Word and 16#0fff#);
                  if Test_W_Bit (Word, 0) then -- write command
                     BMC_DCH.Write_Reg (Map_Reg_Addr, Lower_Word(CPU.AC(I.Acd)));
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
               if Lower_Word (CPU.AC(0)) = 16#ffff# then
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

      procedure Eagle_Op (I : in Decoded_Instr_T) is
         Acd_S32, Acs_S32, S32: Integer_32;
         S64   : Integer_64;
         -- Word  : Word_T;
         Shift : Integer;
      begin
         case I.Instruction is

            when I_CRYTC =>
               CPU.Carry := not CPU.Carry;

            when I_CRYTO =>
               CPU.Carry := true;

            when I_CRYTZ =>
               CPU.Carry := false;
            
            when I_NADD =>
               S32 := Integer_32(Word_To_Integer_16(Lower_Word(CPU.AC(I.Acd)))) + 
                      Integer_32(Word_To_Integer_16(Lower_Word(CPU.AC(I.Acs))));
               CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Acd) := Integer_32_To_Dword(S32);

            when I_NLDAI =>
               CPU.AC(I.Ac) := Sext_Word_To_Dword(I.Word_2);

            when I_NMUL =>
               S32 := Integer_32(Word_To_Integer_16(Lower_Word(CPU.AC(I.Acd)))) * 
                      Integer_32(Word_To_Integer_16(Lower_Word(CPU.AC(I.Acs))));
               CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Acd) := Integer_32_To_Dword(S32); 

            when I_NSUB =>
               S32 := Integer_32(Word_To_Integer_16(Lower_Word(CPU.AC(I.Acd)))) - 
                      Integer_32(Word_To_Integer_16(Lower_Word(CPU.AC(I.Acs))));
               CPU.Carry := (S32 > Max_Pos_S16) or (S32 < Min_Neg_S16);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Acd) := Integer_32_To_Dword(S32);

            when I_SSPT =>  -- NO-OP - see p.8-5 of MV/10000 Sys Func Chars 
               Loggers.Debug_Print(Debug_Log, "INFO: SSPT is a No-Op on this VM, continuing...");

            when I_WADC =>
               Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
               Acs_S32 := Dword_To_Integer_32(not CPU.AC(I.Acs));
               S64 := Integer_64(Acd_S32) + Integer_64(Acs_S32);
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR (CPU.Carry);
               S64 := Integer_64(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);
               CPU.AC(I.Acd) := Dword_T(S64);

            when I_WADD =>
               Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
               Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
               S64 := Integer_64(Acd_S32) + Integer_64(Acs_S32);
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Acd) := Dword_T(S64);

            when I_WADI =>
               S32 := Integer_32(Integer_16(I.Imm_U16));
               S64 := Integer_64(Dword_To_Integer_32(CPU.AC(I.Ac))) + Integer_64(S32);
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR(CPU.Carry);
               CPU.AC(I.Ac) := Dword_T(S64);

            when I_WAND =>
               CPU.AC(I.Acd) := CPU.AC(I.Acd) and CPU.AC(I.Acs);

            when I_WANDI =>
               CPU.AC(I.Ac) := CPU.AC(I.Ac) and I.Imm_DW;

            when I_WCOM =>
               CPU.AC(I.Acd) := not CPU.AC(I.Acs);

            when I_WINC =>
               CPU.Carry := CPU.AC(I.Acs) = 16#ffff_ffff#; -- TODO handle overflow flag
               CPU.AC(I.Acd) := CPU.AC(I.Acs) + 1;

            when I_WIORI =>
               CPU.AC(I.Ac) := CPU.AC(I.Ac) or I.Imm_DW;

            when I_WLDAI =>
               CPU.AC(I.Ac) := I.Imm_DW;

            when I_WLSHI =>
               Shift := Integer(Byte_To_Integer_8(Byte_T(I.Word_2 and 16#00ff#)));
               if Shift < 0 then -- shift right
                  CPU.AC(I.Ac) := Shift_Right (CPU.AC(I.Ac), -Shift);
               elsif Shift > 0 then -- shift left
                  CPU.AC(I.Ac) := Shift_Left (CPU.AC(I.Ac), Shift);
               end if;

            when I_WNADI =>
               Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Ac));
               S64 := Integer_64(Acd_S32) + Integer_64(Word_To_Integer_16(I.Word_2));
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Ac) := Dword_T(Integer_64_To_Unsigned_64(S64) and 16#0000_0000_ffff_ffff#);

            when I_WNEG =>
               CPU.Carry := CPU.AC(I.Acs) = 16#8000_0000#; -- TODO Error in PoP?
               Set_OVR(CPU.Carry);
               CPU.AC(I.Acd) := (not CPU.AC(I.Acs)) + 1;

            when I_WMOV =>
               CPU.AC(I.Acd) := CPU.AC(I.Acs);

            when I_WSBI =>
               S32 := Integer_32(Integer_16(I.Imm_U16));
               S64 := Integer_64(Dword_To_Integer_32(CPU.AC(I.Ac))) - Integer_64(S32);
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR(CPU.Carry);
               CPU.AC(I.Ac) := Dword_T(S64);

            when I_WSUB =>
               Acd_S32 := Dword_To_Integer_32(CPU.AC(I.Acd));
               Acs_S32 := Dword_To_Integer_32(CPU.AC(I.Acs));
               S64 := Integer_64(Acd_S32) - Integer_64(Acs_S32);
               CPU.Carry := (S64 > Max_Pos_S32) or (S64 < Min_Neg_S32);
               Set_OVR (CPU.Carry);
               CPU.AC(I.Acd) := Dword_T(S64);

            when I_ZEX =>
               CPU.AC(I.Acd) := 0 or Dword_T(Lower_Word(CPU.AC(I.Acs)));

            when others =>
               Put_Line ("ERROR: EAGLE_Op instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_Op instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eagle_Op;

      procedure Eagle_PC (I : in Decoded_Instr_T) is
         Addr : Phys_Addr_T;
         Word : Word_T;
         DW   : Dword_T;
         Skip : Boolean;
         S32_S, S32_D : Integer_32;
      begin
         case I.Instruction is

            when I_LJMP =>
               CPU.PC := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);

            when I_LJSR =>
               CPU.AC(3) := Dword_T(CPU.PC) + 3;
               CPU.PC := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);

            when I_LNDSZ =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               Word := RAM.Read_Word(Addr) - 1;
               RAM.Write_Word (Addr, Word);
               if Word = 0 then
                  CPU.PC := CPU.PC + 4;
               else
                  CPU.PC := CPU.PC + 3;
               end if;
                         
            when I_LNISZ =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               Word := RAM.Read_Word(Addr) + 1;
               RAM.Write_Word (Addr, Word);
               if Word = 0 then
                  CPU.PC := CPU.PC + 4;
               else
                  CPU.PC := CPU.PC + 3;
               end if;  

            when I_LWDSZ =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               DW := RAM.Read_Dword(Addr) - 1;
               RAM.Write_Dword(Addr, DW);
               if DW = 0 then
                  CPU.PC := CPU.PC + 4;
               else
                  CPU.PC := CPU.PC + 3;
               end if;

            when I_LWISZ =>
               Addr := Resolve_31bit_Disp (I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               DW := RAM.Read_Dword(Addr) + 1;
               RAM.Write_Dword(Addr, DW);
               if DW = 0 then
                  CPU.PC := CPU.PC + 4;
               else
                  CPU.PC := CPU.PC + 3;
               end if;

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

            when I_WSEQI | I_WSGTI | I_WSLEI | I_WSNEI =>
               if I.Instruction = I_WSEQI then
                  Skip := CPU.AC(I.Ac) = Sext_Word_To_Dword (I.Word_2);
               elsif I.Instruction = I_WSGTI then
                  Skip := Integer_32(CPU.AC(I.Ac)) >= Integer_32(Sext_Word_To_Dword (I.Word_2));
               elsif I.Instruction = I_WSLEI then
                  Skip := Integer_32(CPU.AC(I.Ac)) <= Integer_32(Sext_Word_To_Dword (I.Word_2));
               else
                  Skip := CPU.AC(I.Ac) /= Sext_Word_To_Dword (I.Word_2);
               end if;
               if Skip then
                  CPU.PC := CPU.PC + 3;
               else
                  CPU.PC := CPU.PC + 2;
               end if;
                           
            when I_WSGE | I_WSGT | I_WSLE | I_WSLT =>
               if I.Acs = I.Acd then
                  S32_D := 0;
               else
                  S32_D := Dword_To_Integer_32(CPU.AC(I.Acd));
               end if;
               S32_S := Dword_To_Integer_32(CPU.AC(I.Acs));
               if I.Instruction = I_WSGE then
                  Skip := S32_S >= S32_D;
               elsif I.Instruction = I_WSGT then
                  Skip := S32_S > S32_D;
               elsif I.Instruction = I_WSLE then
                  Skip := S32_S <= S32_D;
               elsif I.Instruction = I_WSLT then
                  Skip := S32_S < S32_D; 
               end if;
               if Skip then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
               end if;

            when I_WSKBO =>
               if Test_DW_Bit (CPU.AC(0), I.Bit_Number) then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
            end if;

            when I_WSKBZ =>
               if not Test_DW_Bit (CPU.AC(0), I.Bit_Number) then
                  CPU.PC := CPU.PC + 2;
               else
                  CPU.PC := CPU.PC + 1;
            end if;

            when I_XJMP =>
               CPU.PC := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) or (CPU.PC and 16#7000_0000#);

            when I_XJSR =>
               CPU.AC(3) := Dword_T(CPU.PC + 2);
               CPU.PC := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) or (CPU.PC and 16#7000_0000#);

            when I_XNDSZ =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Word := RAM.Read_Word (Addr) - 1;
               RAM.Write_Word (Addr, Word);
               if Word = 0 then 
                  CPU.PC := CPU.PC + 3;
               else
                  CPU.PC := CPU.PC + 2;
               end if;
                           
            when I_XNISZ =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Word := RAM.Read_Word (Addr) + 1;
               RAM.Write_Word (Addr, Word);
               if Word = 0 then 
                  CPU.PC := CPU.PC + 3;
               else
                  CPU.PC := CPU.PC + 2;
               end if;
                           
            when I_XWDSZ =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               DW   := RAM.Read_Dword (Addr) - 1;
               RAM.Write_Dword (Addr, DW);
               if DW = 0 then 
                  CPU.PC := CPU.PC + 3;
               else
                  CPU.PC := CPU.PC + 2;
               end if;
                         
            when I_XWISZ =>
               Addr := Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               DW   := RAM.Read_Dword (Addr) + 1;
               RAM.Write_Dword (Addr, DW);
               if DW = 0 then 
                  CPU.PC := CPU.PC + 3;
               else
                  CPU.PC := CPU.PC + 2;
               end if;  

            when others =>
               Put_Line ("ERROR: EAGLE_PC instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_PC instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
      end Eagle_PC;

      -- Wide Stack Helper subprograms...

      procedure WS_Pop (DW : out Dword_T) is
      begin
         DW := RAM.Read_Dword (CPU.WSP);
         CPU.WSP := CPU.WSP - 2;
      end WS_Pop;

      procedure WS_Push (Datum : in Dword_T) is
      begin
         CPU.WSP := CPU.WSP + 2;
         RAM.Write_Dword (CPU.WSP, Datum);
      end WS_Push;

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

      procedure Eagle_Stack (I : in Decoded_Instr_T) is
         Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         OK   : Boolean;
         DW, Primary_Fault, Secondary_Fault : Dword_T;
         Req_Space : Integer;
      begin
         case I.Instruction is

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

            when others =>
               Put_Line ("ERROR: EAGLE_STACK instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: EAGLE_STACK instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eagle_Stack;

      procedure Eclipse_Mem_Ref (I : in Decoded_Instr_T) is
         Addr : Phys_Addr_T;
         Ring : Phys_Addr_T := CPU.PC and 16#7000_0000#;
         Bit_Num : Natural;
         Word : Word_T;
      begin
         case I.Instruction is

            when I_BLM => -- AC0 - unused, AC1 - no. wds to move, AC2 - src, AC3 - dest
               declare
                  Num_Wds :  Word_T := Lower_Word(CPU.AC(1));
                  Src, Dest : Phys_Addr_T;
               begin
                  if (Num_Wds = 0)  or (Num_Wds > 32768) then
                     Loggers.Debug_Print (Debug_Log, "WARNING: BLM called with AC1 out of bounds, No-Op");
                  else
                     Src  := Ring or Phys_Addr_T(Lower_Word(CPU.AC(2)));
                     Dest := Ring or Phys_Addr_T(Lower_Word(CPU.AC(3)));
                     if CPU.Debug_Logging then
                        Loggers.Debug_Print (Debug_Log, "BLM moving" & Num_Wds'Image & " words from" &
                           Src'Image & " to" & Dest'Image);
                     end if;
                     while Num_Wds /= 0 loop
                        RAM.Write_Word (Dest, RAM.Read_Word(Src));
                        Num_Wds := Num_Wds - 1;
                        Src := Src + 1;
                        Dest := Dest + 1;
                     end loop;
                     CPU.AC(1) := 0;
                     CPU.AC(2) := Dword_T(Src); -- TODO verify this
                     CPU.AC(3) := Dword_T(Dest);
                  end if;
               end;

            when I_BTO | I_BTZ =>
               Resolve_Eclipse_Bit_Addr (I.Acd, I.Acs, Addr, Bit_Num);
               Addr := Addr or Ring;
               Word := RAM.Read_Word (Addr);
               if I.Instruction = I_BTO then
                  Set_W_Bit (Word, Bit_Num);
               else
                  Clear_W_Bit (Word, Bit_Num);
               end if;
               RAM.Write_Word (Addr, Word);

            when I_CMP =>
               declare
                  Byte_1, Byte_2 : Byte_T;
                  Res            : Dword_T := 0;
                  Str_1_BP, Str_2_BP : Word_T;
                  Str_1_Len : Integer_16 := Word_To_Integer_16(Lower_Word(CPU.AC(1)));
                  Str_2_Len : Integer_16 := Word_To_Integer_16(Lower_Word(CPU.AC(0)));
               begin
                  if (Str_1_Len = 0) and (Str_2_Len = 0) then
                     CPU.AC(1) := 0;
                  else
                     Str_1_BP := Lower_Word(CPU.AC(3));
                     Str_2_BP := Lower_Word(CPU.AC(2));
                     loop
                        if Str_1_Len = 0 then 
                           Byte_1 := 32; 
                        else 
                           Byte_1 := RAM.Read_Byte_Eclipse_BA(Ring,Str_1_BP); 
                        end if;
                        if Str_2_Len = 0 then 
                           Byte_2 := 32; 
                        else 
                           Byte_2 := RAM.Read_Byte_Eclipse_BA(Ring,Str_2_BP); 
                        end if;     
                        if Byte_1 > Byte_2 then
                           Res := 1;
                           exit;
                        end if;
                        if Byte_1 < Byte_2 then
                           Res := 16#ffff_ffff#;
                           exit;
                        end if;
                        if Str_1_Len > 0 then
                           Str_1_BP  := Str_1_BP + 1;
                           Str_1_Len := Str_1_Len - 1;
                        end if;
                        if Str_1_Len < 0 then
                           Str_1_BP  := Str_1_BP - 1;
                           Str_1_Len := Str_1_Len + 1;
                        end if;
                        if Str_2_Len > 0 then
                           Str_2_BP  := Str_2_BP + 1;
                           Str_2_Len := Str_2_Len - 1;
                        end if;
                        if Str_2_Len < 0 then
                           Str_2_BP  := Str_2_BP - 1;
                           Str_2_Len := Str_2_Len + 1;
                        end if;
                        exit when (Str_1_Len = 0) and (Str_2_Len = 0);
                     end loop;
                     CPU.AC(0) := Dword_T(Str_2_Len);
                     CPU.AC(1) := Res;
                     CPU.AC(2) := Dword_T(Str_2_BP);
                     CPU.AC(3) := Dword_T(Str_1_BP);                    
                  end if;
               end;
               
            when I_CMV =>
               declare
                  Dest_Ascend, Src_Ascend : Boolean;
                  Dest_Cnt, Src_Cnt : Integer_16;
               begin
                  Dest_Cnt := Word_To_Integer_16(Lower_Word(CPU.AC(0)));
                  if Dest_Cnt = 0 then
                     Loggers.Debug_Print (Debug_Log, "WARNING: CMV called with AC0 = 0, not moving anything");
                     CPU.Carry := false;
                  else
                     Dest_Ascend := Dest_Cnt > 0;
                     Src_Cnt := Word_To_Integer_16(Lower_Word(CPU.AC(3)));
                     Src_Ascend := Src_Cnt > 0;
                     CPU.Carry := (Abs Src_Cnt) > (Abs Dest_Cnt);
                     -- move Src_Cnt bytes
                     loop
                        RAM.Write_Byte_Eclipse_BA(Ring, Lower_Word(CPU.AC(2)), 
                                                  RAM.Read_Byte_Eclipse_BA(Ring, Lower_Word(CPU.AC(3))));
                        if Src_Ascend then
                           CPU.AC(3) := CPU.AC(3) + 1;
                           Src_Cnt := Src_Cnt - 1;
                        else
                           CPU.AC(3) := CPU.AC(3) - 1;
                           Src_Cnt := Src_Cnt + 1;
                        end if;
                        if Dest_Ascend then
                           CPU.AC(2) := CPU.AC(2) + 1;
                           Dest_Cnt := Dest_Cnt - 1;
                        else
                           CPU.AC(2) := CPU.AC(2) - 1;
                           Dest_Cnt := Dest_Cnt + 1;
                        end if;
                        exit when (Src_Cnt = 0) or (Dest_Cnt = 0);
                     end loop;
                     -- now fill any excess bytes with ASCII spaces
                     while Dest_Cnt /= 0 loop
                        RAM.Write_Byte_Eclipse_BA(Ring, Lower_Word(CPU.AC(2)), 32);
                        if Dest_Ascend then
                           CPU.AC(2) := CPU.AC(2) + 1;
                           Dest_Cnt := Dest_Cnt - 1;
                        else
                           CPU.AC(2) := CPU.AC(2) - 1;
                           Dest_Cnt := Dest_Cnt + 1;
                        end if;
                     end loop;
                     CPU.AC(0) := 0;
                     CPU.AC(1) := Dword_T(Src_Cnt);
                  end if;                  
               end;
            when I_ELDA =>
               Addr := (Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               CPU.AC(I.Ac) := Dword_T(RAM.Read_Word(Addr));

            when I_ELEF =>
               Addr := (Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               CPU.AC(I.Ac) := Dword_T(Addr);

            when I_ESTA =>
               Addr := (Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               RAM.Write_Word (Addr, Lower_Word(CPU.AC(I.Ac)));

            when I_LDB =>
               CPU.AC(I.Acd) := Dword_T (RAM.Read_Byte_Eclipse_BA (Ring, Lower_Word(CPU.AC(I.Acs))));

            when I_LEF =>
               Addr := Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15);
               Addr := (Addr and 16#0000_7fff#) or Ring;

            when I_STB =>
               declare
                  Low_Byte : Boolean := Test_DW_Bit(CPU.AC(I.Acs), 31);
               begin
                  Addr := Shift_Right (Phys_Addr_T(Lower_Word(CPU.AC(I.Acs))), 1);
                  Addr := (Addr and 16#7fff#) or Ring;
                  RAM.Write_Byte(Addr, Low_Byte, Byte_T(CPU.AC(I.Acd)));
               end;


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
         Dword : Dword_T;
         S16   : Integer_16;
         Shift : Integer;
      begin
         case I.Instruction is

            when I_ADDI =>
               S16 := Word_To_Integer_16(Lower_Word(CPU.AC(I.Ac)));
               S16 := S16 + Word_To_Integer_16(I.Word_2);
               CPU.AC(I.Ac) := Dword_T(Integer_16_To_Word(S16)) and 16#0000_ffff#;

            when I_ADI =>
               Word := Lower_Word (CPU.AC(I.Ac));
               Word := Word + Word_T(I.Imm_U16);
               CPU.AC(I.Ac) := Dword_T(Word);

            when I_ANDI =>
               Word := Lower_Word (CPU.AC(I.Ac));
               CPU.AC(I.Ac) := Dword_T(Word and Integer_16_To_Word(I.Imm_S16)) and 16#0000_ffff#;

            when I_DHXL =>
               declare
                  D_Plus_1 : AC_ID;
               begin
                  if I.Acd = 3 then
                     D_Plus_1 := 0;
                  else
                     D_Plus_1 := I.Acd + 1;
                  end if;
                  Dword := Dword_From_Two_Words (Lower_Word(CPU.AC(I.Acd)), Lower_Word(CPU.AC(D_Plus_1)));
                  Dword := Shift_Left (Dword, Natural(I.Imm_U16) * 4);
                  CPU.AC(I.Acd) := Dword_T(Upper_Word (Dword));
                  CPU.AC(D_Plus_1) := Dword_T(Lower_Word (Dword));
               end;

            when I_DLSH =>
               declare
                  D_Plus_1 : AC_ID;
               begin
                  if I.Acd = 3 then
                     D_Plus_1 := 0;
                  else
                     D_Plus_1 := I.Acd + 1;
                  end if;
                  Shift := Integer(Byte_To_Integer_8 (Get_Lower_Byte (Lower_Word (CPU.AC(I.Acs)))));
                  Dword := Dword_From_Two_Words (Lower_Word(CPU.AC(I.Acd)), Lower_Word(CPU.AC(D_Plus_1)));
                  if Shift /= 0 then
                     if (Shift < -31) or (Shift > 31) then
                        Dword := 0;
                     else
                        if Shift > 0 then
                           Dword := Shift_Left (Dword, Shift);
                        else
                           Dword := Shift_Right (Dword, Shift * (-1));
                        end if;
                     end if;
                  end if;
                  CPU.AC(I.Acd) := Dword_T(Upper_Word (Dword));
                  CPU.AC(D_Plus_1) := Dword_T(Lower_Word (Dword));
               end;

            when I_IOR =>
               Word := Lower_Word (CPU.AC(I.Acd)) or Lower_Word (CPU.AC(I.Acs));
               CPU.AC(I.Acd) := Dword_T(Word);

            when I_IORI =>
               Word := Lower_Word (CPU.AC(I.Ac)) or I.Word_2;
               CPU.AC(I.Ac) := Dword_T(Word);

            when I_HXL =>
               Dword := Shift_Left (CPU.AC(I.Ac), Integer(I.Imm_U16) * 4);
               CPU.AC(I.Ac) := Dword and 16#0000_ffff#;
                           
            when I_HXR =>
               Dword := Shift_Right (CPU.AC(I.Ac), Integer(I.Imm_U16) * 4);
               CPU.AC(I.Ac) := Dword and 16#0000_ffff#;

            when I_LSH =>
               Word := Lower_Word (CPU.AC(I.Acd));
               Shift := Integer(Byte_To_Integer_8 (Get_Lower_Byte (Lower_Word (CPU.AC(I.Acs)))));
               if Shift /= 0 then
                  if (Shift < -15) or (Shift > 15) then
                     Word := 0;
                  else
                     if Shift > 0 then
                        Word := Shift_Left (Word, Shift);
                     else
                        Word := Shift_Right (Word, Shift * (-1));
                     end if;
                  end if;
               end if;
               CPU.AC(I.Acd) := Dword_T(Word);

            when I_SBI =>
               Word := Lower_Word(CPU.AC(I.Ac));
               Word := Word - Word_T(I.Imm_U16);
               CPU.AC(I.Ac) := Dword_T(Word);
                   
            when I_XCH =>
               DWord := CPU.AC(I.Acs);
               CPU.AC(I.Acs) := CPU.AC(I.Acd) and 16#0000_ffff#;
               CPU.AC(I.Acd) := DWord and 16#0000_ffff#;

            when others =>
               Put_Line ("ERROR: ECLIPSE_OP instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: ECLIPSE_OP instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end Eclipse_Op;

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
                  Acs := Word_To_Integer_16(Lower_Word(CPU.AC(I.Acs)));
                  if I.Acs = I.Acd then
                     L := Word_To_Integer_16(RAM.Read_Word(CPU.PC + 1));
                     H := Word_To_Integer_16(RAM.Read_Word(CPU.PC + 2));
                     if (Acs < L) or (Acs > H) then
                        Incr := 3;
                     else
                        Incr := 4;
                     end if;
                  else
                     L := Word_To_Integer_16(RAM.Read_Word (Phys_Addr_T (Lower_Word (CPU.AC(I.Acd))) or Ring));
                     H := Word_To_Integer_16(RAM.Read_Word (Phys_Addr_T (Lower_Word (CPU.AC(I.Acd)) + 1) or Ring));
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
                  Table_Start := (Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
                  Offset     := Phys_Addr_T(Lower_Word(CPU.AC(I.Ac)));
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
               Addr := (Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               CPU.PC := Addr;

            when I_EJSR =>
               CPU.AC(3) := Dword_T(CPU.PC) + 2;
               Addr := (Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
               CPU.PC := Addr;

            when I_SNB =>
               Resolve_Eclipse_Bit_Addr (I.Acd , I.Acs, Addr, Bit_Num);
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
                  Narrow_Stack.Push (Ring, Lower_Word(CPU.AC(AC_Circle(This_AC))));
               end loop;

            when I_PSHJ =>
               Narrow_Stack.Push (Ring, Lower_Word(DWord_T(CPU.PC)) + 2);
               Addr := (Resolve_15bit_Disp (I.Ind, I.Mode, I.Disp_15, I.Disp_Offset) and 16#7fff#) or Ring;
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
                  RAM.Write_Word (NFP_Loc or Ring, Lower_Word(CPU.AC(3)));
                  return; -- because PC has been set
               end;

            when I_SAVE =>
               declare
                  NFP_Sav, NSP_Sav, Word : Word_T;
               begin
                  NFP_Sav := RAM.Read_Word (NFP_Loc or Ring);
                  NSP_Sav := RAM.Read_Word (NSP_Loc or Ring);
                  Narrow_Stack.Push(Ring, Lower_Word(CPU.AC(0))); -- 1
                  Narrow_Stack.Push(Ring, Lower_Word(CPU.AC(1))); -- 2
                  Narrow_Stack.Push(Ring, Lower_Word(CPU.AC(2))); -- 3 
                  Narrow_Stack.Push(Ring, NFP_Sav);               -- 4
                  Word := Lower_Word(CPU.AC(3));
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
                  case I.Instruction is
                     when I_DIC =>
                        Put_Line ("INFO: Reseting I/O Devices due to DIC CPU instruction");
                        Devices.Bus.Actions.Reset_All_IO_Devices;

                     when others =>
                        raise Execution_Failure with "CPU I/O not yet implemented";
                  end case;
               else
                  if Bus.Actions.Is_Connected(I.IO_Dev) and Bus.Actions.Is_IO_Dev(I.IO_Dev) then
                     if I.IO_Dir = Data_In then
                           Devices.Bus.Actions.Data_In(I.IO_Dev, I.IO_Reg, I.IO_Flag, Datum);
                           CPU.AC(I.Ac) := Dword_T(Datum);
                        else
                           Datum := Lower_Word (CPU.AC(I.Ac));
                           Devices.Bus.Actions.Data_Out(I.IO_Dev, Datum, I.IO_Reg, I.IO_Flag);
                     end if;
                  else
                     if I.IO_Dev = 2 then
                        Loggers.Debug_Print(Debug_Log, "WARNING: Ignoring I/O to device " & I.IO_Dev'Image);
                     else
                        Loggers.Debug_Print(Debug_Log, "WARNING: I/O Attempted to unattached or non-I/O capable device ");
                        raise IO_Device_Error;
                     end if;
                  end if;
               end if;

            when I_NIO =>
               -- catch CPU I/O instructions
               if I.IO_Dev = Devices.CPU then
                  raise Execution_Failure with "CPU I/O not yet implemented";
               end if;
               -- case I.IO_Flag is
               --    when None => null;
               --    when S => 
               --       Devices.Bus.States.Set_Busy (I.IO_Dev, true);
               --       Devices.Bus.States.Set_Done (I.IO_Dev, false);
               --    when C => 
               --       Devices.Bus.States.Set_Busy (I.IO_Dev, false);
               --       Devices.Bus.States.Set_Done (I.IO_Dev, false);   
               --    when P =>
               --       raise Not_Yet_Implemented with "NIO Pulse";
               -- end case;
               Devices.Bus.Actions.Data_Out(I.IO_Dev, 0, N, I.IO_Flag);

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
                  when DN => 
                     if Done then 
                        CPU.PC := CPU.PC + 1; 
                     end if;
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

      procedure Nova_Math (I : in Decoded_Instr_T) is
         DW : Dword_T;
      begin
         case I.Instruction is
            when I_DIV =>
               declare
                  UW, LW, Quot : Word_T;
               begin
                  UW := Lower_Word (CPU.AC(0));
                  LW := Lower_Word (CPU.AC(1));
                  DW := Dword_From_Two_Words (UW, LW);
                  Quot := Lower_Word (CPU.AC(2));
                  if (UW >= Quot) or (Quot = 0) then
                     CPU.Carry := true;
                  else
                     CPU.Carry := false;
                     CPU.AC(0) := (Dw mod Dword_T(Quot)) and 16#0000_ffff#;
                     CPU.AC(1) := (Dw / Dword_T(Quot)) and 16#0000_ffff#;
                  end if;
               end;     

            when I_MUL =>      
               DW := ((CPU.AC(1) and 16#0000_ffff#) *
                      (CPU.AC(2) and 16#0000_ffff#)) +
                      (CPU.AC(0) and 16#0000_ffff#);
               CPU.AC(0) := Dword_T(Upper_Word (DW));
               CPU.AC(1) := Dword_T(Lower_Word (DW));

            when others =>
               Put_Line ("ERROR: NOVA_MATH instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented");
               raise Execution_Failure with "ERROR: NOVA_MATH instruction " & To_String(I.Mnemonic) & 
                         " not yet implemented";
         end case;
         CPU.PC := CPU.PC + 1;
      end Nova_Math;

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
            when I_DSZ =>
               Addr := (Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
               Word := RAM.Read_Word (Addr) - 1;
               RAM.Write_Word (Addr, Word);
               if Word = 0 then CPU.PC := CPU.PC + 1; end if;

            when I_ISZ =>
               Addr := (Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
               Word := RAM.Read_Word (Addr) + 1;
               RAM.Write_Word (Addr, Word);
               if Word = 0 then CPU.PC := CPU.PC + 1; end if;

            when I_LDA =>
               Addr := (Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
               Word := RAM.Read_Word (Addr);
               CPU.AC(I.Ac) := Dword_T(Word);

            when I_STA =>
               Addr := (Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) and 16#7fff#) or Ring_Mask;
               Word := Lower_Word (CPU.AC(I.Ac));
               RAM.Write_Word (Addr, Word);

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

            when I_JSR =>
               declare
                  Tmp_PC : Dword_T := Dword_T(CPU.PC) + 1;
               begin
                  CPU.PC := Resolve_8bit_Disp (I.Ind, I.Mode, I.Disp_15) or Ring_Mask;
                  CPU.AC(3) := Tmp_PC;
               end;

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
            when EAGLE_STACK    => Eagle_Stack(Instr);
            when ECLIPSE_MEMREF => Eclipse_Mem_Ref(Instr);
            when ECLIPSE_OP     => Eclipse_Op(Instr);
            when ECLIPSE_PC     => Eclipse_PC(Instr);
            when ECLIPSE_STACK  => Eclipse_Stack(Instr);
            when NOVA_IO        => Nova_IO(Instr);  
            when NOVA_MATH      => Nova_MATH(Instr);  
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
            Any_Breakpoints : Boolean := Breakpoints.Length /= 0;
         begin
         Run_Loop:
            loop
               PC := Actions.Get_PC;

               -- FETCH
               This_Op := RAM.Read_Word(PC);

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

end CPU;
