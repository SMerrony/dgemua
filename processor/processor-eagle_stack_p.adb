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

with Ada.Text_IO; use Ada.Text_IO;

with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with Resolver;    use Resolver;

package body Processor.Eagle_Stack_P is 

   procedure WS_Pop (CPU : CPU_T; DW : out Dword_T) is
   begin
      DW := RAM.Read_Dword (CPU.WSP);
      if CPU.Debug_Logging then
         Loggers.Debug_Print (Debug_Log, "... Popped " & Dword_To_String (DW, Octal, 11) &
                                         " from: " & Dword_To_String (Dword_T(CPU.WSP), Octal, 11));
      end if;
      CPU.WSP := CPU.WSP - 2;
   end WS_Pop;

   procedure WS_Push (CPU : CPU_T; DW : Dword_T) is
   begin
      CPU.WSP := CPU.WSP + 2;
      RAM.Write_Dword (CPU.WSP, DW);
      if CPU.Debug_Logging then
         Loggers.Debug_Print (Debug_Log, "... Pushed " & Dword_To_String (DW, Octal, 11) &
                                           " to: " & Dword_To_String (Dword_T(CPU.WSP), Octal, 11));
      end if;
   end WS_Push;

   procedure WS_Pop_QW (CPU : CPU_T; QW : out Qword_T) is
      High_DW, Low_DW : Dword_T;
   begin
      WS_Pop (CPU, Low_DW);
      WS_Pop (CPU, High_DW);
      QW := Qword_From_Two_Dwords (High_DW, Low_DW);
   end WS_Pop_QW;

   procedure WS_Push_QW (CPU : CPU_T; Datum : Qword_T) is
      High_DW : constant Dword_T := Upper_Dword (Datum);
      Low_DW  : constant Dword_T := Lower_Dword (Datum);
   begin
      WS_Push (CPU, High_DW);
      WS_Push (CPU, Low_DW);
   end WS_Push_QW;

   procedure Do_Eagle_Stack (I : Decoded_Instr_T; CPU : CPU_T) is
      Ring : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
      OK   : Boolean;
      DW, Primary_Fault, Secondary_Fault : Dword_T;
      QW : Qword_T;
      Req_Space : Integer;
      First, Last, This_Ac : Natural;
      Addr : Phys_Addr_T;

      -- Wide Stack Helper subprograms...

      function Dwords_Reserved (Instr : Instr_Mnemonic_T) return Phys_Addr_T is
         DW_Buffer : Phys_Addr_T;
      begin
         case Instr is 
            when I_BKPT | I_WPOPB | I_WRTN | I_WSSVR | I_WSSVS => DW_Buffer := 12;
            when I_LCALL | I_LPEF | I_LPEFB | I_LPSHJ | I_WPOPJ | I_XCALL | I_XPSHJ => DW_Buffer := 2;
            when I_WFPOP | I_WFPSH => DW_Buffer := 20;
            when I_WSAVR | I_WSAVS => DW_Buffer := 10;
            when I_WMSP | I_WPOP | I_WPSH | I_XPEF | I_XPEFB => DW_Buffer := 0;
            when others =>
               raise Not_Yet_Implemented with "WS Bounds checking for this instruction";
         end case; 
         return DW_Buffer;
      end Dwords_Reserved;

      -- WSP_Check_Bounds does a check to see if the intended change of WSP would cause a stack fault
      -- Is_Save must be set by WMSP, WSSVR, WSSVS, WSAVR & WSAVS
      procedure WSP_Check_Bounds (Instr : Instr_Mnemonic_T; Delta_Words : Integer; Is_Save : Boolean;
                                  OK : out boolean; Primary_Fault, Secondary_Fault : out Dword_T) is
         OOB_Buffer : constant Phys_Addr_T := Dwords_Reserved (Instr);
      begin
         OK := true;
         -- see p.4-8 of 1988 PoP...
         if Delta_Words > 0 then
            if CPU.WSP + Phys_Addr_T(Delta_Words) > (CPU.WSL - OOB_Buffer) then
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

      procedure WSP_Check_Overflow (Instr : Instr_Mnemonic_T; OK : out boolean; Primary_Fault, Secondary_Fault : out Dword_T) is
         OOB_Buffer : constant Phys_Addr_T := Dwords_Reserved (Instr);
      begin
         OK := true;
         if CPU.WSP > (CPU.WSL - OOB_Buffer) then
            OK := false;
            Secondary_Fault := WSF_Overflow;
            Primary_Fault := WSF_Overflow; 
         end if;      
      end WSP_Check_Overflow;

      procedure WSP_Check_Underflow (OK : out boolean; Primary_Fault, Secondary_Fault : out Dword_T) is
      begin
         OK := true;
         if CPU.WSP < CPU.WSB then
            OK := false;
            Secondary_Fault := WSF_Underflow;
            Primary_Fault := WSF_Underflow;
         end if;
      end WSP_Check_Underflow;

      procedure WSP_Handle_Fault (Ring : Phys_Addr_T; I_Len : Positive; Primary_Fault, Secondary_Fault : Dword_T) is
         DW : Dword_T;
         WSFH_Addr : Phys_Addr_T;
      begin
         -- from pp.5-23 of PoP
         -- step 1
         if Primary_Fault = WSF_Underflow then
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
         WS_Push (CPU, Dword_From_Two_Words(CPU.PSR, 0));
         WS_Push (CPU, CPU.AC(0));
         WS_Push (CPU, CPU.AC(1));
         WS_Push (CPU, CPU.AC(2));
         WS_Push (CPU, Dword_T(CPU.WFP));
         WS_Push (CPU, DW);
         -- step 3
         Clear_W_Bit (CPU.PSR, 0); -- OVK
         Clear_W_Bit (CPU.PSR, 1); -- OVR
         Clear_W_Bit (CPU.PSR, 2); -- IRES
         -- step 4
         CPU.WSP := CPU.WSP and 16#7fff_ffff#;
         -- step 5
         CPU.WSL := CPU.WSL or 16#8000_0000#;
         -- step 6
         RAM.Write_Dword (Ring or WFP_Loc, Dword_T(CPU.WFP));
         RAM.Write_Dword (Ring or WSP_Loc, Dword_T(CPU.WSP));
         RAM.Write_Dword (Ring or WSL_Loc, Dword_T(CPU.WSL));
         RAM.Write_Dword (Ring or WSB_Loc, Dword_T(CPU.WSB));
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

      Have_Set_PC : Boolean := false;

   begin
      if CPU.Debug_Logging then
         Loggers.Debug_Print (Debug_Log, "... Before - WSB: " & Dword_To_String (Dword_T(CPU.WSB), Octal, 11, true) &
                                             " WSP: " & Dword_To_String (Dword_T(CPU.WSP), Octal, 11, true) &
                                             " WSL: " & Dword_To_String (Dword_T(CPU.WSL), Octal, 11, true) &
                                             " WFP: " & Dword_To_String (Dword_T(CPU.WFP), Octal, 11, true));
      end if;
      case I.Instruction is

         when I_LCALL => -- FIXME - LCALL only handling trivial case
            declare
               OK : Boolean;
               Primary_Fault, Secondary_Fault : Dword_T;
               Ring : constant Phys_Addr_T := CPU.PC and 16#7000_0000#;
               PC_4 : constant Dword_T := Dword_T(CPU.PC) + 4;
            begin
               Set_OVR (CPU, false);
               if I.Arg_Count >= 0 then
                  DW := Dword_T(I.Arg_Count) and 16#0000_ffff#;
               else
                  DW := RAM.Read_Dword (CPU.WSP) and 16#0000_7fff#;
               end if;
               DW := DW or Shift_Left(Dword_T(CPU.PSR), 16);
               WSP_Check_Bounds (Instr => I_LCALL,
                                 Delta_Words => 2, 
                                 Is_Save => false, 
                                 OK => OK, 
                                 Primary_Fault => Primary_Fault, 
                                 Secondary_Fault => Secondary_Fault);
               if not OK then
                  Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by LCALL");
                  raise Not_Yet_Implemented with "LCALL Stack fault";
                  --WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                  -- return;
               end if;
               WS_Push (CPU, DW);
               CPU.PC := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset); 
               CPU.AC(3) := PC_4;
               Have_Set_PC := true; -- we have set the PC
            end;

         when I_LDAFP =>
            CPU.AC(I.Ac) := Dword_T(CPU.WFP);
            Set_OVR (CPU, false);
         when I_LDASB =>
            CPU.AC(I.Ac) := Dword_T(CPU.WSB);
            Set_OVR (CPU, false); 
         when I_LDASL =>
            CPU.AC(I.Ac) := Dword_T(CPU.WSL);
            Set_OVR (CPU, false);
         when I_LDASP =>
            CPU.AC(I.Ac) := Dword_T(CPU.WSP);
            Set_OVR (CPU, false); 
         when I_LDATS =>
            CPU.AC(I.Ac) := RAM.Read_Dword (CPU.WSP);
            Set_OVR (CPU, false);

         when I_LPEF =>
               Addr := Resolve_31bit_Disp (CPU, I.Ind, I.Mode, I.Disp_31, I.Disp_Offset);
               WS_Push (CPU, Dword_T(Addr));
               Set_OVR (CPU, false);
               WSP_Check_Overflow (
                  I_LPEF,
                  OK => OK, 
                  Primary_Fault => Primary_Fault, 
                  Secondary_Fault => Secondary_Fault);
               if not OK then
                  Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by LPEFB");
                  WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                  Have_Set_PC := true; -- We have set the PC
               end if;

         when I_LPEFB =>
            -- FIXME should the byte address have sign preserved?
            Addr := Resolve_31bit_Disp (CPU, false, I.Mode, Integer_32(Shift_Right(I.Disp_32,1)), I.Disp_Offset);
            Addr := Shift_Left(Addr, 1);
            if I.Disp_32 mod 2 = 1 then
               Addr := Addr + 1;
            end if;
            WS_Push (CPU, Dword_T(Addr));
            Set_OVR (CPU, false);
            WSP_Check_Overflow ( I_LPEFB, OK => OK, Primary_Fault => Primary_Fault, Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by LPEFB");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            end if;

         when I_STAFP =>
            CPU.WFP := Phys_Addr_T(CPU.AC(I.Ac));
            -- FIXME according the PoP does not write through to page zero...
            RAM.Write_Dword (Ring or WFP_Loc, CPU.AC(I.Ac));
            Set_OVR (CPU, false);
         
         when I_STASB =>
            CPU.WSB := Phys_Addr_T(CPU.AC(I.Ac));
            RAM.Write_Dword (Ring or WSB_Loc, CPU.AC(I.Ac));
            Set_OVR (CPU, false);

         when I_STASL =>
            CPU.WSL := Phys_Addr_T(CPU.AC(I.Ac));
            RAM.Write_Dword (Ring or WSL_Loc, CPU.AC(I.Ac));
            Set_OVR (CPU, false);

         when I_STASP =>
            CPU.WSP := Phys_Addr_T(CPU.AC(I.Ac));
            -- FIXME according the PoP does not write through to page zero...
            RAM.Write_Dword (Ring or WSP_Loc, CPU.AC(I.Ac));
            Set_OVR (CPU, false);

         when I_STATS =>
            RAM.Write_Dword (CPU.WSP, CPU.AC(I.Ac));
            Set_OVR (CPU, false);

         when I_WFPOP =>
            WS_Pop_QW (CPU, QW);   CPU.FPAC(3) := Long_Float(QW);
            WS_Pop_QW (CPU, QW);   CPU.FPAC(2) := Long_Float(QW);
            WS_Pop_QW (CPU, QW);   CPU.FPAC(1) := Long_Float(QW);
            WS_Pop_QW (CPU, QW);   CPU.FPAC(0) := Long_Float(QW);
            WS_Pop_QW (CPU, QW);
            -- CPU.FPSR := 0;
            CPU.FPSR := QW and 16#7ff0_0000_0000_0000#; -- copy bits 1..11
            if (QW and 16#7800_0000_0000_0000#)  /= 0 then
               Set_QW_Bit (CPU.FPSR, 0);
               CPU.FPSR := QW and 16#0000_000f_0000_0000#; -- copy bits 28..31
               CPU.FPSR := QW and 16#0000_0000_7fff_ffff#; -- copy bits 33..63
            end if;

         when I_WFPSH =>
            DW := Upper_Dword (CPU.FPSR) and 16#FFFF_0000#;
            if Test_QW_Bit (CPU.FPSR, FPSR_Any) then
               DW := DW or (Upper_Dword (CPU.FPSR) and 16#0000_000F#);
            end if;
            WS_Push (CPU, DW);
            WS_Push (CPU, Lower_Dword (CPU.FPSR) and 16#7fff_ffff#);

            WS_Push_QW (CPU, Qword_T(CPU.FPAC(0))); 
            WS_Push_QW (CPU, Qword_T(CPU.FPAC(1)));
            WS_Push_QW (CPU, Qword_T(CPU.FPAC(2)));
            WS_Push_QW (CPU, Qword_T(CPU.FPAC(3)));
            WSP_Check_Overflow ( 
                           I_WFPSH,
                           OK => OK, 
                           Primary_Fault => Primary_Fault, 
                           Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WFPSH");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            end if;

         when I_WMSP =>
            declare
               DeltaWds : constant Integer := Integer(CPU.AC_I32(I.Ac)) * 2;
            begin
               WSP_Check_Bounds (Instr => I_WMSP,
                                 Delta_Words => DeltaWds, 
                                 Is_Save => false, 
                                 OK => OK, 
                                 Primary_Fault => Primary_Fault, 
                                 Secondary_Fault => Secondary_Fault);
               if not OK then
                  Loggers.Debug_Print(Debug_Log, "... Stack Fault trapped by WMSP");
                  WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
                  Have_Set_PC := true; -- We have set the PC
               else
                  CPU.WSP := CPU.WSP + Phys_Addr_T(DeltaWds);
                  Set_OVR (CPU, false);
               end if;
            end;


         when I_WPOP =>
            First := Natural(I.Acs);
            Last  := Natural(I.Acd);
            if Last > First then First := First + 4; end if;
            This_Ac := First;
            loop
               if CPU.Debug_Logging then
                  Loggers.Debug_Print (Debug_Log, "... WPOP popping AC" & AC_Circle(This_AC)'Image);
               end if;
               WS_Pop(CPU,CPU.AC(AC_Circle(This_AC)));
               exit when This_Ac = Last;
               This_Ac := This_Ac -1;
            end loop;

         when I_WPOPJ =>
            WS_Pop(CPU,DW);
            DW := (DW and 16#0fff_ffff#) or Dword_T(Ring);
            CPU.PC := Phys_Addr_T(DW);
            WSP_Check_Bounds (Instr => I_WPOPJ,
                              Delta_Words => 0, 
                              Is_Save => false, 
                              OK => OK, 
                              Primary_Fault => Primary_Fault, 
                              Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "... Stack Fault trapped by WPOPJ");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            end if;
            Have_Set_PC := true; -- we've set PC


         when I_WPSH =>
            First := Natural(I.Acs);
            Last := Natural(I.Acd);
            if Last < First then Last := Last + 4; end if;
            for This_AC in First .. Last loop
               if CPU.Debug_Logging then
                  Loggers.Debug_Print (Debug_Log, "... WPSH pushing AC" & AC_Circle(This_AC)'Image);
               end if;   
               WS_Push (CPU, CPU.AC(AC_Circle(This_AC)));
            end loop;
            Set_OVR (CPU, false);
            WSP_Check_Overflow ( 
                           I_WPSH,
                           OK => OK, 
                           Primary_Fault => Primary_Fault, 
                           Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WPSH");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            end if;
          
         when I_WRTN => 
            CPU.WSP := CPU.WFP;
            WS_Pop (CPU, DW);                  -- 1
            CPU.Carry := Test_DW_Bit (DW, 0);
            CPU.PC := Phys_Addr_T(DW and 16#7fff_ffff#);
            WS_Pop (CPU, CPU.AC(3));           -- 2
            CPU.WFP := Phys_Addr_T(CPU.AC(3));
            WS_Pop (CPU, CPU.AC(2));           -- 3
            WS_Pop (CPU, CPU.AC(1));           -- 4
            WS_Pop (CPU, CPU.AC(0));           -- 5
            WS_Pop (CPU, DW);                  -- 6
            CPU.PSR := Upper_Word (DW);
            CPU.WSP := CPU.WSP - Phys_Addr_T (Shift_Left ((DW and 16#0000_7fff#), 1));
            Have_Set_PC := true; -- We've set PC

         when I_WSAVR | I_WSAVS =>
            Req_Space := Integer(Word_To_Integer_16(I.Word_2));
            WSP_Check_Bounds (Instr => I_WSAVR,
                              Delta_Words => (Req_Space * 2) + 12, 
                              Is_Save => true, 
                              OK => OK, 
                              Primary_Fault => Primary_Fault, 
                              Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WSAVR/S");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            else
               DW := CPU.AC(3) and 16#7fff_ffff#;
               if CPU.Carry then
                  DW := DW or 16#8000_0000#;
               end if;
               WS_Push (CPU, CPU.AC(0));
               WS_Push (CPU, CPU.AC(1));
               WS_Push (CPU, CPU.AC(2));
               WS_Push (CPU, Dword_T(CPU.WFP));
               WS_Push (CPU, DW);
               CPU.WFP := CPU.WSP;
               CPU.AC(3) := Dword_T(CPU.WSP);
               if Req_Space > 0 then
                  CPU.WSP := CPU.WSP + Phys_Addr_T(Req_Space * 2);
               end if;
               if I.Instruction = I_WSAVR then
                  Set_OVK (CPU, false);
               else
                  Set_OVK (CPU, true);
               end if;
            end if;

         when I_WSSVR | I_WSSVS =>
            Req_Space := Integer(Word_To_Integer_16(I.Word_2));
            WSP_Check_Bounds (Instr => I_WSSVR,
                              Delta_Words => (Req_Space * 2) + 12, 
                              Is_Save => true, 
                              OK => OK, 
                              Primary_Fault => Primary_Fault, 
                              Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by WSSVR/S");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            else
               -- push the special return block
               DW := CPU.AC(3) and 16#7fff_ffff#;
               if CPU.Carry then
                  DW := DW or 16#8000_0000#;
               end if;
               WS_Push (CPU, Dword_From_Two_Words(CPU.PSR, 0));
               WS_Push (CPU, CPU.AC(0));
               WS_Push (CPU, CPU.AC(1));
               WS_Push (CPU, CPU.AC(2));
               WS_Push (CPU, Dword_T(CPU.WFP));
               WS_Push (CPU, DW);
               CPU.WFP := CPU.WSP;
               CPU.AC(3) := Dword_T(CPU.WSP);
               if Req_Space > 0 then
                  CPU.WSP := CPU.WSP + Phys_Addr_T(Req_Space * 2);
               end if;
               if I.Instruction = I_WSSVR then
                  Set_OVK (CPU, false);
                  Set_OVR (CPU, false);
               else
                  Set_OVK (CPU, true);
                  Set_OVR (CPU, false);
               end if;
            end if;

         when I_XPEF =>
            Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
            WS_Push (CPU, Dword_T(Addr));
            Set_OVR (CPU, false);
            WSP_Check_Overflow ( 
                           I_XPEF,
                           OK => OK, 
                           Primary_Fault => Primary_Fault, 
                           Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by XPEF");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            end if;

         when I_XPEFB =>
            Addr := Resolve_15bit_Disp (CPU, false, I.Mode, I.Disp_15, I.Disp_Offset);
            Addr := Shift_Left(Addr, 1);
            if I.Low_Byte then
               Addr := Addr + 1;
            end if;
            WS_Push (CPU, Dword_T(Addr));
            Set_OVR (CPU, false);
            WSP_Check_Overflow ( 
                           I_XPEFB,
                           OK => OK, 
                           Primary_Fault => Primary_Fault, 
                           Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by XPEFB");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
               Have_Set_PC := true; -- We have set the PC
            end if;

         when I_XPSHJ =>
            WS_Push (CPU, Dword_T(CPU.PC) + 2);
            WSP_Check_Overflow (
                              I_XPSHJ,
                              OK => OK, 
                              Primary_Fault => Primary_Fault, 
                              Secondary_Fault => Secondary_Fault);
            if not OK then
               Loggers.Debug_Print(Debug_Log, "Stack Fault trapped by XPSHJ");
               WSP_Handle_Fault (Ring, I.Instr_Len, Primary_Fault, Secondary_Fault);
            else
               Addr := Resolve_15bit_Disp (CPU, I.Ind, I.Mode, I.Disp_15, I.Disp_Offset);
               Addr := (Addr and 16#0fff_ffff#) or Ring;
               CPU.PC := Addr;
            end if;
            Have_Set_PC := true; -- We have set the PC

         when others =>
            Put_Line ("ERROR: EAGLE_STACK instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented");
            raise Execution_Failure with "ERROR: EAGLE_STACK instruction " & To_String(I.Mnemonic) & 
                        " not yet implemented";
      end case;
      if CPU.Debug_Logging then
         Loggers.Debug_Print (Debug_Log, "... After  - WSB: " & Dword_To_String (Dword_T(CPU.WSB), Octal, 11, true) &
                                       " WSP: " & Dword_To_String (Dword_T(CPU.WSP), Octal, 11, true) &
                                       " WSL: " & Dword_To_String (Dword_T(CPU.WSL), Octal, 11, true)  &
                                       " WFP: " & Dword_To_String (Dword_T(CPU.WFP), Octal, 11, true));
      end if;
      if not Have_Set_PC then
         CPU.PC := CPU.PC + Phys_Addr_T(I.Instr_Len);
      end if;

   end Do_Eagle_Stack;

 end Processor.Eagle_Stack_P;