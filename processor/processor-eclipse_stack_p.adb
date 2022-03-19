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
with Resolver;    use Resolver;

package body Processor.Eclipse_Stack_P is 

   procedure Do_Eclipse_Stack (I : in Decoded_Instr_T; CPU : in out CPU_T) is
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
               Narrow_Stack.Push (Ring, CPU.AC_Wd(AC_Circle(This_AC)));
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
               RAM.Write_Word (NFP_Loc or Ring, CPU.AC_Wd(3));
               return; -- because PC has been set
            end;

         when I_SAVE =>
            declare
               NFP_Sav, NSP_Sav, Word : Word_T;
            begin
               NFP_Sav := RAM.Read_Word (NFP_Loc or Ring);
               NSP_Sav := RAM.Read_Word (NSP_Loc or Ring);
               Narrow_Stack.Push(Ring, CPU.AC_Wd(0)); -- 1
               Narrow_Stack.Push(Ring, CPU.AC_Wd(1)); -- 2
               Narrow_Stack.Push(Ring, CPU.AC_Wd(2)); -- 3 
               Narrow_Stack.Push(Ring, NFP_Sav);               -- 4
               Word := CPU.AC_Wd(3);
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
   
   end Do_Eclipse_Stack;

 end Processor.Eclipse_Stack_P;