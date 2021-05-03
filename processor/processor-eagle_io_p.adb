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

with Debug_Logs;      use Debug_Logs;
with Memory_Channels; use Memory_Channels;
with Resolver;        use Resolver;

package body Processor.Eagle_IO_P is 

   procedure Do_Eagle_IO (I : in Decoded_Instr_T; CPU : in out CPU_T) is
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
            Dwd := Dwd or Mem_Size_LCPID;
            CPU.AC(0) := Dwd;

         when I_NCLID =>
            CPU.AC(0) := Dword_T(Model_No) and 16#ffff#;
            CPU.AC(1) := Dword_T(Microcode_Rev) and 16#ffff#;
            CPU.AC(2) := Dword_T(Mem_Size_NCLID) and 16#ffff#;

         when I_PRTSEL =>
            -- only handle the query mode, setting is a no-op on this 'single-channel' machine
            -- if DG_Types.Lower_Word (CPU.AC(0)) = 16#ffff# then
            --    -- return default I/O channel if -1 passed in
            --    CPU.AC(0) := 0;
            -- end if;
            NULL;

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
   end Do_Eagle_IO;

 end Processor.Eagle_IO_P;