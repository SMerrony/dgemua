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

with Debug_Logs;  use Debug_Logs;
with Devices;     use Devices;
with Devices.Bus; use Devices.Bus;

package body Processor.Nova_IO_P is 
   procedure Do_Nova_IO (I : in Decoded_Instr_T; CPU : in out CPU_T) is
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
                  
                  when I_DOC =>
                     if I.Ac = 0 then
                        raise CPU_Halt;
                     else
                        raise Execution_Failure with "CPU I/O not yet implemented";
                     end if;

                  when others =>
                     raise Execution_Failure with "CPU I/O not yet implemented";
               end case;
            else
               if Bus.Actions.Is_Connected(I.IO_Dev) and Bus.Actions.Is_IO_Dev(I.IO_Dev) then
                  if I.IO_Dir = Data_In then
                        Devices.Bus.Actions.Data_In(I.IO_Dev, I.IO_Reg, I.IO_Flag, Datum);
                        CPU.AC(I.Ac) := Dword_T(Datum);
                     else
                        Datum := DG_Types.Lower_Word (CPU.AC(I.Ac));
                        Devices.Bus.Actions.Data_Out(I.IO_Dev, Datum, I.IO_Reg, I.IO_Flag);
                  end if;
               else
                  if (I.IO_Dev = 2) OR (I.IO_Dev = 10) OR (I.IO_Dev = 11) then
                     Loggers.Debug_Print(Debug_Log, "WARNING: Ignoring I/O to device " & I.IO_Dev'Image);
                     Put_Line("WARNING: Ignoring I/O to device " & I.IO_Dev'Image & ".");
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
                  Put_Line ("WARNING: Ignoring SKP instruction to device " & I.IO_Dev'Image & ".");
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

   end Do_Nova_IO;
 end Processor.Nova_IO_P;