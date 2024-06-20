-- MIT License

-- Copyright ©2022,2024 Stephen Merrony

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

with Ada.Text_IO;

with Debug_Logs;  use Debug_Logs;
with Devices.Bus;

package body Devices.DRP_SCP is

    procedure Init (Debug_Logging : Boolean) is
    begin
        Logging := Debug_Logging;
        Devices.Bus.Actions.Set_Reset_Proc    (Devices.SCP, SCP.Reset'Access);
        Devices.Bus.Actions.Set_Data_In_Proc  (Devices.SCP, SCP.Data_In'Access);
        Devices.Bus.Actions.Set_Data_Out_Proc (Devices.SCP, SCP.Data_Out'Access);
        Ada.Text_IO.Put_line ("INFO: DRP - SCP device initialised");
    end Init;

    protected body SCP is

        procedure Reset is
            begin
                Devices.Bus.States.Set_Busy (Devices.SCP, false);
                Devices.Bus.States.Set_Done (Devices.SCP, false); 
            end Reset;
                    
        procedure Data_In  (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T) is
            begin
                if ABC /= B then
                    raise Unknown_IO_Command with "SCP DIA/DIC command not supported";
                end if;
                -- Datum := Counter;
                -- Handle_Flag (IO_Flag);
            end Data_In;

        procedure Data_Out (Datum : Word_T; ABC : IO_Reg_T; IO_Flag : IO_Flag_T) is
            Cmd : Word_T;
        begin
            if ABC = B then
                Cmd := Get_W_Bits (Word => Datum, First_Bit => 1, Num_Bits => 5);
                if Logging then
                    Loggers.Debug_Print (Debug_Log, "DEBUG: SCP got command #: " & 
                        Word_To_String (WD => Cmd, Base => Octal, Width => 4, Zero_Pad => True));
                end if;
                Ada.Text_IO.Put_Line ("INFO: Ignoring SCP subcommand #" &
                                      Word_To_String (WD => Cmd, Base => Octal, Width => 4, Zero_Pad => True));
                Devices.Bus.States.Set_Done (Devices.SCP, True); 
                -- TODO send interrupt
            elsif ABC /= N then -- N for NIOC
                raise Unknown_IO_Command with "SCP command not supported";
            end if;
            -- Handle_Flag (IO_Flag);
        end Data_Out;

    end SCP;

end Devices.DRP_SCP;