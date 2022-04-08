-- MIT License

-- Copyright ©2022 Stephen Merrony

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

package body Devices.PSC_UPSC is

    procedure Init (Debug_Logging : in Boolean) is
    begin
        Logging := Debug_Logging;
        Devices.Bus.Actions.Set_Reset_Proc    (Devices.PSC, PSC.Reset'Access);
        Devices.Bus.Actions.Set_Data_In_Proc  (Devices.PSC, PSC.Data_In'Access);
        Devices.Bus.Actions.Set_Data_Out_Proc (Devices.PSC, PSC.Data_Out'Access);
        Ada.Text_IO.Put_line ("INFO: (U)PSC device initialised");
    end Init;

    protected body PSC is

        procedure Reset is
        begin
            Devices.Bus.States.Set_Busy (Devices.PSC, false);
            Devices.Bus.States.Set_Done (Devices.PSC, false); 
        end Reset;

        procedure Handle_Flag (IO_Flag : in IO_Flag_T) is
        begin
            case IO_Flag is
                when S | P =>
                    Devices.Bus.States.Set_Busy (Devices.PSC, true);
                    Devices.Bus.States.Set_Done (Devices.PSC, false); 
                when C =>
                    Devices.Bus.States.Set_Busy (Devices.PSC, false);
                    Devices.Bus.States.Set_Done (Devices.PSC, false);               
                when None =>
                    null;
            end case;
        end Handle_Flag;

        procedure Data_In  (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T) is
        begin
            if ABC /= A then
                -- raise Unknown_IO_Command with "PSC DIB/DIC command not supported";
                Ada.Text_IO.Put_line ("WARNING: DIB/DIC from (U)PSC is a No-Op");
            else
                case Read_Command is
                    when Read_Control =>
                        Datum := 2#00000000_00000010#; -- BBU disabled
                    when Read_BBU =>
                        Datum := 0; -- no BBU
                    when Read_Status =>
                        Datum := 0; -- running normally
                    when Read_Fault_Code =>
                        Datum := 0; -- no fault
                end case;
                -- Datum := Counter;
                Handle_Flag (IO_Flag);
            end if;
        end Data_In;

        procedure Data_Out (Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T) is
            Reg : Word_T;
        begin
            if ABC = A then
                case IO_Flag is
                    when S =>
                        -- DOAS - Write Data to (U)PSC
                        Reg := Get_W_Bits (Word => Datum, First_Bit => 24, Num_Bits => 2);
                        case Reg is
                            when 0 =>
                                Last_Reg_Addressed := Control;
                            when 1 =>
                                Last_Reg_Addressed := Power;
                            when 2 =>
                                Last_Reg_Addressed := Reserved;
                            when 3 =>
                                Last_Reg_Addressed := Diagnostic;
                                if Test_W_Bit(Datum, 14) then
                                    BTE := True;
                                end if;
                                if Test_W_Bit(Datum, 15) then
                                    Comp := True;
                                end if;
                            when others =>
                                raise Internal_Error with "(U)PSC Impossible DOAS register specified";
                        end case;
                        Handle_Flag (S);
                        -- TODO send interrupt
                    when P =>
                        raise Not_Yet_Implemented with "(U)PSC DOAP";
                    when others =>
                        -- raise Unknown_IO_Command with "(U)PSC DOA or DOAC command not supported";
                        Ada.Text_IO.Put_line ("WARNING: DOA with no flag to (U)PSC is a No-Op");
                end case;                
            elsif ABC /= N then -- N for NIOC
                --raise Unknown_IO_Command with "PSC command not supported";
                Ada.Text_IO.Put_line ("WARNING: DOB/DOC to (U)PSC is a No-Op");
            end if;
            -- Handle_Flag (IO_Flag);
        end Data_Out;

    end PSC;

end Devices.PSC_UPSC;
