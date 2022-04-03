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

with Devices.Bus;

package body Devices.PIT_P is

    procedure Init is
    begin
        Devices.Bus.Actions.Set_Reset_Proc    (Devices.PIT, Timer.Reset'Access);
        Devices.Bus.Actions.Set_Data_In_Proc  (Devices.PIT, Timer.Data_In'Access);
        Devices.Bus.Actions.Set_Data_Out_Proc (Devices.PIT, Timer.Data_Out'Access);
        Ticker.Start;
    end Init;

    protected body Timer is
        
        procedure Reset is
        begin
            Running := False;
            Devices.Bus.States.Set_Busy (Devices.PIT, false);
            Devices.Bus.States.Set_Done (Devices.PIT, false);
            Initial_Count_Register := 0;
            Counter := 0;    
        end Reset;

        procedure Handle_Flag (IO_Flag : in IO_Flag_T) is
        begin
            case IO_Flag is
                when C =>
                    Devices.Bus.States.Set_Busy (Devices.PIT, false);
                    Devices.Bus.States.Set_Done (Devices.PIT, false);
                    Devices.Bus.States.Clear_Interrupt (Devices.PIT);
                    Running := False;
                when P =>
                    null;
                when S =>
                    Devices.Bus.States.Set_Busy (Devices.PIT, true);
                    Devices.Bus.States.Set_Done (Devices.PIT, false);
                    Devices.Bus.States.Clear_Interrupt (Devices.PIT);
                    Counter := Initial_Count_Register;
                    Running := True;
                when None =>
                    null;
            end case;
        end Handle_Flag;        

        procedure Data_In  (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T) is
        begin
            if ABC /= A then
                raise Unknown_IO_Command with "PIT command not supported";
            end if;
            Datum := Counter;
            Handle_Flag (IO_Flag);
        end Data_In;

        procedure Data_Out (Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T) is
        begin
            if ABC = A then
                Initial_Count_Register := Datum;
            elsif ABC /= N then -- N for NIOC
                raise Unknown_IO_Command with "PIT command not supported";
            end if;
            Handle_Flag (IO_Flag);
        end Data_Out;

        function Get_Counter return Word_T is (Counter);
        function Get_Initial_Count return Word_T is (Initial_Count_Register);
        function Get_Running return Boolean is (Running);

        procedure Set_Counter (C : in Word_T) is
        begin
            Counter := C;
        end Set_Counter;

        procedure Increment_Counter(Is_Zero : out Boolean) is
        begin
            Counter := Counter + 1;
            Is_Zero := Counter = 0;
        end Increment_Counter;

    end Timer;

    task body Ticker is
        Currently_Running : Boolean := False;
        Is_Zero : Boolean;
    begin
        accept Start do
            Ada.Text_IO.Put_line ("INFO: PIT Ticker started");
        end Start;
        loop
            if not Currently_Running and Timer.Get_Running then
                Timer.Set_Counter (Timer.Get_Initial_Count);
                Currently_Running := True;
            end if;
            if Currently_Running and not Timer.Get_Running then
                Currently_Running := False;
            end if;

            if Currently_Running then
                Timer.Increment_Counter (Is_Zero);
                if Is_Zero then
                    Devices.Bus.States.Send_Interrupt(Devices.PIT);
                end if;
            end if;

            delay Tick_Interval;

        end loop;        
    end Ticker;

end Devices.PIT_P;
