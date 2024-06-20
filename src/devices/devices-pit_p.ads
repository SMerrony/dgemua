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

with Interfaces; use Interfaces; 

with DG_Types;   use DG_Types;

package Devices.PIT_P is

    Tick_Interval : constant  Duration := 0.0001; -- 100us

    procedure Init;

    protected Timer is
        procedure Reset;
        procedure Data_In  (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T);
        -- Handle DIA instruction
        procedure Data_Out (Datum : Word_T; ABC : IO_Reg_T; IO_Flag : IO_Flag_T);
        -- Handle DOA and NIOC instructions

        function  Get_Counter return Word_T;
        function  Get_Initial_Count return Word_T;
        function  Get_Running return Boolean;
        procedure Set_Counter (C : Word_T);
        procedure Increment_Counter(Is_Zero : out Boolean);
    private
        Initial_Count_Register : Word_T;
        Counter                : Word_T;
        Running                : Boolean;
    end Timer;


    task Ticker is
        entry Start;
    end Ticker;

    Unknown_IO_Command : exception;

end Devices.PIT_P;