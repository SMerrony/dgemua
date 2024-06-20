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

-- The PSC_UPSC is a minimal emulation of the (Univeral) Power Suppy Controller.
-- This device just provides some of the I/O instruction support.

package Devices.PSC_UPSC is

    type Register is (Control, Power, Reserved, Diagnostic, Invalid);
    type Registers_T is array (Register) of Word_T;
    type Read_Commands is (Read_Control, Read_BBU, Read_Status, Read_Fault_Code);

    procedure Init (Debug_Logging : Boolean);

    protected PSC is
        procedure Reset;
        procedure Data_In  (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T);
        -- Handle DIA instruction
        procedure Data_Out (Datum : Word_T; ABC : IO_Reg_T; IO_Flag : IO_Flag_T);
        -- Handle DOA and NIOC instructions
    private
        Registers : Registers_T;
        Last_Reg_Addressed : Register := Invalid;
        Read_Command : Read_Commands := Read_Fault_Code;
        -- Diagnostic flags
        BTE, Comp : Boolean := false;
    end PSC;

    Logging : Boolean := False;
    
    Internal_Error,
    Not_Yet_Implemented,
    Unknown_IO_Command : exception;

end Devices.PSC_UPSC;