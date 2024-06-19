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

-- The DRP_SCP models parts of the Diagnostic Remote Processor and System Console Processor.
-- A lot of the real functionality is actually provided elsewhere in the emulator, notably in
-- the main Mvemua procedure.
-- This device just provides some of the I/O instruction support.
package Devices.DRP_SCP is


    procedure Init (Debug_Logging : in Boolean);

    protected SCP is
        procedure Reset;
        procedure Data_In  (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T);
        -- Handle DIA instruction
        procedure Data_Out (Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T);
        -- Handle DOA and NIOC instructions
    end SCP;

    Logging : Boolean := False;
    
    Unknown_IO_Command : exception;

end Devices.DRP_SCP;
