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

with DG_Types;   use DG_Types;
with Interfaces; use Interfaces;

package Memory is

    Mem_Size_Words : constant Integer := 8_388_608;

    -- BMC/DCH Stuff...
    Num_BMC_Regs        : constant Integer := 2_048;
    First_DCH_Slot_Reg  : constant Integer := Num_BMC_Regs;
    First_DCH_Slot      : constant Integer := Num_BMC_Regs / 2;
    Num_DCH_Regs        : constant Integer := 1_024;
    Num_DCH_Slots       : constant Integer := Num_DCH_Regs / 2;
    Last_Reg            : constant Integer := 4_095;
    IO_Chan_Def_Reg     : constant Integer := 8#6000#; -- 3072.
    IO_Chan_Status_Reg  : constant Integer := 8#7700#; -- 4032.
    IO_Chan_Mask_Reg    : constant Integer := 8#7701#; -- 4033.
    CPU_Dedication_Ctrl : constant Integer := 8#7702#; -- 4034.

    IOC_CDR_ICE  : constant Word_T := 2#1000_0000_0000_0000#;
    IOC_CDR_BVE  : constant Word_T := 2#0001_0000_0000_0000#;
    IOC_CDR_DVE  : constant Word_T := 2#0000_1000_0000_0000#;
    IOC_CDR_DCH  : constant Word_T := 2#0000_0100_0000_0000#;
    IOC_CDR_BMC  : constant Word_T := 2#0000_0010_0000_0000#;
    IOC_CDR_BAP  : constant Word_T := 2#0000_0001_0000_0000#;
    IOC_CDR_BDP  : constant Word_T := 2#0000_0000_1000_0000#;
    IOC_CDR_DME  : constant Word_T := 2#0000_0000_0000_0010#;
    IOC_CDR_1    : constant Word_T := 2#0000_0000_0000_0001#;

    IOC_SR_ERR : constant Word_T := 2#1000_0000_0000_0000#;
	IOC_SR_DTO : constant Word_T := 2#0000_0000_0010_0000#;
	IOC_SR_MPE : constant Word_T := 2#0000_0000_0001_0000#;
	IOC_SR_1A  : constant Word_T := 2#0000_0000_0000_1000#;
	IOC_SR_1B  : constant Word_T := 2#0000_0000_0000_0100#;
	IOC_SR_CMB : constant Word_T := 2#0000_0000_0000_0010#;
	IOC_SR_INT : constant Word_T := 2#0000_0000_0000_0001#;

	IOC_MR_MK0 : constant Word_T := 2#0000_0000_1000_0000#;
	IOC_MR_MK1 : constant Word_T := 2#0000_0000_0100_0000#;
	IOC_MR_MK2 : constant Word_T := 2#0000_0000_0010_0000#;
	IOC_MR_MK3 : constant Word_T := 2#0000_0000_0001_0000#;
	IOC_MR_MK4 : constant Word_T := 2#0000_0000_0000_1000#;
	IOC_MR_MK5 : constant Word_T := 2#0000_0000_0000_0100#;
	IOC_MR_MK6 : constant Word_T := 2#0000_0000_0000_0010#;

    type BMC_DCH_Regs_Array is array (0 .. Last_Reg) of Word_T;

    type RAM_Array is array (0 .. Mem_Size_Words - 1) of Word_T;

    protected RAM is
        procedure Init (Debug_Logging : in Boolean);
        function Read_Word (Word_Addr : in Phys_Addr_T) return Word_T;
        procedure Write_Word (Word_Addr : in Phys_Addr_T; Datum : Word_T);
    private
        RAM         : RAM_Array;
        ATU_Enabled : Boolean;
        Is_Logging  : Boolean;
    end RAM;

    protected BMC_DCH is
        procedure Init (Debug_Logging : in Boolean);
        procedure Reset;
    private
        Registers  : BMC_DCH_Regs_Array;
        Is_Logging : Boolean;
    end BMC_DCH;

    procedure Clear_W_Bit (Word : in out Word_T; Bit_Num : in Integer);
    procedure Set_W_Bit (Word : in out Word_T; Bit_Num : in Integer);  
    function Test_W_Bit (Word : in Word_T; Bit_Num : in Integer) return Boolean;
    function Get_Lower_Byte (Word : in Word_T) return Byte_T;

end Memory;
