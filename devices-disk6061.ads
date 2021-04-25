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

with Ada.Direct_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;       use Interfaces; 

with DG_Types;   use DG_Types;

package Devices.Disk6061 is

   Surfaces_Per_Disk  : constant Integer := 19;
   Cylinders_Per_Disk : constant Integer := 815; -- i.e. Tracks
   Sectors_Per_Track  : constant Integer := 24;
   Words_Per_Sector   : constant Integer := 256;
   Bytes_Per_Sector   : constant Integer := Words_Per_Sector * 2;
   Sectors_Per_Disk   : constant Integer := Surfaces_Per_Disk * Sectors_Per_Track * Cylinders_Per_Disk;
   Physical_Byte_Size : constant Integer := Sectors_Per_Disk * Bytes_Per_Sector;

	-- Cmd_T vals are in numeric order - do not change!
    type Cmd_T is (Read, Recal, Seek, Stop, Offset_Fwd, Offset_Rev, Write_Disable, 
                    Release, Trespass, Set_Alt_Mode_1, Set_Alt_Mode_2, No_Op, 
                    Verify, Read_Buffers, Write, Format);

    type Ins_Mode is (Normal, Alt_1, Alt_2);

	Drive_Stat_Drive_Fault  : Word_T := 2#0000_0000_0000_0001#;
	Drive_Stat_Write_Fault  : Word_T := 2#0000_0000_0000_0010#;
	Drive_Stat_Clock_Fault  : Word_T := 2#0000_0000_0000_0100#;
	Drive_Stat_Posn_Fault   : Word_T := 2#0000_0000_0000_1000#;
	Drive_Stat_Pack_Unsafe  : Word_T := 2#0000_0000_0001_0000#;
	Drive_Stat_Power_Fault  : Word_T := 2#0000_0000_0010_0000#;
	Drive_Stat_Illegal_Cmd  : Word_T := 2#0000_0000_0100_0000#;
	Drive_Stat_Invalid_Addr : Word_T := 2#0000_0000_1000_0000#;
	Drive_Stat_Unused       : Word_T := 2#0000_0001_0000_0000#;
	Drive_Stat_Write_Dis    : Word_T := 2#0000_0010_0000_0000#;
	Drive_Stat_Offset       : Word_T := 2#0000_0100_0000_0000#;
	Drive_Stat_Busy         : Word_T := 2#0000_1000_0000_0000#;
	Drive_Stat_Ready        : Word_T := 2#0001_0000_0000_0000#;
	Drive_Stat_Trespassed   : Word_T := 2#0010_0000_0000_0000#;
	Drive_Stat_Reserved     : Word_T := 2#0100_0000_0000_0000#;
	Drive_Stat_Invalid      : Word_T := 2#1000_0000_0000_0000#;

    RW_Stat_RW_Fault        : Word_T := 2#0000_0000_0000_0001#;
	RW_Stat_Late            : Word_T := 2#0000_0000_0000_0010#;
	RW_Stat_RW_Timeout      : Word_T := 2#0000_0000_0000_0100#;
	RW_Stat_Verify          : Word_T := 2#0000_0000_0000_1000#;
	RW_Stat_Surf_Sect       : Word_T := 2#0000_0000_0001_0000#;
	RW_Stat_Cylinder        : Word_T := 2#0000_0000_0010_0000#;
	RW_Stat_Bad_sector      : Word_T := 2#0000_0000_0100_0000#;
	RW_Stat_ECC             : Word_T := 2#0000_0000_1000_0000#;
	RW_Stat_Illegal_Sector  : Word_T := 2#0000_0001_0000_0000#;
	RW_Stat_Parity          : Word_T := 2#0000_0010_0000_0000#;
	RW_Stat_Drive_3_Done    : Word_T := 2#0000_0100_0000_0000#;
	RW_Stat_Drive_2_Done    : Word_T := 2#0000_1000_0000_0000#;
	RW_Stat_Drive_1_Done    : Word_T := 2#0001_0000_0000_0000#;
	RW_Stat_Drive_0_Done    : Word_T := 2#0010_0000_0000_0000#;
	RW_Stat_RW_Done         : Word_T := 2#0100_0000_0000_0000#;
	RW_Stat_Control_Full    : Word_T := 2#1000_0000_0000_0000#;

    type Sector is array (0 .. Words_Per_Sector - 1) of Word_T;

    package Sector_IO is new Ada.Direct_IO (Sector);
	use Sector_IO;

    type State_Rec is record
	   -- Emulator internals...
       Image_Attached   : Boolean;
       Image_Filename   : Unbounded_String;
       Image_File       : Sector_IO.File_Type;
	   Reads, Writes    : Unsigned_64;
	   Read_Buff, Write_Buff : Sector;
	   Debug_Logging    : Boolean;
	   -- DG Data...
	   Cmd_Drv_Addr     : Byte_T;
	   Command          : Word_T;
	   Drive            : Word_T;
	   Map_Enabled      : Boolean;
	   Mem_Addr         : Word_T;
	   EMA              : Word_T;
	   Cylinder         : Word_T;
	   Surface, Sector  : Word_T;                 
	   Sector_Cnt       : Integer_8;
	   ECC              : Dword_T;
	   Drive_Status     : Word_T;
	   RW_Status        : Word_T;
	   Instruction_Mode : Ins_Mode;
	   Last_DOA_Was_Seek : Boolean;
    end record;

	-- the data reported to the status collector
	type Status_Rec is record
	   Image_Attached   : Boolean;
	   Image_Filename   : Unbounded_String;
	   Cylinder         : Word_T;
	   Surface, Sector  : Word_T;
	   Reads, Writes    : Unsigned_64;
	end record;

	protected Drives is
		procedure Init (Debug_Logging : in Boolean);
		procedure Reset;
		procedure Attach (Unit : in Natural; Image_Name : in String; OK : out Boolean);
		procedure Data_In (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T);
        procedure Data_Out (Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T);
		function  Get_Status return Status_Rec;
		procedure Load_DKBT;
	private
		State : State_Rec;
	end Drives;

	procedure Create_Blank (Image_Name : in String; OK : out Boolean);

    task Status_Sender is
        entry Start;
    end Status_Sender;

	Not_Yet_Implemented : exception;

end Devices.Disk6061;
