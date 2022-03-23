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

-- Here we are emulating the disk6239 device, specifically model 6239/6240
-- controller/drive combination with 14-inch platters which provide 592MB of formatted capacity.
--
-- All communication with the drive is via CPU PIO instructions and memory
-- accessed via the BMC interface running at 2.2MB/sec in mapped or physical mode.
-- There is also a small set of flags and pulses shared between the controller and the CPU.
--
-- ASYNCHRONOUS interrupts occur on completion of a CB (list), or when an error
-- occurs during CB processing.
--
-- SYNCHRONOUS interrupts occur after a PIO command executes.
--
-- N.B. Assembler mnemonic: 'DSKP', AOS/VS mnemonic: 'DPJ'

with Ada.Direct_IO;

with Interfaces;    use Interfaces;

with DG_Types;      use DG_Types;

package Devices.Disk6239 is

    -- Disk characteristics
	Surfaces_Per_Disk  : constant Integer := 8;
	Heads_Per_Surface  : constant Integer := 2;
	Sectors_Per_Track  : constant Integer := 75;
	Words_Per_Sector   : constant Integer := 256;
	Bytes_Per_Sector   : constant Integer := Words_Per_Sector * 2;
	Physical_Cylinders : constant Integer := 981;
	User_Cylinders     : constant Integer := 978;
	Logical_Blocks     : constant Integer := 1157952; -- ??? 1147943 17<<16 | 43840
	-- Logical_Blocks_H   : constant Integer := LogicalBlocks >> 16;
	-- Logical_Blocks_L   : constant Integer := LogicalBlocks & 0x0ffff;
	Ucode_Rev          : constant Integer := 99;

	Max_Queued_CBs     : constant Integer:= 30; -- See p.2-13

	Int_Inf_Blk_Size   : constant Integer:= 8;
	Ctrlr_Inf_Blk_Size : constant Integer:= 2;
	Unit_Inf_Blk_Size  : constant Integer:= 7;
	Cb_Max_Size        : constant Integer:= 21;
	Cb_Min_Size        : constant Integer:= 10;

	Async_Stat_Retry_Interval_ms : constant Integer:= 1;

	Stat_Xec_State_Resetting  : constant Integer := 16#00#;
	Stat_Xec_State_Reset_Done : constant Integer := 16#01#;
	Stat_Xec_State_Begun      : constant Integer := 16#08#;
	Stat_Xec_State_Mapped     : constant Integer := 16#0c#;
	Stat_Xec_State_Diag_Mode  : constant Integer := 16#04#;

	Stat_Ccs_Async          : constant Integer := 0;
	Stat_Ccs_Pio_Inv_Cmd    : constant Integer := 1;
	Stat_Ccs_Pio_Cmd_Failed : constant Integer := 2;
	Stat_Ccs_Pio_Cmd_Ok     : constant Integer := 3;

	Stat_Async_No_Errors    : constant Integer := 5;

	-- disk6239 PIO Command Set
	Pio_Prog_Load        : constant Integer := 8#000#;
	Pio_Begin            : constant Integer := 8#002#;
	Pio_Sysgen           : constant Integer := 8#025#;
	Diag_Mode            : constant Integer := 8#024#;
	Set_Mapping          : constant Integer := 8#026#;
	Get_Mapping          : constant Integer := 8#027#;
	Set_Interface        : constant Integer := 8#030#;
	Get_Interface        : constant Integer := 8#031#;
	Set_Controller       : constant Integer := 8#032#;
	Get_Controller       : constant Integer := 8#033#;
	Set_Unit             : constant Integer := 8#034#;
	Get_Unit             : constant Integer := 8#035#;
	Get_ExtendedStatus_0 : constant Integer := 8#040#;
	Get_ExtendedStatus_1 : constant Integer := 8#041#;
	Get_ExtendedStatus_2 : constant Integer := 8#042#;
	Get_ExtendedStatus_3 : constant Integer := 8#043#;
	Start_List           : constant Integer := 8#0100#;
	Start_List_Hp        : constant Integer := 8#0103#;
	Restart              : constant Integer := 8#0116#;
	Cancel_List          : constant Integer := 8#0123#;
	Unit_Status          : constant Integer := 8#0131#;
	Trespass             : constant Integer := 8#0132#;
	Get_List_Status      : constant Integer := 8#0133#;
	Pio_Reset            : constant Integer := 8#0777#;

	--//  CB Command Set/OpCodes
	Cb_Op_No_Op               : constant Integer := 8#0#;
	Cb_Op_Write               : constant Integer := 8#0100#;
	Cb_Op_Write_Verify        : constant Integer := 8#0101#;
	Cb_Op_Write_1_Word        : constant Integer := 8#0104#;
	Cb_Op_Write_Verify_1_Word : constant Integer := 8#0105#;
	Cb_Op_Write_Mod_Bitmap    : constant Integer := 8#0142#;
	Cb_Op_Read                : constant Integer := 8#0200#;
	Cb_Op_Read_Verify         : constant Integer := 8#0201#;
	Cb_Op_Read_Verify_1_Word  : constant Integer := 8#0205#;
	Cb_Op_Read_Raw_Data       : constant Integer := 8#0210#;
	Cb_Op_Read_Headers        : constant Integer := 8#0220#;
	Cb_Op_Read_Mod_Bitmap     : constant Integer := 8#0242#;
	Cb_Op_Recalibrate_Disk    : constant Integer := 8#0400#;

	-- disk6239 CB field offsets
	Cb_Link_Addr_High        : constant Integer := 0;
	Cb_Link_Addr_Low         : constant Integer := 1;
	Cb_Ina_Flags_Opcode      : constant Integer := 2;
	Cb_Pageno_List_Addr_High : constant Integer := 3;
	Cb_Pageno_List_Addr_Low  : constant Integer := 4;
	Cb_Txfer_Addr_High       : constant Integer := 5;
	Cb_Txfer_Addr_Low        : constant Integer := 6;
	Cb_Dev_Addr_High         : constant Integer := 7;
	Cb_Dev_Addr_Low          : constant Integer := 8;
	Cb_Unit_No               : constant Integer := 9;
	Cb_Txfer_Count           : constant Integer := 10;
	Cb_Cb_Status             : constant Integer := 11;
	Cb_Res_1                 : constant Integer := 12;
	Cb_Res_2                 : constant Integer := 13;
	Cb_Err_Status            : constant Integer := 14;
	Cb_Unit_Status           : constant Integer := 15;
	Cb_Retries_Done          : constant Integer := 16;
	Cb_Soft_Rtn_Txfer_Count  : constant Integer := 17;
	Cb_Phy_sCyl              : constant Integer := 18;
	Cb_Phys_Head_Sect        : constant Integer := 19;
	Cb_DiskErr_Code          : constant Integer := 20;

	--Mapping bits
	Map_Slot_Load_Ints : constant Word_T := 2#1000_0000_0000_0000#;
	Map_Int_Bmc_Phys   : constant Word_T := 2#0100_0000_0000_0000#;
	Map_Upstream_Load  : constant Word_T := 2#0010_0000_0000_0000#;
	Map_Upstream_Hpt   : constant Word_T := 2#0001_0000_0000_0000#;

    -- calculated consts

	-- PhysicalByteSize is the total  # bytes on a disk6239-type disk
	Physical_Byte_Size : constant Integer := Surfaces_Per_Disk * 
                                             Heads_Per_Surface * 
                                             Sectors_Per_Track * 
                                             Bytes_Per_Sector * 
                                             Physical_Cylinders;

	-- PhysicalBlockSize is the total # blocks on a disk6239-type disk
	Physical_Block_Size : constant Integer := Surfaces_Per_Disk * 
                                              Heads_Per_Surface * 
                                              Sectors_Per_Track * 
                                              Physical_Cylinders;

    type Sector is array (0 .. Words_Per_Sector - 1) of Word_T;
    package Sector_IO is new Ada.Direct_IO (Sector);
	use Sector_IO;

    type Int_Inf_Block_T  is array(0 .. Int_Inf_Blk_Size-1)   of Word_T; 
    type Ctrl_Inf_Block_T is array(0 .. Ctrlr_Inf_Blk_Size-1) of Word_T; 
    type Unit_Inf_Block_T is array(0 .. Unit_Inf_Blk_Size-1)  of Word_T; 

    type State_Rec is record
	   -- Emulator internals...
       Image_Attached   : Boolean;
       Image_Filename   : Unbounded_String;
       Image_File       : Sector_IO.File_Type;
	   Reads, Writes    : Unsigned_64;
	   Read_Buff, Write_Buff : Sector;
	   Debug_Logging    : Boolean;
       -- DG Data...
       Command_Reg_A, Command_Reg_B, Command_Reg_C : Word_T;
       Status_Reg_A, Status_Reg_B, Status_Reg_C    : Word_T;
       Is_Mapped                                   : Boolean;
       Mapping_Reg_A, Mapping_Reg_B                : Word_T;
       Int_Inf_Block                               : Int_Inf_Block_T;
       Ctrl_Inf_Block                              : Ctrl_Inf_Block_T;
       Unit_Inf_Block                              : Unit_Inf_Block_T;
       -- cylinder, head, sector                dg_word
       Sector_No                                   : Dword_T;
    end record;

    Status_Period_S : constant Duration := 1.0;

    type Status_Rec is record
	   Image_Attached   : Boolean;
	   Image_Filename   : Unbounded_String;
	   Status_Reg_A, Status_Reg_B, Status_Reg_C : Word_T;
       Sector_No        : Dword_T;
	   Reads, Writes    : Unsigned_64;
    end record;

    procedure Create_Blank (Image_Name : in String; OK : out Boolean);
	-- Simply creates an empty disk file of the correct size for the disk6239 emulator to use

    protected Drives is
        procedure Init (Debug_Logging : in Boolean);
        procedure Attach (Unit : in Natural; Image_Name : in String; OK : out Boolean);
        function  Get_Status return Status_Rec;

    private
        State : State_Rec;
    end Drives;

    task Status_Sender is
        entry Start;
    end Status_Sender;

    Not_Yet_Implemented : exception;

end Devices.Disk6239;   