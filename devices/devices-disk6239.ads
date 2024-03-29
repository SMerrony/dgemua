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

package Devices.Disk6239 is

    -- Disk characteristics
	Surfaces_Per_Disk  : constant Integer := 8;
	Heads_Per_Surface  : constant Integer := 2;
	Sectors_Per_Track  : constant Integer := 75;
	Words_Per_Sector   : constant Integer := 256;
	Bytes_Per_Sector   : constant Integer  := Words_Per_Sector * 2;
	Physical_Cylinders : constant Integer := 981;
	User_Cylinders     : constant Integer  := 978;
	Logical_Blocks     : constant Integer := 1157952; -- ??? 1147943 17<<16 | 43840
	Logical_Blocks_H   : constant Word_T  := 17;
	Logical_Blocks_L   : constant Word_T  := 43840;
	Ucode_Rev          : constant Word_T  := 99;

	Max_Queued_CBs     : constant Word_T:= 30; -- See p.2-13

	Int_Inf_Blk_Size   : constant Integer:= 8;
	Ctrlr_Inf_Blk_Size : constant Integer:= 2;
	Unit_Inf_Blk_Size  : constant Integer:= 7;
	CB_Max_Size        : constant Integer:= 21;
	CB_Min_Size        : constant Integer:= 10;

	Async_Stat_Retry_Interval : constant Duration := 1.0 / 1000; -- 1ms

	Stat_Xec_State_Resetting  : constant Byte_T := 16#00#;
	Stat_Xec_State_Reset_Done : constant Byte_T := 16#01#;
	Stat_Xec_State_Begun      : constant Byte_T := 16#08#;
	Stat_Xec_State_Mapped     : constant Byte_T := 16#0c#;
	Stat_Xec_State_Diag_Mode  : constant Byte_T := 16#04#;

	Stat_Ccs_Async          : constant Byte_T := 0;
	Stat_Ccs_Pio_Inv_Cmd    : constant Byte_T := 1;
	Stat_Ccs_Pio_Cmd_Failed : constant Byte_T := 2;
	Stat_Ccs_Pio_Cmd_Ok     : constant Byte_T := 3;

	Stat_Async_No_Errors    : constant Byte_T := 5;

	-- disk6239 PIO Command Set
	Pio_Prog_Load        : constant Word_T := 8#000#;
	Pio_Begin            : constant Word_T := 8#002#;
	Pio_Sysgen           : constant Word_T := 8#025#;
	Diag_Mode            : constant Word_T := 8#024#;
	Set_Mapping          : constant Word_T := 8#026#;
	Get_Mapping          : constant Word_T := 8#027#;
	Set_Interface        : constant Word_T := 8#030#;
	Get_Interface        : constant Word_T := 8#031#;
	Set_Controller       : constant Word_T := 8#032#;
	Get_Controller       : constant Word_T := 8#033#;
	Set_Unit             : constant Word_T := 8#034#;
	Get_Unit             : constant Word_T := 8#035#;
	Get_ExtendedStatus_0 : constant Word_T := 8#040#;
	Get_ExtendedStatus_1 : constant Word_T := 8#041#;
	Get_ExtendedStatus_2 : constant Word_T := 8#042#;
	Get_ExtendedStatus_3 : constant Word_T := 8#043#;
	Start_List           : constant Word_T := 8#0100#;
	Start_List_Hp        : constant Word_T := 8#0103#;
	Restart              : constant Word_T := 8#0116#;
	Cancel_List          : constant Word_T := 8#0123#;
	Unit_Status          : constant Word_T := 8#0131#;
	Trespass             : constant Word_T := 8#0132#;
	Get_List_Status      : constant Word_T := 8#0133#;
	Pio_Reset            : constant Word_T := 8#0777#;

	--//  CB Command Set/OpCodes
	CB_Op_No_Op               : constant Word_T := 8#0#;
	CB_Op_Write               : constant Word_T := 8#0100#;
	CB_Op_Write_Verify        : constant Word_T := 8#0101#;
	CB_Op_Write_1_Word        : constant Word_T := 8#0104#;
	CB_Op_Write_Verify_1_Word : constant Word_T := 8#0105#;
	CB_Op_Write_Mod_Bitmap    : constant Word_T := 8#0142#;
	CB_Op_Read                : constant Word_T := 8#0200#;
	CB_Op_Read_Verify         : constant Word_T := 8#0201#;
	CB_Op_Read_Verify_1_Word  : constant Word_T := 8#0205#;
	CB_Op_Read_Raw_Data       : constant Word_T := 8#0210#;
	CB_Op_Read_Headers        : constant Word_T := 8#0220#;
	CB_Op_Read_Mod_Bitmap     : constant Word_T := 8#0242#;
	CB_Op_Recalibrate_Disk    : constant Word_T := 8#0400#;

	-- disk6239 CB field offsets
	CB_Link_Addr_High        : constant Integer := 0;
	CB_Link_Addr_Low         : constant Integer := 1;
	CB_Ina_Flags_Opcode      : constant Integer := 2;
	CB_Pageno_List_Addr_High : constant Integer := 3;
	CB_Pageno_List_Addr_Low  : constant Integer := 4;
	CB_Txfer_Addr_High       : constant Integer := 5;
	CB_Txfer_Addr_Low        : constant Integer := 6;
	CB_Dev_Addr_High         : constant Integer := 7;
	CB_Dev_Addr_Low          : constant Integer := 8;
	CB_Unit_No               : constant Integer := 9;
	CB_Txfer_Count           : constant Integer := 10;
	CB_CB_Status             : constant Integer := 11;
	CB_Res_1                 : constant Integer := 12;
	CB_Res_2                 : constant Integer := 13;
	CB_Err_Status            : constant Integer := 14;
	CB_Unit_Status           : constant Integer := 15;
	CB_Retries_Done          : constant Integer := 16;
	CB_Soft_Rtn_Txfer_Count  : constant Integer := 17;
	CB_Phy_sCyl              : constant Integer := 18;
	CB_Phys_Head_Sect        : constant Integer := 19;
	CB_DiskErr_Code          : constant Integer := 20;

	--Mapping bits
	Map_Slot_Load_Ints : constant Word_T := 2#1000_0000_0000_0000#;
	Map_Int_Bmc_Phys   : constant Word_T := 2#0100_0000_0000_0000#;
	Map_Upstream_Load  : constant Word_T := 2#0010_0000_0000_0000#;
	Map_Upstream_Hpt   : constant Word_T := 2#0001_0000_0000_0000#;

	type Stat_Reg_T is (A, B, C);

	type CB_T is array(0 .. CB_Max_Size-1) of Word_T;

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
	   Debug_Logging    : Boolean;
       -- DG Data...
       Command_Reg_A, Command_Reg_B, Command_Reg_C : Word_T;
       Status_Reg_A, Status_Reg_B, Status_Reg_C    : Word_T;
       Is_Mapped                                   : Boolean;
       Mapping_Reg_A, Mapping_Reg_B                : Word_T;
       Int_Inf_Block                               : Int_Inf_Block_T;
       Ctrl_Inf_Block                              : Ctrl_Inf_Block_T;
       Unit_Inf_Block                              : Unit_Inf_Block_T;
    end record;

    Status_Period_S : constant Duration := 1.0;

    type Status_Rec is record
	   Image_Attached   : Boolean;
	   Image_Filename   : Unbounded_String;
	   Status_Reg_A, Status_Reg_B, Status_Reg_C : Word_T;
       Sector_No        : Dword_T;
	   Reads, Writes    : Unsigned_64;
    end record;

    procedure Create_Blank (Image_Name : String; OK : out Boolean);
	-- Simply creates an empty disk file of the correct size for the disk6239 emulator to use
    procedure Init (Debug_Logging : Boolean);

    protected Drives is
        procedure Data_In (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T);
        -- Data_In services the DIA/B/C I/O instructions
        procedure Data_Out (Datum : Word_T; ABC : IO_Reg_T; IO_Flag : IO_Flag_T);
        -- Data_Out services the DOA/B/C I/O instructions
		function  Get_Extended_Status_Size return Natural;
		procedure Set_Debug_Logging (TF : Boolean);
		procedure Set_Status_Reg (Reg : Stat_Reg_T; Contents : Word_T);
		-- procedure Write_Sector (Buffer : Sector);
        procedure Reset;

    private
        function  Extract_PIO_Command (Wd : Word_T) return Word_T;
        procedure Handle_Flag (IO_Flag : IO_Flag_T);
        -- Handle the flag or Pulse
        procedure Do_PIO_Command;
		-- procedure Process_CB_Chain (CB_Addr : Phys_Addr_T);

		procedure Set_PIO_Status_Reg_C (Status, CCS : Byte_T; Cmd_Echo : Word_T; RR : Boolean);
        -- Set the SYNCHRONOUS standard refturn as per p.3-22

        State : State_Rec;
        -- The protected State record holds the current state of the emulated disk
    end Drives;

	task CB_Processor is
		entry Start (Debug_Logging : Boolean);
		entry Attach (Unit : Natural; Image_Name : String; OK : out Boolean);
		entry Get_Stats (S : out Status_Rec);
		entry Program_Load;
		-- Read the 1st sector off the disk and put at the start of physical memory
		entry Process (Start_CB_Addr : Phys_Addr_T);
	end CB_Processor;

    task Status_Sender is
        entry Start;
    end Status_Sender;

    Not_Yet_Implemented : exception;

end Devices.Disk6239;   