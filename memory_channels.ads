--  Copyright ©2021 - 2024 Stephen Merrony
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

with DG_Types;   use DG_Types;

package Memory_Channels is

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

    IOC_CDR_ICE : constant Word_T := 2#1000_0000_0000_0000#;
    IOC_CDR_BVE : constant Word_T := 2#0001_0000_0000_0000#;
    IOC_CDR_DVE : constant Word_T := 2#0000_1000_0000_0000#;
    IOC_CDR_DCH : constant Word_T := 2#0000_0100_0000_0000#;
    IOC_CDR_BMC : constant Word_T := 2#0000_0010_0000_0000#;
    IOC_CDR_BAP : constant Word_T := 2#0000_0001_0000_0000#;
    IOC_CDR_BDP : constant Word_T := 2#0000_0000_1000_0000#;
    IOC_CDR_DME : constant Word_T := 2#0000_0000_0000_0010#;
    IOC_CDR_1   : constant Word_T := 2#0000_0000_0000_0001#;

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

    -- These are from the S140 Prog Ref, not PARU...
    SR_ERROR    : constant Word_T := 2#1000_0000_0000_0000#;
    SR_DUMP     : constant Word_T := 2#0100_0000_0000_0000#;
    SR_DIAG     : constant Word_T := 2#0010_0000_0000_0000#;
    SR_VAL_ERR  : constant Word_T := 2#0001_0000_0000_0000#;
    SR_ADDR_ERR : constant Word_T := 2#0000_0001_0000_0000#;    
    SR_BMC      : constant Word_T := 2#0000_0000_0000_0001#;            

    type BMC_DCH_Regs_Array is array (0 .. Last_Reg) of Word_T;

    type BMC_Addr_T is record
        Is_Logical : Boolean;
        -- Physical...
        Bk  : Natural;      -- Bank Selector
        XCA : Natural;      -- eXtended Channel Addr
        CA  : Phys_Addr_T;  -- Channel Addr
        -- Logical...
        TT  : Natural;      -- Translation Table
        TTR : Natural;      -- TT Register
        P_Low : Phys_Addr_T; -- Page Low Order
    end record;

    protected BMC_DCH is
        procedure Init (Debug_Logging : Boolean);
        procedure Reset;
        procedure Data_In  (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T);
        -- Handle DIx instruction
        -- procedure Data_Out (Datum : Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T);
        -- Handle DOx and NIOC instructions
        procedure Set_Logging (Debug_Logging : Boolean);
        function  Read_Reg (Reg : Integer) return Word_T;
        procedure Read_Word_BMC_Chan (Unmapped : in out Phys_Addr_T; Datum : out Word_T);
        procedure Read_Word_BMC_16 (Unmapped : in out Word_T; Datum : out Word_T);
        procedure Write_Reg (Reg : Integer; Datum : Word_T); 
        procedure Write_Slot (Slot : Integer; Datum : Dword_T);
        procedure Write_Word_BMC_Chan (Unmapped : in out Phys_Addr_T; Datum : Word_T);
        procedure Write_Word_DCH_Chan (Unmapped : in out Phys_Addr_T; Datum : Word_T);
        procedure Write_Word_BMC_16 (Unmapped : in out Word_T; Datum : Word_T);
    private
        function Decode_BMC_Addr (Unmapped : Phys_Addr_T) return BMC_Addr_T;
        function Get_DCH_Mode return Boolean;
        function Resolve_BMC_Mapped_Addr (M_Addr : Phys_Addr_T) return Phys_Addr_T;
        function Resolve_DCH_Mapped_Addr (M_Addr : Phys_Addr_T) return Phys_Addr_T;

        Registers  : BMC_DCH_Regs_Array;
        Status_Reg : Word_T;
        Is_Logging : Boolean;
    end BMC_DCH;

    Invalid_DCH_Slot,     
    Not_Yet_Implemented,
    Unsupported_IO_Channel : exception;

end Memory_Channels;
