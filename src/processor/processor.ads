-- Copyright ©2021,2022,2024 Stephen Merrony
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

with CPU_Instructions; use CPU_Instructions;
with Decoder;          use Decoder;
with DG_Types; use DG_Types;

package Processor is

   Model_No : constant Word_T :=
      -- 16#111C#; -- MV/4000 
      -- 16#224C#; -- => MV/10000 according to p.2-19 of AOS/VS Internals
      -- 16#21FC#; -- MV/9500
      16#247C#; -- FIXME fake model_no from code examination...
   Microcode_Rev : constant Byte_T := 16#04#;


   type DW_Acc_T  is array (AC_ID) of Dword_T;
   type PA_Acc_T  is array (AC_ID) of Phys_Addr_T;
   type I32_Acc_T is array (AC_ID) of Integer_32;
   type U32_Acc_T is array (AC_ID) of Unsigned_32;
   type Wd_Acc_T  is array (AC_ID) of Word_T;
      for Wd_Acc_T'Component_Size use 32;
   type FPacc_T is array (AC_ID) of Long_Float;
   -- TODO SBR_T is currently an abstraction of the Segment Base Registers - may need to represent physically
   -- via a 32-bit DWord in the future
   type SBR_Phys_Addr is mod 2**19;
   type SBR_T is record
      V, Len, LEF, IO : Boolean;
      Physical_Addr   : SBR_Phys_Addr; -- 19-bit !
   end record;
   type SBRs is array (0 .. 7) of SBR_T;

   type AC_Types is (DW, PA, I32, U32, WD); 

   type CPU_Rec (Option : AC_Types := AC_Types'First) is record
      PC                       : Phys_Addr_T; -- 32-bit PC
      PSR                      : Word_T;      -- Processor Status Register - see PoP 2-11 & A-4
      FPAC                     : FPacc_T;     -- 4 x 64-bit Floating Point Acs N.B Not same internal fmt as DG
      FPSR                     : Qword_T;     -- 64-bit Floating-Point Status Register
      Carry, ATU, ION, PF_Flag : Boolean;     -- flag bits
      SBR                      : SBRs;        -- Segment Base Registers (see above)
      SR                       : Word_T;      -- Not sure about this... fake Switch Register
      WFP, WSP, WSL, WSB       : Phys_Addr_T; -- Active Wide Stack values
      -- emulator internals
      XCT_Mode          : Boolean;
      XCT_Opcode        : Word_T;
      Debug_Logging     : Boolean;
      Instruction_Count : Unsigned_64;
      case Option is
         when DW  => AC        : DW_Acc_T;  -- 4 x 32-bit Accumulators as Double-Words
         when PA  => AC_PA     : PA_Acc_T;  -- The same as Physical Adddresses
         when I32 => AC_I32    : I32_Acc_T; -- The same as 32-bit (signed) integers
         when U32 => AC_U32    : U32_Acc_T; -- The same as 32-bit unsigned integers
         when WD  => AC_Wd     : Wd_Acc_T;  -- The same as 16-bit (unsigned) Words natrrow-aligned
      end case;
   end record;
   pragma Unchecked_Union (CPU_Rec);

   type CPU_T is access CPU_Rec;

   -- PSR bits
   PSR_OVK  : constant Integer := 0;
   PSR_OVR  : constant Integer := 1;
   PSR_IRES : constant Integer := 2;
   PSR_IXCT : constant Integer := 3;
   PSR_FFP  : constant Integer := 4;

   -- FPU SR bits
   FPSR_Any : constant Integer := 0;
   FPSR_Ovr : constant Integer := 1;
   FPSR_Unf : constant Integer := 2;
   FPSR_Inv : constant Integer := 3;
   FPSR_Mof : constant Integer := 4;
   FPSR_Te  : constant Integer := 5;
   FPSR_Z   : constant Integer := 6;
   FPSR_N   : constant Integer := 7;  
   FPSR_Rnd : constant Integer := 8;
   FPSR_Par : constant Integer := 22;          

   -- Data sent to the Status_Monitor
   type CPU_Monitor_Rec is record
      PC                       : Phys_Addr_T; -- 32-bit PC
      AC                       : DW_Acc_T;       -- 4 x 32-bit Accumulators
      Carry, ATU, ION          : Boolean;     -- flag bits
      Instruction_Count        : Unsigned_64;
   end record;

   package BP_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Phys_Addr_T);

   type Instr_Count_T is array (Instr_Mnemonic_T range Instr_Mnemonic_T'Range) of Unsigned_64;

   function  Make return CPU_T;
   procedure Run (CPU : CPU_T;
                  Disassemble : Boolean; 
                  Radix : Number_Base_T; 
                  Breakpoints : BP_Sets.Set;
                  I_Counts : out Instr_Count_T);

   procedure VRun (CPU : CPU_T;
                   Disassemble : Boolean; 
                   Radix : Number_Base_T;
                   I_Counts : in out Instr_Count_T; 
                   Syscall_Trap : out Boolean);

   function  Disassemble_Range (Low_Addr, High_Addr : Phys_Addr_T; Radix : Number_Base_T) return String;

   procedure Reset (CPU : CPU_T);
   procedure Boot  (CPU : CPU_T; Dev : Dev_Num_T; PC : Phys_Addr_T); 
   procedure Prepare_For_Running (CPU : CPU_T);
   procedure Set_Debug_Logging   (CPU : CPU_T; OnOff : Boolean);
   procedure Single_Step (CPU : CPU_T; Radix : Number_Base_T; Disass : out Unbounded_String);
   procedure Execute     (CPU : CPU_T; Instr : Decoded_Instr_T);
   function  Get_Compact_Status    (CPU :  CPU_T; Radix : Number_Base_T) return string;
   function  Get_Instruction_Count (CPU :  CPU_T) return Unsigned_64;
   function  Get_ATU        (CPU : CPU_T) return Boolean;
   function  Get_LEF        (CPU : CPU_T; Seg : Natural) return Boolean;
   function  Get_IO         (CPU : CPU_T; Seg : Natural) return Boolean;
   function  Get_ION        (CPU : CPU_T) return Boolean;
   procedure Set_PC         (CPU : CPU_T; PC : Phys_Addr_T);
   function  Get_PC         (CPU : CPU_T) return Phys_Addr_T;
   function  Get_Status     (CPU : CPU_T) return CPU_Monitor_Rec;
   function  Get_XCT_Mode   (CPU : CPU_T) return Boolean;
   procedure Set_XCT_Mode   (CPU : CPU_T; YN : Boolean);
   function  Get_XCT_Opcode (CPU : CPU_T) return Word_T;
   procedure Set_Ac         (CPU : CPU_T; AC : AC_ID; Datum : Dword_T);
   procedure Set_N          (CPU : CPU_T; N : Boolean);
   procedure Set_Z          (CPU : CPU_T; Z : Boolean);
   function  Get_N          (CPU : CPU_T) return Boolean;
   function  Get_Z          (CPU : CPU_T) return Boolean;
   procedure Set_OVK        (CPU : CPU_T; New_OVK : Boolean);
   procedure Set_OVR        (CPU : CPU_T; New_OVR : Boolean);

   Status_Period_S : constant Duration := 1.0;

   task Status_Sender is
        entry Start (CPU : CPU_T);
   end Status_Sender;

   CPU_Halt            : exception;
   Execution_Failure   : exception;
   Indirection_Failure : exception;
   IO_Device_Error     : exception;
   Out_Of_Bounds       : exception;
   Not_Yet_Implemented : exception;

end Processor;
