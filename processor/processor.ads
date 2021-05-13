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

with Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

with CPU_Instructions; use CPU_Instructions;
with Decoder;          use Decoder;
with Devices;
with DG_Types; use DG_Types;
with Memory;   use Memory;

package Processor is

   Model_No : constant Word_T :=
     -- 16#224C#; -- => MV/10000 according to p.2-19 of AOS/VS Internals
     16#21FC#; -- MV/9500
   Microcode_Rev : constant Byte_T := 16#04#;


   type Acc_T is array (AC_ID) of Dword_T;
   type FPacc_T is array (AC_ID) of Long_Float;
   -- TODO SBR_T is currently an abstraction of the Segment Base Registers - may need to represent physically
   -- via a 32-bit DWord in the future
   type SBR_Phys_Addr is mod 2**19;
   type SBR_T is record
      V, Len, LEF, IO : Boolean;
      Physical_Addr   : SBR_Phys_Addr; -- 19-bit !
   end record;
   type SBRs is array (0 .. 7) of SBR_T;

   type CPU_Rec is tagged record
      PC                       : Phys_Addr_T; -- 32-bit PC
      AC                       : Acc_T;       -- 4 x 32-bit Accumulators
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
   end record;

   type CPU_T is access CPU_Rec;

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
      AC                       : Acc_T;       -- 4 x 32-bit Accumulators
      Carry, ATU, ION          : Boolean;     -- flag bits
      Instruction_Count        : Unsigned_64;
   end record;

   package BP_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Phys_Addr_T);

   type Instr_Count_T is array (Instr_Mnemonic_T range Instr_Mnemonic_T'Range) of Unsigned_64;

   function  Make return CPU_T;
   procedure Run (CPU : in out CPU_T;
                  Disassemble : in Boolean; 
                  Radix : in Number_Base_T; 
                  Breakpoints : in BP_Sets.Set;
                  I_Counts : out Instr_Count_T);

   procedure VRun (CPU : in out CPU_T;
                   Disassemble : in Boolean; 
                   Radix : in Number_Base_T;
                   I_Counts : in out Instr_Count_T; 
                   Syscall_Trap : out Boolean);

   function  Disassemble_Range (Low_Addr, High_Addr : Phys_Addr_T; Radix : Number_Base_T) return String;

   procedure Reset (CPU : in out CPU_T);
   procedure Boot  (CPU : in out CPU_T; Dev : Dev_Num_T; PC : Phys_Addr_T);
   procedure Prepare_For_Running (CPU : in out CPU_T);
   procedure Set_Debug_Logging   (CPU : in out CPU_T; OnOff : in Boolean);
   procedure Single_Step (CPU : in out CPU_T; Radix : in Number_Base_T; Disass : out Unbounded_String);
   procedure Execute     (CPU : in out CPU_T; Instr : in Decoded_Instr_T);
   function  Get_Compact_Status    (CPU : in CPU_T; Radix : Number_Base_T) return string;
   function  Get_Instruction_Count (CPU : in CPU_T) return Unsigned_64;
   function  Get_ATU        (CPU : in CPU_T) return Boolean;
   function  Get_LEF        (CPU : in CPU_T; Seg : in Natural) return Boolean;
   function  Get_IO         (CPU : in CPU_T; Seg : in Natural) return Boolean;
   function  Get_ION        (CPU : in CPU_T) return Boolean;
   procedure Set_PC         (CPU : in out CPU_T; PC : in Phys_Addr_T);
   function  Get_PC         (CPU : in CPU_T) return Phys_Addr_T;
   function  Get_Status     (CPU : in CPU_T) return CPU_Monitor_Rec;
   function  Get_XCT_Mode   (CPU : in CPU_T) return Boolean;
   procedure Set_XCT_Mode   (CPU : in out CPU_T; YN : in Boolean);
   function  Get_XCT_Opcode (CPU : in CPU_T) return Word_T;
   procedure Set_Ac         (CPU : in out CPU_T; AC : in AC_ID; Datum : in Dword_T);
   procedure Set_N          (CPU : in out CPU_T; N : in Boolean);
   procedure Set_Z          (CPU : in out CPU_T; Z : in Boolean);


   task Status_Sender is
        entry Start (CPU : in CPU_T);
   end Status_Sender;

   CPU_Halt            : exception;
   Execution_Failure   : exception;
   Indirection_Failure : exception;
   IO_Device_Error     : exception;
   Out_Of_Bounds       : exception;
   Not_Yet_Implemented : exception;

end Processor;
