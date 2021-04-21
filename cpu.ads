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

package CPU is

   Model_No : constant Word_T :=
     16#224C#; -- => MV/10000 according to p.2-19 of AOS/VS Internals
   Microcode_Rev : constant Byte_T := 16#04#;


   type Acc_T is array (AC_ID) of Dword_T;
   type FPacc_T is array (AC_ID) of Float;
   -- TODO SBR_T is currently an abstraction of the Segment Base Registers - may need to represent physically
   -- via a 32-bit DWord in the future
   type SBR_Phys_Addr is mod 2**19;
   type SBR_T is record
      V, Len, LEF, IO : Boolean;
      Physical_Addr   : SBR_Phys_Addr; -- 19-bit !
   end record;
   type SBRs is array (0 .. 7) of SBR_T;

   type CPU_T is record
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
      Debug_Logging     : Boolean;
      Instruction_Count : Unsigned_64;
      -- SCP_IO            : Boolean;

   end record;

   -- Data sent to the Status_Monitor
   type CPU_Monitor_Rec is record
      PC                       : Phys_Addr_T; -- 32-bit PC
      AC                       : Acc_T;       -- 4 x 32-bit Accumulators
      Carry, ATU, ION          : Boolean;     -- flag bits
      Instruction_Count        : Unsigned_64;
   end record;

   package BP_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Phys_Addr_T);

   type Instr_Count_T is array (Instr_Mnemonic_T range Instr_Mnemonic_T'Range) of Unsigned_64;

   protected Actions is
      procedure Reset;
      procedure Boot (Dev : Dev_Num_T; PC : Phys_Addr_T);
      procedure Prepare_For_Running;
      procedure Set_Debug_Logging (OnOff : in Boolean);
      procedure Single_Step (Radix : in Number_Base_T; Disass : out Unbounded_String);
      procedure Execute (Instr : in Decoded_Instr_T);
      function  Disassemble_Range (Low_Addr, High_Addr : Phys_Addr_T; Radix : Number_Base_T) 
         return String;
      procedure Set_OVK (New_OVK : in Boolean);
      procedure Set_OVR (New_OVR : in Boolean);
      function  Get_Compact_Status (Radix : Number_Base_T) return string;
      function  Get_Instruction_Count return Unsigned_64;
      function  Get_ATU return Boolean;
      function  Get_LEF (Seg : in Natural) return Boolean;
      function  Get_IO (Seg : in Natural) return Boolean;
      function  Get_PC return Phys_Addr_T;
      function  Get_Status return CPU_Monitor_Rec;
      -- procedure Set_SCP_IO (SCP : in Boolean);
      -- function  Get_SCP_IO return Boolean;
   private
      CPU : CPU_T;
   end Actions;

   procedure Init;
   procedure Run (Disassemble : in Boolean; 
                  Radix : in Number_Base_T; 
                  Breakpoints : in BP_Sets.Set;
                  I_Counts : out Instr_Count_T);

   task Status_Sender is
        entry Start;
   end Status_Sender;

   Execution_Failure : exception;
   Indirection_Failure : exception;
   IO_Device_Error     : exception;
   Out_Of_Bounds       : exception;
   Not_Yet_Implemented : exception;

end CPU;
