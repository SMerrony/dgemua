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

with Interfaces; use Interfaces;

with DG_Types; use DG_Types;

package CPU is

   Model_No : constant Word_T :=
     16#224C#; -- => MV/10000 according to p.2-19 of AOS/VS Internals
   Microcode_Rev : constant Byte_T := 16#04#;

   type Acc_T is array (0 .. 3) of Dword_T;
   type FPacc_T is array (0 .. 3) of Float;
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
      SCP_IO            : Boolean; -- True if console I/O is directed to the SCP
   end record;

   -- Data sent to the Status_Monitor
   type CPU_Monitor_Rec is record
      PC                       : Phys_Addr_T; -- 32-bit PC
      AC                       : Acc_T;       -- 4 x 32-bit Accumulators
      Carry, ATU, ION          : Boolean;     -- flag bits
      Instruction_Count        : Unsigned_64;
   end record;

   protected Actions is
      procedure Init;
      procedure Reset;
      function  Disassemble_Range( Low_Addr, High_Addr : Phys_Addr_T) return String;
      procedure Set_OVR (New_OVR : in Boolean);
      procedure Set_SCP_IO (SCP_IO : in Boolean);
      function  Get_Status return CPU_Monitor_Rec;
   private
      CPU : CPU_T;
   end Actions;

   task Status_Sender is
        entry Start;
   end Status_Sender;

end CPU;
