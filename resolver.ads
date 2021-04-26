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

with Decoder;   use Decoder;
with DG_Types;  use DG_Types;
with Processor; use Processor;

package Resolver is

    function Resolve_8bit_Disp (CPU      : in CPU_T; 
                                Indirect : in Boolean; Mode : in Mode_T;
                                Disp15   : in Integer_16)  return Phys_Addr_T;

    function Resolve_15bit_Disp (CPU         : in CPU_T;
                                 Indirect    : in Boolean; 
                                 Mode        : in Mode_T;
                                 Disp15      : in Integer_16;
                                 Disp_Offset : in Natural) return Phys_Addr_T; 

    function Resolve_31bit_Disp (CPU         : in CPU_T;
                                 Indirect    : in Boolean; 
                                 Mode        : in Mode_T;
                                 Disp        : in Integer_32;
                                 Disp_Offset : in Natural) return Phys_Addr_T;

    function Resolve_32bit_Indirectable_Addr (ATU : in Boolean; 
                                              I_Addr : in Dword_T) return Phys_Addr_T;

    procedure Resolve_Eclipse_Bit_Addr (CPU       : in CPU_T; 
                                        Acd, Acs  : in AC_ID; 
                                        Word_Addr : out Phys_Addr_T; 
                                        Bit_Num   : out Natural);

    Not_Yet_Implemented : exception;  

end Resolver;
