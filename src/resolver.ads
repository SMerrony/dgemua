-- Copyright ©2021,2022 Stephen Merrony
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

with Interfaces; use Interfaces;

with Decoder;   use Decoder;
with DG_Types;  use DG_Types;
with Processor; use Processor;

package Resolver is

    function Resolve_8bit_Disp (CPU      : CPU_T; 
                                Indirect : Boolean; Mode : Mode_T;
                                Disp15   : Integer_16)  return Phys_Addr_T;

    function Resolve_Eclipse_15bit_Disp (CPU         : CPU_T;
                                         Indirect    : Boolean; 
                                         Mode        : Mode_T;
                                         Disp15      : Integer_16;
                                         Disp_Offset : Integer_32) return Phys_Addr_T; 
                                         
    function Resolve_15bit_Disp (CPU         : CPU_T;
                                 Indirect    : Boolean; 
                                 Mode        : Mode_T;
                                 Disp15      : Integer_16;
                                 Disp_Offset : Integer_32) return Phys_Addr_T; 

    function Resolve_31bit_Disp (CPU         : CPU_T;
                                 Indirect    : Boolean; 
                                 Mode        : Mode_T;
                                 Disp        : Integer_32;
                                 Disp_Offset : Integer_32) return Phys_Addr_T;

    function Resolve_32bit_Indirectable_Addr (ATU    : Boolean; 
                                              I_Addr : Dword_T) return Phys_Addr_T;
    
    procedure Resolve_Eagle_Bit_Addr (CPU       : CPU_T; 
                                      Acd, Acs  : AC_ID; 
                                      Word_Addr : out Phys_Addr_T; 
                                      Bit_Num   : out Natural);
                                      
    procedure Resolve_Eclipse_Bit_Addr (CPU       : CPU_T; 
                                        Acd, Acs  : AC_ID; 
                                        Word_Addr : out Phys_Addr_T; 
                                        Bit_Num   : out Natural);

    Illegal_Address, 
    Not_Yet_Implemented : exception;  

end Resolver;
