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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with DG_Types; use DG_Types;

package AOSVS is

	type Args_Arr is array (0..31) of Unbounded_String;

	type PID_T is new Integer range 0 .. 255;

	PC_In_Pr          : constant Natural := 8#0574#;
	Page8_Offset      : constant Natural := 8192;
	WSFH_In_Pr        : constant Natural := Page8_Offset + 12;
	WFP_In_Pr         : constant Natural := Page8_Offset + 16;
	WSP_In_Pr         : constant Natural := Page8_Offset + 18;
	WSL_In_Pr         : constant Natural := Page8_Offset + 20;
	WSB_In_Pr         : constant Natural := Page8_Offset + 22;

    type UST_T is record
        -- our representation of AOS/VS UST
        Ext_Var_Wd_Count        : Word_T;
        Ext_Var_P0_Start        : Word_T;
        Syms_Start              : Dword_T;
        Syms_End                : Dword_T;
        Debug_Addr              : Phys_Addr_T;
        Revision                : Dword_T;
        Task_Count              : Word_T;
        Impure_Blocks           : Word_T;
        Shared_Start_Block      : Dword_T; -- this seems to be a page #, not a block #
        Int_Addr                : Phys_Addr_T;
        Shared_Block_Count      : Word_T; -- this seems to be a page count, not blocks
        PR_Type                 : Word_T;
        Shared_Start_Page_In_PR : Dword_T;
    end record;

    type PR_Addrs_T is record
        -- some key addresses from the PR file
        PR_Start,
        WFP, WSB, WSFH, WSL, WSP : Phys_Addr_T;
    end record;


    procedure Start (PR_Name   : in String;
                     Virt_Root : in String;
                     Segment   : in Natural;
                     Arg_Count : in Positive;
                     Args      : in Args_Arr;
                     Logging   : in Boolean);
                 
    Invalid_PR_File : exception;

end AOSVS;