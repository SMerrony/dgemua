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

with GNAT.Sockets;

package AOSVS.Agent is

    -- type Pseudo_Agent_Call is
    --    (Allocate_PID, 
	--     Allocate_TID, 
	-- 	Create_IPC, 
	-- 	File_Close, 
	-- 	File_Open,
    --     File_Read, 
	-- 	File_Recreate, 
	-- 	File_Write, 
	-- 	Get_Chars, 
	-- 	Get_Message, 
	-- 	Ilkup,
    --     Shared_Open, 
	-- 	Shared_Read, 
	-- 	Create_Task);

	-- type PID_T is new Integer range 0 .. 255;
	type PIDS_Arr is array (PID_T) of Boolean;
	type TIDs_Arr is array (1..32) of Boolean;

	type Per_Process_Data_T is record
	-- the data Agent holds for each process
	   Invocation_Args : Args_Arr;
	   Virtual_Root    : Unbounded_String;
	   Sixteen_Bit     : Boolean;
	   Proc_Name       : Unbounded_String;
	   Console         : GNAT.Sockets.Stream_Access;
	   TIDs_In_Use	   : TIDs_Arr;

	end record;

	type PPD_Arr is array (PID_T) of Per_Process_Data_T;

	protected Actions is
		procedure Init (Cons : in GNAT.Sockets.Stream_Access);
		procedure Allocate_PID (Invocation_Args : in Args_Arr;
								Virtual_Root    : in Unbounded_String;
								Sixteen_Bit     : in Boolean;
								Proc_Name       : in Unbounded_String;
								-- Console         : in GNAT.Sockets.Stream_Access;
								PID             : out PID_T);
	    procedure Allocate_TID (PID : in PID_T; TID : out Word_T);

	private
		PIDs_In_Use      : PIDs_Arr;
		Per_Process_Data : PPD_Arr;
		Console          : GNAT.Sockets.Stream_Access;
	end Actions;

	NO_MORE_TIDS : exception;

end AOSVS.Agent;
