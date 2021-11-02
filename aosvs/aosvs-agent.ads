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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Direct_IO;
with Ada.Strings.Hash;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Sockets;

with Memory; use Memory;
with PARU_32;

package AOSVS.Agent is

	-- type PID_T is new Integer range 0 .. 255;
	type PIDS_Arr is array (PID_T) of Boolean;
	type TIDs_Arr is array (1..32) of Boolean;

	type Per_Process_Data_T is record
	-- the data Agent holds for each process
        PR_Name             : Unbounded_String;
        Num_Invocation_Args : Natural;
        Invocation_Args     : Args_Arr;
		Working_Directory   : Unbounded_String;
        Sixteen_Bit         : Boolean;
        Proc_Name           : Unbounded_String;
		User_Name           : Unbounded_String;
        Console             : GNAT.Sockets.Stream_Access;
        TIDs_In_Use         : TIDs_Arr;
		Superuser_On        : Boolean;
	end record;
	type PPD_Arr is array (PID_T) of Per_Process_Data_T;

	-- Terminal Device characteristics
	type Chars_Arr is array (1..3) of Word_T;
	package Characteristics_Maps is new Ada.Containers.Indefinite_Hashed_Maps (
		Key_Type => String, 
		Element_Type => Chars_Arr,
		Hash         => Ada.Strings.Hash,
		Equivalent_Keys => "=");

	Default_Chars : constant Chars_Arr := (
		PARU_32.CST or PARU_32.CSFF or PARU_32.COTT or PARU_32.CEB0, -- 8-col tabs, Form-feeds, default echoing
		PARU_32.CULC or PARU_32.CRT3 or PARU_32.cwrp or PARU_32.cfkt or PARU_32.cnnl,  -- Upper and lower case, D200 style, wraparound, Fn keys are delimiters
		16#18_50# 	 -- 24x80 chars
	);

	-- Shared Page I/O
	type Page_T is array  (0 .. 1023) of Word_T; -- 4 disk blocks, 2kB
	type Block_T is array (0 .. 255) of Word_T;  -- a disk block is 512B or 256W
	package Block_IO is new Ada.Direct_IO (Block_T);
	package Direct_IO is new Ada.Direct_IO (Byte_T);
	type Block_Arr_T is array (Natural range <>) of Block_T;

	-- File channels
	type Agent_Channel_T is record
	   Opener_PID  : PID_T;
	   Path        : Unbounded_String;
	   Is_Console  : Boolean;
	   Read, Write,
	   For_Shared  : Boolean;
	   Dynamic, Data_Sens, Fixed_Length, Var_Length, Undefined, IBM_VB
	   			   : Boolean;
	   Rec_Len     : Natural;
	   Con         : GNAT.Sockets.Stream_Access;
	   Echo        : Boolean;
	   File_Direct : Direct_IO.File_Type;
	   File_Shared : Block_IO.File_Type;
	end record;
	type Agent_Channel_Arr is array (1 .. 128) of Agent_Channel_T;

	-- IPCs
	type Agent_IPC_T is record
		Owner_PID   : PID_T;
		Name        : Unbounded_String;
		Local_Port  : Word_T;
		Global_Port : Dword_T;
		File_Type   : Word_T;
		-- may need spool here...
	end record;
	package IPC_Maps is new Ada.Containers.Indefinite_Hashed_Maps (
		Key_Type => String, 
		Element_Type => Agent_IPC_T,
		Hash         => Ada.Strings.Hash,
		Equivalent_Keys => "=");

	protected Actions is
		procedure Init (Cons       : in GNAT.Sockets.Stream_Access;
						Virt_Root  : in String);

		-- Process and Task supporting subprograms...

		procedure Allocate_PID (PR_Name         : in Unbounded_String;
								Num_Invocation_Args : in Natural;
								Invocation_Args : in Args_Arr;
								Working_Dir     : in Unbounded_String;
								Sixteen_Bit     : in Boolean;
								Proc_Name       : in Unbounded_String;
								User_Name       : in Unbounded_String;
								PID             : out PID_T);
	    procedure Allocate_TID (PID : in PID_T; TID : out Word_T);
		function Get_Proc_Name (PID : in PID_T) return String;
		function Get_Virtual_Root return Unbounded_String;

		-- System Call supporting subprograms...
        -- file I/O...
		procedure File_Open (PID     : in Word_T; 
							 Path    : in String;
							 Options, File_Type : in Word_T;
							 Rec_Len : in Integer;
							 Chan_No : out Word_T;
							 Err     : out Word_T);
		procedure File_Close (Chan_No : in Natural; Err : out Word_T);
	    function  Get_Device_For_Channel(Chan_No : in Word_T) return Unbounded_String;
		procedure File_Read (Chan_No : in Word_T;
                              Is_Extended,
                              Is_Absolute,
                              Is_Dynamic,
                              Is_DataSens : in Boolean;
                              Rec_Len     : in Integer;
                              Bytes       : in out Byte_Arr_T;
                              Transferred : out Word_T;
                              Err         : out Word_T); 
		procedure File_Write (Chan_No : in Word_T;
							  Defaults,
							  Is_Extended,
							  Is_Absolute,
							  Is_Dynamic,
							  Is_DataSens : in Boolean;
							  Rec_Len     : in Integer;
							  Bytes_BA    : in Dword_T;
							  Position    : in Integer;
							  Transferred : out Word_T;
							  Err         : out Word_T);
		-- CLI environment...
		function Get_Nth_Arg   (PID : in Word_T; Arg_Num : in Word_T) return Unbounded_String;
		function Get_Num_Args  (PID : in Word_T) return Natural;
		function Get_PR_Name   (PID : in Word_T) return Unbounded_String;
		function Get_User_Name (PID : in Word_T) return Unbounded_String;
		function Get_Working_Directory (PID : in Word_T) return String;
		function Get_Superuser (PID : in Word_T) return Boolean;
		procedure Set_Superuser (PID : in Word_T; SU : in Boolean);

		-- terminal I/O...
		procedure Get_Default_Chars (Device : in Unbounded_String;
									 WD_1, WD_2, WD_3 : out Word_T);
		procedure Get_Current_Chars (Device : in Unbounded_String;
									 WD_1, WD_2, WD_3 : out Word_T);
		procedure Send_Msg (Dest_PID : in Word_T; Msg : in String; Send_PID : in Word_T);

		-- IPCs...
		procedure I_Lookup (PID : in Word_T; Filename : in String;
							Glob_Port : out Dword_T; 
							F_Type : out Word_T;
							Err    : out Word_T);
		procedure I_Create (PID : in Word_T; Filename : in String; Local_Port : in Word_T;
		                    Err : out Word_T);
							
		-- Shared Files...
		procedure Shared_Open (PID : in PID_T; S_Path : in String; Read_Only : in Boolean;
							   Chan_No : out Word_T;
							   Err     : out Word_T);
		procedure Shared_Read (PID : in PID_T;
							   Chan_No : in Natural;
							   Base_Addr : in Phys_Addr_T;
							   Num_Pages : in Natural;
							   Start_Block : in Natural;
							   Page_Arr    : in out Page_Arr_T;
							   Err         : out Word_T);

	private
		Virtual_Root     : Unbounded_String;
		PIDs_In_Use      : PIDs_Arr;
		Per_Process_Data : PPD_Arr;
		Console          : GNAT.Sockets.Stream_Access;
		Agent_Chans      : Agent_Channel_Arr;
		Device_Chars     : Characteristics_Maps.Map; -- should probably be per-Process
		IPCs			 : IPC_Maps.Map;
	end Actions;

	Channel_Not_Open,
	NO_MORE_CHANNELS,
	NO_MORE_TIDS,
	No_Such_Argument,
	Not_Yet_Implemented    : exception;


end AOSVS.Agent;
