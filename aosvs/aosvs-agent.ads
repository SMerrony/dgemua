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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Direct_IO;
with Ada.Strings.Hash;
-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

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
		Searchlist			: Unbounded_String;
        Sixteen_Bit         : Boolean;
        Proc_Name           : Unbounded_String;
		User_Name           : Unbounded_String;
        Console             : GNAT.Sockets.Stream_Access;
        TIDs_In_Use         : TIDs_Arr;
		Superuser_On        : Boolean;
		Default_ACL         : Unbounded_String;
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
		PARU_32.CST or PARU_32.CEB0 or PARU_32.COTT, -- 8-col tabs, Form-feeds, default echoing
		PARU_32.CRT3 or PARU_32.cfkt or PARU_32.CWRP,  -- Upper and lower case, D200 style, wraparound, Fn keys are delimiters
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
	type Agent_Channel_Arr is array (0 .. 122) of Agent_Channel_T;

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
		procedure Init (Cons       : GNAT.Sockets.Stream_Access;
						Virt_Root  : String);

		-- Process and Task supporting subprograms...

		procedure Allocate_PID (PR_Name         : Unbounded_String;
								Num_Invocation_Args : Natural;
								Invocation_Args : Args_Arr;
								Working_Dir     : Unbounded_String;
								Sixteen_Bit     : Boolean;
								Proc_Name       : Unbounded_String;
								User_Name       : Unbounded_String;
								PID             : out PID_T);
	    procedure Allocate_TID (PID : PID_T; TID : out Word_T);
		function Get_Proc_Name (PID : PID_T) return String;
		function Get_Virtual_Root return Unbounded_String;

		-- System Call supporting subprograms...
        -- file I/O...
		procedure File_Open (PID     : Word_T; 
							 Path    : String;
							 Options, File_Type : Word_T;
							 Rec_Len : Integer;
							 Chan_No : out Word_T;
							 Err     : out Word_T);
		procedure File_Close (Chan_No : Natural; Err : out Word_T);
		function  Get_Default_ACL (PID : PID_T) return String;
	    function  Get_Device_For_Channel(Chan_No : Word_T) return Unbounded_String;
		procedure File_Read (Chan_No : Word_T;
                              Is_Extended,
                              Is_Absolute,
                              Is_Dynamic,
                              Is_DataSens : Boolean;
                              Rec_Len     : Integer;
                              Bytes       : out Byte_Arr_T;
                              Transferred : out Word_T;
                              Err         : out Word_T); 
		procedure File_Write (Chan_No : Word_T;
							  Defaults,
							  Is_Extended,
							  Is_Absolute,
							  Is_Dynamic,
							  Is_DataSens : Boolean;
							  Rec_Len     : Integer;
							  Bytes_BA    : Dword_T;
							  Position    : Integer;
							  Transferred : out Word_T;
							  Err         : out Word_T);
		-- CLI environment...
		function Get_Nth_Arg   (PID : Word_T; Arg_Num : Word_T) return Unbounded_String;
		function Get_Num_Args  (PID : Word_T) return Natural;
		function Get_PR_Name   (PID : Word_T) return Unbounded_String;
		function Get_User_Name (PID : Word_T) return Unbounded_String;
		function Get_Working_Directory (PID : Word_T) return String;
		function Get_Searchlist (PID : PID_T) return Unbounded_String; 
		function Get_Superuser (PID : Word_T) return Boolean;
		procedure Set_Superuser (PID : Word_T; SU : Boolean);

		-- terminal I/O...
		procedure Get_Default_Chars (Device : Unbounded_String;
									 WD_1, WD_2, WD_3 : out Word_T);
		procedure Get_Current_Chars (Device : Unbounded_String;
									 WD_1, WD_2, WD_3 : out Word_T);
		procedure Send_Msg (Dest_PID : Word_T; Msg : String; Send_PID : Word_T);

		-- IPCs...
		procedure I_Lookup (PID : Word_T; Filename : String;
							Glob_Port : out Dword_T; 
							F_Type : out Word_T;
							Err    : out Word_T);
		procedure I_Create (PID : Word_T; Filename : String; Local_Port : Word_T;
		                    Err : out Word_T);
							
		-- Shared Files...
		procedure Shared_Open (PID : PID_T; S_Path : String; Read_Only : Boolean;
							   Chan_No : out Word_T;
							   Err     : out Word_T);
		procedure Shared_Read (PID : PID_T;
							   Chan_No : Natural;
							   Base_Addr : Phys_Addr_T;
							   Num_Pages : Natural;
							   Start_Block : Natural;
							   Page_Arr    : out Page_Arr_T;
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
	IO_Error,
	NO_MORE_CHANNELS,
	NO_MORE_TIDS,
	No_Such_Argument,
	Not_Yet_Implemented    : exception;


end AOSVS.Agent;
