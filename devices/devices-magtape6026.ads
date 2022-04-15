-- MIT License

-- Copyright ©2021,2022 Stephen Merrony

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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;       use Interfaces; 

with DG_Types;   use DG_Types;
with Simh_Tapes; use Simh_Tapes;

package Devices.Magtape6026 is

    Max_Record_Size_W : constant Integer := 16_384;
    Max_Record_Size_B : constant Integer := Max_Record_Size_W * 2;

    Cmd_Mask : constant Word_T := 16#00b8#;

    -- Cmd_Read_Bits         : constant Word_T := 16#0000#;
    -- Cmd_Rewind_Bits       : constant Word_T := 16#0008#;
    -- Cmd_Ctrl_Mode_Bits    : constant Word_T := 16#0010#;
    -- Cmd_Space_Fwd_Bits    : constant Word_T := 16#0018#;
    -- Cmd_Space_Rev_Bits    : constant Word_T := 16#0020#;
    -- Cmd_Wite_Bits         : constant Word_T := 16#0028#;
    -- Cmd_Write_EOF_Bits    : constant Word_T := 16#0030#;
    -- Cmd_Erase_Bits        : constant Word_T := 16#0038#;
    -- Cmd_Read_Nonstop_Bits : constant Word_T := 16#0080#;
    -- Cmd_Unload_Bits       : constant Word_T := 16#0088#;
    -- Cmd_Drive_Mode_Bits   : constant Word_T := 16#0090#;

    type Cmd_T is (Read,Rewind,Ctrl_Mode,Space_Fwd,Space_Rev,Write,Write_EOF,Erase,Read_Nonstop,Unload,Drive_Mode);
    type Cmd_Bits_T is array (Cmd_T range Cmd_T'Range) of Word_T;
    Commands : constant Cmd_Bits_T := (
        Read        => 16#0000#,
        Rewind      => 16#0008#,
        Ctrl_Mode   => 16#0010#,
        Space_Fwd   => 16#0018#,
        Space_Rev   => 16#0020#,
        Write       => 16#0028#,
        Write_EOF   => 16#0030#,
        Erase       => 16#0038#,
        Read_Nonstop=> 16#0080#,
        Unload      => 16#0088#,
        Drive_Mode  => 16#0090#
    );

    -- Cmd_Read         : constant Integer := 0;
    -- Cmd_Rewind       : constant Integer := 1;
    -- Cmd_Ctrl_Mode    : constant Integer := 2;
    -- Cmd_Space_Fwd    : constant Integer := 3;
    -- Cmd_Space_Rev    : constant Integer := 4;
    -- Cmd_Write        : constant Integer := 5;
    -- Cmd_Write_EOF    : constant Integer := 6;
    -- Cmd_Erase        : constant Integer := 7;
    -- Cmd_Read_Nonstop : constant Integer := 8;
    -- Cmd_Unload       : constant Integer := 9;
    -- Cmd_Drive_Mode   : constant Integer := 10;

    SR_1_Error         : constant Word_T := Shift_Left (1, 15);
    SR_1_DataLate      : constant Word_T := Shift_Left (1, 14);
    SR_1_Rewinding     : constant Word_T := Shift_Left (1, 13);
    SR_1_Illegal       : constant Word_T := Shift_Left (1, 12);
    SR_1_HiDensity     : constant Word_T := Shift_Left (1, 11);
    SR_1_DataError     : constant Word_T := Shift_Left (1, 10);
    SR_1_EOT           : constant Word_T := Shift_Left (1, 9);
    SR_1_EOF           : constant Word_T := Shift_Left (1, 8);
    SR_1_BOT           : constant Word_T := Shift_Left (1, 7);
    SR_1_9Track        : constant Word_T := Shift_Left (1, 6);
    SR_1_BadTape       : constant Word_T := Shift_Left (1, 5);
    SR_1_Reserved      : constant Word_T := Shift_Left (1, 4);
    SR_1_StatusChanged : constant Word_T := Shift_Left (1, 3);
    SR_1_WriteLock     : constant Word_T := Shift_Left (1, 2);
    SR_1_OddChar       : constant Word_T := Shift_Left (1, 1);
    SR_1_UnitReady     : constant Word_T := 1;

    SR_1_Readable : constant String := "ELRIHDEFB9TrSWOR";

    SR_2_Error   : constant Word_T := Shift_Left (1, 15);
    SR_2_PE_Mode : constant Word_T := 1;

    Max_Tapes : constant Integer := 7;

    type Att_Arr is array (0 .. Max_Tapes) of Boolean;
    type FN_Arr is array (0 .. Max_Tapes) of Unbounded_String;
    type File_Arr is array (0 .. Max_Tapes) of File_Type;
    type MT6026_Rec is record
        -- DG device state
        Mem_Addr_Reg               : Phys_Addr_T;
        Current_Cmd                : Cmd_T;
        Current_Unit               : Natural;
        Status_Reg_1, Status_Reg_2 : Word_T;
        Neg_Word_Count             : Integer_16;
        Image_Attached             : Att_Arr;
        Image_Filename             : FN_Arr;
        SIMH_File                  : File_Arr;
    end record;

    Status_Period_S : constant Duration := 2.0;

    type Status_Rec is record
        Image_Attached             : Att_Arr;
        Image_Filename             : FN_Arr;
        Mem_Addr_Reg               : Phys_Addr_T;
        Current_Cmd                : Cmd_T;
        Status_Reg_1, Status_Reg_2 : Word_T;
    end record;

    protected Drives is
        procedure Init;
        procedure Reset;
        procedure Attach
           (Unit : in Natural; Image_Name : in String; OK : out Boolean);
        procedure Detach (Unit : in Natural);
        procedure Data_In (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T);
        procedure Data_Out (Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T);
        function  Get_Image_Name (Unit : in Natural) return String;
        function  Get_Status return Status_Rec;
        procedure Load_TBOOT;
    private
        State : MT6026_Rec;
    end Drives;

    task Status_Sender is
        entry Start;
    end Status_Sender;

    Not_Yet_Implemented,
    Unexpected_Return_Code : exception;

end Devices.Magtape6026;
