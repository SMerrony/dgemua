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

package Devices is

    -- Device IDs and PMBs
    -- Standard device codes in octal, Priority Mask Bits in decimal
    -- as per DG docs!
    PWRFL : constant Dev_Num_T := 8#00#;
    WCS   : constant Dev_Num_T := 8#01#;
    MAP   : constant Dev_Num_T := 8#03#;
    PSC   : constant Dev_Num_T := 8#04#;
    BMC   : constant Dev_Num_T := 8#05#;
    TTI   : constant Dev_Num_T := 8#10#;
    TTO   : constant Dev_Num_T := 8#11#;
    RTC   : constant Dev_Num_T := 8#14#;
    LPT   : constant Dev_Num_T := 8#17#;
    MTB   : constant Dev_Num_T := 8#22#;
    MTJ   : constant Dev_Num_T := 8#23#;
    DSKP  : constant Dev_Num_T := 8#24#;
    DPF   : constant Dev_Num_T := 8#27#;
    ISC   : constant Dev_Num_T := 8#34#;
    PIT   : constant Dev_Num_T := 8#43#;
    SCP   : constant Dev_Num_T := 8#45#;
    IAC1  : constant Dev_Num_T := 8#50#;
    MTB1  : constant Dev_Num_T := 8#62#;
    MTJ1  : constant Dev_Num_T := 8#63#;
    DSKP1 : constant Dev_Num_T := 8#64#;
    IAC   : constant Dev_Num_T := 8#65#;
    DPF1  : constant Dev_Num_T := 8#67#;
    FPU   : constant Dev_Num_T := 8#76#;
    CPU   : constant Dev_Num_T := 8#77#;

    type Reset_Proc_T is access protected procedure;
    type Data_Out_Proc_T is access protected procedure
       (Datum : in Word_T; ABC : in Character; Flag : in IO_Flag_T);
    type Data_In_Proc_T is access protected procedure
       (ABC : in Character; Flag : in IO_Flag_T; Datum : out Word_T);

    type Device_Rec is record
        Mnemonic           : Unbounded_String;
        PMB                : Integer;
        IO_Device          : Boolean;
        Bootable           : Boolean;
        Connected          : Boolean;
        Reset_Proc         : Reset_Proc_T;
        Data_Out_Proc      : Data_Out_Proc_T;
        Data_In_Proc       : Data_In_Proc_T;
        Sim_Image_Attached : Boolean;
        Sim_Image_Name     : Unbounded_String;
        -- Busy               : Boolean;
        -- Done               : Boolean;
    end record;

    type Devices_Arr_T is array (Dev_Num_T'Range) of Device_Rec;

    type State_Rec is record
        Busy               : Boolean;
        Done               : Boolean;
    end record;   

    type State_Arr_T is array (Dev_Num_T'Range) of State_Rec;

    -- function Mnemonic_Or_Octal (Dev_Num : Dev_Num_T) return Unbounded_String;

end Devices;
