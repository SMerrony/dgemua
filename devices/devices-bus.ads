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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with DG_Types;   use DG_Types;

package Devices.Bus is

    type IRQ_Prio_Arr is array(0..15) of Boolean;

    package Dev_Prio_Vec is new Ada.Containers.Vectors 
        (Index_Type   => Natural, Element_Type => Dev_Num_T);
    use Dev_Prio_Vec;

    type Dev_Prio_Arr is array(0 .. 15) of Vector;
    type Int_Dev_Arr  is array(Dev_Num_T'Range) of Boolean;

    type Bus_T is record
       Devices  : Devices_Arr_T;
    --    IRQ_Mask : Word_T;
    --    IRQ      : Boolean;
    --    IRQs_By_Priority : IRQ_Prio_Arr;
       Devs_By_Priority : Dev_Prio_Arr;
    --    Interrupting_Dev : Int_Dev_Arr;
    end record;

    type State_T is record
        Statuses : State_Arr_T;
        IRQ_Mask : Word_T;
        IRQ      : Boolean;
        IRQs_By_Priority : IRQ_Prio_Arr;
        Interrupting_Dev : Int_Dev_Arr; 
    end record;

    PMBs : PMB_Arr_T;

    protected Actions is
        procedure Init;
        procedure Connect (Dev : in Dev_Num_T);
        procedure Reset_IO_Device(Dev : in Dev_Num_T);
        procedure Reset_All_IO_Devices;
        procedure Set_Reset_Proc (Dev : in Dev_Num_T; Reset_Proc : in Reset_Proc_T);
        procedure Set_Data_Out_Proc (Dev : in Dev_Num_T; Data_Out_Proc : in Data_Out_Proc_T);
        procedure Data_Out (Dev : in Dev_Num_T; Datum : in Word_T; ABC : in IO_Reg_T; Flag : in IO_Flag_T); 
        procedure Set_Data_In_Proc (Dev : in Dev_Num_T; Data_In_Proc : in Data_In_Proc_T);
        procedure Data_In  (Dev : in Dev_Num_T; ABC : in IO_Reg_T; Flag : in IO_Flag_T; Datum : out Word_T);
        function  Is_Attached (Dev : in Dev_Num_T) return Boolean;
        function  Is_Bootable (Dev : in Dev_Num_T) return Boolean;
        function  Is_Connected (Dev : in Dev_Num_T) return Boolean;
        function  Is_IO_Dev (Dev : in Dev_Num_T) return Boolean;
        procedure Set_Image_Attached (Dev : in Dev_Num_T; Image_Name : String);
        procedure Set_Image_Detached (Dev : in Dev_Num_T);
        function  Get_Printable_Device_List return String;
        function  Get_Device_Name_Or_Number (Dev : in Dev_Num_T) return String;
    private
        Bus : Bus_T;
    end Actions;

    protected States is
        procedure Init;
        function  Get_Busy (Dev : in Dev_Num_T) return Boolean;
        function  Get_Done (Dev : in Dev_Num_T) return Boolean;
        procedure Set_Busy (Dev : in Dev_Num_T; Busy_State : in Boolean);
        procedure Set_Done (Dev : in Dev_Num_T; Done_State : in Boolean);
        
        function  Is_Dev_Masked (PMB : in Integer) return Boolean;
        procedure Send_Interrupt (Dev : in Dev_Num_T);
        procedure Clear_Interrupt (Dev : in Dev_Num_T); 
    private 
        State : State_T;
    end States;
end Devices.Bus;