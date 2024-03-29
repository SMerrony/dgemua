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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

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
        procedure Connect (Dev : Dev_Num_T);
        procedure Reset_IO_Device(Dev : Dev_Num_T);
        procedure Reset_All_IO_Devices;
        procedure Set_Reset_Proc (Dev : Dev_Num_T; Reset_Proc : Reset_Proc_T);
        procedure Set_Data_Out_Proc (Dev : Dev_Num_T; Data_Out_Proc : Data_Out_Proc_T);
        procedure Data_Out (Dev : Dev_Num_T; Datum : Word_T; ABC : IO_Reg_T; Flag : IO_Flag_T); 
        procedure Set_Data_In_Proc (Dev : Dev_Num_T; Data_In_Proc : Data_In_Proc_T);
        procedure Data_In  (Dev : Dev_Num_T; ABC : IO_Reg_T; Flag : IO_Flag_T; Datum : out Word_T);
        function  Is_Attached (Dev : Dev_Num_T) return Boolean;
        function  Is_Bootable (Dev : Dev_Num_T) return Boolean;
        function  Is_Connected (Dev : Dev_Num_T) return Boolean;
        function  Is_IO_Dev (Dev : Dev_Num_T) return Boolean;
        procedure Set_Image_Attached (Dev : Dev_Num_T; Image_Name : String);
        procedure Set_Image_Detached (Dev : Dev_Num_T);
        function  Get_Printable_Device_List return String;
        function  Get_Device_Name_Or_Number (Dev : Dev_Num_T) return String;
    private
        Bus : Bus_T;
    end Actions;

    protected States is
        procedure Init;
        function  Get_Busy (Dev : Dev_Num_T) return Boolean;
        function  Get_Done (Dev : Dev_Num_T) return Boolean;
        procedure Set_Busy (Dev : Dev_Num_T; Busy_State : Boolean);
        procedure Set_Done (Dev : Dev_Num_T; Done_State : Boolean);
        function  Get_IRQ return Boolean;
        procedure Set_IRQ_Mask (Mask : Word_T);
        function  Is_Dev_Masked (PMB : Integer) return Boolean;
        procedure Send_Interrupt (Dev : Dev_Num_T);
        procedure Clear_Interrupt (Dev : Dev_Num_T); 
    private 
        State : State_T;
    end States;
end Devices.Bus;