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
with Ada.Text_IO; use Ada.Text_IO;

with Memory;

package body Devices.Bus is
    protected body Actions is

        procedure Init is
        begin
            for D in Bus.Devices'Range loop
               Bus.Devices(D).Mnemonic := To_Unbounded_String("");
               Bus.Devices(D).PMB      := 0;
               Bus.Devices(D).Reset_Proc := null;
               Bus.Devices(D).Data_Out_Proc := null;
               Bus.Devices(D).Data_In_Func := null;
               Bus.Devices(D).Sim_Image_Attached := false;
               Bus.Devices(D).Sim_Image_Name := To_Unbounded_String("");
               Bus.Devices(D).IO_Device      := false;
               Bus.Devices(D).Bootable       := false;
               Bus.Devices(D).Connected      := false;
               Bus.Devices(D).Busy           := false;
               Bus.Devices(D).Done           := false;
            end loop;
            
            Bus.Devices(PWRFL) := (To_Unbounded_String("PWRFL"), 0, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(WCS)   := (To_Unbounded_String("WCS"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(MAP)   := (To_Unbounded_String("MAP"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(PSC)   := (To_Unbounded_String("PSC"), 13, false, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(BMC)   := (To_Unbounded_String("BMC"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(TTI)   := (To_Unbounded_String("TTI"), 14, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(TTO)   := (To_Unbounded_String("TTO"), 15, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(RTC)   := (To_Unbounded_String("RTC"), 13, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(LPT)   := (To_Unbounded_String("LPT"), 12, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(MTB)   := (To_Unbounded_String("MTB"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(MTJ)   := (To_Unbounded_String("MTJ"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(DSKP)  := (To_Unbounded_String("DSKP"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(DPF)   := (To_Unbounded_String("DPF"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(ISC)   := (To_Unbounded_String("ISC"), 4, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(PIT)   := (To_Unbounded_String("PIT"), 11, false, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(SCP)   := (To_Unbounded_String("SCP"), 15, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(IAC1)  := (To_Unbounded_String("IAC1"), 11, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(MTB1)  := (To_Unbounded_String("MTB1"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(MTJ1)  := (To_Unbounded_String("MTJ1"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(DSKP1) := (To_Unbounded_String("DSKP1"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(IAC)   := (To_Unbounded_String("IAC"), 11, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(DPF1)  := (To_Unbounded_String("DPF1"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(FPU)   := (To_Unbounded_String("FPU"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
            Bus.Devices(CPU)   := (To_Unbounded_String("CPU"), 0, true, false, false, null, null, null, false, To_Unbounded_String(""), false, false);
        
            Bus.IRQ_Mask := 0;
            Bus.IRQ      := false;
            for D in Dev_Prio_Arr'Range loop
               Bus.Devs_By_Priority(D).Clear;
            end loop;
            for D in Int_Dev_Arr'Range loop
               Bus.Interrupting_Dev(D) := false;
            end loop;

            Put_Line ("INFO: Bus initialised");
        end Init;

       procedure Connect (Dev : in Dev_Num_T) is
       begin
          Bus.Devices(Dev).Connected := True;
          if Bus.Devices(Dev).PMB < 16 then
             Bus.Devs_By_Priority(Bus.Devices(Dev).PMB).Append(Dev);
          end if;
          Put_Line ("INFO: " & To_String(Bus.Devices(Dev).Mnemonic) & " connected to bus");
       end Connect;

       procedure Set_Reset_Proc (Dev : in Dev_Num_T; Reset_Proc : in Reset_Proc_T) is
       begin
          Bus.Devices(Dev).Reset_Proc := Reset_Proc;
       end Set_Reset_Proc;

       procedure Set_Data_Out_Proc (Dev : in Dev_Num_T; Data_Out_Proc : in Data_Out_Proc_T) is
       begin
          Bus.Devices(Dev).Data_Out_Proc := Data_Out_Proc;
       end Set_Data_Out_Proc;

       procedure Set_Data_In_Func (Dev : in Dev_Num_T; Data_In_Func : in Data_In_Func_T) is
       begin
          Bus.Devices(Dev).Data_In_Func := Data_In_Func;
       end Set_Data_In_Func;

       function Is_Attached (Dev : in Dev_Num_T) return Boolean is
       begin
          return Bus.Devices(Dev).Sim_Image_Attached;
       end Is_Attached;

       function Is_Bootable (Dev : in Dev_Num_T) return Boolean is
       begin
          return Bus.Devices(Dev).Bootable;
       end Is_Bootable;

       function Is_Dev_Masked (Dev : in Dev_Num_T) return Boolean is
       begin
          return Memory.Test_W_Bit (Bus.IRQ_Mask, Bus.Devices(Dev).PMB);
       end Is_Dev_Masked;

       procedure Send_Interrupt (Dev : in Dev_Num_T) is
       begin
         Bus.Interrupting_Dev(Dev) := true;
         Bus.IRQs_By_Priority(Bus.Devices(Dev).PMB) := true;
         Bus.IRQ := true;
       end Send_Interrupt;

       procedure Clear_Interrupt (Dev : in Dev_Num_T) is
       begin
         Bus.Interrupting_Dev(Dev) := false;
         Bus.IRQs_By_Priority(Bus.Devices(Dev).PMB) := false;
         --Bus.IRQ := false;
       end Clear_Interrupt;

       procedure Set_Busy (Dev : in Dev_Num_T; Busy_State : in Boolean) is
       begin
          Bus.Devices(Dev).Busy := Busy_State;
       end Set_Busy;

       procedure Set_Done (Dev : in Dev_Num_T; Done_State : in Boolean) is
       begin
          Bus.Devices(Dev).Done := Done_State;
       end Set_Done;

       procedure Set_Image_Attached (Dev : in Dev_Num_T; Image_Name : String) is
       begin
         Bus.Devices(Dev).Sim_Image_Attached := true;
         Bus.Devices(Dev).Sim_Image_Name := To_Unbounded_String (Image_Name);
       end Set_Image_Attached;

       procedure Set_Image_Detached (Dev : in Dev_Num_T) is
       begin
         Bus.Devices(Dev).Sim_Image_Attached := false;
         Bus.Devices(Dev).Sim_Image_Name := Null_Unbounded_String;
       end Set_Image_Detached;

       function  Get_Printable_Device_List return String is
         Lst : Unbounded_String := To_Unbounded_String (" #     Mnem    PMB    I/O     Busy    Done     Status") & Dasher_NL;
       begin
         for D in Dev_Num_T'Range loop
            if Bus.Devices(D).Connected then
               Lst := Lst &
                      Dev_Num_T'Image(D) & ASCII.HT &
                      Bus.Devices(D).Mnemonic & ASCII.HT &
                      Integer'Image(Bus.Devices(D).PMB) & ASCII.HT &
                      Memory.Boolean_To_YN (Bus.Devices(D).IO_Device) & ASCII.HT &
                      Memory.Boolean_To_YN (Bus.Devices(D).Busy) & ASCII.HT &
                      Memory.Boolean_To_YN (Bus.Devices(D).Done);
               if Bus.Devices(D).Sim_Image_Attached then
                  Lst := Lst & " Attached to image: " & Bus.Devices(D).Sim_Image_Name;
               end if;
               Lst := Lst & Dasher_NL;
            end if;
         end loop;
         return To_String (Lst);
       end Get_Printable_Device_List;

       function  Get_Device_Name_Or_Number (Dev : in Dev_Num_T) return String is
       begin
          if Bus.Devices(Dev).Mnemonic /= "" then 
            return To_String(Bus.Devices(Dev).Mnemonic);
          end if;
          return Dev'Image;
       end Get_Device_Name_Or_Number;

    end Actions;

end Devices.Bus;