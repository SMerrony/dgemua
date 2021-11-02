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

with DG_Types;    use DG_Types;
with Memory;

package body Devices.Bus is
    protected body Actions is

        procedure Init is
        begin
            for D in Bus.Devices'Range loop
               Bus.Devices(D).Mnemonic := To_Unbounded_String("");
               Bus.Devices(D).PMB      := 0;
               Bus.Devices(D).IO_Device      := false;
               Bus.Devices(D).Bootable       := false;
               Bus.Devices(D).Connected      := false;
               Bus.Devices(D).Reset_Proc := null;
               Bus.Devices(D).Data_Out_Proc := null;
               Bus.Devices(D).Data_In_Proc := null;
               Bus.Devices(D).Sim_Image_Attached := false;
               Bus.Devices(D).Sim_Image_Name := To_Unbounded_String("");
            end loop;
            
            Bus.Devices(PWRFL) := (To_Unbounded_String("PWRFL"), 0, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(WCS)   := (To_Unbounded_String("WCS"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(MAP)   := (To_Unbounded_String("MAP"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(PSC)   := (To_Unbounded_String("PSC"), 13, false, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(BMC)   := (To_Unbounded_String("BMC"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(TTI)   := (To_Unbounded_String("TTI"), 14, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(TTO)   := (To_Unbounded_String("TTO"), 15, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(RTC)   := (To_Unbounded_String("RTC"), 13, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(LPT)   := (To_Unbounded_String("LPT"), 12, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(MTB)   := (To_Unbounded_String("MTB"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(MTJ)   := (To_Unbounded_String("MTJ"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(DSKP)  := (To_Unbounded_String("DSKP"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(DPF)   := (To_Unbounded_String("DPF"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(ISC)   := (To_Unbounded_String("ISC"), 4, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(PIT)   := (To_Unbounded_String("PIT"), 11, false, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(SCP)   := (To_Unbounded_String("SCP"), 15, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(IAC1)  := (To_Unbounded_String("IAC1"), 11, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(MTB1)  := (To_Unbounded_String("MTB1"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(MTJ1)  := (To_Unbounded_String("MTJ1"), 10, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(DSKP1) := (To_Unbounded_String("DSKP1"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(IAC)   := (To_Unbounded_String("IAC"), 11, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(DPF1)  := (To_Unbounded_String("DPF1"), 7, true, true, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(FPU)   := (To_Unbounded_String("FPU"), 99, true, false, false, null, null, null, false, To_Unbounded_String(""));
            Bus.Devices(CPU)   := (To_Unbounded_String("CPU"), 0, true, false, false, null, null, null, false, To_Unbounded_String(""));
        
            States.Init;
            for D in Dev_Prio_Arr'Range loop
               Bus.Devs_By_Priority(D).Clear;
            end loop;

            for D in Bus.Devices'Range loop
               PMBs(D) := Bus.Devices(D).PMB;
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

       procedure Reset_IO_Device(Dev : in Dev_Num_T) is
       begin
          if Bus.Devices(Dev).Connected and Bus.Devices(Dev).IO_Device and (Bus.Devices(Dev).Reset_Proc /= null) then
             Bus.Devices(Dev).Reset_Proc.all;
          else
             Put_Line ("INFO: Ignoring attempt to reset non-I/O or non-resettable device No. " & Get_Device_Name_Or_Number (Dev));
          end if;

       end Reset_IO_Device;

       procedure Reset_All_IO_Devices is
       begin
          for D in Dev_Num_T'Range loop
             if Bus.Devices(D).Connected and Bus.Devices(D).IO_Device then
                Reset_IO_Device (D);
             end if;
          end loop;
       end Reset_All_IO_Devices;

       procedure Set_Reset_Proc (Dev : in Dev_Num_T; Reset_Proc : in Reset_Proc_T) is
       begin
          Bus.Devices(Dev).Reset_Proc := Reset_Proc;
       end Set_Reset_Proc;

       procedure Set_Data_Out_Proc (Dev : in Dev_Num_T; Data_Out_Proc : in Data_Out_Proc_T) is
       begin
          Bus.Devices(Dev).Data_Out_Proc := Data_Out_Proc;
       end Set_Data_Out_Proc;

       procedure Data_Out (Dev : in Dev_Num_T; Datum : in Word_T; ABC : in IO_Reg_T; Flag : in IO_Flag_T) is
       begin
          Bus.Devices(Dev).Data_Out_Proc (Datum, ABC, Flag);
       end Data_Out;

       procedure Data_In  (Dev : in Dev_Num_T; ABC : in IO_Reg_T; Flag : in IO_Flag_T; Datum : out Word_T) is
       begin
          Bus.Devices(Dev).Data_In_Proc(ABC, Flag, Datum);
       end Data_In;

       procedure Set_Data_In_Proc (Dev : in Dev_Num_T; Data_In_Proc : in Data_In_Proc_T) is
       begin
          Bus.Devices(Dev).Data_In_Proc := Data_In_Proc;
       end Set_Data_In_Proc;

       function Is_Attached (Dev : in Dev_Num_T) return Boolean is
       begin
          return Bus.Devices(Dev).Sim_Image_Attached;
       end Is_Attached;

       function Is_Bootable (Dev : in Dev_Num_T) return Boolean is
       begin
          return Bus.Devices(Dev).Bootable;
       end Is_Bootable;

       function Is_Connected (Dev : in Dev_Num_T) return Boolean is
       begin
          return Bus.Devices(Dev).Connected;
       end Is_Connected;



       function Is_IO_Dev (Dev : in Dev_Num_T) return Boolean is
       begin
          return Bus.Devices(Dev).IO_Device;
       end Is_IO_Dev;

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
         use Memory;
         Lst : Unbounded_String := To_Unbounded_String (" #     Mnem     PMB   I/O     Busy    Done     Status") & Dasher_NL;
       begin
         for D in Dev_Num_T'Range loop
            if Bus.Devices(D).Connected then
               Lst := Lst &
                      Byte_To_String(Byte_T(D), Octal, 3, true) & ASCII.HT &
                      Bus.Devices(D).Mnemonic & ASCII.HT &
                      Byte_To_String(Byte_T(Bus.Devices(D).PMB), Decimal, 3) & "." & ASCII.HT &
                      Boolean_To_YN (Bus.Devices(D).IO_Device) & ASCII.HT &
                      Boolean_To_YN (States.Get_Busy(D)) & ASCII.HT &
                      Boolean_To_YN (States.Get_Done(D));
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
       
       function Get_PMB (Dev : in Dev_Num_T) return Integer is
       begin
         return Bus.Devices(Dev).PMB;
       end Get_PMB;

    end Actions;

    protected body States is

       procedure Init is
       begin
         State.IRQ := false;
         State.IRQ_Mask := 0;
         for D in Int_Dev_Arr'Range loop
               State.Interrupting_Dev(D) := false;
         end loop;
       end Init;

       function Get_Busy (Dev : in Dev_Num_T) return Boolean is
       begin
         return State.Statuses(Dev).Busy;
       end Get_Busy;

       procedure Set_Busy (Dev : in Dev_Num_T; Busy_State : in Boolean) is
       begin
          State.Statuses(Dev).Busy := Busy_State;
       end Set_Busy;

       function Get_Done (Dev : in Dev_Num_T) return Boolean is
       begin
         return State.Statuses(Dev).Done;
       end Get_Done;

       procedure Set_Done (Dev : in Dev_Num_T; Done_State : in Boolean) is
       begin
          State.Statuses(Dev).Done := Done_State;
       end Set_Done;

       function Get_IRQ return Boolean is
       begin
         return State.IRQ;
       end Get_IRQ;

       function Is_Dev_Masked (PMB : in Integer) return Boolean is
       begin
          return Test_W_Bit (State.IRQ_Mask, PMB);
       end Is_Dev_Masked;

       procedure Send_Interrupt (Dev : in Dev_Num_T) is
       begin
          State.Interrupting_Dev(Dev) := true;
          State.IRQs_By_Priority(PMBs(Dev)) := true;
          State.IRQ := true;
       end Send_Interrupt;

       procedure Clear_Interrupt (Dev : in Dev_Num_T) is
       begin
          State.Interrupting_Dev(Dev) := false;
          State.IRQs_By_Priority(PMBs(Dev)) := false;
          --Bus.IRQ := false;
       end Clear_Interrupt;

    end States;

end Devices.Bus;