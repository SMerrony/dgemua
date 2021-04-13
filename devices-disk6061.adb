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

with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;

with Debug_Logs;       use Debug_Logs;
with Devices;
with Devices.Bus;
with Status_Monitor;

package body Devices.Disk6061 is

    protected body Drives is
       
        procedure Init (Debug_Logging : in Boolean) is
        begin
            State.Debug_Logging := Debug_Logging;
            Devices.Bus.Actions.Set_Reset_Proc (Devices.DPF, Reset'Access);
            -- Devices.Bus.Actions.Set_Data_In_Proc (Devices.DPF, Data_In'Access);
            -- Devices.Bus.Actions.Set_Data_Out_Proc (Devices.DPF, Data_Out'Access);
            State.Image_Attached := false;
            State.Map_Enabled := false;
            Reset;
            Status_Sender.Start;
        end Init;

        procedure Reset is
        begin
            State.RW_Status := 0;
            State.Instruction_Mode := Normal;
            State.Command := 0;
            State.Cylinder := 0;
            State.Surface := 0;
            State.Sector := 0;
            State.Sector_Cnt := 0;
            State.Drive_Status := Drive_Stat_Ready;
        end Reset;

        procedure Attach (Unit : in Natural; Image_Name : in String; OK : out Boolean) is
        begin
            if Unit /= 0 then
                raise Not_Yet_Implemented with "DPF - Multiple disks not yet supported";
            end if;
            Sector_IO.Open (State.Image_File, Inout_File, Image_Name);
            State.Image_Filename := To_Unbounded_String(Image_Name);
            State.Image_Attached := true;
            Devices.Bus.Actions.Set_Image_Attached (Devices.DPF, Image_Name);
            OK := true;
        exception
            when Error : others =>
                Loggers.Debug_Print (Debug_Log, "WARNING: Could not open disk image due to " & 
                    Exception_Information (Error)); 
                OK := false;
        end Attach;

        function Get_Status return Status_Rec is
            Stat : Status_Rec;
        begin
            Stat.Image_Attached := State.Image_Attached;
            Stat.Image_Filename := State.Image_Filename;
            Stat.Cylinder := State.Cylinder;
            Stat.Surface  := State.Surface;
            Stat.Sector   := State.Sector;
            Stat.Reads    := State.Reads;
            Stat.Writes   := State.Writes;
            return Stat;
        end Get_Status;

    end Drives;

    -- Create_Blank creates an empty disk file of the correct size for the disk6061 emulator to use
    procedure Create_Blank (Image_Name : in String; OK : out Boolean) is
        Tmp_File : Sector_IO.File_Type;
        Empty_Sector : constant Sector := (others => 0);
    begin
        Sector_IO.Create (Tmp_File, Out_File, Image_Name);
        for S in 1 .. Sectors_Per_Disk loop
           Sector_IO.Write (Tmp_File, Empty_Sector);
        end loop;
        Sector_IO.Close (Tmp_File);
        OK := true;
    exception
        when Error : others =>
            Loggers.Debug_Print (Debug_Log, "WARNING: Could not create disk image due to " & 
                Exception_Information (Error)); 
            OK := false;
    end Create_Blank;
    
    task body Status_Sender is
        Status :  Status_Rec;
    begin
      accept Start do
         Ada.Text_IO.Put_line ("INFO: DPF Status Sender started");
      end Start;
      loop
         Status := Drives.Get_Status;
         Status_Monitor.Monitor.DPF_Update (Status);
         delay 0.5;
      end loop;
   end Status_Sender;

end Devices.Disk6061;