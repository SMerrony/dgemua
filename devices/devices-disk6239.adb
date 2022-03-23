-- MIT License

-- Copyright ©2022 Stephen Merrony

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

with Ada.Text_IO;

with Status_Monitor;

package body Devices.Disk6239 is

    protected body Drives is

        procedure Init (Debug_Logging : in Boolean) is
        begin
            State.Debug_Logging := Debug_Logging;
            --Devices.Bus.Actions.Set_Reset_Proc (Devices.DSKP, Reset'Access);
            --Devices.Bus.Actions.Set_Data_In_Proc (Devices.DSKP, Data_In'Access);
            --Devices.Bus.Actions.Set_Data_Out_Proc (Devices.DSKP, Data_Out'Access);
            State.Image_Attached := False;
            --State.Map_Enabled    := False;
            --Reset;
            Status_Sender.Start;
        end Init;

        function Get_Status return Status_Rec is
            Stat : Status_Rec;
        begin
            Stat.Image_Attached := State.Image_Attached;
            Stat.Image_Filename := State.Image_Filename;
            Stat.Status_Reg_A   := State.Status_Reg_A;
            Stat.Status_Reg_B   := State.Status_Reg_B;
            Stat.Status_Reg_C   := State.Status_Reg_C;
            Stat.Sector_No      := State.Sector_No;
            Stat.Reads          := State.Reads;
            Stat.Writes         := State.Writes;
            return Stat;
        end Get_Status;
        
    end Drives;


    task body Status_Sender is
        Status : Status_Rec;
    begin
        accept Start do
            Ada.Text_IO.Put_Line ("INFO: DSKP Status Sender started");
        end Start;
        loop
            Status := Drives.Get_Status;
            Status_Monitor.Monitor.DSKP_Update (Status);
            delay Status_Period_S;
        end loop;
    end Status_Sender;

end Devices.Disk6239;
