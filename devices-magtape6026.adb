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

with Ada.Exceptions;         use Ada.Exceptions;
with Debug_Logs;             use Debug_Logs;
with Devices;
with Devices.Bus;

package body Devices.Magtape6026 is

    protected body Drives is

        procedure Attach (Unit : in Natural; Image_Name : in String; OK : out Boolean) is
        begin
            -- Open (File => Img_File, Mode => In_File, Name => Image_Name);
            Open (File => State.SIMH_File(Unit), Mode => In_File, Name => Image_Name);
            State.Image_Filename(Unit) := To_Unbounded_String(Image_Name);
            State.Image_Attached(Unit) := true;
            State.Status_Reg_1 := SR_1_Error or SR_1_HiDensity or SR_1_9Track or SR_1_BOT or SR_1_StatusChanged or SR_1_UnitReady;
            State.Status_Reg_2 := SR_2_PE_Mode;
            Devices.Bus.Actions.Attach (Devices.MTB);
            OK := true;
        exception
            when Error : others =>
                Loggers.Debug_Print (Debug_Log, "WARNING: Could not open SimH tape image due to " & 
                    Exception_Information (Error));
                OK := false;
        end Attach;

        function Get_Image_Name (Unit : in Natural) return String is
        begin
           return To_String (State.Image_Filename(Unit));
        end Get_Image_Name;


    end Drives;

end Devices.Magtape6026;