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

-- Status_Monitor maintains a near real-time status screen available on STAT_PORT.
--
-- The screen uses DG DASHER control codes for formatting, so a DASHER terminal emulator
-- should be attached to it for good results.
--
-- The Monitor task waits for status updates
-- from known senders and upon receiving an update refreshes the display of that status
-- on the monitor page.  It is therefore the responsibility of the sender to update the
-- status as often as it sees fit.

with GNAT.Sockets;

with Processor;
with Devices.Disk6061;
with Devices.Disk6239;
with Devices.Magtape6026;

package Status_Monitor is

    task Monitor is
        entry Start (Port : in GNAT.Sockets.Port_Type);
        entry CPU_Update  (Stats : in Processor.CPU_Monitor_Rec);
        entry DPF_Update  (Stats : in Devices.Disk6061.Status_Rec);
        entry DSKP_Update (Stats : in Devices.Disk6239.Status_Rec);
        entry MTB_Update  (Stats : in Devices.Magtape6026.Status_Rec);
        entry Stop;
    end Monitor;

    CPU_Row_1 : constant Integer := 3;
    CPU_Row_2 : constant Integer := 5;
    DPF_Row_1 : constant Integer := 7;
    DPF_Row_2 : constant Integer := 8;
    DSKP_Row_1 : constant Integer := 10;
    DSKP_Row_2 : constant Integer := 11;
    MTB_Row_1 : constant Integer := 13;
    MTB_Row_2 : constant Integer := 14;

end Status_Monitor;