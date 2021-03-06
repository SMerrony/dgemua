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

with GNAT.Sockets;

with Processor;
with Devices.Disk6061;
with Devices.Magtape6026;

package Status_Monitor is

    task Monitor is
        entry Start (Port : in GNAT.Sockets.Port_Type);
        entry CPU_Update (Stats : in Processor.CPU_Monitor_Rec);
        entry DPF_Update (Stats : in Devices.Disk6061.Status_Rec);
        entry MTB_Update (Stats : in Devices.Magtape6026.Status_Rec);
        entry Stop;
    end Monitor;

    CPU_Row_1 : constant Integer := 3;
    CPU_Row_2 : constant Integer := 5;
    DPF_Row_1 : constant Integer := 7;
    MTB_Row_1 : constant Integer := 11;
    MTB_Row_2 : constant Integer := 12;

end Status_Monitor;