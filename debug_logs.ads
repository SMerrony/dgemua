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

package Debug_Logs is

    Num_Lines : constant Integer :=
       100_000; -- each circular buffer has this many lines

    type Logs is
       (Debug_Log,        -- Debug_Log, is the general-purpose _log,
        Mt_Log,           -- Mt_Log, is for the type 6026 MT tape module
        Dkp_Log,          -- Dkp_Log, is for the type 4231a Moving-Head Disk
        Dpf_Log,          -- Dpf_Log, is for the type 6061 DPF disk module
        Dskp_Log,         -- Dskp_Log, is for the type 6239 DSKP disk module
        Map_Log,          -- Map_Log, is for BMC/DCH-related _log,ging
        Sc_Log            -- Sc_Log, is for System Call _log,ging in the VS emulator
    );

    type Log_Filenames_T is array (Logs'Range) of Unbounded_String;

    Log_Filenames : constant Log_Filenames_T :=
       (Debug_Log => To_Unbounded_String ("debug.log"),
        Mt_Log    => To_Unbounded_String ("mt_debug.log"),
        Dkp_Log   => To_Unbounded_String ("dkp_debug.log"),
        Dpf_Log   => To_Unbounded_String ("dpf_debug.log"),
        Dskp_Log  => To_Unbounded_String ("dskp_debug.log"),
        Map_Log   => To_Unbounded_String ("bmcdch_debug.log"),
        Sc_Log    => To_Unbounded_String ("syscall_debug.log"));

    type Log_Arr_T is array (Logs'Range, 1..Num_Lines) of Unbounded_String;
    type Log_Ptr_T is array (Logs'Range) of Integer;

    protected Loggers is
        procedure Debug_Logs_Dump (Directory : Unbounded_String);
        procedure Debug_Print (Log : Logs; Msg : Unbounded_String);
    private
        Log_Array : Log_Arr_T;
        First_Line, Last_Line : Log_Ptr_T;
    end Loggers;

end Debug_Logs;
