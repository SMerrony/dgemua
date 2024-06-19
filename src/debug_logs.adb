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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with DG_Types;   use DG_Types;

package body Debug_Logs is

    protected body Loggers is

    procedure Init is
    begin
        for L in Logs'Range loop
            First_Line(L) := 1;
            Last_Line(L) := 1;
        end loop;
    end Init;

    procedure Debug_Logs_Dump (Directory : String) is
        Write_Path : Unbounded_String;
        Write_File : File_Type;
        This_Line  : Positive;
    begin
        for L in Logs'Range loop
           if Last_Line(L) /= 1 then -- ignore unused or empty logs
              Write_Path := Directory & "/" & Log_Filenames(L);
              Create (Write_File, Out_File, To_String (Write_Path));
              This_Line := First_Line(L);
             loop
                 String'Write (Stream(Write_File), To_String(Log_Array(L, This_Line) & Dasher_NL));
                 This_Line := This_Line + 1;
                 if This_Line = Num_Lines then
                    This_Line := 1;
                 end if;
                 exit when This_Line = Last_Line(L) + 1;
              end loop;
              -- String'Write (Stream(Write_File), To_String(Log_Array(L, This_Line) & Dasher_NL));
              String'Write (Stream(Write_File), ">>> Debug Log Ends <<<");
              Close (Write_File);
           end if;
        end loop;
    end Debug_Logs_Dump;

    -- Debug_Print doesn't print anything!  It stores the log message
    -- in array-backed circular arrays written out when debugLogsDump() is invoked.
    -- This proc can be called very often, KISS...
    procedure Debug_Print (Log : Logs; Msg : String) is
    begin
        Last_Line(Log) := Last_Line(Log) + 1;

        if Last_Line(Log) >= Num_Lines then
            Last_Line(Log) := 1;    -- wrap-around
        end if;

        -- has the tail hit the head of the circular buffer?
        if Last_Line(Log) = First_Line(Log) then
            First_Line(Log) := First_Line(Log) + 1; -- advance the head pointer
            if First_Line(Log) = Num_Lines then
                First_Line(Log) := 1;               -- wrap-around
            end if;
        end if;

        Log_Array(Log, Last_Line(Log)) := To_Unbounded_String(Msg);
    end Debug_Print;

    end Loggers;

end Debug_Logs;