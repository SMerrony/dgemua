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

with Debug_Logs; use Debug_logs;

package body AOSVS.Agent.Tasking is

    procedure Create_Task (PID : in PID_T; 
                           -- TID : in Word_T;
                           Priority : in Word_T;
                           PR_Addrs : in PR_Addrs_T) is
        VS_Task : Task_Data_T;
        TID     : Word_T;
    begin
        VS_Task.PID := Word_T(PID);
        Loggers.Debug_Print (Sc_Log, "Setting up task for PID:" & PID'Image);
        VS_Task.Sixteen_Bit := false; -- TODO
        VS_Task.Start_Addr := PR_Addrs.PR_Start;
        VS_Task.Ring_Mask  := PR_Addrs.PR_Start and 16#7000_0000#;
        VS_Task.WFP := (if PR_Addrs.WFP /= 0 then PR_Addrs.WFP else PR_Addrs.WSP);
        VS_Task.WSP := PR_Addrs.WSP;
        VS_Task.WSB := PR_Addrs.WSB;
        VS_Task.WSL := PR_Addrs.WSL;
        VS_Task.WSFH := PR_Addrs.WSFH;

        VS_Task.Debug_Logging := true; -- FIXME

        AOSVS.Agent.Actions.Allocate_TID (PID, TID);
        if TID = 0 then
            raise NO_MORE_TIDS with "PID: " & PID'Image;
        end if;
        VS_Task.TID := TID;
        Loggers.Debug_Print (Sc_Log, "... got TID:" & TID'Image);

    end Create_Task;

end AOSVS.Agent.Tasking;