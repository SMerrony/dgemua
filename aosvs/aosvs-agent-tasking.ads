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

with Processor;

package AOSVS.Agent.Tasking is

    type Task_Data_T is record
        -- CPU                : Processor.CPU_T;
        PID, TID, UTID, Priority : Word_T;
        Sixteen_Bit        : Boolean;
        Dir                : Unbounded_String;
        Start_Addr, 
        Ring_Mask          : Phys_Addr_T;
        Initial_AC2        : Dword_T;
        WFP, WSP, WSB,
        WSL, WSFH          : Phys_Addr_T;
        Kill_Addr          : Phys_Addr_T;
        Debug_Logging      : Boolean;    
    end record;

    procedure Create_Task (PID : in PID_T; 
                           -- TID : in Word_T;
                           Priority : in Word_T;
                           PR_Addrs : in PR_Addrs_T;
                           Console : in GNAT.Sockets.Stream_Access;
                           Logging : in Boolean);

    function Get_Unique_TID (PID : in PID_T; TID : in Word_T) return Word_T;

    task type VS_Task is
        entry Start (TD : in Task_Data_T; Console : in GNAT.Sockets.Stream_Access);
    end VS_Task;

    System_Call_Not_Implemented : exception;

end AOSVS.Agent.Tasking;