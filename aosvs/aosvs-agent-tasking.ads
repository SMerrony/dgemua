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

package AOSVS.Agent.Tasking is

    type Task_Data_T is record
        PID, TID, Priority : Word_T;
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

    task type VS_Task is
        entry Start (TD : in Task_Data_T);
    end VS_Task;

    procedure Create_Task (PID : in PID_T; 
                           -- TID : in Word_T;
                           Priority : in Word_T;
                           PR_Addrs : in PR_Addrs_T);

end AOSVS.Agent.Tasking;