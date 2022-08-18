--  Copyright ©2021,2022 Stephen Merrony
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
        Is_TLOCKed         : Boolean; 
    end record;

    procedure Create_Task (PID      : PID_T; 
                           -- TID   : Word_T;
                           Priority : Word_T;
                           PR_Addrs : PR_Addrs_T;
                           Console  : GNAT.Sockets.Stream_Access;
                           Logging  : Boolean);

    function Get_Unique_TID (PID : PID_T; TID : Word_T) return Word_T;

    task type VS_Task is
        entry Start (TD : Task_Data_T; Console : GNAT.Sockets.Stream_Access);
    end VS_Task;

    System_Call_Not_Implemented : exception;

end AOSVS.Agent.Tasking;