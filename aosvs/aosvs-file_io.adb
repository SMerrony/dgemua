-- Copyright ©2021,2022 Stephen Merrony
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Characters.Handling;

with Interfaces;  use Interfaces;

with AOSVS.Agent; use AOSVS.Agent;
with Debug_Logs;  use Debug_Logs;
with Memory;      use Memory;
with PARU_32;     use PARU_32;

package body AOSVS.File_IO is

    function Sys_CLOSE (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
        Pkt_Addr    : constant Phys_Addr_T := Phys_Addr_T(CPU.AC(2));
        Chan_No     : constant Word_T      := RAM.Read_Word(Pkt_Addr + ICH);
        Err         : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?CLOSE - Chan. No:" & Chan_No'Image & " for PID:" & PID'Image & " TID:" & TID'Image);
        Loggers.Debug_Print (Debug_Log, "?CLOSE - Channel:" &Chan_No'Image);
        if Chan_No = 16#ffff# then
            Loggers.Debug_Print (Sc_Log, "------ Ignoring attempt to ?CLOSE -1");
            CPU.AC(0) := Dword_T(PARU_32.ERPRE);
            return false;
        end if;
        AOSVS.Agent.Actions.File_Close(Natural(Chan_No), Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        return true;
    end Sys_Close;
    
    function Sys_GOPEN (CPU : CPU_T; PID : Word_T) return Boolean is
        Filename    : constant String := RAM.Read_String_BA(CPU.AC(0), false);
        Path        : constant String := (if Filename(Filename'First) = '@' then Filename else To_String(Agent.Actions.Get_Virtual_Root) &
                                   Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                                   ":" & Filename));
        Req_Channel : constant Integer := Integer(CPU.AC_I32(1)); -- = -1 for us to assign
        Pkt_Addr    : constant Phys_Addr_T := CPU.AC_PA(2);
        Exclusive   : Boolean;
        File_Type   : Word_T;
        Assigned_Chan : Word_T;
        File_Size   : Integer_32;
        Err         : Word_T;
    begin
        Loggers.Debug_Print (Debug_Log, "?GOPEN");
        Loggers.Debug_Print (Sc_Log, "?GOPEN Filename: " & Filename & " for PID:" & PID'Image);
        Loggers.Debug_Print (Sc_Log, "------ Resolved to local file: " & Path);
        File_Type := RAM.Read_Word (Pkt_Addr + PARU_32.OPTY) and 16#0000_ffff#;
        Loggers.Debug_Print (Sc_Log, "------ Requested channel:" & Req_Channel'Image);
        Loggers.Debug_Print (Sc_Log, "------ File Type No." & File_Type'Image);
        if File_Type /= 0 then 
            raise Not_Yet_Implemented with "?GOPEN specific file type";
        end if;
        if Req_Channel /= -1 then 
            raise Not_Yet_Implemented with "?GOPEN specific channel";
        end if;
        Exclusive := (RAM.Read_Word (Pkt_Addr + PARU_32.OPFL) and PARU_32.OPME) = PARU_32.OPME;
        Agent.Actions.Block_File_Open (PID, Path, Exclusive, Assigned_Chan, File_Type, File_Size, Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        RAM.Write_Word (Pkt_Addr + PARU_32.OPFL, Assigned_Chan); -- TODO shouldn't overwrite Exclusive bit
        RAM.Write_Dword (Pkt_Addr + PARU_32.OPEH, Integer_32_To_Dword (File_Size));
        Loggers.Debug_Print (Sc_Log, "----- Returned channel No.:" & Assigned_Chan'Image & " and Size:" & File_Size'Image);
        return true;
    end Sys_GOPEN;

    function Sys_GCLOSE(CPU : CPU_T; PID : Word_T) return Boolean is 
        Chan_No : constant Word_T := CPU.AC_Wd(1);
        Err     : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?GCLOSE - Channel:" & Chan_No'Image);
        Loggers.Debug_Print (Debug_Log, "?GCLOSE");
        AOSVS.Agent.Actions.File_Close(Natural(Chan_No), Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        return true;
    end Sys_GCLOSE;

    function Sys_OPEN (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
        Chan_No, Err : Word_T;
        Pkt_Addr  : constant Phys_Addr_T := CPU.AC_PA(2);
        File_Opts : constant Word_T      := RAM.Read_Word(Pkt_Addr + ISTI);
        File_Type : constant Word_T      := RAM.Read_Word(Pkt_Addr + ISTO);
        Rec_Len   : constant Integer     := Integer(Word_To_Integer_16(RAM.Read_Word(Pkt_Addr + IRCL)));
        Path_Name : constant Dword_T     := RAM.Read_Dword(Pkt_Addr + IFNP);
        Name      : constant String      := Ada.Characters.Handling.To_Upper(RAM.Read_String_BA(Path_Name, false));
        Path      : constant String      := (if Name(Name'First) = '@' then Name else To_String(Agent.Actions.Get_Virtual_Root) &
                                   Slashify_Path(Agent.Actions.Get_Working_Directory(PID) & 
                                   ":" & Name)); 
    begin
        Loggers.Debug_Print (Sc_Log, "?OPEN Pathname: " & Name & " for PID:" & PID'Image & " TID:" & TID'Image); 
        Loggers.Debug_Print (Debug_Log, "?OPEN Pathname: " & Name);
        Loggers.Debug_Print (Sc_Log, "----- Resolved to local file: " & Path);
        Agent.Actions.File_Open (PID, Path, File_Opts, File_Type, Rec_Len, Chan_No, Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        RAM.Write_Word(Pkt_Addr + ICH, Chan_No);
        Dump_Packet (Pkt_Addr, IOSZ);
        Loggers.Debug_Print (Sc_Log, "----- Returned channel No. " & Chan_No'Image);
        return true;
    end Sys_OPEN;

    function Sys_READ (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean is
        Pkt_Addr    : constant Phys_Addr_T := CPU.AC_PA(2);
        Chan_No     : constant Word_T      := RAM.Read_Word(Pkt_Addr + ICH);
        File_Spec   : constant Word_T      := RAM.Read_Word(Pkt_Addr + ISTI);
        Format_Bits : constant Word_T      := File_Spec and 7;
        Open_Fmt    : constant Boolean     := ((File_Spec and ICRF) = 0);
        Is_Extd     : constant Boolean     := ((File_Spec and IPKL) /= 0);
        Is_Abs      : constant Boolean     := ((File_Spec and IPST) /= 0);
        Rec_Format  : Record_Format_T;
        Rec_Len     : Integer     := Integer(Word_To_Integer_16(RAM.Read_Word(Pkt_Addr + IRCL)));
        Dest        : constant Dword_T     := RAM.Read_Dword(Pkt_Addr + IBAD);
        -- Read_Line   : Boolean     := (RAM.Read_Word(Pkt_Addr + IBIN) = 0);
        Bytes       : Byte_Arr_T(0 .. (if Rec_Len > 0 then Rec_Len-1 else Agent.Actions.Get_Default_Len_For_Channel (Chan_No)));
        Position    : Integer     := Dword_To_Integer(RAM.Read_Dword(Pkt_Addr + IRNH));
        Txfrd, Err  : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?READ - Channel:" &Chan_No'Image & " for PID:" & PID'Image & " TID:" & TID'Image);
        Loggers.Debug_Print (Debug_Log, "?READ - Channel:" &Chan_No'Image);
        if Open_Fmt then
            Loggers.Debug_Print (Sc_Log, "----- Default Record Type from ?OPEN");
        else 
            case Format_Bits is
                when PARU_32.RTDY => Rec_Format := Dynamic;
                when PARU_32.RTDS => Rec_Format := Data_Sensitive;
                when PARU_32.RTFX => Rec_Format := Fixed_Length;
                when PARU_32.RTVR => Rec_Format := Variable_Length;
                when PARU_32.RTUN => Rec_Format := Undefined_Length;
                when PARU_32.RTVB => Rec_Format := Variable_Block;
                when others =>
                    raise Unknown_Record_Type;
            end case;
            Loggers.Debug_Print (Sc_Log, "----- Default Record Type: " & Rec_Format'Image);
        end if;

        if Rec_Format = Variable_Length then
            Rec_Len := Rec_Len + 4;
        end if;

        if Is_Extd then
            Loggers.Debug_Print (Sc_Log, "----- Extended!");
        end if;
        if Test_DW_Bit(RAM.Read_Dword(Pkt_Addr + ETSP), 0) then
            Loggers.Debug_Print (Sc_Log, "----- Contains Screen Management Pkt");
        end if;
        if Test_DW_Bit(RAM.Read_Dword(Pkt_Addr + ETFT), 0) then
            Loggers.Debug_Print (Sc_Log, "----- Contains Field Translation Pkt");
        end if;
        AOSVS.Agent.Actions.File_Read (Chan_No,
                                        Is_Extd,
                                        Is_Abs,
                                        Open_Fmt,
                                        Rec_Format,
                                        Rec_Len,
                                        Bytes,
                                        Txfrd,
                                        Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        RAM.Write_Word(Pkt_Addr + IRLR, Txfrd);
        if Txfrd > 0 then
            for B in 0 .. Txfrd - 1 loop
                RAM.Write_Byte_BA(Dest + Dword_T(B), Bytes(Integer(B)));
            end loop;
        end if;
        Loggers.Debug_Print (Sc_Log, "----- Bytes Read:" & Txfrd'Image);
        return true;
    end Sys_READ;
    
    function Sys_RDB   (CPU : CPU_T; PID : Word_T) return Boolean is
        Pkt_Addr    : constant Phys_Addr_T := CPU.AC_PA(2);
        Chan_No     : constant Word_T      := CPU.AC_Wd(1);
        Blocks      : constant Unsigned_8  := Unsigned_8 (Get_Lower_Byte (RAM.Read_Word (Pkt_Addr + PARU_32.PSTI)));
        Start_Block : constant Unsigned_32 := Unsigned_32(RAM.Read_Dword(Pkt_Addr + PARU_32.PRNH));
        Buff_Addr   : constant Phys_Addr_T := Phys_Addr_T(RAM.Read_Dword(Pkt_Addr + PARU_32.PCAD));
        Txfrd, Err  : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?RDB - Channel:" & Chan_No'Image & 
                                     " Start Block: " & Start_Block'Image &
                                     " Blocks:" & Blocks'Image);
        Loggers.Debug_Print (Debug_Log, "?RDB");
        AOSVS.Agent.Actions.File_Read_Blocks (Chan_No, Blocks, Start_Block, Buff_Addr, Txfrd, Err);

        -- if there was no exception then we have read the number of bytes requested
        CPU.AC_Wd(1) := Txfrd;
        return true;
    end Sys_RDB;

    function Sys_WRITE (CPU : CPU_T; PID : Word_T; TID : Word_T; Logging : Boolean) return Boolean is
        Pkt_Addr    : constant Phys_Addr_T := CPU.AC_PA(2);
        Chan_No     : constant Word_T      := RAM.Read_Word(Pkt_Addr + ICH);
        File_Spec   : constant Word_T      := RAM.Read_Word(Pkt_Addr + ISTI);
        Format_Bits : constant Word_T      := File_Spec and 7;
        Open_Fmt    : constant Boolean     := ((File_Spec and ICRF) = 0);
        Change_Fmt  : constant Boolean     := ((File_Spec and ICRF) /= 0);
        Is_Extd     : constant Boolean     := ((File_Spec and IPKL) /= 0);
        Is_Abs      : constant Boolean     := ((File_Spec and IPST) /= 0);
        Is_Fixed    : constant Boolean     := ((File_Spec and 7) = RTFX);
        Is_DataSens : constant Boolean     := ((File_Spec and 7) = RTDS); -- ((File_Spec and Word_T(RTDS)) /= 0);
        Is_Dynamic  : constant Boolean     := ((File_Spec and 7) = RTDY); -- overrides Data Sens
        Rec_Format  : Record_Format_T;
        Rec_Len     : constant Integer     := Integer(Word_To_Integer_16(RAM.Read_Word(Pkt_Addr + IRCL)));
        Bytes_BA    : constant Dword_T     := RAM.Read_Dword(Pkt_Addr + IBAD);
        Position    : constant Integer     := Dword_To_Integer(RAM.Read_Dword(Pkt_Addr + IRNH));
        Txfrd, Err  : Word_T;
    begin
        if Logging then
            Loggers.Debug_Print (Sc_Log, "?WRITE - Channel:" & Chan_No'Image & " for PID:" & PID'Image & " TID:" & TID'Image);
            Loggers.Debug_Print (Debug_Log, "?WRITE - Channel:" & Chan_No'Image);
            -- raise Debugging with "Stopping at ?WRITE";
            Loggers.Debug_Print (Sc_Log, "------ ?ISTI: " & Word_To_String (File_Spec, Binary, 16, true) & " " & Word_To_String (File_Spec, Octal, 6, true));
            if Open_Fmt then
                Loggers.Debug_Print (Sc_Log, "------ Default Type from ?OPEN");
            end if;
            if Change_Fmt then
                Loggers.Debug_Print (Sc_Log, "------ ?ICRF - Change Format!");
            end if;
            if Is_Fixed then
                Loggers.Debug_Print (Sc_Log, "------ Fixed Len");
            end if;
            if Is_DataSens then
                Loggers.Debug_Print (Sc_Log, "------ Data Sensitive");
            end if;
            if Is_Dynamic then
                Loggers.Debug_Print (Sc_Log, "------ Dynamic, Rec Len: " & Rec_Len'Image);
            end if;
            if Is_Extd then
                Loggers.Debug_Print (Sc_Log, "------ Extended!");
            end if;
            if Test_DW_Bit(RAM.Read_Dword(Pkt_Addr + ETSP), 0) then
                Loggers.Debug_Print (Sc_Log, "------ Contains Screen Management Pkt");
            end if;
            if Test_DW_Bit(RAM.Read_Dword(Pkt_Addr + ETFT), 0) then
                Loggers.Debug_Print (Sc_Log, "------ Contains Field Translation Pkt");
            end if;
        end if;
        if Open_Fmt then
            Loggers.Debug_Print (Sc_Log, "------ Default Record Type from ?OPEN");
        else 
            case Format_Bits is
                when PARU_32.RTDY => Rec_Format := Dynamic;
                when PARU_32.RTDS => Rec_Format := Data_Sensitive;
                when PARU_32.RTFX => Rec_Format := Fixed_Length;
                when PARU_32.RTVR => Rec_Format := Variable_Length;
                when PARU_32.RTUN => Rec_Format := Undefined_Length;
                when PARU_32.RTVB => Rec_Format := Variable_Block;
                when others =>
                    raise Unknown_Record_Type;
            end case;
            Loggers.Debug_Print (Sc_Log, "----- Default Record Type: " & Rec_Format'Image);
        end if;
        AOSVS.Agent.Actions.File_Write (Chan_No,
                                        Is_Extd,
                                        Is_Abs,
                                        Open_Fmt,
                                        Rec_Format,
                                        Rec_Len,
                                        Bytes_BA,
                                        Position,
                                        Txfrd,
                                        Err);
        if Err /= 0 then
            CPU.AC(0) := Dword_T(Err);
            return false;
        end if;
        RAM.Write_Word(Pkt_Addr + IRLR, Txfrd);
        Dump_Packet (Pkt_Addr, IOSZ);
        if Logging then
            Loggers.Debug_Print (Sc_Log, "------ Bytes Written:" & Txfrd'Image);
        end if;
        return true;
    end Sys_Write;

    function Sys_GCHR (CPU : CPU_T; PID : Word_T) return Boolean is
        Device_Name  : Unbounded_String;
        Get_Defaults : constant Boolean := Test_DW_Bit (CPU.AC(1), 1);
        WD_1, WD_2, WD_3 : Word_T := 0;
    begin
        Loggers.Debug_Print (Sc_Log, "?GCHR"); Loggers.Debug_Print (Debug_Log, "?GCHR");
        if Test_DW_Bit (CPU.AC(1), 0) then
           -- ACO should contain a channel number which should already be open
           Loggers.Debug_Print (Sc_Log, "----- for channel no. " & CPU.AC(0)'Image);
           Device_Name := AOSVS.Agent.Actions.Get_Device_For_Channel(Lower_Word(CPU.AC(0)));
           if Device_Name = "***ERROR***" then
              CPU.AC(0) := Dword_T(ERICN); -- Illegal Channel No.
              return false;
           end if;
        else
           -- AC0 should be a BP to the target device name
           Device_Name := To_Unbounded_String (RAM.Read_String_BA(CPU.AC(0), false));
        end if;
        Loggers.Debug_Print (Sc_Log, "----- for device: " & To_String(Device_Name));
        if Get_Defaults then
            Loggers.Debug_Print (Sc_Log, "----- Fetching Default characteristics");
            AOSVS.Agent.Actions.Get_Default_Chars(Device_Name, WD_1, WD_2, WD_3);
        else
            Loggers.Debug_Print (Sc_Log, "----- Fetching Current characteristics");
            AOSVS.Agent.Actions.Get_Current_Chars(Device_Name, WD_1, WD_2, WD_3);
        end if;
        Loggers.Debug_Print (Sc_Log, "----- Word 1: " & Word_To_String (WD_1, Binary, 16, True));
        Loggers.Debug_Print (Sc_Log, "----- Word 2: " & Word_To_String (WD_2, Binary, 16, True));
        Loggers.Debug_Print (Sc_Log, "----- Word 3: " & Word_To_String (WD_3, Binary, 16, True));
        RAM.Write_Word(Phys_Addr_T(CPU.AC(2)), WD_1);
        RAM.Write_Word(Phys_Addr_T(CPU.AC(2))+1, WD_2);
        RAM.Write_Word(Phys_Addr_T(CPU.AC(2))+2, WD_3);
        return true;
    end Sys_GCHR;

    function Sys_SCHR (CPU : CPU_T; PID : Word_T) return Boolean is
        Device_Name  : Unbounded_String;
        Set_Defaults : Boolean := Test_DW_Bit (CPU.AC(1), 1);
        WD_1 : constant Word_T := RAM.Read_Word(CPU.AC_PA(2));
        WD_2 : constant Word_T := RAM.Read_Word(CPU.AC_PA(2)+1);
        WD_3 : constant Word_T := RAM.Read_Word(CPU.AC_PA(2)+2);
        Old_WD_1, Old_Wd_2, Old_Wd_3 : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?SCHR");
        if Test_DW_Bit (CPU.AC(1), 0) then
           -- ACO should contain a channel number which should already be open
           Device_Name := AOSVS.Agent.Actions.Get_Device_For_Channel(Lower_Word(CPU.AC(0)));
           if Device_Name = "***ERROR***" then
              CPU.AC(0) := Dword_T(ERICN); -- Illegal Channel No.
              return false;
           end if;
        else
           -- AC0 should be a BP to the target device name
           Device_Name := To_Unbounded_String (RAM.Read_String_BA(CPU.AC(0), false));
        end if;
        Loggers.Debug_Print (Sc_Log, "----- for device: " & To_String(Device_Name));
        Agent.Actions.Get_Current_Chars(Device_Name, Old_WD_1, Old_WD_2, Old_WD_3);
        Loggers.Debug_Print (Sc_Log, "----- Old Chars: " &
                            Word_To_String(Old_WD_1, Binary, 16, true) & " " &
                            Word_To_String(Old_WD_2, Binary, 16, true) & " " &
                            Word_To_String(Old_WD_3, Binary, 16, true));
        Loggers.Debug_Print (Sc_Log, "----- New Chars: " &
                            Word_To_String(WD_1, Binary, 16, true) & " " &
                            Word_To_String(WD_2, Binary, 16, true) & " " &
                            Word_To_String(WD_3, Binary, 16, true));   
        Loggers.Debug_Print (Sc_Log, "----- Not Yet Implemented!");                           
        return true;
    end Sys_SCHR;

    function Sys_SEND  (CPU : CPU_T; PID : Word_T) return Boolean is
        Msg_BA    : constant Dword_T := CPU.AC(1);
        Dest_Type : constant Dword_T := Get_DW_Bits (CPU.AC(3), 22, 2);
        Msg_Len   : Natural := Dword_To_Integer(CPU.AC(2) and 16#0000_00ff#);
        Msg_Str   : constant String  := RAM.Read_String_BA (Msg_BA, true); -- TODO should we use length?
        Dest_PID  : Word_T;
    begin
        Loggers.Debug_Print (Sc_Log, "?SEND");
        case Dest_Type is
            when 0 => -- PID
                Dest_PID := Lower_Word (CPU.AC(0));
            --when 1 => -- BA of Proc Name
            --when 2 => -- BA of Console Name
            when others =>
                CPU.AC(0) := Dword_T(PARU_32.ERPRE);
                Loggers.Debug_Print (Sc_Log, "----- Unknown Destination type:" & Dest_Type'Image);
                return false;
        end case;
        Loggers.Debug_Print (Sc_Log, "----- Dest, PID:" & Dest_PID'Image);
        Loggers.Debug_Print (Sc_Log, "----- Message  : " & Msg_Str);
        Agent.Actions.Send_Msg(Dest_PID, Msg_Str, PID);
        return true;
    end Sys_SEND;

end AOSVS.File_IO;