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

with Ada.Text_IO;
with GNAT.OS_Lib;

with Devices.Bus;
with Memory;

package body Devices.Console is

    protected body TTIn is

        procedure Init is
        begin
            Devices.Bus.Actions.Set_Reset_Proc (Devices.TTI, Reset'Access);
            Devices.Bus.Actions.Set_Data_In_Proc (Devices.TTI, Data_In'Access);
            Devices.Bus.Actions.Set_Data_Out_Proc (Devices.TTI, Data_Out'Access);
        end Init;

        procedure Reset is
        begin
            -- Stub
            Ada.Text_IO.Put_Line ("INFO: TTI Reset");
        end Reset;

        -- Insert_Byte places one byte in the TTI buffer fr handling by the CPU
        procedure Insert_Byte (B : in Byte_T) is
        begin
            TTI_Dev.One_Char_Buff := B;
            Devices.Bus.States.Set_Done( Devices.TTI, true);
            -- send IRQ if not masked out
            if Devices.Bus.States.Is_Dev_Masked (14) = false then
               Devices.Bus.States.Send_Interrupt(Devices.TTI, 14);
            end if;
        end Insert_Byte;

        procedure  Data_In (ABC : in Character; IO_Flag : in IO_Flag_T; Datum : out Word_T) is
        begin
            Datum := Word_T(TTI_Dev.One_Char_Buff);
            if ABC = 'A' then
                case IO_Flag is
                    when S =>
                        Devices.Bus.States.Set_Busy( Devices.TTI, true);
                        Devices.Bus.States.Set_Done( Devices.TTI, false);
                    when  C =>
                        Devices.Bus.States.Set_Busy( Devices.TTI, false);
                        Devices.Bus.States.Set_Done( Devices.TTI, false);
                    when others =>
                        Ada.Text_IO.Put_Line("ERROR: Unknown I/O flag");
                        GNAT.OS_Lib.OS_Exit (1);
                end case;
            else
                Ada.Text_IO.Put_Line("ERROR: Unknown Data I/O Buffer for DIx ac,TTI instruction");
                GNAT.OS_Lib.OS_Exit (1);
            end if;
        end Data_In;

        -- Data_Out is only here to support NIO commands to TTI
        procedure Data_Out( Datum : in Word_T; ABC : in Character; IO_Flag : in IO_Flag_T) is
        begin
            case ABC is
                when 'N' =>
                    case IO_Flag is
                        when S =>
                            Devices.Bus.States.Set_Busy( Devices.TTI, true);
                            Devices.Bus.States.Set_Done( Devices.TTI, false);
                        when C =>
                            Devices.Bus.States.Set_Busy( Devices.TTI, false);
                            Devices.Bus.States.Set_Done( Devices.TTI, false);
                        when others =>
                            Ada.Text_IO.Put_Line("ERROR: Unknown I/O flag");
                            GNAT.OS_Lib.OS_Exit (1);
                    end case;
                when others =>
                    Ada.Text_IO.Put_Line("ERROR: Unknown Data I/O Buffer for DOx ac,TTI instruction");
                    GNAT.OS_Lib.OS_Exit (1);
            end case;
        end Data_Out;
       
    end TTIn;

    protected body TTOut is

        procedure Init (Conn : GNAT.Sockets.Stream_Access) is
        begin
            TTO_Dev.Conn_Stream := Conn;
            Devices.Bus.Actions.Set_Reset_Proc (Devices.TTO, Reset'Access);
            Devices.Bus.Actions.Set_Data_Out_Proc (Devices.TTO, Data_Out'Access);
        end;

        procedure Put_Byte (B : in Byte_T) is
        begin
            Byte_T'Output ( TTO_Dev.Conn_Stream, B);
        end Put_Byte;

        procedure Put_Char (C : in Character) is
        begin
            Character'Output ( TTO_Dev.Conn_Stream, C);
        end Put_Char;

        procedure Put_String (S : in String) is
        begin
            for Ix in S'Range loop
               Put_Char ( S(Ix) );
            end loop;
        end Put_String;

        -- Reset simply clears the screen or throws a page
        procedure Reset is
        begin
            Put_Char (ASCII.FF);
            Ada.Text_IO.Put_Line ("INFO: TTO Reset");
        end Reset;

        procedure Data_Out( Datum : in Word_T; ABC : in Character; IO_Flag : in IO_Flag_T) is
            ASCII_Byte : Byte_T;
        begin
            case ABC is
                when 'A' =>
                    ASCII_Byte := Memory.Get_Lower_Byte(Datum);
                    if IO_Flag = S then
                        Devices.Bus.States.Set_Busy( Devices.TTO, true);
                        Devices.Bus.States.Set_Done( Devices.TTO, false);
                    end if;
                    Put_Byte (ASCII_Byte);
                    Devices.Bus.States.Set_Busy( Devices.TTO, false);
                    Devices.Bus.States.Set_Done( Devices.TTO, true);
                    -- send IRQ if not masked out
                    if Devices.Bus.States.Is_Dev_Masked (15) = false then
                        Devices.Bus.States.Send_Interrupt(Devices.TTO, 15);
                    end if;
                when 'N' =>
                    case IO_Flag is
                        when S =>
                            Devices.Bus.States.Set_Busy( Devices.TTO, true);
                            Devices.Bus.States.Set_Done( Devices.TTO, false);
                        when C =>
                            Devices.Bus.States.Set_Busy( Devices.TTO, false);
                            Devices.Bus.States.Set_Done( Devices.TTO, false);
                        when others =>
                            Ada.Text_IO.Put_Line("ERROR: Unknown I/O flag");
                            GNAT.OS_Lib.OS_Exit (1);
                    end case;
                when others =>
                    Ada.Text_IO.Put_Line("ERROR: Unknown Data I/O Buffer for DOx ac,TTO instruction");
                    GNAT.OS_Lib.OS_Exit (1);
            end case;
        end Data_Out;

    end TTOut;


end Devices.Console;