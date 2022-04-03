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

with Ada.Streams;
with Ada.Text_IO;
with GNAT.OS_Lib;

use type Ada.Streams.Stream_Element_Count;

with Processor;
with Devices.Bus;
with DG_Types;    use DG_Types;
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

        -- Insert_Byte places one byte in the TTI buffer fr handling by the Processor
        procedure Insert_Byte (B : in Byte_T) is
        begin
            TTI_Dev.One_Char_Buff := B;
            Devices.Bus.States.Set_Done( Devices.TTI, true);
            -- send IRQ if not masked out
            if Devices.Bus.States.Is_Dev_Masked (14) = false then
               Devices.Bus.States.Send_Interrupt(Devices.TTI);
            end if;
        end Insert_Byte;

        procedure  Data_In (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T) is
        begin
            Datum := Word_T(TTI_Dev.One_Char_Buff);
            if ABC = A then
                case IO_Flag is
                    when S =>
                        Devices.Bus.States.Set_Busy( Devices.TTI, true);
                        Devices.Bus.States.Set_Done( Devices.TTI, false);
                    when  C =>
                        Devices.Bus.States.Set_Busy( Devices.TTI, false);
                        Devices.Bus.States.Set_Done( Devices.TTI, false);
                    when None =>
                        null;
                    when others =>
                        -- Ada.Text_IO.Put_Line("ERROR: Unknown I/O flag");
                        -- GNAT.OS_Lib.OS_Exit (1);
                        raise IO_Error with "Unknown I/O flag in DIx instruction";
                end case;
            else
                Ada.Text_IO.Put_Line("ERROR: Unknown Data I/O Buffer for DIx ac,TTI instruction");
                GNAT.OS_Lib.OS_Exit (1);
            end if;
        end Data_In;

        -- Data_Out is only here to support NIO commands to TTI
        procedure Data_Out( Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T) is
        begin
            case ABC is
                when N =>
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
                    -- Ada.Text_IO.Put_Line("ERROR: Unknown Data I/O Buffer for DOx ac,TTI instruction");
                    -- GNAT.OS_Lib.OS_Exit (1);
                    raise IO_Error with "Unknown Data I/O Buffer for DOx ac,TTI instruction";
            end case;
        end Data_Out;
       
    end TTIn;

    protected body TTOut is

        procedure Init (Sock : in GNAT.Sockets.Socket_Type) is
        begin
            Devices.Bus.Actions.Set_Reset_Proc (Devices.TTO, Reset'Access);
            Devices.Bus.Actions.Set_Data_Out_Proc (Devices.TTO, Data_Out'Access);
            SCP_Chan := GNAT.Sockets.Stream (Sock);
        end;

        -- Reset simply clears the screen or throws a page
        procedure Reset is
        begin
            TTOut.Put_Char (ASCII.FF);
            Ada.Text_IO.Put_Line ("INFO: TTO Reset");
        end Reset;
        
        procedure Put_Byte (B : in Byte_T) is
        begin
               Byte_T'Output (SCP_Chan, B);
        end Put_Byte;
 
        procedure Put_Char (C : in Character) is
        begin
            Character'Output (SCP_Chan, C);
        end Put_Char;

        procedure Put_String (S : in String) is
        begin
            for C of S loop
                Character'Output (SCP_Chan, C);
            end loop;
        end Put_String;

        procedure Data_Out( Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T) is
            ASCII_Byte : Byte_T;
        begin
            case ABC is
                when A =>
                    ASCII_Byte := Get_Lower_Byte(Datum);
                    if IO_Flag = S then
                        Devices.Bus.States.Set_Busy( Devices.TTO, true);
                        Devices.Bus.States.Set_Done( Devices.TTO, false);
                    end if;
                    -- if ASCII_Byte = 93 then raise IO_Error with "Debugging dev #"; end if;
                    TTOut.Put_Byte (ASCII_Byte);
                    Devices.Bus.States.Set_Busy( Devices.TTO, false);
                    Devices.Bus.States.Set_Done( Devices.TTO, true);
                    -- send IRQ if not masked out
                    if Devices.Bus.States.Is_Dev_Masked (15) = false then
                        Devices.Bus.States.Send_Interrupt(Devices.TTO);
                    end if;
                when N =>
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

    task body SCP_Handler is
      -- SCP_IO  : Boolean; -- True if console I/O is directed to the SCP
      SCP_Buffer : Unbounded_String;
      SCP_Line_Ready : Boolean := false;
    begin
      loop
         select
            accept Set_SCP_Line_Ready (Buffer : in Unbounded_String) do  
                SCP_Buffer := Buffer;  
                SCP_Line_Ready := true;
            end Set_SCP_Line_Ready;
         or
            when SCP_Line_Ready =>
               accept SCP_Get_Line (Line : out Unbounded_String) do
                  Line := SCP_Buffer;
               end SCP_Get_Line;
               SCP_Buffer := Null_Unbounded_String;
               SCP_Line_Ready := false;
         or
            accept Set_SCP_IO (SCP : in Boolean) do
               SCP_IO := SCP;
            end Set_SCP_IO;   
         or
            accept Get_SCP_IO (SCP : out Boolean) do
               SCP := SCP_IO;
            end Get_SCP_IO;  
         or
            terminate; 
         end select;
      end loop;
   end SCP_Handler;

   task body Console_Handler is
      SCP_Buffer : Unbounded_String;
      SCP_Line_Ready : Boolean := false;
      SCP_Chan    : GNAT.Sockets.Stream_Access;
      One_Char : Character;
      SCP_IO : Boolean;
   begin
      accept Start (Sock : in GNAT.Sockets.Socket_Type) do
         SCP_Chan := GNAT.Sockets.Stream (Sock);
         SCP_Handler.Set_SCP_IO (true);
      end Start;

      loop   
        One_Char := Byte_To_Char(Byte_T'Input (SCP_Chan));
        SCP_Handler.Get_SCP_IO (SCP_IO);
        if One_Char = ASCII.ESC then
            SCP_Handler.Set_SCP_IO (true);
            -- Processor.Set_SCPIO(true);
        elsif SCP_IO then
            case One_Char is
                when Dasher_NL | Dasher_CR =>
                    SCP_Handler.Set_SCP_Line_Ready (SCP_Buffer);
                    SCP_Buffer := Null_Unbounded_String;
                when Dasher_Delete =>
                    if length (SCP_Buffer) > 0 then
                    Character'Output (SCP_Chan, Dasher_Cursor_Left);
                    SCP_Buffer := Head(SCP_Buffer, length (SCP_Buffer) - 1);
                    end if;
                when others =>
                    Character'Output (SCP_Chan, One_Char);
                    SCP_Buffer := SCP_Buffer & One_Char;
            end case;
        else
            TTIn.Insert_Byte (Char_To_Byte(One_Char));
        end if;
            
      end loop;
   end Console_Handler;

end Devices.Console;