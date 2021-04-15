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

with GNAT.Sockets;

with DG_Types;   use DG_Types;

package Devices.Console is

    type TTI_T is record
       One_Char_Buff : Byte_T;
    end record;

    protected TTIn is
        procedure Init;
        procedure Reset;
        procedure Insert_Byte (B : in Byte_T);
        procedure Data_In (ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T; Datum : out Word_T);
        procedure Data_Out( Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T);
    private
        TTI_Dev : TTI_T;
    end TTIn;

    protected TTOut is
        procedure Init (Sock : in GNAT.Sockets.Socket_Type);
        procedure Reset;
        procedure Put_Byte (B : in Byte_T);
        procedure Put_Char (C : in Character);
        procedure Put_String (S : in String);
        procedure Data_Out( Datum : in Word_T; ABC : in IO_Reg_T; IO_Flag : in IO_Flag_T);
    private 
        SCP_Chan : GNAT.Sockets.Stream_Access;
    end TTOut;

    task SCP_Handler is
      entry Set_SCP_IO (SCP : in Boolean);
      entry Get_SCP_IO (SCP : out Boolean);
      entry Set_SCP_Line_Ready (Buffer : in Unbounded_String);
      entry SCP_Get_Line (Line : out Unbounded_String);
    end SCP_Handler;

    task Console_Handler is   
      entry Start (Sock : in GNAT.Sockets.Socket_Type);
   end Console_Handler;
   
   IO_Error : exception;

end Devices.Console;