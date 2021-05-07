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

with Ada.Directories;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Interfaces;            use Interfaces;

with DG_Types; use DG_Types;
with PARU_32;  use PARU_32;

with AOSVS.Agent;
with Memory;        use Memory;

package body AOSVS is

    type Word_Arr_T is array (Positive range <>) of Word_T;

    function Read_Whole_File (Name : in String) return Word_Arr_T is
        File_ByteSize : Integer := Integer(Ada.Directories.Size (Name));
        File_WordSize : Integer := File_ByteSize / 2;
        PR_File       : File_Type;
        PR_Stream     : Stream_Access;
        PR_Arr        : Word_Arr_T (0 .. File_WordSize - 1);
        Word          : Word_T;
    begin
        if (File_ByteSize mod 2) /= 0 then
            raise Invalid_PR_File with "Size must be even";
        end if;
        begin
            Open (File => PR_File, Mode => In_File, Name => Name);
        exception
            when others =>
            raise Invalid_PR_File with "Could not open file";
        end;
        PR_Stream := Stream (PR_File);
        for W in 0 .. File_WordSize - 1 loop
           Word_T'Read(PR_Stream, Word);
           PR_Arr(W) := Word;
        end loop;

        return PR_Arr;

        exception
           when others =>
           raise Invalid_PR_File;
    end Read_Whole_File;

    function Load_UST (PR_Arr : in Word_Arr_T) return UST_T is
        This_UST : UST_T;
    begin
        This_UST.Ext_Var_Wd_Count := PR_Arr(Integer(UST + USTEZ));
        This_UST.Ext_Var_P0_Start := PR_Arr(Integer(UST + USTES));
        This_UST.Syms_Start       := Dword_From_Two_Words (PR_Arr(Integer(UST + USTSS)), PR_Arr(Integer(UST + USTSS+1)));
        This_UST.Syms_End         := Dword_From_Two_Words (PR_Arr(Integer(UST + USTSE)), PR_Arr(Integer(UST + USTSE+1)));
        This_UST.Debug_Addr       := Phys_Addr_T(Dword_From_Two_Words (PR_Arr(Integer(UST + USTDA)), PR_Arr(Integer(UST + USTDA+1))));
        This_UST.Revision         := Dword_From_Two_Words (PR_Arr(Integer(UST + USTRV)), PR_Arr(Integer(UST + USTRV+1)));
        This_UST.Task_Count       := PR_Arr(Integer(UST + USTTC));
        This_UST.Impure_Blocks    := PR_Arr(Integer(UST + USTBL + 1));
        This_UST.Shared_Start_Block := Dword_From_Two_Words (PR_Arr(Integer(UST + USTST)), PR_Arr(Integer(UST + USTST+1)));
        This_UST.Int_Addr         := Phys_Addr_T(Dword_From_Two_Words (PR_Arr(Integer(UST + USTIT)), PR_Arr(Integer(UST + USTIT+1))));
        This_UST.Shared_Block_Count := PR_Arr(Integer(UST + USTSZ));
        This_UST.Shared_Start_Page_In_PR := Dword_From_Two_Words (PR_Arr(Integer(UST + USTSH)), PR_Arr(Integer(UST + USTSH+1)));
        This_UST.PR_Type          := PR_Arr(Integer(UST + USTPR));
        return This_UST;
    end Load_UST;

    procedure Start (PR_Name   : in String;
                    Virt_Root : in String;
                    Segment   : in Natural;
                    Arg_Count : in Positive;
                    Args      : in Args_Arr;
                    Logging   : in Boolean) is
        PR_Arr : Word_Arr_T := Read_Whole_File (PR_Name);
        PID    : PID_T;
        Sixteen_Bit : Boolean;
        PR_UST : UST_T;
        Segment_Base : Phys_Addr_T;
    begin
        PR_Arr := Read_Whole_File (PR_Name);
        PR_UST := Load_UST (PR_Arr);
        Sixteen_Bit := Test_W_Bit (PR_UST.PR_Type, 0);
        AOSVS.Agent.Actions.Allocate_PID(Args,Virt_Root,Sixteen_Bit,"DUMMY",PID);
        Ada.Text_IO.Put_Line ("INFO: Obtained PID" & PID'Image & " for process");
        Ada.Text_IO.Put_Line ("INFO: Preparing Ring" & Segment'Image & " process with up to" &
                              PR_UST.Task_Count'Image & " tasks");
        Segment_Base := Shift_Left( Phys_Addr_T(Segment), 28);

        -- map the program into memory, 1st the unshared, then the shared portion
        RAM.Map_Range(Segment_Base, 
                      Memory_Region(PR_Arr(8192 .. Integer(Shift_Left(PR_UST.Shared_Start_Page_In_PR, 10)) - 8)), 
                      false);
        RAM.Map_Range(Segment_Base + Phys_Addr_T(Shift_Left(PR_UST.Shared_Start_Block, 10)),
                      Memory_Region(PR_Arr(Integer(Shift_Left(PR_UST.Shared_Start_Page_In_PR, 10)) .. PR_Arr'Last)), 
                      true);


    end Start;

end AOSVS;