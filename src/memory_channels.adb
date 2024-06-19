--  Copyright ©2021 - 2024 Stephen Merrony
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

with Ada.Text_IO;           use Ada.Text_IO;

with Debug_Logs; use Debug_Logs;
with Devices;
with Devices.Bus;
with Memory;     use Memory;

package body Memory_Channels is

   protected body BMC_DCH is

      procedure Init (Debug_logging : Boolean) is
      begin
         Is_Logging := Debug_logging;
         Devices.Bus.Actions.Set_Reset_Proc    (Devices.BMC, Reset'Access);
         Devices.Bus.Actions.Set_Data_In_Proc  (Devices.BMC, Data_In'Access);
         -- Devices.Bus.Actions.Set_Data_Out_Proc (Devices.BMC, Data_Out'Access);
         -- Reset;
      end Init;

      procedure Reset is
      begin
         for R in Registers'Range loop
            Registers (R) := 0;
         end loop;
         Registers (IO_Chan_Def_Reg)    := IOC_CDR_1;
         Registers (IO_Chan_Status_Reg) := IOC_SR_1A or IOC_SR_1B;
         Registers (IO_Chan_Mask_Reg)   :=
           IOC_MR_MK1 or IOC_MR_MK2 or IOC_MR_MK3 or IOC_MR_MK4 or
           IOC_MR_MK5 or IOC_MR_MK6;
         Status_Reg := SR_BMC; 
         Put_Line ("INFO: BMC_DCH Registers Reset");
      end Reset;

      procedure Set_Logging (Debug_Logging : Boolean) is
      begin
         Is_Logging := Debug_Logging;
      end Set_Logging;

      procedure Data_In  (ABC : IO_Reg_T; IO_Flag : IO_Flag_T; Datum : out Word_T) is
      begin
         case ABC is
            when A => 
               Datum := Read_Reg (IO_Chan_Status_Reg); -- FIXME this is a guess!
            when B => 
               Datum := Read_Reg (IO_Chan_Mask_Reg);   -- FIXME this is a guess!  
            when C => 
               Datum := Status_Reg;    -- FIXME this is a guess, but see p.5-82 of S140 Prog Ref.
            when others =>
               raise Not_Yet_Implemented with "DIB/C to BMC/DCH";
         end case;
      end Data_In;


      function Read_Reg (Reg : Integer) return Word_T is (Registers (Reg));

      -- Write_Reg populates a given 16-bit register with the supplied data
      -- N.B. Addressed by REGISTER not slot
      procedure Write_Reg (Reg : Integer; Datum : Word_T) is
      begin
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Write_Reg with Register: " & Reg'Image & " Datum:" &
               Datum'Image);
         end if;
         if Reg = IO_Chan_Def_Reg then
            -- certain bits in the new data cause IOCDR bits to be flipped rather than set
            for B in 0 .. 15 loop
               case B is
                  when 3 | 4 | 7 | 8 | 14 =>
                     if Test_W_Bit (Datum, B) then
                        Flip_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     end if;
                  when others =>
                     if Test_W_Bit (Datum, B) then
                        Set_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     else
                        Clear_W_Bit (Registers (IO_Chan_Def_Reg), B);
                     end if;
               end case;
            end loop;
         else
            Registers (Reg) := Datum;
         end if;
      end Write_Reg;

      -- Write_Slot populates a whole SLOT (pair of registers) with the supplied doubleword
      -- N.B. Addressed by SLOT not register
      procedure Write_Slot (Slot : Integer; Datum : Dword_T) is
      begin
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Write_Slot with Slot: " & Slot'Image & " Datum:" &
               Datum'Image);
         end if;
         Registers (Slot * 2)       := Upper_Word (Datum);
         Registers ((Slot * 2) + 1) := DG_Types.Lower_Word (Datum);
      end Write_Slot;

      function Get_DCH_Mode return Boolean is
         (Test_W_Bit (Registers (IO_Chan_Def_Reg), 14));

      function Resolve_BMC_Mapped_Addr (M_Addr : Phys_Addr_T) return Phys_Addr_T is
         Slot           : constant Integer := Integer(Shift_Right(M_Addr, 10));
         P_Addr, P_Page : Phys_Addr_T;
      begin
         -- N.B. at some point between 1980 and 1987 the lower 5 bits of the odd word were
	      -- prepended to the even word to extend the mappable space 
         P_Page := Shift_Left (Phys_Addr_T(Registers(Slot * 2)) and 16#0000_001f#, 16) +
                   Shift_Left (Phys_Addr_T(Registers((Slot * 2) + 1)), 10);
         P_Addr := (M_Addr and 16#0000_03ff#) or P_Page;
         return P_Addr;
      end Resolve_BMC_Mapped_Addr;

      function Resolve_DCH_Mapped_Addr (M_Addr : Phys_Addr_T) return Phys_Addr_T is
         P_Addr, P_Page : Phys_Addr_T;
         Slot           : Integer;
         Offset         : Phys_Addr_T;
      begin
         -- the slot is up to 9 bits long
         Slot :=
           --  Integer (Shift_Right (M_Addr, 10) and 16#001f#) + First_DCH_Slot;
           Integer (Shift_Right (M_Addr, 10)) + First_DCH_Slot;
         if (Slot < First_DCH_Slot) or
           (Slot > (First_DCH_Slot + Num_DCH_Slots))
         then
            raise Invalid_DCH_Slot;
         end if;
         --  Offset := M_Addr and 16#0000_03ff#;
         Offset := M_Addr;
         -- N.B. at some point between 1980 and 1987 the lower 5 bits of the odd word were
         -- prepended to the even word to extend the mappable space
         P_Page :=
           Shift_Left
             --  (Phys_Addr_T (Registers (Slot * 2) and 16#0000_001f#), 16) or
             (Phys_Addr_T (Registers (Slot * 2)), 16) or
           Phys_Addr_T (Registers ((Slot * 2) + 1));
         P_Addr := Shift_Left (P_Page, 10) or Offset;
         if Is_Logging then
            Loggers.Debug_Print
              (Map_Log,
               "Resolve_DCH_Mapped_Addr got: " & M_Addr'Image &
               " Returning: " & P_Addr'Image);
         end if;
         return P_Addr;
      end Resolve_DCH_Mapped_Addr;

      procedure Write_Word_BMC_Chan (Unmapped : in out Phys_Addr_T; Datum : Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : constant BMC_Addr_T := Decode_BMC_Addr (Unmapped);
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Unmapped); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         RAM.Write_Word (P_Addr, Datum);
         Unmapped := Unmapped + 1;
      end Write_Word_BMC_Chan;

      procedure Write_Word_DCH_Chan
        (Unmapped : in out Phys_Addr_T; Datum : Word_T)
      is
         P_Addr : Phys_Addr_T;
      begin
         if Get_DCH_Mode then
            P_Addr := Resolve_DCH_Mapped_Addr (Unmapped);
         else
            P_Addr := Unmapped;
         end if;
         RAM.Write_Word (P_Addr, Datum);
         -- auto-increment the supplied address
         Unmapped := Unmapped + 1;
      end Write_Word_DCH_Chan;

      function Decode_BMC_Addr (Unmapped : Phys_Addr_T) return BMC_Addr_T is
         In_Addr : constant Phys_Addr_T := Shift_Left(Unmapped, 10);
         Res : BMC_Addr_T;
      begin
         Res.Is_Logical := Test_DW_Bit (Dword_T(In_Addr), 0);
         if Res.Is_Logical then
            Res.TT   := Natural(Get_DW_Bits(Dword_T(In_Addr), 2, 5));
            Res.TTR  := Natural(Get_DW_Bits(Dword_T(In_Addr), 7, 5));
            Res.P_Low := Unmapped and 16#0000_3fff#;
         else
            Res.Bk  := Natural(Get_DW_Bits(Dword_T(In_Addr), 1, 3));
            Res.XCA := Natural(Get_DW_Bits(Dword_T(In_Addr), 4, 3));
            Res.CA  := Unmapped and 16#0000_7fff#;
         end if;
         return Res;
      end Decode_BMC_Addr;

      procedure Read_Word_BMC_Chan (Unmapped : in out Phys_Addr_T; Datum : out Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : constant BMC_Addr_T := Decode_BMC_Addr (Unmapped);
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Unmapped); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         Datum := RAM.Read_Word (P_Addr);
         Unmapped := Unmapped + 1;
      end Read_Word_BMC_Chan;

      procedure Read_Word_BMC_16 (Unmapped : in out Word_T; Datum : out Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : constant BMC_Addr_T := Decode_BMC_Addr (Phys_Addr_T(Unmapped));
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Phys_Addr_T(Unmapped)); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         Datum := RAM.Read_Word (P_Addr);
         Unmapped := Unmapped + 1;
      end Read_Word_BMC_16;

      procedure Write_Word_BMC_16 (Unmapped : in out Word_T; Datum : Word_T) is
         P_Addr : Phys_Addr_T;
         Decoded : constant BMC_Addr_T := Decode_BMC_Addr (Phys_Addr_T(Unmapped));
      begin
         if Decoded.Is_Logical then
            P_Addr := Resolve_BMC_Mapped_Addr (Phys_Addr_T(Unmapped)); -- FIXME
         else
            P_Addr := Decoded.CA;
         end if;
         RAM.Write_Word (P_Addr, Datum);
         Unmapped := Unmapped + 1;
      end Write_Word_BMC_16;

   end BMC_DCH;

end Memory_Channels;
