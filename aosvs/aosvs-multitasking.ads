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

with Processor;     use Processor;

package AOSVS.Multitasking is

   function Sys_IFPU    (CPU : CPU_T) return Boolean;
   function Sys_KILAD   (CPU : CPU_T; PID : Word_T; Kill_Addr : out Phys_Addr_T) return Boolean;
   function Sys_PRI     (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean;
   function Sys_REC     (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean;
   function Sys_TLOCK   (CPU : CPU_T; Is_Locked : in out Boolean) return Boolean;
   function Sys_UIDSTAT (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean;
   function Sys_WDELAY  (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean;

end AOSVS.Multitasking;