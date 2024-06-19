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

package AOSVS.File_IO is

    function Sys_CLOSE (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean;
    function Sys_GCLOSE(CPU : CPU_T; PID : Word_T) return Boolean;
    function Sys_GOPEN (CPU : CPU_T; PID : Word_T) return Boolean;
    function Sys_OPEN  (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean;
    function Sys_READ  (CPU : CPU_T; PID : Word_T; TID : Word_T) return Boolean;
    function Sys_RDB   (CPU : CPU_T; PID : Word_T) return Boolean;
    function Sys_WRITE (CPU : CPU_T; PID : Word_T; TID : Word_T; Logging : Boolean) return Boolean;

    function Sys_GCHR  (CPU : CPU_T; PID : Word_T) return Boolean;
    function Sys_SCHR  (CPU : CPU_T; PID : Word_T) return Boolean;

    function Sys_SEND  (CPU : CPU_T; PID : Word_T) return Boolean;

end AOSVS.File_IO;