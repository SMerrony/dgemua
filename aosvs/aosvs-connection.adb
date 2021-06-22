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

with Debug_Logs;    use Debug_Logs;

package body AOSVS.Connection is

   function Sys_CON (CPU : in out CPU_T; PID : in Word_T) return Boolean is
   begin
      Loggers.Debug_Print (Sc_Log, "?CON - to PID" & CPU.AC(0)'Image & " - Ignoring");
      return true;
   end Sys_CON;

   function Sys_DCON (CPU : in out CPU_T; PID : in Word_T) return Boolean is
   begin
      Loggers.Debug_Print (Sc_Log, "?DCON - to PID" & CPU.AC(1)'Image & " - Ignoring");
      CPU.AC(0) := CPU.AC(1);
      return true;
   end Sys_DCON;

   function Sys_SERVE (CPU : in out CPU_T; PID : in Word_T) return Boolean is
   begin
      Loggers.Debug_Print (Sc_Log, "?SERVE - Ignoring");
      return true;
   end Sys_SERVE;

end AOSVS.Connection;