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

with Memory;    use Memory;

package body Resolver is

    function Resolve_32bit_Indirectable_Addr (ATU : in Boolean; I_Addr : in Dword_T) return Phys_Addr_T is
        Eff : Dword_T := I_Addr;
    begin
        while Test_DW_Bit(Eff, 0) loop
           Eff := RAM.Read_Dword(Phys_Addr_T(Eff and 16#7fff_ffff#));
        end loop;
        if not ATU then
            Eff := Eff and 16#01ff_ffff#;
        end if;
        return Phys_Addr_T(Eff);
    end Resolve_32bit_Indirectable_Addr;

end Resolver;