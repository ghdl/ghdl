--  GHDL Run Time (GRT) stdio subprograms for GRT types.
--  Copyright (C) 2002 - 2014 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Vhdl_Types; use Grt.Vhdl_Types;

package Grt.Astdio.Vhdl is
   pragma Preelaborate (Grt.Astdio.Vhdl);

   --  Display time with unit, without space.
   --  Eg: 10ns, 100ms, 97ps...
   procedure Put_Time (Stream : FILEs; Time : Std_Time);

   --  Put STR using put procedures.
   procedure Put_Str_Len (Stream : FILEs; Str : Ghdl_Str_Len_Type);

   --  Put " to " or " downto ".
   procedure Put_Dir (Stream : FILEs; Dir : Ghdl_Dir_Type);
end Grt.Astdio.Vhdl;
