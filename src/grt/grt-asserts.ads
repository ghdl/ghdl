--  GHDL Run Time (GRT) -  misc subprograms.
--  Copyright (C) 2002 - 2016 Tristan Gingold
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
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Severity; use Grt.Severity;

package Grt.Asserts is
   procedure Inc_Assert_Count (Level : Severity_Level);

   function Ghdl_Get_Assert_Count (Level : Ghdl_E8) return Std_Integer_64;
   procedure Ghdl_Clear_Assert_Count;

private
   pragma Export (C, Ghdl_Get_Assert_Count,
                  "std__env__get_assert_count");
   pragma Export (C, Ghdl_Clear_Assert_Count,
                  "std__env__clear_assert_count");
end Grt.Asserts;
