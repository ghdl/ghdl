--  GHDL Run Time (GRT) -  asserts subprograms.
--  Copyright (C) 2025 Tristan Gingold
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

package body Grt.Asserts is
   type Natural_Array is array (Natural range <>) of Std_Integer;
   Assert_Count : Natural_Array (Severity_Level) := (others => 0);

   procedure Inc_Assert_Count (Level : Severity_Level) is
   begin
      Assert_Count (Level) := Assert_Count (Level) + 1;
   end Inc_Assert_Count;

   function Ghdl_Get_Assert_Count (Level : Ghdl_E8) return Std_Integer is
   begin
      return Assert_Count (Severity_Level (Level));
   end Ghdl_Get_Assert_Count;

   procedure Ghdl_Clear_Assert_Count is
   begin
      Assert_Count := (others => 0);
   end Ghdl_Clear_Assert_Count;
end Grt.Asserts;
