--  GHDL Run Time (GRT) -  heap subprograms.
--  Copyright (C) 2023 Tristan Gingold
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

with System; use System;
with Ada.Unchecked_Conversion;

with Grt.C; use Grt.C;

package body Grt.Heap is
   function Ghdl_Allocate (Size : Ghdl_Index_Type) return Ghdl_Ptr
   is
      function To_Ghdl_Ptr is new Ada.Unchecked_Conversion
        (Source => Address, Target => Ghdl_Ptr);
   begin
      return To_Ghdl_Ptr (Malloc (size_t (Size)));
   end Ghdl_Allocate;

   procedure Ghdl_Deallocate (Ptr : Ghdl_Ptr) is
      function To_Address is new Ada.Unchecked_Conversion
        (Source => Ghdl_Ptr, Target => Address);
   begin
      Free (To_Address (Ptr));
   end Ghdl_Deallocate;
end Grt.Heap;
