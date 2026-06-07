--  Heap for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Grt.Errors;

package body Elab.Vhdl_Heap.Rt is
   function Ghdl_Deref (Slot : Heap_Slot) return Memory_Ptr is
   begin
      if Slot = Null_Heap_Slot then
         Grt.Errors.Error ("NULL access dereferenced");
      else
         return Get_Pointer (Slot);
      end if;
   end Ghdl_Deref;
end Elab.Vhdl_Heap.Rt;
