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

with Types; use Types;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

package Elab.Vhdl_Heap is

   --  Allocate a value.
   function Allocate_By_Type (Acc_Typ : Type_Acc; T : Type_Acc)
                             return Heap_Ptr;
   function Allocate_By_Value (Acc_Typ : Type_Acc; V : Valtyp)
                              return Heap_Ptr;

   function Synth_Dereference (Ptr : Heap_Ptr) return Memtyp;

   procedure Synth_Deallocate (Ptr : Heap_Ptr);

   type Heap_Slot is new Uns32;

   function Get_Index (Ptr : Heap_Ptr) return Heap_Slot;
   function Get_Pointer (Idx : Heap_Slot) return Heap_Ptr;
end Elab.Vhdl_Heap;
