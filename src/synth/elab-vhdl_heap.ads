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

with Grt.Types; use Grt.Types;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

package Elab.Vhdl_Heap is

   --  Allocate a value.
   function Allocate_By_Type (Acc_Def : Node; Acc_Typ : Type_Acc; T : Type_Acc)
                             return Heap_Ptr;
   function Allocate_By_Value (Acc_Def : Node; Acc_Typ : Type_Acc; V : Valtyp)
                              return Heap_Ptr;

   function Synth_Dereference (Ptr : Heap_Ptr) return Memtyp;

   procedure Synth_Deallocate (Ptr : Heap_Ptr);

   type Heap_Slot is new Uns32;
   Null_Heap_Slot : constant Heap_Slot := 0;
   First_Heap_Slot : constant Heap_Slot := 1;

   function Get_Index (Ptr : Heap_Ptr) return Heap_Slot;
   function Get_Pointer (Idx : Heap_Slot) return Heap_Ptr;

   --  For compiled environment:
   --  * conversion of types to bounds,
   --  * raw allocation of memory.
   function Get_Last_Slot return Heap_Slot;
   function Get_Slot_Acc_Type (Slot : Heap_Slot) return Type_Acc;
   function Get_Slot_Obj_Type (Slot : Heap_Slot) return Type_Acc;
   function Get_Slot_Type_Def (Slot : Heap_Slot) return Node;

   function Insert_Bounds (Slot : Heap_Slot; Bnd_Sz : Size_Type)
                          return Memory_Ptr;

   --  When called by generated code.
   function Ghdl_Allocate (Sz : Ghdl_Index_Type) return Heap_Ptr;
   procedure Ghdl_Deallocate (Ptr : Heap_Ptr);
end Elab.Vhdl_Heap;
