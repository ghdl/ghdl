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

with Ada.Unchecked_Conversion;

with Types; use Types;
with Tables;

package body Elab.Vhdl_Heap is
   --  Each allocated object on the heap is referenced in the heap table.
   --  This is the entry in the table.
   type Heap_Entry is record
      --  Pointer to the prefix.
      Ptr : Memory_Ptr;
      --  Type of the object.
      Obj_Typ : Type_Acc;
      --  Type of the access
      Acc_Typ : Type_Acc;
      --  Type definition of the object
      Def : Node;
   end record;

   package Heap_Table is new Tables
     (Table_Component_Type => Heap_Entry,
      Table_Index_Type => Heap_Slot,
      Table_Low_Bound => First_Heap_Slot,
      Table_Initial => 16);

   function Alloc_Mem (Sz : Size_Type) return Memory_Ptr;
   pragma Import (C, Alloc_Mem, "malloc");

   procedure Free_Mem (Ptr : Memory_Ptr);
   pragma Import (C, Free_Mem, "free");

   function Get_Last_Slot return Heap_Slot is
   begin
      return Heap_Table.Last;
   end Get_Last_Slot;

   --  ACC_TYP is the access type,
   --  OBJ_TYP is the object type.
   procedure Allocate (Acc_Def : Node;
                       Acc_Typ : Type_Acc;
                       Obj_Typ : Type_Acc;
                       Res : out Heap_Slot;
                       Data_Mem : out Memory_Ptr)
   is
      Typ_Sz : constant Size_Type := Acc_Typ.Acc_Type_Sz;
      E : Heap_Entry;
   begin
      pragma Assert (Acc_Typ.Kind = Type_Access);

      --  Allocate memory for the object, the bounds and the prefix.
      E.Ptr := Alloc_Mem (Obj_Typ.Sz);
      Data_Mem := E.Ptr;

      --  Allocate the memory for the type.
      if Typ_Sz > 0 then
         declare
            Mem : Memory_Ptr;
         begin
            Mem := Alloc_Mem (Typ_Sz);
            E.Obj_Typ := Save_Type (Obj_Typ, Mem, Typ_Sz);
         end;
      else
         E.Obj_Typ := Obj_Typ;
      end if;

      E.Def := Acc_Def;
      E.Acc_Typ := Acc_Typ;

      Heap_Table.Append (E);
      Res := Heap_Table.Last;
   end Allocate;

   function Allocate_By_Type (Acc_Def : Node; Acc_Typ : Type_Acc; T : Type_Acc)
                             return Heap_Slot
   is
      Obj_Ptr : Memory_Ptr;
      Res : Heap_Slot;
   begin
      Allocate (Acc_Def, Acc_Typ, T, Res, Obj_Ptr);
      Write_Value_Default (Obj_Ptr, T);
      return Res;
   end Allocate_By_Type;

   function Allocate_By_Value (Acc_Def : Node; Acc_Typ : Type_Acc; V : Valtyp)
                              return Heap_Slot
   is
      Obj_Ptr : Memory_Ptr;
      Res : Heap_Slot;
   begin
      Allocate (Acc_Def, Acc_Typ, V.Typ, Res, Obj_Ptr);
      Write_Value (Obj_Ptr, V);
      return Res;
   end Allocate_By_Value;

   function Get_Pointer (Idx : Heap_Slot) return Memory_Ptr is
   begin
      if Idx = Null_Heap_Slot then
         return null;
      else
         declare
            E : Heap_Entry renames Heap_Table.Table (Idx);
         begin
            return E.Ptr;
         end;
      end if;
   end Get_Pointer;

   function Synth_Dereference (Slot : Heap_Slot) return Memtyp
   is
      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      return (E.Obj_Typ, E.Ptr);
   end Synth_Dereference;

   procedure Free (Obj : in out Heap_Entry) is
   begin
      --  TODO: free memory
      --  But not until there are no more references.
      Obj := (null, null, null, Null_Node);
   end Free;

   procedure Synth_Deallocate (Slot : Heap_Slot)
   is
   begin
      if Heap_Table.Table (Slot).Ptr = null then
         return;
      end if;
      Free (Heap_Table.Table (Slot));
   end Synth_Deallocate;

   function Get_Slot_Acc_Type (Slot : Heap_Slot) return Type_Acc
   is
      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      return E.Acc_Typ;
   end Get_Slot_Acc_Type;

   function Get_Slot_Obj_Type (Slot : Heap_Slot) return Type_Acc
   is
      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      return E.Obj_Typ;
   end Get_Slot_Obj_Type;

   function Get_Slot_Type_Def (Slot : Heap_Slot) return Node
   is
      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      return E.Def;
   end Get_Slot_Type_Def;

   procedure Replace_Object (Slot : Heap_Slot; Nptr : Memory_Ptr)
   is
      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      Free_Mem (E.Ptr);
      E.Ptr := Nptr;
   end Replace_Object;

   function Ghdl_Allocate (Sz : Ghdl_Index_Type) return Heap_Slot
   is
      Ptr : Memory_Ptr;
   begin
      --  Allocate memory for the object and the prefix.
      Ptr := Alloc_Mem (Size_Type (Sz));
      Heap_Table.Append ((Ptr => Ptr,
                          Obj_Typ => null,
                          Acc_Typ => null,
                          Def => Null_Node));

      return Heap_Table.Last;
   end Ghdl_Allocate;

   procedure Ghdl_Deallocate (Slot : Heap_Slot) is
   begin
      if Slot = Null_Heap_Slot then
         return;
      end if;

      Free_Mem (Heap_Table.Table (Slot).Ptr);
      Heap_Table.Table (Slot).Ptr := null;
   end Ghdl_Deallocate;
end Elab.Vhdl_Heap;
