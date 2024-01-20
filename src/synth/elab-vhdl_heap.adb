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

with Tables;

package body Elab.Vhdl_Heap is

   --  Each object on the heap is prefixed by this prefix (to easily convert
   --  to an index).
   type Slot_Prefix is record
      --  The slot number.
      Slot : Heap_Slot;

      --  Alignment pad.
      Pad : Uns32;
   end record;

   pragma Assert (Slot_Prefix'Size = 8 * 8);

   --  Maximum alignment required to store any object.
--   Max_Align : constant := 8;

   --  Size of the prefix.
   Prefix_Size : constant Size_Type := Size_Type (Slot_Prefix'Size / 8);

   type Slot_Prefix_Acc is access all Slot_Prefix;

   function To_Slot_Prefix_Acc is new Ada.Unchecked_Conversion
     (Source => Memory_Ptr, Target => Slot_Prefix_Acc);

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

   function Realign (Res : Size_Type;
                     Align : Size_Type) return Size_Type is
   begin
      return (Res + Align - 1) and not (Align - 1);
   end Realign;

   --  Return the data memory address from an heap entry.
   function Entry_To_Obj_Ptr (E : Heap_Entry) return Memory_Ptr
   is
      Bnd_Sz : Size_Type;
   begin
      Bnd_Sz := Realign (E.Acc_Typ.Acc_Bnd_Sz, 2**Natural (E.Obj_Typ.Al));

      return E.Ptr + Prefix_Size + Bnd_Sz;
   end Entry_To_Obj_Ptr;

   function Get_Last_Slot return Heap_Slot is
   begin
      return Heap_Table.Last;
   end Get_Last_Slot;

   --  ACC_TYP is the access type,
   --  OBJ_TYP is the object type.
   procedure Allocate (Acc_Def : Node;
                       Acc_Typ : Type_Acc;
                       Obj_Typ : Type_Acc;
                       Res : out Memory_Ptr;
                       Data_Mem : out Memory_Ptr)
   is
      Typ_Sz : constant Size_Type := Acc_Typ.Acc_Type_Sz;
      Bnd_Sz : Size_Type;
      E : Heap_Entry;
   begin
      pragma Assert (Acc_Typ.Kind = Type_Access);

      Bnd_Sz := Realign (Acc_Typ.Acc_Bnd_Sz, 2**Natural (Obj_Typ.Al));

      --  Allocate memory for the object, the bounds and the prefix.
      E.Ptr := Alloc_Mem (Prefix_Size + Bnd_Sz + Obj_Typ.Sz);
      Res := E.Ptr + Prefix_Size;
      Data_Mem := Res + Bnd_Sz;

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
      To_Slot_Prefix_Acc (E.Ptr).Slot := Heap_Table.Last;
   end Allocate;

   function Allocate_By_Type (Acc_Def : Node; Acc_Typ : Type_Acc; T : Type_Acc)
                             return Heap_Ptr
   is
      Res, Obj_Ptr : Memory_Ptr;
   begin
      Allocate (Acc_Def, Acc_Typ, T, Res, Obj_Ptr);
      Write_Value_Default (Obj_Ptr, T);
      return Heap_Ptr (Res);
   end Allocate_By_Type;

   function Allocate_By_Value (Acc_Def : Node; Acc_Typ : Type_Acc; V : Valtyp)
                              return Heap_Ptr
   is
      Mem, Obj_Ptr : Memory_Ptr;
   begin
      Allocate (Acc_Def, Acc_Typ, V.Typ, Mem, Obj_Ptr);
      Write_Value (Obj_Ptr, V);
      return Heap_Ptr (Mem);
   end Allocate_By_Value;

   function Get_Index (Ptr : Heap_Ptr) return Heap_Slot
   is
      Pfx : constant Memory_Ptr := Memory_Ptr (Ptr) - Prefix_Size;
   begin
      if Ptr = null then
         return Null_Heap_Slot;
      end if;
      return To_Slot_Prefix_Acc (Pfx).Slot;
   end Get_Index;

   function Get_Pointer (Idx : Heap_Slot) return Heap_Ptr
   is
      E : Heap_Entry renames Heap_Table.Table (Idx);
   begin
      return Heap_Ptr (Entry_To_Obj_Ptr (E));
   end Get_Pointer;

   function Synth_Dereference (Ptr : Heap_Ptr) return Memtyp
   is
      Slot : constant Heap_Slot := Get_Index (Ptr);

      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      return (E.Obj_Typ, Entry_To_Obj_Ptr (E));
   end Synth_Dereference;

   procedure Free (Obj : in out Heap_Entry) is
   begin
      --  TODO: free memory
      --  But not until there are no more references.
      Obj := (null, null, null, Null_Node);
   end Free;

   procedure Synth_Deallocate (Ptr : Heap_Ptr)
   is
      Slot : constant Heap_Slot := Get_Index (Ptr);
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

   function Insert_Bounds (Slot : Heap_Slot; Bnd_Sz : Size_Type)
                          return Memory_Ptr
   is
      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      pragma Assert (E.Acc_Typ.Acc_Bnd_Sz = Bnd_Sz);

      return E.Ptr + Prefix_Size;
   end Insert_Bounds;

   function Ghdl_Allocate (Sz : Ghdl_Index_Type) return Heap_Ptr
   is
      Ptr : Memory_Ptr;
   begin
      --  Allocate memory for the object and the prefix.
      Ptr := Alloc_Mem (Prefix_Size + Size_Type (Sz));
      Heap_Table.Append ((Ptr => Ptr,
                          Obj_Typ => null,
                          Acc_Typ => null,
                          Def => Null_Node));

      To_Slot_Prefix_Acc (Ptr).Slot := Heap_Table.Last;

      return Heap_Ptr (Ptr + Prefix_Size);
   end Ghdl_Allocate;

   procedure Ghdl_Deallocate (Ptr : Heap_Ptr)
   is
      Slot : Heap_Slot;
   begin
      if Ptr = null then
         return;
      end if;

      Slot := Get_Index (Ptr);

      Free_Mem (Heap_Table.Table (Slot).Ptr);
      Heap_Table.Table (Slot).Ptr := null;
   end Ghdl_Deallocate;

end Elab.Vhdl_Heap;
