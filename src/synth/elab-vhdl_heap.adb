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
      Slot : Heap_Slot;
      Pad : Uns32;
   end record;

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
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Alloc_Mem (Sz : Size_Type) return Memory_Ptr;
   pragma Import (C, Alloc_Mem, "malloc");

   procedure Free_Mem (Ptr : Memory_Ptr);
   pragma Import (C, Free_Mem, "free");

   --  Return the data memory address from an heap entry.
   function Entry_To_Data_Mem (E : Heap_Entry) return Memory_Ptr is
   begin
      return E.Ptr + Prefix_Size;
   end Entry_To_Data_Mem;

   function Get_Last_Slot return Heap_Slot is
   begin
      return Heap_Table.Last;
   end Get_Last_Slot;

   --  ACC_TYP is the access type,
   --  OBJ_TYP is the object type.
   procedure Allocate (Acc_Def : Node;
                       Acc_Typ : Type_Acc;
                       Obj_Typ : Type_Acc;
                       Res : out Memory_Ptr)
   is
      Typ_Sz : constant Size_Type := Acc_Typ.Acc_Type_Sz;
      E : Heap_Entry;
   begin
      pragma Assert (Acc_Typ.Kind = Type_Access);

      --  Allocate memory for the object and the prefix.
      E.Ptr := Alloc_Mem (Prefix_Size + Obj_Typ.Sz);
      Res := E.Ptr + Prefix_Size;

      --  Allocate the memory for the type.
      if Typ_Sz > 0 then
         declare
            T : Type_Acc;
            function To_Type_Acc is new Ada.Unchecked_Conversion
              (Memory_Ptr, Type_Acc);
            function To_Memory_Ptr is new Ada.Unchecked_Conversion
              (Type_Acc, Memory_Ptr);
         begin
            E.Obj_Typ := To_Type_Acc (Alloc_Mem (Typ_Sz));
            T := Save_Type (Obj_Typ, To_Memory_Ptr (E.Obj_Typ), Typ_Sz);
            pragma Unreferenced (T);
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
      Res : Memory_Ptr;
   begin
      Allocate (Acc_Def, Acc_Typ, T, Res);
      Write_Value_Default (Res, T);
      return Heap_Ptr (Res);
   end Allocate_By_Type;

   function Allocate_By_Value (Acc_Def : Node; Acc_Typ : Type_Acc; V : Valtyp)
                              return Heap_Ptr
   is
      Mem : Memory_Ptr;
   begin
      Allocate (Acc_Def, Acc_Typ, V.Typ, Mem);
      Write_Value (Mem, V);
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
      Pfx : constant Memory_Ptr := Heap_Table.Table (Idx).Ptr;
   begin
      return Heap_Ptr (Pfx + Prefix_Size);
   end Get_Pointer;

   function Synth_Dereference (Ptr : Heap_Ptr) return Memtyp
   is
      Slot : constant Heap_Slot := Get_Index (Ptr);

      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      return (E.Obj_Typ, Entry_To_Data_Mem (E));
   end Synth_Dereference;

   procedure Free (Obj : in out Heap_Entry) is
   begin
      -- TODO
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

   function Realign (Res : Size_Type;
                     Align : Size_Type) return Size_Type is
   begin
      return (Res + Align - 1) and not (Align - 1);
   end Realign;

   function Insert_Bounds (Slot : Heap_Slot; Bnd_Sz : Size_Type)
                          return Memory_Ptr
   is
      E : Heap_Entry renames Heap_Table.Table (Slot);
      Ptr : Memory_Ptr;
      Sz : Size_Type;
   begin
      Sz := Realign (Bnd_Sz, 2**Natural (E.Obj_Typ.Al));

      Ptr := Alloc_Mem (Prefix_Size + Sz + E.Obj_Typ.Sz);
      Copy_Memory (Ptr, E.Ptr, Prefix_Size);
      Copy_Memory (Ptr + Prefix_Size + Sz, E.Ptr + Prefix_Size, E.Obj_Typ.Sz);

      if False then
         Free_Mem (E.Ptr);
      end if;

      E.Ptr := Ptr;

      return Ptr + Prefix_Size;
   end Insert_Bounds;

end Elab.Vhdl_Heap;
