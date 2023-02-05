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

with Elab.Memtype; use Elab.Memtype;

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
      Typ : Memory_Ptr;
   end record;

   package Heap_Table is new Tables
     (Table_Component_Type => Heap_Entry,
      Table_Index_Type => Heap_Slot,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Alloc_Mem (Sz : Size_Type) return Memory_Ptr;
   pragma Import (C, Alloc_Mem, "malloc");

   --  ACC_TYP is the access type,
   --  OBJ_TYP is the object type.
   procedure Allocate (Acc_Typ : Type_Acc;
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
         begin
            E.Typ := Alloc_Mem (Typ_Sz);
            T := Save_Type (Obj_Typ, E.Typ, Typ_Sz);
            pragma Unreferenced (T);
         end;
      else
         declare
            function To_Memory_Ptr is new Ada.Unchecked_Conversion
              (Type_Acc, Memory_Ptr);
         begin
            E.Typ := To_Memory_Ptr (Obj_Typ);
         end;
      end if;

      Heap_Table.Append (E);
      To_Slot_Prefix_Acc (E.Ptr).Slot := Heap_Table.Last;
   end Allocate;

   function Allocate_By_Type (Acc_Typ : Type_Acc; T : Type_Acc)
                             return Heap_Ptr
   is
      Res : Memory_Ptr;
   begin
      Allocate (Acc_Typ, T, Res);
      Write_Value_Default (Res, T);
      return Heap_Ptr (Res);
   end Allocate_By_Type;

   function Allocate_By_Value (Acc_Typ : Type_Acc; V : Valtyp)
                              return Heap_Ptr
   is
      Mem : Memory_Ptr;
   begin
      Allocate (Acc_Typ, V.Typ, Mem);
      Write_Value (Mem, V);
      return Heap_Ptr (Mem);
   end Allocate_By_Value;

   function Get_Index (Ptr : Heap_Ptr) return Heap_Slot
   is
      Pfx : constant Memory_Ptr := Memory_Ptr (Ptr) - Prefix_Size;
   begin
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
      function To_Type_Acc is new Ada.Unchecked_Conversion
        (Memory_Ptr, Type_Acc);

      Slot : constant Heap_Slot := Get_Index (Ptr);

      E : Heap_Entry renames Heap_Table.Table (Slot);
   begin
      return (To_Type_Acc (E.Typ), E.Ptr + Prefix_Size);
   end Synth_Dereference;

   procedure Free (Obj : in out Heap_Entry) is
   begin
      -- TODO
      Obj := (null, null);
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

end Elab.Vhdl_Heap;
