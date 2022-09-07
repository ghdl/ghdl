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

with Elab.Memtype; use Elab.Memtype;

package body Elab.Vhdl_Heap is

   type Heap_Entry is record
      Obj : Memory_Ptr;
      Typ : Memory_Ptr;
   end record;

   package Heap_Table is new Tables
     (Table_Component_Type => Heap_Entry,
      Table_Index_Type => Heap_Index,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Alloc_Mem (Sz : Size_Type) return Memory_Ptr;
   pragma Import (C, Alloc_Mem, "malloc");

   --  ACC_TYP is the access type,
   --  OBJ_TYP is the object type.
   procedure Allocate (Acc_Typ : Type_Acc;
                       Obj_Typ : Type_Acc;
                       Res : out Memory_Ptr;
                       Idx : out Heap_Index)
   is
      Typ_Sz : constant Size_Type := Acc_Typ.Acc_Bnd_Sz;
      E : Heap_Entry;
   begin
      pragma Assert (Acc_Typ.Kind = Type_Access);

      E.Obj := Alloc_Mem (Obj_Typ.Sz);

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

      Res := E.Obj;

      Heap_Table.Append (E);
      Idx := Heap_Table.Last;
   end Allocate;

   function Allocate_By_Type (Acc_Typ : Type_Acc; T : Type_Acc)
                             return Heap_Index
   is
      Res : Memory_Ptr;
      Idx : Heap_Index;
   begin
      Allocate (Acc_Typ, T, Res, Idx);
      Write_Value_Default (Res, T);
      return Idx;
   end Allocate_By_Type;

   function Allocate_By_Value (Acc_Typ : Type_Acc; V : Valtyp)
                              return Heap_Index
   is
      Mem : Memory_Ptr;
      Idx : Heap_Index;
   begin
      Allocate (Acc_Typ, V.Typ, Mem, Idx);
      Write_Value (Mem, V);
      return Idx;
   end Allocate_By_Value;

   function Synth_Dereference (Idx : Heap_Index) return Memtyp
   is
      function To_Type_Acc is new Ada.Unchecked_Conversion
        (Memory_Ptr, Type_Acc);

      E : Heap_Entry renames Heap_Table.Table (Idx);
   begin
      return (To_Type_Acc (E.Typ), E.Obj);
   end Synth_Dereference;

   procedure Free (Obj : in out Heap_Entry) is
   begin
      -- TODO
      Obj := (null, null);
   end Free;

   procedure Synth_Deallocate (Idx : Heap_Index) is
   begin
      if Heap_Table.Table (Idx).Obj = null then
         return;
      end if;
      Free (Heap_Table.Table (Idx));
   end Synth_Deallocate;

end Elab.Vhdl_Heap;
