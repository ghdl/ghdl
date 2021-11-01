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
with Tables;

with Elab.Memtype; use Elab.Memtype;

package body Elab.Vhdl_Heap is

   package Heap_Table is new Tables
     (Table_Component_Type => Valtyp,
      Table_Index_Type => Heap_Index,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Alloc_Mem (Sz : Size_Type) return Memory_Ptr;
   pragma Import (C, Alloc_Mem, "malloc");

   function Allocate_Memory (T : Type_Acc) return Value_Acc
   is
      M : Memory_Ptr;
   begin
      M := Alloc_Mem (T.Sz);
      return new Value_Type'(Kind => Value_Memory, Mem => M);
   end Allocate_Memory;

   function Allocate_By_Type (T : Type_Acc) return Value_Acc
   is
      Res : Value_Acc;
   begin
      Res := Allocate_Memory (T);
      Write_Value_Default (Res.Mem, T);
      return Res;
   end Allocate_By_Type;

   function Allocate_By_Type (T : Type_Acc) return Heap_Index is
   begin
      --  FIXME: allocate type.
      Heap_Table.Append ((T, Allocate_By_Type (T)));
      return Heap_Table.Last;
   end Allocate_By_Type;

   function Allocate_By_Value (V : Valtyp) return Value_Acc
   is
      Res : Value_Acc;
   begin
      Res := Allocate_Memory (V.Typ);
      Write_Value (Res.Mem, V);
      return Res;
   end Allocate_By_Value;

   function Allocate_By_Value (V : Valtyp) return Heap_Index is
   begin
      Heap_Table.Append ((V.Typ, Allocate_By_Value (V)));
      return Heap_Table.Last;
   end Allocate_By_Value;

   function Synth_Dereference (Idx : Heap_Index) return Valtyp is
   begin
      return Heap_Table.Table (Idx);
   end Synth_Dereference;

   procedure Free (Obj : in out Valtyp) is
   begin
      -- TODO
      Obj := No_Valtyp;
   end Free;

   procedure Synth_Deallocate (Idx : Heap_Index) is
   begin
      if Heap_Table.Table (Idx) = No_Valtyp then
         return;
      end if;
      Free (Heap_Table.Table (Idx));
   end Synth_Deallocate;

end Elab.Vhdl_Heap;
