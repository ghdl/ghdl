--  Heap for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Types; use Types;
with Tables;

with Vhdl.Nodes; use Vhdl.Nodes;

package body Synth.Heap is

   package Heap_Table is new Tables
     (Table_Component_Type => Value_Acc,
      Table_Index_Type => Heap_Index,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Allocate_By_Type (T : Type_Acc) return Value_Acc is
   begin
      case T.Kind is
         when Type_Bit
           | Type_Logic =>
            return new Value_Type'
              (Kind => Value_Discrete, Typ => T, Scal => 0);
         when Type_Discrete =>
            return new Value_Type'
              (Kind => Value_Discrete, Typ => T, Scal => T.Drange.Left);
         when Type_Array =>
            declare
               Len : constant Uns32 := Get_Array_Flat_Length (T);
               El_Typ : constant Type_Acc := Get_Array_Element (T);
               Arr : Value_Array_Acc;
            begin
               Arr := new Value_Array_Type (Iir_Index32 (Len));
               for I in Arr.V'Range loop
                  Arr.V (I) := Allocate_By_Type (El_Typ);
               end loop;
               return new Value_Type'
                 (Kind => Value_Const_Array, Typ => T, Arr => Arr);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Allocate_By_Type;

   function Allocate_By_Type (T : Type_Acc) return Heap_Index is
   begin
      --  FIXME: allocate type.
      Heap_Table.Append (Allocate_By_Type (T));
      return Heap_Table.Last;
   end Allocate_By_Type;

   function Allocate_By_Value (V : Value_Acc) return Value_Acc is
   begin
      case V.Kind is
         when Value_Net
           | Value_Wire =>
            raise Internal_Error;
         when Value_Discrete =>
            return new Value_Type'
              (Kind => Value_Discrete, Typ => V.Typ, Scal => V.Scal);
         when Value_Array
           | Value_Const_Array =>
            declare
               Arr : Value_Array_Acc;
            begin
               Arr := new Value_Array_Type (V.Arr.Len);
               for I in Arr.V'Range loop
                  Arr.V (I) := Allocate_By_Value (V.Arr.V (I));
               end loop;
               return new Value_Type'
                 (Kind => Value_Const_Array, Typ => V.Typ, Arr => Arr);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Allocate_By_Value;

   function Allocate_By_Value (V : Value_Acc) return Heap_Index is
   begin
      Heap_Table.Append (Allocate_By_Value (V));
      return Heap_Table.Last;
   end Allocate_By_Value;

   function Synth_Dereference (Idx : Heap_Index) return Value_Acc is
   begin
      return Heap_Table.Table (Idx);
   end Synth_Dereference;

   procedure Free (Obj : in out Value_Acc) is
   begin
      -- TODO
      Obj := null;
   end Free;

   procedure Synth_Deallocate (Idx : Heap_Index) is
   begin
      if Heap_Table.Table (Idx) = null then
         return;
      end if;
      Free (Heap_Table.Table (Idx));
   end Synth_Deallocate;

end Synth.Heap;
