--  Type interning - set of unique objects.
--  Copyright (C) 2019 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Ada.Unchecked_Deallocation;
with Dyn_Tables;

package body Interning is
   type Element_Wrapper is record
      Hash : Hash_Value_Type;
      Next : Index_Type;
      Obj  : Object_Type;
   end record;

   package Wrapper_Tables is new Dyn_Tables
     (Table_Index_Type => Index_Type,
      Table_Component_Type => Element_Wrapper,
      Table_Low_Bound => No_Index + 1,
      Table_Initial => 128);

   type Hash_Array is array (Hash_Value_Type range <>) of Index_Type;
   type Hash_Array_Acc is access Hash_Array;

   Initial_Size : constant Hash_Value_Type := 1024;

   Size : Hash_Value_Type;
   Hash_Table : Hash_Array_Acc;
   Els : Wrapper_Tables.Instance;

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Hash_Array, Hash_Array_Acc);

   procedure Init is
   begin
      Size := Initial_Size;
      Hash_Table := new Hash_Array'(0 .. Initial_Size - 1 => No_Index);
      Wrapper_Tables.Init (Els);
      pragma Assert (Wrapper_Tables.Last (Els) = No_Index);
   end Init;

   --  Expand the hash table (double the size).
   procedure Expand
   is
      Old_Hash_Table : Hash_Array_Acc;
      Idx : Index_Type;
   begin
      Old_Hash_Table := Hash_Table;
      Size := Size * 2;
      Hash_Table := new Hash_Array'(0 .. Size - 1 => No_Index);

      --  Rehash.
      for I in Old_Hash_Table'Range loop
         Idx := Old_Hash_Table (I);
         while Idx /= No_Index loop
            --  Note: collisions are put in reverse order.
            declare
               Ent : Element_Wrapper renames Els.Table (Idx);
               Hash_Index : constant Hash_Value_Type :=
                 Ent.Hash and (Size - 1);
               Next_Idx : constant Index_Type := Ent.Next;
            begin
               Ent.Next := Hash_Table (Hash_Index);
               Hash_Table (Hash_Index) := Idx;
               Idx := Next_Idx;
            end;
         end loop;
      end loop;

      Deallocate (Old_Hash_Table);
   end Expand;

   function Get (Params : Params_Type) return Object_Type
   is
      Hash_Value : Hash_Value_Type;
      Hash_Index : Hash_Value_Type;
      Idx : Index_Type;
      Res : Object_Type;
   begin
      --  Check if the package was initialized.
      pragma Assert (Hash_Table /= null);

      Hash_Value := Hash (Params);
      Hash_Index := Hash_Value and (Size - 1);

      Idx := Hash_Table (Hash_Index);
      while Idx /= No_Index loop
         declare
            E : Element_Wrapper renames Els.Table (Idx);
         begin
            if E.Hash = Hash_Value and then Equal (E.Obj, Params) then
               return E.Obj;
            end if;
            Idx := E.Next;
         end;
      end loop;

      --  Maybe expand the table.
      if Hash_Value_Type (Wrapper_Tables.Last (Els)) > 2 * Size then
         Expand;

         --  Recompute hash index.
         Hash_Index := Hash_Value and (Size - 1);
      end if;

      Res := Build (Params);

      --  Insert.
      Wrapper_Tables.Append (Els,
                             (Hash => Hash_Value,
                              Next => Hash_Table (Hash_Index),
                              Obj => Res));
      Hash_Table (Hash_Index) := Wrapper_Tables.Last (Els);
      return Res;
   end Get;

   function Last_Index return Index_Type is
   begin
      return Wrapper_Tables.Last (Els);
   end Last_Index;

   function Get_By_Index (Index : Index_Type) return Object_Type is
   begin
      pragma Assert (Index <= Wrapper_Tables.Last (Els));
      return Els.Table (Index).Obj;
   end Get_By_Index;
end Interning;
