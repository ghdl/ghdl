--  Simple key-element hash table
--  Copyright (C) 2023 Tristan Gingold
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
with Dyn_Tables;
with Hash; use Hash;

--  An hash-table is a map of a Key to an Element.
--  This generic package provides a type (Instance) that implements this
--  hash table.
generic
   --  Element is the data type stored.
   type Element_Type is private;

   --  Key is the data type to index the element.
   type Key_Type is private;

   No_Element : Element_Type;

   --  Reduce the key to a small value.
   --  The required property is: Hash(K1) /= Hash(K2) => K1 /= K2.
   with function Hash (Key : Key_Type) return Hash_Value_Type;

   --  Return True iff Key1 = Key2.
   with function Equal (Key1, Key2 : Key_Type) return Boolean;
package Dyn_Htables is
   type Instance is private;

   --  Initialize HT.  Required before any other operation.
   procedure Init (HT : out Instance);

   --  Add or replace an element of HT.
   procedure Set (HT : in out Instance; K : Key_Type; El : Element_Type);

   --  Get an element from HT or No_Element if it doesn't exist.
   function Get (HT : Instance; K : Key_Type) return Element_Type;

   --  If there is already an element for K in HT, return it in RES.
   --  Otherwise add El in HT for K.
   procedure Unify (HT : in out Instance;
                    K : Key_Type;
                    El : Element_Type;
                    Res : out Element_Type);

   --  Return True iff there is an element for K in HT.
   function Exists (HT : Instance; K : Key_Type) return Boolean;
private
   type Index_Type is new Uns32;
   No_Index : constant Index_Type := 0;

   type Element_Wrapper is record
      Hash : Hash_Value_Type;
      Next : Index_Type;
      Key : Key_Type;
      El  : Element_Type;
   end record;

   package Wrapper_Tables is new Dyn_Tables
     (Table_Index_Type => Index_Type,
      Table_Component_Type => Element_Wrapper,
      Table_Low_Bound => No_Index + 1,
      Table_Initial => 128);

   type Hash_Array is array (Hash_Value_Type range <>) of Index_Type;
   type Hash_Array_Acc is access Hash_Array;

   Initial_Size : constant Hash_Value_Type := 1024;

   type Instance is record
      Size : Hash_Value_Type;
      Hash_Table : Hash_Array_Acc;
      Els : Wrapper_Tables.Instance;
   end record;
end Dyn_Htables;
