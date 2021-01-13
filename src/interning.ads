--  Type interning - set of unique objects.
--  Copyright (C) 2019 Tristan Gingold
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

with Hash; use Hash;
with Dyn_Interning;

--  This generic package provides a factory to build unique objects.
--  Get will return an existing object or create a new one.
generic
   --  Parameters of the object to be created.
   type Params_Type (<>) is private;

   --  Object to be built and stored.
   type Object_Type is private;

   --  Reduce PARAMS to a small value.
   --  The required property is: Hash(P1) /= Hash(P2) => P1 /= P2.
   with function Hash (Params : Params_Type) return Hash_Value_Type;

   --  Create an object from PARAMS.
   with function Build (Params : Params_Type) return Object_Type;

   --  Return True iff OBJ is the object corresponding to PARAMS.
   with function Equal (Obj : Object_Type; Params : Params_Type)
                       return Boolean;
package Interning is
   package Implementation is new Dyn_Interning
     (Params_Type => Params_Type,
      Object_Type => Object_Type,
      Hash => Hash,
      Build => Build,
      Equal => Equal);

   subtype Index_Type is Implementation.Index_Type;

   --  Re-export (some) operators of Index_Type.
   --  FIXME: is there a better way to do this ?
   function "<=" (L, R : Index_Type) return Boolean
     renames Implementation."<=";
   function "+" (L, R : Index_Type) return Index_Type
     renames Implementation."+";

   --  Initialize.  Required before any other operation.
   procedure Init;

   --  If there is already an existing object for PARAMS, return it.
   --  Otherwise create it.
   function Get (Params : Params_Type) return Object_Type;

   --  Get the number of elements in the table.
   function Last_Index return Index_Type;

   --  Get an element by index.  The index has no real meaning, but the
   --  current implementation allocates index incrementally.
   function Get_By_Index (Index : Index_Type) return Object_Type;

   No_Index : constant Index_Type := Implementation.No_Index;
   First_Index : constant Index_Type := Implementation.First_Index;
end Interning;
