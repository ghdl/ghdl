--  Maps - association of unique object with a value.
--  Copyright (C) 2019-2020 Tristan Gingold
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
with Hash; use Hash;
with Dyn_Tables;

--  This generic package provides a factory to build unique objects.
--  The container is iterable through the index.
--  PARAMS_TYPE is the type used to find the key, and if the key does not
--   exists, it is also used to build the new object.
--  The key is of type OBJECT_TYPE.
--  VALUE_TYPE is the value associated to the key.
--
--  FIXME: this is too confusing.
--  Use the usual names KEY_TYPE and VALUE_TYPE.
--  Use BUILD_TYPE instead of PARAMS_TYPE.

generic
   --  Parameters of the object to be created.
   type Params_Type (<>) is private;

   --  Object to be built and stored.
   type Object_Type is private;

   --  Value associated to the object.  Could be modified but not used
   --  for comparision.
   type Value_Type is private;

   --  Reduce PARAMS to a small value.
   --  The required property is: Hash(P1) /= Hash(P2) => P1 /= P2.
   with function Hash (Params : Params_Type) return Hash_Value_Type;

   --  Create an object from PARAMS.
   with function Build (Params : Params_Type) return Object_Type;

   --  Initial value of the object.  Called when an object is created.
   with function Build_Value (Obj : Object_Type) return Value_Type;

   --  Return True iff OBJ is the object corresponding to PARAMS.
   with function Equal (Obj : Object_Type; Params : Params_Type)
                       return Boolean;
package Dyn_Maps is
   type Instance is limited private;

   --  Initialize.  Required before any other operation.
   procedure Init (Inst : out Instance);

   procedure Free (Inst : in out Instance);

   type Index_Type is new Uns32;
   No_Index : constant Index_Type := 0;
   First_Index : constant Index_Type := 1;

   --  If there is already an existing object for PARAMS, return its index.
   --  Otherwise create it.
   --  The index is doesn't change over the lifetime of the map.
   procedure Get_Index
     (Inst : in out Instance; Params : Params_Type; Idx : out Index_Type);

   --  Return No_Index if not found.
   function Get_Index_Soft (Inst : Instance; Params : Params_Type)
                           return Index_Type;

   --  Get the number of elements in the table.
   function Last_Index (Inst : Instance) return Index_Type;

   --  Get an element by index.  The index has no real meaning, but the
   --  current implementation allocates index incrementally.
   function Get_By_Index (Inst : Instance; Index : Index_Type)
                          return Object_Type;

   --  Get/Set the value.
   function Get_Value (Inst : Instance; Index : Index_Type) return Value_Type;
   procedure Set_Value
     (Inst : in out Instance; Index : Index_Type; Val : Value_Type);
private
   type Element_Wrapper is record
      Hash : Hash_Value_Type;
      Next : Index_Type := No_Index;
      Obj  : Object_Type;
      Val  : Value_Type;
   end record;

   package Wrapper_Tables is new Dyn_Tables
     (Table_Index_Type => Index_Type,
      Table_Component_Type => Element_Wrapper,
      Table_Low_Bound => No_Index + 1);

   type Hash_Array is array (Hash_Value_Type range <>) of Index_Type;
   type Hash_Array_Acc is access Hash_Array;

   Initial_Size : constant Hash_Value_Type := 1024;

   type Instance is record
      Els : Wrapper_Tables.Instance;
      Size : Hash_Value_Type;
      Hash_Table : Hash_Array_Acc;
   end record;
end Dyn_Maps;
