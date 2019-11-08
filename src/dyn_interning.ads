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

with Types; use Types;
with Hash; use Hash;
with Dyn_Tables;

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
package Dyn_Interning is
   type Instance is limited private;

   --  Initialize.  Required before any other operation.
   procedure Init (Inst : out Instance);

   procedure Free (Inst : in out Instance);

   type Index_Type is new Uns32;
   No_Index : constant Index_Type := 0;
   First_Index : constant Index_Type := 1;

   --  If there is already an existing object for PARAMS, return it.
   --  Otherwise create it.
   procedure Get
     (Inst : in out Instance; Params : Params_Type; Res : out Object_Type);

   --  Likewise, but return its index.
   procedure Get_Index
     (Inst : in out Instance; Params : Params_Type; Idx : out Index_Type);

   --  Get the number of elements in the table.
   function Last_Index (Inst : Instance) return Index_Type;

   --  Get an element by index.  The index has no real meaning, but the
   --  current implementation allocates index incrementally.
   function Get_By_Index (Inst : Instance; Index : Index_Type)
                         return Object_Type;
private
   type Element_Wrapper is record
      Hash : Hash_Value_Type;
      Next : Index_Type := No_Index;
      Obj  : Object_Type;
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
end Dyn_Interning;
