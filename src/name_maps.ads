--  Simple name_id-element hash table
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

generic
   type T is private;
   No_Element : T;
package Name_Maps is
   type Map_Type is limited private;

   --  Initialize the map.  It is empty after this call.
   procedure Init (Map : out Map_Type);

   --  Get the element from MAP for NAME.  Returns No_Element if NAME is
   --  not present.
   function Get_Element (Map : Map_Type; Name : Name_Id) return T;

   --  Associate VAL to NAME in MAP.  Overwrite the existing association if
   --  NAME was already associated.  VAL cannot be No_Element.
   procedure Set_Element (Map : in out Map_Type; Name : Name_Id; Val : T);
private
   type Element is record
      Name : Name_Id;
      User : T;
   end record;

   type Element_Array is array (Uns32 range <>) of Element;
   type Element_Arr_Acc is access Element_Array;

   type Map_Type is record
      Els : Element_Arr_Acc;
      Count : Uns32;
   end record;
end Name_Maps;
