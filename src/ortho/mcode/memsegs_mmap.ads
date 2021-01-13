--  Memory segments.
--  Copyright (C) 2006 Tristan Gingold
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
with System; use System;

package Memsegs_Mmap is
   --  A memseg is a growable memory space.  It can be resized with Resize.
   --  After each operation the base address can change and must be get
   --  with Get_Address.
   type Memseg_Type is private;

   --  Create a new memseg.
   function Create return Memseg_Type;

   --  Resize the memseg.
   procedure Resize (Seg : in out Memseg_Type; Size : Natural);

   --  Get the base address.
   function Get_Address (Seg : Memseg_Type) return Address;

   --  Free all the memory and initialize the memseg.
   procedure Delete (Seg : in out Memseg_Type);

   --  Set the protection to read+execute.
   procedure Set_Rx (Seg : in out Memseg_Type;
                     Offset : Natural; Size : Natural);

   pragma Inline (Create);
   pragma Inline (Get_Address);
private
   type Memseg_Type is record
      Base : Address := Null_Address;
      Size : Natural := 0;
   end record;
end Memsegs_Mmap;
