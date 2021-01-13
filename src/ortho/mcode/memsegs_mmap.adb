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

with System.Storage_Elements;

package body Memsegs_Mmap is
   function Mmap_Malloc (Size : Natural) return Address;
   pragma Import (C, Mmap_Malloc, "mmap_malloc");

   function Mmap_Realloc (Ptr : Address; Old_Size : Natural; Size : Natural)
                         return Address;
   pragma Import (C, Mmap_Realloc, "mmap_realloc");

   procedure Mmap_Free (Ptr : Address; Size : Natural);
   pragma Import (C, Mmap_Free, "mmap_free");

   procedure Mmap_Rx (Ptr : Address; Size : Natural);
   pragma Import (C, Mmap_Rx, "mmap_rx");

   function Create return Memseg_Type is
   begin
      return (Base => Null_Address, Size => 0);
   end Create;

   procedure Resize (Seg : in out Memseg_Type; Size : Natural) is
   begin
      if Seg.Size = 0 then
         Seg.Base := Mmap_Malloc (Size);
      else
         Seg.Base := Mmap_Realloc (Seg.Base, Seg.Size, Size);
      end if;
      Seg.Size := Size;
   end Resize;

   function Get_Address (Seg : Memseg_Type) return Address is
   begin
      return Seg.Base;
   end Get_Address;

   procedure Delete (Seg : in out Memseg_Type) is
   begin
      Mmap_Free (Seg.Base, Seg.Size);
      Seg.Base := Null_Address;
      Seg.Size := 0;
   end Delete;

   procedure Set_Rx (Seg : in out Memseg_Type;
                     Offset : Natural; Size : Natural)
   is
      use System.Storage_Elements;
   begin
      Mmap_Rx (Seg.Base + Storage_Offset (Offset), Size);
   end Set_Rx;
end Memsegs_Mmap;
