--  Area based memory manager
--  Copyright (C) 2014 Tristan Gingold
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

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

package Areapools is
   type Areapool is limited private;
   type Mark_Type is private;

   type Areapool_Acc is access all Areapool;

   --  Modular type for the size.  We don't use Storage_Offset in order to
   --  make alignment computation efficient (knowing that alignment is a
   --  power of two).
   type Size_Type is mod System.Memory_Size;

   --  Allocate SIZE bytes (aligned on ALIGN bytes) in memory pool POOL and
   --  return the address in RES.
   procedure Allocate (Pool : in out Areapool;
                       Res : out Address;
                       Size : Size_Type;
                       Align : Size_Type);

   --  Return TRUE iff no memory is allocated in POOL.
   function Is_Empty (Pool : Areapool) return Boolean;

   --  Higher level abstraction for Allocate.
   generic
      type T is private;
   function Alloc_On_Pool_Addr (Pool : Areapool_Acc; Val : T)
                               return System.Address;

   --  Get a mark of POOL.
   procedure Mark (M : out Mark_Type;
                   Pool : Areapool);

   --  Release memory allocated in POOL after mark M.
   procedure Release (M : Mark_Type;
                      Pool : in out Areapool);

   Empty_Marker : constant Mark_Type;
private
   --  Minimal size of allocation.
   Default_Chunk_Size : constant Size_Type := 16 * 1024;

   type Chunk_Type;
   type Chunk_Acc is access all Chunk_Type;

   type Data_Array is array (Size_Type range <>) of Storage_Element;
   for Data_Array'Alignment use Standard'Maximum_Alignment;

   type Chunk_Type (Last : Size_Type) is record
      Prev : Chunk_Acc;
      Data : Data_Array (0 .. Last);
   end record;
   for Chunk_Type'Alignment use Standard'Maximum_Alignment;

   type Areapool is limited record
      First, Last : Chunk_Acc := null;
      Next_Use : Size_Type;
   end record;

   type Mark_Type is record
      Last : Chunk_Acc := null;
      Next_Use : Size_Type;
   end record;

   Empty_Marker : constant Mark_Type := (Last => null, Next_Use => 0);

   Erase_When_Released : constant Boolean := True;
end Areapools;
