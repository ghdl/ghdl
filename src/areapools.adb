--  Area based memory manager
--  Copyright (C) 2014 Tristan Gingold
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

with Ada.Unchecked_Deallocation;

package body Areapools is
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Chunk_Type, Chunk_Acc);

   Free_Chunks : Chunk_Acc;

   function Get_Chunk return Chunk_Acc is
      Res : Chunk_Acc;
   begin
      if Free_Chunks /= null then
         Res := Free_Chunks;
         Free_Chunks := Res.Prev;
         return Res;
      else
         return new Chunk_Type (Default_Chunk_Size - 1);
      end if;
   end Get_Chunk;

   procedure Free_Chunk (Chunk : Chunk_Acc) is
   begin
      Chunk.Prev := Free_Chunks;
      Free_Chunks := Chunk;
   end Free_Chunk;

   procedure Allocate (Pool : in out Areapool;
                       Res : out Address;
                       Size : Size_Type;
                       Align : Size_Type)
   is
      Align_M1 : constant Size_Type := Align - 1;

      function Do_Align (X : Size_Type) return Size_Type is
      begin
         return (X + Align_M1) and not Align_M1;
      end Do_Align;

      Chunk : Chunk_Acc;
   begin
      --  Need to allocate a new chunk if there is no current chunk, or not
      --  enough room in the current chunk.
      if Pool.Last = null
        or else Do_Align (Pool.Next_Use) + Size > Pool.Last.Last
      then
         if Size > Default_Chunk_Size then
            Chunk := new Chunk_Type (Size - 1);
         else
            Chunk := Get_Chunk;
         end if;
         Chunk.Prev := Pool.Last;
         Pool.Next_Use := 0;
         if Pool.First = null then
            Pool.First := Chunk;
         end if;
         Pool.Last := Chunk;
      else
         Chunk := Pool.Last;
         Pool.Next_Use := Do_Align (Pool.Next_Use);
      end if;
      Res := Chunk.Data (Pool.Next_Use)'Address;
      Pool.Next_Use := Pool.Next_Use + Size;
   end Allocate;

   procedure Mark (M : out Mark_Type; Pool : Areapool) is
   begin
      M := (Last => Pool.Last, Next_Use => Pool.Next_Use);
   end Mark;

   procedure Release (M : Mark_Type; Pool : in out Areapool)
   is
      Chunk : Chunk_Acc;
      Prev : Chunk_Acc;
   begin
      Chunk := Pool.Last;
      while Chunk /= M.Last loop
         if Erase_When_Released then
            Chunk.Data := (others => 16#DE#);
         end if;

         Prev := Chunk.Prev;
         if Chunk.Last = Default_Chunk_Size - 1 then
            Free_Chunk (Chunk);
         else
            Deallocate (Chunk);
         end if;
         Chunk := Prev;
      end loop;

      if Erase_When_Released
        and then M.Last /= null
        and then M.Next_Use /= 0
      then
         declare
            Last : Size_Type;
         begin
            if Pool.Last = M.Last then
               Last := Pool.Next_Use - 1;
            else
               Last := Chunk.Data'Last;
            end if;
            Chunk.Data (M.Next_Use .. Last) := (others => 16#DE#);
         end;
      end if;

      Pool.Last := M.Last;
      Pool.Next_Use := M.Next_Use;
   end Release;

   function Is_Empty (Pool : Areapool) return Boolean is
   begin
      return Pool.Last = null;
   end Is_Empty;

   function Alloc_On_Pool_Addr (Pool : Areapool_Acc; Val : T)
                               return System.Address
   is
      Res : Address;
   begin
      Allocate (Pool.all, Res, T'Size / Storage_Unit, T'Alignment);
      declare
         Addr1 : constant Address := Res;
         Init : T := Val;
         for Init'Address use Addr1;
      begin
         null;
      end;
      return Res;
   end Alloc_On_Pool_Addr;
end Areapools;
