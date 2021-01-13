--  Fixed-length lists.
--  Copyright (C) 2017 Tristan Gingold
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

with Tables;

package body Flists is
   --  Index of elements.
   type El_Index_Type is new Int32;

   --  Describe an flist.
   type Entry_Type is record
      --  Index of the first element (in the element table).
      Els : El_Index_Type;

      --  Length of the list.
      Len : Nat32;
   end record;

   --  Flists descriptors.
   package Flistt is new Tables
     (Table_Component_Type => Entry_Type,
      Table_Index_Type => Flist_Type,
      Table_Low_Bound => 4,
      Table_Initial => 32);

   --  Table of all elements.
   package Els is new Tables
     (Table_Component_Type => El_Type,
      Table_Index_Type => El_Index_Type,
      Table_Low_Bound => 0,
      Table_Initial => 128);

   type Flist_Array is array (Natural range <>) of Flist_Type;

   --  Linked list of free flist.  For length less than the last index, the
   --  index corresponds to the length.  All free lists whose length is equal
   --  or greater than the last index are grouped to the last index.
   Free_Flists : Flist_Array (0 .. 16) := (others => Null_Flist);

   --  Get the chain for a free flist for large length.  It is stored at the
   --  first element of the list.
   function Free_Next (Flist : Flist_Type) return Flist_Type is
   begin
      return Flist_Type (Els.Table (Flistt.Table (Flist).Els));
   end Free_Next;

   function Create_Flist (Len : Natural) return Flist_Type
   is
      Res : Flist_Type;
      Prev : Flist_Type;
      Next : Flist_Type;
   begin
      if Len >= Free_Flists'Last then
         --  Large length.
         Res := Free_Flists (Free_Flists'Last);
         Prev := Null_Flist;
         while Res /= Null_Flist and then Length (Res) /= Len loop
            Prev := Res;
            Res := Free_Next (Res);
         end loop;
         if Res /= Null_Flist then
            Next := Free_Next (Res);
            if Prev = Null_Flist then
               Free_Flists (Free_Flists'Last) := Next;
            else
               Els.Table (Flistt.Table (Prev).Els) := El_Type (Next);
            end if;
         end if;
      else
         --  Small length.  The Len field contains the next free list.
         Res := Free_Flists (Len);
         if Res /= Null_Flist then
            Free_Flists (Len) := Flist_Type (Flistt.Table (Res).Len);
            Flistt.Table (Res).Len := Nat32 (Len);
         elsif Len = 0 then
            --  Quick case for len = 0.
            Res := Flistt.Allocate (1);
            Flistt.Table (Res) := (Els => 0, Len => 0);
            return Res;
         end if;
      end if;

      if Res = Null_Flist then
         Res := Flistt.Allocate (1);
         Flistt.Table (Res) := (Els => Els.Allocate (Len),
                                Len => Nat32 (Len));
      end if;

      --  Clear the list.
      declare
         Idx : constant El_Index_Type := Flistt.Table (Res).Els;
      begin
         Els.Table (Idx .. Idx + El_Index_Type (Len) - 1) := (others => 0);
      end;

      return Res;
   end Create_Flist;

   procedure Destroy_Flist (Flist : in out Flist_Type)
   is
      Len : constant Natural := Length (Flist);
      Prev : Flist_Type;
   begin
      --  Prepend to the array of free flists.
      if Len >= Free_Flists'Last then
         Prev := Free_Flists (Free_Flists'Last);
         Free_Flists (Free_Flists'Last) := Flist;

         Els.Table (Flistt.Table (Flist).Els) := El_Type (Prev);
      else
         Prev := Free_Flists (Len);
         Free_Flists (Len) := Flist;

         Flistt.Table (Flist).Len := Nat32 (Prev);
      end if;

      Flist := Null_Flist;
   end Destroy_Flist;

   function Flast (Flist : Flist_Type) return Integer is
   begin
      return Integer (Flistt.Table (Flist).Len - 1);
   end Flast;

   function Length (Flist : Flist_Type) return Natural is
   begin
      return Natural (Flistt.Table (Flist).Len);
   end Length;

   function Get_Nth_Element (Flist : Flist_Type; N : Natural) return El_Type
   is
      E : Entry_Type renames Flistt.Table (Flist);
   begin
      pragma Assert (N < Natural (E.Len));
      return Els.Table (E.Els + El_Index_Type (N));
   end Get_Nth_Element;

   procedure Set_Nth_Element (Flist : Flist_Type; N : Natural; V : El_Type)
   is
      E : Entry_Type renames Flistt.Table (Flist);
   begin
      pragma Assert (N < Natural (E.Len));
      Els.Table (E.Els + El_Index_Type (N)) := V;
   end Set_Nth_Element;
end Flists;
