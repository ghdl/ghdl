--  Lists data type.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

package body Lists is
   type List_Record is record
      First : Chunk_Index_Type;
      Last : Chunk_Index_Type;
      Chunk_Idx : Nat32;
      Nbr : Natural;
   end record;

   package Listt is new Tables
     (Table_Component_Type => List_Record,
      Table_Index_Type => List_Type,
      Table_Low_Bound => 2,
      Table_Initial => 128);

   package Chunkt is new Tables
     (Table_Component_Type => Chunk_Type,
      Table_Index_Type => Chunk_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   Chunk_Free_List : Chunk_Index_Type := No_Chunk_Index;

   procedure Free_Chunk (Idx : Chunk_Index_Type) is
   begin
      Chunkt.Table (Idx).Next := Chunk_Free_List;
      Chunk_Free_List := Idx;
   end Free_Chunk;

   function Get_Free_Chunk return Chunk_Index_Type
   is
      Res : Chunk_Index_Type;
   begin
      if Chunk_Free_List /= No_Chunk_Index then
         Res := Chunk_Free_List;
         Chunk_Free_List := Chunkt.Table (Res).Next;
         return Res;
      else
         return Chunkt.Allocate;
      end if;
   end Get_Free_Chunk;

   function Get_Nbr_Elements (List: List_Type) return Natural is
   begin
      return Listt.Table (List).Nbr;
   end Get_Nbr_Elements;

   function Is_Empty (List : List_Type) return Boolean is
   begin
      return Listt.Table (List).Nbr = 0;
   end Is_Empty;

   procedure Append_Element (List: List_Type; Element: El_Type)
   is
      L : List_Record renames Listt.Table (List);
      C : Chunk_Index_Type;
   begin
      L.Chunk_Idx := L.Chunk_Idx + 1;
      if L.Chunk_Idx < Chunk_Len then
         Chunkt.Table (L.Last).Els (L.Chunk_Idx) := Element;
      else
         C := Get_Free_Chunk;
         Chunkt.Table (C).Next := No_Chunk_Index;
         Chunkt.Table (C).Els (0) := Element;
         L.Chunk_Idx := 0;
         if L.Nbr = 0 then
            L.First := C;
         else
            Chunkt.Table (L.Last).Next := C;
         end if;
         L.Last := C;
      end if;
      L.Nbr := L.Nbr + 1;
   end Append_Element;

   function Get_First_Element (List: List_Type) return El_Type
   is
      L : List_Record renames Listt.Table (List);
   begin
      pragma Assert (L.Nbr > 0);
      return Chunkt.Table (L.First).Els (0);
   end Get_First_Element;

   -- Add (append) an element only if it was not already present in the list.
   procedure Add_Element (List: List_Type; El: El_Type)
   is
      It : Iterator;
   begin
      It := Iterate (List);
      while Is_Valid (It) loop
         if Get_Element (It) = El then
            return;
         end if;
         Next (It);
      end loop;

      Append_Element (List, El);
   end Add_Element;

   --  Chain of unused lists.
   List_Free_Chain : List_Type := Null_List;

   function Create_List return List_Type
   is
      Res : List_Type;
   begin
      if List_Free_Chain = Null_List then
         Listt.Increment_Last;
         Res := Listt.Last;
      else
         Res := List_Free_Chain;
         List_Free_Chain := List_Type (Listt.Table (Res).Chunk_Idx);
      end if;
      Listt.Table (Res) := List_Record'(First => No_Chunk_Index,
                                        Last => No_Chunk_Index,
                                        Chunk_Idx => Chunk_Len,
                                        Nbr => 0);
      return Res;
   end Create_List;

   procedure Destroy_List (List : in out List_Type)
   is
      C, Next_C : Chunk_Index_Type;
   begin
      if List = Null_List then
         return;
      end if;

      C := Listt.Table (List).First;
      while C /= No_Chunk_Index loop
         Next_C := Chunkt.Table (C).Next;
         Free_Chunk (C);
         C := Next_C;
      end loop;

      Listt.Table (List).Chunk_Idx := Nat32 (List_Free_Chain);
      List_Free_Chain := List;

      List := Null_List;
   end Destroy_List;

   procedure Finalize is
   begin
      Listt.Free;
      Chunkt.Free;
   end Finalize;

   procedure Initialize is
   begin
      Listt.Init;
      Chunkt.Init;
      List_Free_Chain := Null_List;
      Chunk_Free_List := No_Chunk_Index;
   end Initialize;

   function Iterate (List : List_Valid_Type) return Iterator
   is
      L : List_Record renames Listt.Table (List);
   begin
      return Iterator'(Chunk => L.First,
                       Chunk_Idx => 0,
                       Remain => Int32 (L.Nbr));
   end Iterate;

   function Iterate_Safe (List : List_Type) return Iterator is
   begin
      if List = Null_List then
         return Iterator'(Chunk => No_Chunk_Index,
                          Chunk_Idx => 0,
                          Remain => 0);
      end if;
      return Iterate (List);
   end Iterate_Safe;

   function Is_Valid (It : Iterator) return Boolean is
   begin
      return It.Remain > 0;
   end Is_Valid;

   procedure Next (It : in out Iterator) is
   begin
      It.Chunk_Idx := It.Chunk_Idx + 1;
      if It.Chunk_Idx = Chunk_Len then
         It.Chunk := Chunkt.Table (It.Chunk).Next;
         It.Chunk_Idx := 0;
      end if;
      It.Remain := It.Remain - 1;
   end Next;

   function Get_Element (It : Iterator) return El_Type is
   begin
      return Chunkt.Table (It.Chunk).Els (It.Chunk_Idx);
   end Get_Element;

   procedure Set_Element (It : Iterator; El : El_Type) is
   begin
      Chunkt.Table (It.Chunk).Els (It.Chunk_Idx) := El;
   end Set_Element;
end Lists;
