--  Lists data type.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with System;
with Tables;

package body Lists is
   type Node_Array_Fat is array (Natural) of Node_Type;
   type Node_Array_Fat_Acc is access Node_Array_Fat;

   type List_Record is record
      Max : Natural;
      Nbr : Natural;
      Next : List_Type;
      Els : Node_Array_Fat_Acc;
   end record;

   package Listt is new Tables
     (Table_Component_Type => List_Record,
      Table_Index_Type => List_Type,
      Table_Low_Bound => 2,
      Table_Initial => 128);

   --function Get_Max_Nbr_Elements (List : List_Type) return Natural;
   --pragma Inline (Get_Max_Nbr_Elements);

   --procedure Set_Max_Nbr_Elements (List : List_Type; Max : Natural);
   --pragma Inline (Set_Max_Nbr_Elements);

   procedure List_Set_Nbr_Elements (List : List_Type; Nbr : Natural);
   pragma Inline (List_Set_Nbr_Elements);

   function Get_Nbr_Elements (List: List_Type) return Natural is
   begin
      return Listt.Table (List).Nbr;
   end Get_Nbr_Elements;

   procedure List_Set_Nbr_Elements (List : List_Type; Nbr : Natural) is
   begin
      Listt.Table (List).Nbr := Nbr;
   end List_Set_Nbr_Elements;

   function Is_Empty (List : List_Type) return Boolean is
   begin
      return Listt.Table (List).Nbr = 0;
   end Is_Empty;

   --function Get_Max_Nbr_Elements (List : List_Type) return Natural is
   --begin
   --   return Listt.Table (List).Max;
   --end Get_Max_Nbr_Elements;

   --procedure Set_Max_Nbr_Elements (List : List_Type; Max : Natural) is
   --begin
   --   Listt.Table (List).Max := Max;
   --end Set_Max_Nbr_Elements;

   function Get_Nth_Element (List: List_Type; N: Natural)
     return Node_Type
   is
   begin
      if N >= Listt.Table (List).Nbr then
         return Null_Node;
      end if;
      return Listt.Table (List).Els (N);
   end Get_Nth_Element;

   -- Replace an element selected by position.
   procedure Replace_Nth_Element (List: List_Type; N: Natural; El: Node_Type)
   is
   begin
      if N >= Listt.Table (List).Nbr then
         raise Program_Error;
      end if;
      Listt.Table (List).Els (N) := El;
   end Replace_Nth_Element;

   -- Be sure an element can be added to LIST.
   -- It doesn't change the number of elements.
   procedure List_Grow (List: List_Type)
   is
      L : List_Record renames Listt.Table (List);

      --  Be careful: size in bytes.
      function Alloc (Size : Natural) return Node_Array_Fat_Acc;
      pragma Import (C, Alloc, "malloc");

      function Realloc (Ptr : Node_Array_Fat_Acc; Size : Natural)
        return Node_Array_Fat_Acc;
      pragma Import (C, Realloc, "realloc");

      Tmp : Node_Array_Fat_Acc;
      N : Natural;
   begin
      if L.Nbr < L.Max then
         return;
      end if;
      if L.Max = 0 then
         N := 8;
         Tmp := Alloc (N * Node_Type'Size / System.Storage_Unit);
      else
         N := L.Max * 2;
         Tmp := Realloc (L.Els, N * Node_Type'Size / System.Storage_Unit);
      end if;
      L.Els := Tmp;
      L.Max := N;
   end List_Grow;

   procedure Append_Element (List: List_Type; Element: Node_Type)
   is
      L : List_Record renames Listt.Table (List);
   begin
      if L.Nbr >= L.Max then
         List_Grow (List);
      end if;
      L.Els (L.Nbr) := Element;
      L.Nbr := L.Nbr + 1;
   end Append_Element;

   -- Return the last element of the list, or null.
   -- Return the first element of the list, or null.
   function Get_First_Element (List: List_Type) return Node_Type is
   begin
      if Listt.Table (List).Nbr = 0 then
         return Null_Node;
      else
         return Listt.Table (List).Els (0);
      end if;
   end Get_First_Element;

   -- Add (append) an element only if it was not already present in the list.
   procedure Add_Element (List: List_Type; El: Node_Type)
   is
      Nbr : constant Natural := Get_Nbr_Elements (List);
   begin
      for I in 0 .. Nbr - 1 loop
         if Listt.Table (List).Els (I) = El then
            return;
         end if;
      end loop;

      Append_Element (List, El);
   end Add_Element;

   procedure Set_Nbr_Elements (List: List_Type; N: Natural) is
   begin
      if N > Get_Nbr_Elements (List) then
         raise Program_Error;
      end if;
      List_Set_Nbr_Elements (List, N);
   end Set_Nbr_Elements;

   --  Chain of unused lists.
   Free_Chain : List_Type := Null_List;

   function Create_List return List_Type
   is
      Res : List_Type;
   begin
      if Free_Chain = Null_List then
         Listt.Increment_Last;
         Res := Listt.Last;
      else
         Res := Free_Chain;
         Free_Chain := Listt.Table (Res).Next;
      end if;
      Listt.Table (Res) := List_Record'(Max => 0, Nbr => 0,
                                        Next => Null_List, Els => null);
      return Res;
   end Create_List;

   procedure Free (Ptr : Node_Array_Fat_Acc);
   pragma Import (C, Free, "free");

   procedure Destroy_List (List : in out List_Type)
   is
   begin
      if List = Null_List then
         return;
      end if;
      if Listt.Table (List).Max > 0 then
         Free (Listt.Table (List).Els);
         Listt.Table (List).Els := null;
      end if;
      Listt.Table (List).Next := Free_Chain;
      Free_Chain := List;
      List := Null_List;
   end Destroy_List;

   procedure Initialize is
   begin
      for I in Listt.First .. Listt.Last loop
         if Listt.Table (I).Els /= null then
            Free (Listt.Table (I).Els);
         end if;
      end loop;
      Listt.Free;
      Listt.Init;
   end Initialize;

   function Iterate (List : List_Type) return Iterator is
   begin
      return Iterator'(List => List,
                       Len => Get_Nbr_Elements (List),
                       Idx => 0);
   end Iterate;

   function Iterate_Safe (List : List_Type) return Iterator is
   begin
      if List = Null_List then
         return Iterator'(List => Null_List,
                          Len => 0,
                          Idx => 0);
      end if;
      return Iterate (List);
   end Iterate_Safe;

   function Is_Valid (It : Iterator) return Boolean is
   begin
      return It.Idx < It.Len;
   end Is_Valid;

   function Is_First (It : Iterator) return Boolean is
   begin
      return It.Idx = 0;
   end Is_First;

   procedure Next (It : in out Iterator) is
   begin
      It.Idx := It.Idx + 1;
   end Next;

   function Get_Element (It : Iterator) return Node_Type is
   begin
      return Get_Nth_Element (It.List, It.Idx);
   end Get_Element;

   procedure Set_Element (It : Iterator; El : Node_Type) is
   begin
      Replace_Nth_Element (It.List, It.Idx, El);
   end Set_Element;

   procedure Truncate (It : Iterator) is
   begin
      Set_Nbr_Elements (It.List, It.Idx);
   end Truncate;
end Lists;
