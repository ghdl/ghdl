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
with GNAT.Table;

package body Lists is
   type Node_Array_Fat is array (Natural) of Node_Type;
   type Node_Array_Fat_Acc is access Node_Array_Fat;

   type List_Record is record
      Max : Natural;
      Nbr : Natural;
      Next : List_Type;
      Els : Node_Array_Fat_Acc;
   end record;

   package Listt is new GNAT.Table
     (Table_Component_Type => List_Record,
      Table_Index_Type => List_Type,
      Table_Low_Bound => 4,
      Table_Initial => 128,
      Table_Increment => 100);

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
   function Get_Last_Element (List: List_Type) return Node_Type
   is
      L : List_Record renames Listt.Table (List);
   begin
      if L.Nbr = 0 then
         return Null_Node;
      else
         return L.Els (L.Nbr - 1);
      end if;
   end Get_Last_Element;

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

   procedure Remove_Nth_Element (List: List_Type; N: Natural)
   is
      Nbr : constant Natural := Get_Nbr_Elements (List);
   begin
      if N >= Nbr then
         raise Program_Error;
      end if;
      for I in N .. Nbr - 2 loop
         Listt.Table (List).Els (I) := Listt.Table (List).Els (I + 1);
      end loop;
      Listt.Table (List).Nbr := Nbr - 1;
   end Remove_Nth_Element;

   procedure Set_Nbr_Elements (List: List_Type; N: Natural) is
   begin
      if N > Get_Nbr_Elements (List) then
         raise Program_Error;
      end if;
      List_Set_Nbr_Elements (List, N);
   end Set_Nbr_Elements;

   -- Return the position of the last element.
   -- Return -1 if the list is empty.
   function Get_Last_Element_Position (List: List_Type) return Integer is
   begin
      return Get_Nbr_Elements (List) - 1;
   end Get_Last_Element_Position;

   function Get_Nbr_Elements_Safe (List: List_Type) return Natural is
   begin
      if List = Null_List then
         return 0;
      else
         return Get_Nbr_Elements (List);
      end if;
   end Get_Nbr_Elements_Safe;

   -- Empty the list
   procedure Empty_List (List: List_Type) is
   begin
      Set_Nbr_Elements (List, 0);
   end Empty_List;

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

end Lists;
