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
with Types; use Types;
with Nodes; use Nodes;

package Lists is
   type List_Type is new Nat32;
   for List_Type'Size use 32;

   Null_List : constant List_Type := 0;

   List_Others : constant List_Type := 1;
   List_All : constant List_Type := 2;

   -----------
   -- Lists --
   -----------

   -- Iir_Kinds_List
   -- Lists of elements.
   -- index is 0 .. nbr_elements - 1.
   --
   -- Append an element to (the end of) the list.
   --   procedure Append_Element (List: in Iir; Element: Iir);
   --
   -- Get the N th element in list, starting from 0.
   -- Return an access to the element or null_iir, if beyond bounds.
   --   function Get_Nth_Element (List: in Iir; N: Natural) return Iir;
   --
   -- Return the last element of the list, or null_iir.
   --   function Get_Last_Element (List: in Iir) return Iir;
   --
   -- Return the first element of the list, or null_iir.
   --   function Get_First_Element (List: in Iir) return Iir;
   --
   -- Replace an element selected by position.
   --   procedure Replace_Nth_Element (List: in Iir_List; N: Natural; El:Iir);
   --
   -- Add (append) an element only if it was not already present in the list.
   -- Return its position.
   --   procedure Add_Element (List: in Iir; El: Iir; Position: out integer);
   --   procedure Add_Element (List: in Iir_List; El: Iir);
   --
   -- Return the number of elements in the list.
   -- This is also 1 + the position of the last element.
   --   function Get_Nbr_Elements (List: in Iir_List) return Natural;
   --
   -- Set the number of elements in the list.
   -- Can be used only to shrink the list.
   --   procedure Set_Nbr_Elements (List: in Iir_List; N: Natural);
   --
   -- Remove an element from the list.
   --   procedure remove_Nth_Element (List: in Iir_List; N: Natural);
   --
   -- Return the position of the last element.
   -- Return -1 if the list is empty.
   --   function Get_Last_Element_Position (List: in Iir_List) return Integer;
   --
   -- Empty the list.
   -- This is also set_nbr_elements (list, 0);
   --   procedure Empty_List (List: in Iir_List);
   --
   -- Alias a list.  TARGET must be empty.
   --   procedure Alias_List (Target: in out Iir; Source: in Iir);

   procedure Append_Element (List: List_Type; Element: Node_Type);

   -- Get the N th element in list, starting from 0.
   -- Return the element or null_iir, if beyond bounds.
   function Get_Nth_Element (List: List_Type; N: Natural) return Node_Type;

   function Get_Last_Element (List: List_Type) return Node_Type;

   function Get_First_Element (List: List_Type) return Node_Type;

   procedure Replace_Nth_Element (List: List_Type; N: Natural; El: Node_Type);

   procedure Add_Element (List: List_Type; El: Node_Type);

   -- Return the number of elements in the list.
   -- This is also 1 + the position of the last element.
   function Get_Nbr_Elements (List: List_Type) return Natural;
   pragma Inline (Get_Nbr_Elements);

   --  Same as get_nbr_elements but returns 0 if LIST is NULL_IIR.
   function Get_Nbr_Elements_Safe (List : List_Type) return Natural;

   -- Set the number of elements in the list.
   -- Can be used only to shrink the list.
   procedure Set_Nbr_Elements (List: List_Type; N: Natural);

   procedure Remove_Nth_Element (List : List_Type; N: Natural);

   function Get_Last_Element_Position (List: List_Type) return Integer;

   --  Clear the list.
   procedure Empty_List (List: List_Type);

   --  Create a list.
   function Create_List return List_Type;

   --  Destroy a list.
   procedure Destroy_List (List : in out List_Type);

   --  Free all the lists and reset to initial state.
   --  Must be used to free the memory used by the lists.
   procedure Initialize;
end Lists;
