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
   List_All : constant List_Type := 1;

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

   --  Create a list.
   function Create_List return List_Type;

   --  Destroy a list.
   procedure Destroy_List (List : in out List_Type);

   --  Free all the lists and reset to initial state.
   --  Must be used to free the memory used by the lists.
   procedure Initialize;

   --  Append ELEMENT to the list.  It's an O(1) operation.
   procedure Append_Element (List : List_Type; Element : Node_Type);

   --  Return the first element of the list.
   function Get_First_Element (List : List_Type) return Node_Type;

   --  Append EL if not already in LIST.  It's an O(n) operation.
   procedure Add_Element (List : List_Type; El : Node_Type);

   -- Return the number of elements in the list.
   -- This is also 1 + the position of the last element.
   function Get_Nbr_Elements (List: List_Type) return Natural;
   pragma Inline (Get_Nbr_Elements);

   --  True if LIST is empty.
   function Is_Empty (List : List_Type) return Boolean;

   --  Iterator.  The idiomatic way to iterate is:
   --  It := Iterate (List);
   --  while Is_Valid (It) loop
   --     El := Get_Element (It);
   --     ...
   --     Next (It);
   --  end loop;
   type Iterator is private;

   function Iterate (List : List_Type) return Iterator;
   function Is_Valid (It : Iterator) return Boolean;
   procedure Next (It : in out Iterator);
   function Get_Element (It : Iterator) return Node_Type;
   procedure Set_Element (It : Iterator; El : Node_Type);

   --  Use the C convention for all these subprograms, so that the Iterator is
   --  always passed by reference.
   pragma Convention (C, Is_Valid);
   pragma Convention (C, Next);
   pragma Convention (C, Get_Element);
   pragma Convention (C, Set_Element);

   --  Like Iterate, but if LIST is Null_List, it returns an iterator that is
   --  never valid.
   function Iterate_Safe (List : List_Type) return Iterator;

private
   type Chunk_Index_Type is new Int32;
   for Chunk_Index_Type'Size use 32;
   No_Chunk_Index : constant Chunk_Index_Type := 0;

   Chunk_Len : constant := 7;

   type Node_Type_Array is
     array (Nat32 range 0 .. Chunk_Len - 1) of Node_Type;

   type Chunk_Type is record
      Next : Chunk_Index_Type;
      Els : Node_Type_Array;
   end record;

   type Iterator is record
      Chunk : Chunk_Index_Type;
      Chunk_Idx : Nat32;
      Remain : Nat32;
   end record;
   pragma Convention (C, Iterator);

   pragma Inline (Iterate);
   pragma Inline (Is_Valid);
   pragma Inline (Next);
   pragma Inline (Get_Element);
end Lists;
