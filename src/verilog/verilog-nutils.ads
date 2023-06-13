--  Node utilities
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.
with Ada.Unchecked_Deallocation;

with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Nutils is
   --  Set location of DEST using the one of SRC.
   procedure Location_Copy (Dest : Node; Src : Node);

   --  Return True if K = V1 or K = V2.
   function Nkind_In (K : Nkind; V1 : Nkind; V2 : Nkind) return Boolean;
   pragma Inline (Nkind_In);

   --  Set FIRST and LAST to Null_Node.
   procedure Init_Chain (First, Last : out Node);
   pragma Inline (Init_Chain);

   --  Append EL to the FIRST/LAST chain.  FIRST is the first element, LAST
   --  the last one.  Initially, when the chain is empty, both FIRST and LAST
   --  must be Null_Node.
   procedure Append_Chain (First : in out Node; Last : in out Node; El : Node);

   type Node_Tuple is record
      First : Node;
      Last : Node;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Array, Node_Arr_Acc);

   ---  Build chains and set parent of elements.
   type Items_Constr is limited private;

   procedure Init_Constr (Constr : out Items_Constr; Parent : Node);
   procedure Append_Node (Constr : in out Items_Constr; Item : Node);
   procedure Append_Constr (Constr : in out Items_Constr; Els : Items_Constr);
   function Get_Constr_Chain (Constr : Items_Constr) return Node;
   function Get_Parent (Constr : Items_Constr) return Node;

   --  If for any bad reason the new elements have been appended to the last
   --  element of CONSTR, update the pointer to the last element.
   procedure Update_Constr (Constr : in out Items_Constr);

   --  Return the number of elements of a chain.
   function Get_Chain_Length (Head : Node) return Natural;

   --  Return the type of a declaration.
   --  A declaration has a Data_Type field, which is how the type was written.
   --  It is usually a name, a packed array, ...
   --  Return the interned form.
   function Get_Type_Data_Type (Decl : Node) return Node;

   --  Return the type of the base class of KLASS, or Null_Node if it has no
   --  base class.
   function Get_Type_Base_Class_Type (Klass : Node) return Node;
private
   type Items_Constr is record
      Parent : Node;
      First_Item : Node;
      Last_Item : Node;
   end record;
end Verilog.Nutils;
