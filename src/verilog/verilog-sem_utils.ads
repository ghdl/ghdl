--  Verilog semantic analyzer (misc)
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

with Types; use Types;
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Sem_Utils is
   --  Return the declaration for the base name of LVALUE.
   function Get_Base_Lvalue (Lvalue : Node) return Node;

   function Is_Method (Rtn : Node) return Boolean;

   --  Return the parent node for the attribute, ie the nearest parent
   --  that holds attributes.
   --  They are:
   --  -  N_Module
   --  -  N_Seq_Block
   --  -  N_Par_Block
   function Get_Attribute_Parent (Parent : Node) return Node;

   --  Return the base class of KLASS.  Will return the Base_Root_Class if
   --  there is no parent.
   function Iterate_Base_Class_Type (Klass : Node) return Node;

   --  Return TRUE iff STMT is 'super.new(xx)'
   function Is_Call_To_Super_New (Stmt : Node) return Boolean;

   --  Get rid of names and ports prefixes.
   function Strip_Names_And_Ports (N : Node) return Node;

   --  Return the length of MSB:LSB.
   function Compute_Length (Msb : Int32; Lsb : Int32) return Int32;
   function Compute_Length (Rng : Node) return Int32;

   --  Return TRUE iff analyzed N is a replication_cst with a count of 0.
   --  Such node should be ignored.
   function Is_Null_Replication (N : Node) return Boolean;

   --  True iff number has X or Z values
   function Has_Number_X_Z (Num : Node) return Boolean;

   --  Create an N_Number node.
   function Build_Number (Val : Uns32;
                          Ntype : Node := Null_Node;
                          Loc : Location_Type := No_Location)
                         return Node;

   function Build_Add (Num : Node;
                       Val : Uns32;
                       Loc : Location_Type := No_Location)
                      return Node;

   function Find_Member_By_Id (Id : Name_Id; Chain : Node) return Node;
end Verilog.Sem_Utils;
