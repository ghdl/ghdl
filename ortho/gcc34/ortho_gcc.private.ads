--  Ortho implementation for GCC.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Agcc.Trees; use Agcc.Trees;

package Ortho_Nodes is
   --  PUBLIC PART is defined in ortho_nodes.common.ads
   type O_Cnode is private;
   type O_Dnode is private;
   type O_Enode is private;
   type O_Fnode is private;
   type O_Lnode is private;
   type O_Tnode is private;
   type O_Snode is private;

   --  Alloca builtin, to be set during initialization.
   Alloca_Function_Ptr : Tree;

   --  Must be called during initialization, before use of any subprograms.
   procedure Init;
private
   type O_Cnode is new Tree;
   type O_Dnode is new Tree;
   type O_Enode is new Tree;
   type O_Fnode is new Tree;
   type O_Lnode is new Tree;
   type O_Tnode is new Tree;
   type O_Snode is new Nesting;

   O_Cnode_Null : constant O_Cnode := O_Cnode (NULL_TREE);
   O_Dnode_Null : constant O_Dnode := O_Dnode (NULL_TREE);
   O_Enode_Null : constant O_Enode := O_Enode (NULL_TREE);
   O_Fnode_Null : constant O_Fnode := O_Fnode (NULL_TREE);
   O_Lnode_Null : constant O_Lnode := O_Lnode (NULL_TREE);
   O_Tnode_Null : constant O_Tnode := O_Tnode (NULL_TREE);
   O_Snode_Null : constant O_Snode := O_Snode (Nesting_Null);


   --  Efficiently append element EL to a chain.
   --  FIRST is the first element of the chain (must NULL_TREE if the chain
   --   is empty),
   --  LAST is the last element of the chain (idem).
   type Chain_Constr_Type is record
      First : Tree;
      Last : Tree;
   end record;
   procedure Chain_Init (Constr : out Chain_Constr_Type);
   procedure Chain_Append (Constr : in out Chain_Constr_Type; El : Tree);

   --  Efficiently append element EL to a list.
   type List_Constr_Type is record
      First : Tree;
      Last : Tree;
   end record;
   procedure List_Init (Constr : out List_Constr_Type);
   procedure List_Append (Constr : in out List_Constr_Type; El : Tree);

   type O_Inter_List is record
      Ident : O_Ident;
      Storage : O_Storage;
      --  Return type.
      Rtype : O_Tnode;
      --  List of parameter types.
      Param_List : List_Constr_Type;
      --  Chain of parameters declarations.
      Param_Chain : Chain_Constr_Type;
   end record;

   type O_Element_List is record
      Res : Tree;
      Chain : Chain_Constr_Type;
   end record;

   type O_Case_Block is record
      Expr : Tree;
      First : Boolean;
      Label : Tree;
   end record;

   type O_If_Block is record
      null;
   end record;

   type O_Aggr_List is record
      Atype : Tree;
      Chain : Chain_Constr_Type;
   end record;

   type O_Record_Aggr_List is new O_Aggr_List;
   type O_Array_Aggr_List is new O_Aggr_List;

   type O_Assoc_List is record
      Subprg : Tree;
      List : List_Constr_Type;
   end record;

   type O_Enum_List is record
      --  The enumeral_type node.
      Res : Tree;
      --  Chain of literals.
      Chain : Chain_Constr_Type;
      --  Numeral value (from 0 to nbr - 1) of the next literal to be declared.
      Num : Natural;
      --  Size of the enumeration type.
      Size : Natural;
   end record;

end Ortho_Nodes;
