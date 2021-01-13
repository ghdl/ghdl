--  Chain handling.
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
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Nodes_Meta;

package Vhdl.Nodes_Utils is
   --  Chains are simply linked list of iirs.
   --  Elements of the chain are ordered.
   --  Each element of a chain have a Chain field, which points to the next
   --  element.
   --  All elements of a chain have the same parent.  This parent contains
   --  a field which points to the first element of the chain.
   --  Note: the parent is often the value of the Parent field, but sometimes
   --    not.

   --  Chains can be covered very simply:
   --      El : Iir;
   --   begin
   --      El := Get_xxx_Chain (Parent);
   --      while El /= Null_Iir loop
   --         * Handle element EL of the chain.
   --         El := Get_Chain (El);
   --      end loop;

   --  However, building a chain is a little bit more difficult if elements
   --  have to be appended.  Indeed, there is no direct access to the last
   --  element of a chain.
   --  An efficient way to build a chain is to keep the last element of it.

   --  Return the number of elements in a chain starting with FIRST.
   --  Not very efficient since O(N).
   function Get_Chain_Length (First : Iir) return Natural;

   --  Append CHAIN to the chain FIELD of node N.  Not very efficient.
   procedure Append_Chain
     (N : Iir; Field : Vhdl.Nodes_Meta.Fields_Enum; Chain : Iir);

   --  These two subprograms can be used to build a sub-chain.
   --  FIRST and LAST designates respectively the first and last element of
   --  the sub-chain.

   --  Set FIRST and LAST to Null_Iir.
   procedure Chain_Init (First, Last : out Iir);
   pragma Inline (Chain_Init);

   --  Append element EL to the sub-chain.
   procedure Chain_Append (First, Last : in out Iir; El : Iir);
   pragma Inline (Chain_Append);

   --  Append chain to the sub-chain.  FIRST_SUB and LAST_SUB must not be
   --  Null_Iir.
   procedure Chain_Append_Chain (First, Last : in out Iir;
                                 First_Sub, Last_Sub : Iir);
   procedure Chain_Append_Subchain (First, Last : in out Iir;
                                    Sub : Iir);

   --  Return TRUE iff CHAIN is of length one, ie CHAIN is not NULL_IIR
   --  and chain (CHAIN) is NULL_IIR.
   function Is_Chain_Length_One (Chain : Iir) return Boolean;
   pragma Inline (Is_Chain_Length_One);

   --  Insert EL after LAST and set LAST to EL.
   procedure Insert_Incr (Last : in out Iir; El : Iir);
end Vhdl.Nodes_Utils;
