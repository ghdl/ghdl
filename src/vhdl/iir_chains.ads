--  Chain handling.
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
with Iirs; use Iirs;
with Iir_Chain_Handling;
pragma Elaborate_All (Iir_Chain_Handling);
with Nodes_Meta;

package Iir_Chains is
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
   --  See Iir_Chain_Handling package.

   package Declaration_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Declaration_Chain,
      Set_Chain_Start => Set_Declaration_Chain);

   package Interface_Declaration_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Interface_Declaration_Chain,
      Set_Chain_Start => Set_Interface_Declaration_Chain);

   package Context_Items_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Context_Items,
      Set_Chain_Start => Set_Context_Items);

   package Unit_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Unit_Chain,
      Set_Chain_Start => Set_Unit_Chain);

   package Configuration_Item_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Configuration_Item_Chain,
      Set_Chain_Start => Set_Configuration_Item_Chain);

   package Entity_Class_Entry_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Entity_Class_Entry_Chain,
      Set_Chain_Start => Set_Entity_Class_Entry_Chain);

   package Selected_Waveform_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Selected_Waveform_Chain,
      Set_Chain_Start => Set_Selected_Waveform_Chain);

   package Association_Choices_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Association_Choices_Chain,
      Set_Chain_Start => Set_Association_Choices_Chain);

   package Case_Statement_Alternative_Chain_Handling is new Iir_Chain_Handling
     (Get_Chain_Start => Get_Case_Statement_Alternative_Chain,
      Set_Chain_Start => Set_Case_Statement_Alternative_Chain);

   --  Return the number of elements in a chain starting with FIRST.
   --  Not very efficient since O(N).
   function Get_Chain_Length (First : Iir) return Natural;

   --  Append CHAIN to the chain FIELD of node N.  Not very efficient.
   procedure Append_Chain
     (N : Iir; Field : Nodes_Meta.Fields_Enum; Chain : Iir);

   --  These two subprograms can be used to build a sub-chain.
   --  FIRST and LAST designates respectively the first and last element of
   --  the sub-chain.

   --  Set FIRST and LAST to Null_Iir.
   procedure Sub_Chain_Init (First, Last : out Iir);
   pragma Inline (Sub_Chain_Init);

   --  Append element EL to the sub-chain.
   procedure Sub_Chain_Append (First, Last : in out Iir; El : Iir);
   pragma Inline (Sub_Chain_Append);

   --  Append chain to the sub-chain.  FIRST_SUB and LAST_SUB must not be
   --  Null_Iir.
   procedure Sub_Chain_Append_Chain (First, Last : in out Iir;
                                     First_Sub, Last_Sub : Iir);

   --  Return TRUE iff CHAIN is of length one, ie CHAIN is not NULL_IIR
   --  and chain (CHAIN) is NULL_IIR.
   function Is_Chain_Length_One (Chain : Iir) return Boolean;
   pragma Inline (Is_Chain_Length_One);

   --  Insert EL after LAST.
   procedure Insert (Last : Iir; El : Iir);

   --  Insert EL after LAST and set LAST to EL.
   procedure Insert_Incr (Last : in out Iir; El : Iir);
end Iir_Chains;
