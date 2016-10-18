--  Internal node type and operations.
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

package Nodes is
   type Node_Type is new Int32;
   for Node_Type'Size use 32;

   Null_Node : constant Node_Type := 0;
   Error_Node : constant Node_Type := 1;

   --  A simple type that needs only 2 bits.
   type Bit2_Type is range 0 .. 2 ** 2 - 1;
   type Bit3_Type is range 0 .. 2 ** 3 - 1;

   type Kind_Type is range 0 .. 2 ** 9 - 1;

   --  Format of a node.
   type Format_Type is
     (
      Format_Short,
      Format_Medium
     );

   -- Common fields are:
   --   Flag1 : Boolean
   --   Flag2 : Boolean
   --   Flag3 : Boolean
   --   Flag4 : Boolean
   --   Flag5 : Boolean
   --   Flag6 : Boolean
   --   Flag7 : Boolean
   --   Flag8 : Boolean
   --   Flag9 : Boolean
   --   Flag10 : Boolean
   --   Flag11 : Boolean
   --   Flag12 : Boolean
   --   Flag13 : Boolean
   --   Flag14 : Boolean
   --   Flag15 : Boolean
   --   Nkind : Kind_Type
   --   State1 : Bit2_Type
   --   State2 : Bit2_Type
   --   Location : Location_Type
   --   Field0 : Iir
   --   Field1 : Iir
   --   Field2 : Iir
   --   Field3 : Iir
   --   Field4 : Iir
   --   Field5 : Iir

   -- Fields of Format_Short:

   -- Fields of Format_Medium:
   --   State3 : Bit2_Type
   --   State4 : Bit2_Type
   --   Field6 : Iir (location)
   --   Field7 : Iir (field0)
   --   Field8 : Iir (field1)
   --   Field9 : Iir (field2)
   --   Field10 : Iir (field3)
   --   Field11 : Iir (field4)
   --   Field12 : Iir (field5)

   function Create_Node (Format : Format_Type) return Node_Type;
   procedure Free_Node (N : Node_Type);
   function Next_Node (N : Node_Type) return Node_Type;

   function Get_Nkind (N : Node_Type) return Kind_Type;
   pragma Inline (Get_Nkind);
   procedure Set_Nkind (N : Node_Type; Kind : Kind_Type);
   pragma Inline (Set_Nkind);

   function Get_Location (N: Node_Type) return Location_Type;
   pragma Inline (Get_Location);
   procedure Set_Location (N : Node_Type; Location: Location_Type);
   pragma Inline (Set_Location);

   function Get_Field0 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field0);
   procedure Set_Field0 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field0);

   function Get_Field1 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field1);
   procedure Set_Field1 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field1);

   function Get_Field2 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field2);
   procedure Set_Field2 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field2);

   function Get_Field3 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field3);
   procedure Set_Field3 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field3);

   function Get_Field4 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field4);
   procedure Set_Field4 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field4);


   function Get_Field5 (N : Node_Type) return Node_Type;
   pragma Inline (Get_Field5);
   procedure Set_Field5 (N : Node_Type; V : Node_Type);
   pragma Inline (Set_Field5);

   function Get_Field6 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field6);
   procedure Set_Field6 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field6);

   function Get_Field7 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field7);
   procedure Set_Field7 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field7);

   function Get_Field8 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field8);
   procedure Set_Field8 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field8);

   function Get_Field9 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field9);
   procedure Set_Field9 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field9);

   function Get_Field10 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field10);
   procedure Set_Field10 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field10);

   function Get_Field11 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field11);
   procedure Set_Field11 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field11);

   function Get_Field12 (N: Node_Type) return Node_Type;
   pragma Inline (Get_Field12);
   procedure Set_Field12 (N: Node_Type; Val: Node_Type);
   pragma Inline (Set_Field12);


   function Get_Flag1 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag1);
   procedure Set_Flag1 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag1);

   function Get_Flag2 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag2);
   procedure Set_Flag2 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag2);

   function Get_Flag3 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag3);
   procedure Set_Flag3 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag3);

   function Get_Flag4 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag4);
   procedure Set_Flag4 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag4);

   function Get_Flag5 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag5);
   procedure Set_Flag5 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag5);

   function Get_Flag6 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag6);
   procedure Set_Flag6 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag6);

   function Get_Flag7 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag7);
   procedure Set_Flag7 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag7);

   function Get_Flag8 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag8);
   procedure Set_Flag8 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag8);

   function Get_Flag9 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag9);
   procedure Set_Flag9 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag9);

   function Get_Flag10 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag10);
   procedure Set_Flag10 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag10);

   function Get_Flag11 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag11);
   procedure Set_Flag11 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag11);

   function Get_Flag12 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag12);
   procedure Set_Flag12 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag12);

   function Get_Flag13 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag13);
   procedure Set_Flag13 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag13);

   function Get_Flag14 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag14);
   procedure Set_Flag14 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag14);

   function Get_Flag15 (N : Node_Type) return Boolean;
   pragma Inline (Get_Flag15);
   procedure Set_Flag15 (N : Node_Type; V : Boolean);
   pragma Inline (Set_Flag15);


   function Get_State1 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State1);
   procedure Set_State1 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State1);

   function Get_State2 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State2);
   procedure Set_State2 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State2);

   function Get_State3 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State3);
   procedure Set_State3 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State3);

   function Get_State4 (N : Node_Type) return Bit2_Type;
   pragma Inline (Get_State4);
   procedure Set_State4 (N : Node_Type; V : Bit2_Type);
   pragma Inline (Set_State4);

   --  Get the last node allocated.
   function Get_Last_Node return Node_Type;
   pragma Inline (Get_Last_Node);

   --  Free all and reinit.
   procedure Initialize;
private
   type Node_Record is record
      --  First byte:
      Format : Format_Type;
      Flag1 : Boolean;
      Flag2 : Boolean;
      Flag3 : Boolean;
      Flag4 : Boolean;
      Flag5 : Boolean;
      Flag6 : Boolean;
      Flag7 : Boolean;

      --  Second byte:
      Flag8 : Boolean;
      Flag9 : Boolean;
      Flag10 : Boolean;
      Flag11 : Boolean;
      Flag12 : Boolean;
      Flag13 : Boolean;
      Flag14 : Boolean;
      Flag15 : Boolean;

      --  Third byte:
      Flag16 : Boolean;
      Flag17 : Boolean;
      Flag18 : Boolean;

      --  2*2 = 4 bits
      State1 : Bit2_Type;
      State2 : Bit2_Type;

      --  9 bits
      Kind : Kind_Type;

      -- Location.
      Location: Location_Type;

      Field0 : Node_Type;
      Field1 : Node_Type;
      Field2 : Node_Type;
      Field3 : Node_Type;
      Field4 : Node_Type;
      Field5 : Node_Type;
   end record;
   pragma Pack (Node_Record);
   for Node_Record'Size use 8*32;
   for Node_Record'Alignment use 4;
   pragma Suppress_Initialization (Node_Record);

   Init_Node : constant Node_Record := Node_Record'
     (Format => Format_Short,
      Kind => 0,
      State1 | State2 => 0,
      Location => Location_Nil,
      Field0 | Field1 | Field2 | Field3 | Field4 | Field5 => Null_Node,
      others => False);

end Nodes;
