--  Tree node definitions.
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

with Ada.Unchecked_Conversion;
with Tables;
with Logging; use Logging;
with Vhdl.Lists; use Vhdl.Lists;
with Vhdl.Nodes_Meta; use Vhdl.Nodes_Meta;
with Vhdl.Nodes_Priv; use Vhdl.Nodes_Priv;

package body Vhdl.Nodes is
   --  A simple type that needs only 2 bits.
   type Bit2_Type is range 0 .. 2 ** 2 - 1;

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

   function Get_Nkind (N : Node_Type) return Kind_Type;
   pragma Inline (Get_Nkind);
   procedure Set_Nkind (N : Node_Type; Kind : Kind_Type);
   pragma Inline (Set_Nkind);

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

      --  Suppress the access check of the table base.  This is really safe to
   --  suppress this check because the table base cannot be null.
   pragma Suppress (Access_Check);

   --  Suppress the index check on the table.
   --  Could be done during non-debug, since this may catch errors (reading
   --  Null_Node or Error_Node).
   --pragma Suppress (Index_Check);

   package Nodet is new Tables
     (Table_Component_Type => Node_Record,
      Table_Index_Type => Node_Type,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   function Get_Last_Node return Iir is
   begin
      return Nodet.Last;
   end Get_Last_Node;

   Free_Chain : Node_Type := Null_Node;

   function Create_Node (Format : Format_Type) return Node_Type
   is
      Res : Node_Type;
   begin
      case Format is
         when Format_Medium =>
            --  Allocate a first node.
            Nodet.Increment_Last;
            Res := Nodet.Last;
            --  Check alignment.
            if Res mod 2 = 1 then
               Set_Field1 (Res, Free_Chain);
               Free_Chain := Res;
               Nodet.Increment_Last;
               Res := Nodet.Last;
            end if;
            --  Allocate the second node.
            Nodet.Increment_Last;
            Nodet.Table (Res) := Init_Node;
            Nodet.Table (Res).Format := Format_Medium;
            Nodet.Table (Res + 1) := Init_Node;
         when Format_Short =>
            --  Check from free pool
            if Free_Chain = Null_Node then
               Nodet.Increment_Last;
               Res := Nodet.Last;
            else
               Res := Free_Chain;
               Free_Chain := Get_Field1 (Res);
            end if;
            Nodet.Table (Res) := Init_Node;
      end case;
      return Res;
   end Create_Node;

   type Free_Node_Hook_Array is
     array (Natural range 1 .. 8) of Free_Iir_Hook;
   Nbr_Free_Hooks : Natural := 0;

   Free_Hooks : Free_Node_Hook_Array;

   procedure Register_Free_Hook (Hook : Free_Iir_Hook) is
   begin
      if Nbr_Free_Hooks >= Free_Hooks'Last then
         --  Not enough room in Free_Hooks.
         raise Internal_Error;
      end if;
      Nbr_Free_Hooks := Nbr_Free_Hooks + 1;
      Free_Hooks (Nbr_Free_Hooks) := Hook;
   end Register_Free_Hook;

   procedure Free_Node (N : Node_Type) is
   begin
      if N = Null_Node then
         return;
      end if;

      --  Call hooks.
      for I in Free_Hooks'First .. Nbr_Free_Hooks loop
         Free_Hooks (I).all (N);
      end loop;

      --  Really free the node.
      Set_Nkind (N, 0);
      Set_Field1 (N, Free_Chain);
      Free_Chain := N;
      if Nodet.Table (N).Format = Format_Medium then
         Set_Field1 (N + 1, Free_Chain);
         Free_Chain := N + 1;
      end if;
   end Free_Node;

   procedure Free_Iir (Target : Iir) renames Free_Node;

   function Next_Node (N : Node_Type) return Node_Type is
   begin
      case Nodet.Table (N).Format is
         when Format_Medium =>
            return N + 2;
         when Format_Short =>
            return N + 1;
      end case;
   end Next_Node;

   function Get_Nkind (N : Node_Type) return Kind_Type is
   begin
      return Nodet.Table (N).Kind;
   end Get_Nkind;

   procedure Set_Nkind (N : Node_Type; Kind : Kind_Type) is
   begin
      Nodet.Table (N).Kind := Kind;
   end Set_Nkind;


   procedure Set_Location (N : Iir; Location: Location_Type) is
   begin
      Nodet.Table (N).Location := Location;
   end Set_Location;

   function Get_Location (N: Iir) return Location_Type is
   begin
      return Nodet.Table (N).Location;
   end Get_Location;


   procedure Set_Field0 (N : Node_Type; V : Node_Type) is
   begin
      Nodet.Table (N).Field0 := V;
   end Set_Field0;

   function Get_Field0 (N : Node_Type) return Node_Type is
   begin
      return Nodet.Table (N).Field0;
   end Get_Field0;


   function Get_Field1 (N : Node_Type) return Node_Type is
   begin
      return Nodet.Table (N).Field1;
   end Get_Field1;

   procedure Set_Field1 (N : Node_Type; V : Node_Type) is
   begin
      Nodet.Table (N).Field1 := V;
   end Set_Field1;

   function Get_Field2 (N : Node_Type) return Node_Type is
   begin
      return Nodet.Table (N).Field2;
   end Get_Field2;

   procedure Set_Field2 (N : Node_Type; V : Node_Type) is
   begin
      Nodet.Table (N).Field2 := V;
   end Set_Field2;

   function Get_Field3 (N : Node_Type) return Node_Type is
   begin
      return Nodet.Table (N).Field3;
   end Get_Field3;

   procedure Set_Field3 (N : Node_Type; V : Node_Type) is
   begin
      Nodet.Table (N).Field3 := V;
   end Set_Field3;

   function Get_Field4 (N : Node_Type) return Node_Type is
   begin
      return Nodet.Table (N).Field4;
   end Get_Field4;

   procedure Set_Field4 (N : Node_Type; V : Node_Type) is
   begin
      Nodet.Table (N).Field4 := V;
   end Set_Field4;

   function Get_Field5 (N : Node_Type) return Node_Type is
   begin
      return Nodet.Table (N).Field5;
   end Get_Field5;

   procedure Set_Field5 (N : Node_Type; V : Node_Type) is
   begin
      Nodet.Table (N).Field5 := V;
   end Set_Field5;

   function Get_Field6 (N: Node_Type) return Node_Type is
   begin
      return Node_Type (Nodet.Table (N + 1).Location);
   end Get_Field6;

   procedure Set_Field6 (N: Node_Type; Val: Node_Type) is
   begin
      Nodet.Table (N + 1).Location := Location_Type (Val);
   end Set_Field6;

   function Get_Field7 (N: Node_Type) return Node_Type is
   begin
      return Nodet.Table (N + 1).Field0;
   end Get_Field7;

   procedure Set_Field7 (N: Node_Type; Val: Node_Type) is
   begin
      Nodet.Table (N + 1).Field0 := Val;
   end Set_Field7;

   function Get_Field8 (N: Node_Type) return Node_Type is
   begin
      return Nodet.Table (N + 1).Field1;
   end Get_Field8;

   procedure Set_Field8 (N: Node_Type; Val: Node_Type) is
   begin
      Nodet.Table (N + 1).Field1 := Val;
   end Set_Field8;

   function Get_Field9 (N: Node_Type) return Node_Type is
   begin
      return Nodet.Table (N + 1).Field2;
   end Get_Field9;

   procedure Set_Field9 (N: Node_Type; Val: Node_Type) is
   begin
      Nodet.Table (N + 1).Field2 := Val;
   end Set_Field9;

   function Get_Field10 (N: Node_Type) return Node_Type is
   begin
      return Nodet.Table (N + 1).Field3;
   end Get_Field10;

   procedure Set_Field10 (N: Node_Type; Val: Node_Type) is
   begin
      Nodet.Table (N + 1).Field3 := Val;
   end Set_Field10;

   function Get_Field11 (N: Node_Type) return Node_Type is
   begin
      return Nodet.Table (N + 1).Field4;
   end Get_Field11;

   procedure Set_Field11 (N: Node_Type; Val: Node_Type) is
   begin
      Nodet.Table (N + 1).Field4 := Val;
   end Set_Field11;

   function Get_Field12 (N: Node_Type) return Node_Type is
   begin
      return Nodet.Table (N + 1).Field5;
   end Get_Field12;

   procedure Set_Field12 (N: Node_Type; Val: Node_Type) is
   begin
      Nodet.Table (N + 1).Field5 := Val;
   end Set_Field12;


   function Get_Flag1 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag1;
   end Get_Flag1;

   procedure Set_Flag1 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag1 := V;
   end Set_Flag1;

   function Get_Flag2 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag2;
   end Get_Flag2;

   procedure Set_Flag2 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag2 := V;
   end Set_Flag2;

   function Get_Flag3 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag3;
   end Get_Flag3;

   procedure Set_Flag3 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag3 := V;
   end Set_Flag3;

   function Get_Flag4 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag4;
   end Get_Flag4;

   procedure Set_Flag4 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag4 := V;
   end Set_Flag4;

   function Get_Flag5 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag5;
   end Get_Flag5;

   procedure Set_Flag5 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag5 := V;
   end Set_Flag5;

   function Get_Flag6 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag6;
   end Get_Flag6;

   procedure Set_Flag6 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag6 := V;
   end Set_Flag6;

   function Get_Flag7 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag7;
   end Get_Flag7;

   procedure Set_Flag7 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag7 := V;
   end Set_Flag7;

   function Get_Flag8 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag8;
   end Get_Flag8;

   procedure Set_Flag8 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag8 := V;
   end Set_Flag8;

   function Get_Flag9 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag9;
   end Get_Flag9;

   procedure Set_Flag9 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag9 := V;
   end Set_Flag9;

   function Get_Flag10 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag10;
   end Get_Flag10;

   procedure Set_Flag10 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag10 := V;
   end Set_Flag10;

   function Get_Flag11 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag11;
   end Get_Flag11;

   procedure Set_Flag11 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag11 := V;
   end Set_Flag11;

   function Get_Flag12 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag12;
   end Get_Flag12;

   procedure Set_Flag12 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag12 := V;
   end Set_Flag12;

   function Get_Flag13 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag13;
   end Get_Flag13;

   procedure Set_Flag13 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag13 := V;
   end Set_Flag13;

   function Get_Flag14 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag14;
   end Get_Flag14;

   procedure Set_Flag14 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag14 := V;
   end Set_Flag14;

   function Get_Flag15 (N : Node_Type) return Boolean is
   begin
      return Nodet.Table (N).Flag15;
   end Get_Flag15;

   procedure Set_Flag15 (N : Node_Type; V : Boolean) is
   begin
      Nodet.Table (N).Flag15 := V;
   end Set_Flag15;


   function Get_State1 (N : Node_Type) return Bit2_Type is
   begin
      return Nodet.Table (N).State1;
   end Get_State1;

   procedure Set_State1 (N : Node_Type; V : Bit2_Type) is
   begin
      Nodet.Table (N).State1 := V;
   end Set_State1;

   function Get_State2 (N : Node_Type) return Bit2_Type is
   begin
      return Nodet.Table (N).State2;
   end Get_State2;

   procedure Set_State2 (N : Node_Type; V : Bit2_Type) is
   begin
      Nodet.Table (N).State2 := V;
   end Set_State2;

   function Get_State3 (N : Node_Type) return Bit2_Type is
   begin
      return Nodet.Table (N + 1).State1;
   end Get_State3;

   procedure Set_State3 (N : Node_Type; V : Bit2_Type) is
   begin
      Nodet.Table (N + 1).State1 := V;
   end Set_State3;

   procedure Initialize is
   begin
      Free_Chain := Null_Node;
      Nodet.Init;
   end Initialize;

   procedure Finalize is
   begin
      Nodet.Free;
   end Finalize;

   function Is_Null (Node : Iir) return Boolean is
   begin
      return Node = Null_Iir;
   end Is_Null;

   function Is_Null_List (Node : Iir_List) return Boolean is
   begin
      return Node = Null_Iir_List;
   end Is_Null_List;

   function Is_Valid (Node : Iir) return Boolean is
   begin
      return Node /= Null_Iir;
   end Is_Valid;

   ---------------------------------------------------
   -- General subprograms that operate on every iir --
   ---------------------------------------------------

   function Get_Format (Kind : Iir_Kind) return Format_Type;

   function Create_Iir (Kind : Iir_Kind) return Iir
   is
      Res : Iir;
      Format : Format_Type;
   begin
      Format := Get_Format (Kind);
      Res := Create_Node (Format);
      Set_Nkind (Res, Iir_Kind'Pos (Kind));
      return Res;
   end Create_Iir;

   --  Statistics.
   procedure Disp_Stats
   is
      type Num_Array is array (Iir_Kind) of Natural;
      Num : Num_Array := (others => 0);
      type Format_Array is array (Format_Type) of Natural;
      Formats : Format_Array := (others => 0);
      Kind : Iir_Kind;
      I : Iir;
      Last_I : Iir;
      Format : Format_Type;
   begin
      I := Error_Node + 1;
      Last_I := Get_Last_Node;
      while I < Last_I loop
         Kind := Get_Kind (I);
         Num (Kind) := Num (Kind) + 1;
         Format := Get_Format (Kind);
         Formats (Format) := Formats (Format) + 1;
         I := Next_Node (I);
      end loop;

      Log_Line ("Stats per iir_kind:");
      for J in Iir_Kind loop
         if Num (J) /= 0 then
            Log_Line (' ' & Iir_Kind'Image (J) & ':'
                        & Natural'Image (Num (J)));
         end if;
      end loop;
      Log_Line ("Stats per formats:");
      for J in Format_Type loop
         Log_Line (' ' & Format_Type'Image (J) & ':'
                     & Natural'Image (Formats (J)));
      end loop;
   end Disp_Stats;

   function Kind_In (K : Iir_Kind; K1, K2 : Iir_Kind) return Boolean is
   begin
      return K = K1 or K = K2;
   end Kind_In;

   function Iir_Predefined_Shortcut_P (Func : Iir_Predefined_Functions)
     return Boolean is
   begin
      case Func is
         when Iir_Predefined_Bit_And
           | Iir_Predefined_Bit_Or
           | Iir_Predefined_Bit_Nand
           | Iir_Predefined_Bit_Nor
           | Iir_Predefined_Boolean_And
           | Iir_Predefined_Boolean_Or
           | Iir_Predefined_Boolean_Nand
           | Iir_Predefined_Boolean_Nor =>
            return True;
         when others =>
            return False;
      end case;
   end Iir_Predefined_Shortcut_P;

   function Create_Iir_Error return Iir
   is
      Res : Iir;
   begin
      Res := Create_Node (Format_Short);
      Set_Nkind (Res, Iir_Kind'Pos (Iir_Kind_Error));
      return Res;
   end Create_Iir_Error;

   procedure Location_Copy (Target : Iir; Src : Iir) is
   begin
      Set_Location (Target, Get_Location (Src));
   end Location_Copy;

   -- Get kind
   function Get_Kind (N : Iir) return Iir_Kind
   is
      --  Speed up: avoid to check that nkind is in the bounds of Iir_Kind.
      pragma Suppress (Range_Check);
   begin
      pragma Assert (N /= Null_Iir);
      return Iir_Kind'Val (Get_Nkind (N));
   end Get_Kind;

   function Time_Stamp_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => Time_Stamp_Id, Target => Iir);

   function Iir_To_Time_Stamp_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Time_Stamp_Id);

   function File_Checksum_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => File_Checksum_Id, Target => Iir);

   function Iir_To_File_Checksum_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => File_Checksum_Id);

   function Iir_To_Iir_List is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_List);
   function Iir_List_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_List, Target => Iir);

   function Iir_To_Iir_Flist is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_Flist);
   function Iir_Flist_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_Flist, Target => Iir);

   function Iir_To_Token_Type (N : Iir) return Token_Type is
   begin
      return Token_Type'Val (N);
   end Iir_To_Token_Type;

   function Token_Type_To_Iir (T : Token_Type) return Iir is
   begin
      return Token_Type'Pos (T);
   end Token_Type_To_Iir;

--     function Iir_To_Iir_Index32 (N : Iir) return Iir_Index32 is
--     begin
--        return Iir_Index32 (N);
--     end Iir_To_Iir_Index32;

--     function Iir_Index32_To_Iir (V : Iir_Index32) return Iir is
--     begin
--        return Iir_Index32'Pos (V);
--     end Iir_Index32_To_Iir;

   function Iir_To_Name_Id (N : Iir) return Name_Id is
   begin
      return Iir'Pos (N);
   end Iir_To_Name_Id;
   pragma Inline (Iir_To_Name_Id);

   function Name_Id_To_Iir (V : Name_Id) return Iir is
   begin
      return Name_Id'Pos (V);
   end Name_Id_To_Iir;

   function Iir_To_Iir_Int32 is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_Int32);

   function Iir_Int32_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_Int32, Target => Iir);

   function Iir_To_Source_Ptr (N : Iir) return Source_Ptr is
   begin
      return Source_Ptr (N);
   end Iir_To_Source_Ptr;

   function Source_Ptr_To_Iir (P : Source_Ptr) return Iir is
   begin
      return Iir (P);
   end Source_Ptr_To_Iir;

   function Iir_To_Source_File_Entry is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Source_File_Entry);
   function Source_File_Entry_To_Iir is new Ada.Unchecked_Conversion
     (Source => Source_File_Entry, Target => Iir);

   function Boolean_To_Iir_Delay_Mechanism is new Ada.Unchecked_Conversion
     (Source => Boolean, Target => Iir_Delay_Mechanism);
   function Iir_Delay_Mechanism_To_Boolean is new Ada.Unchecked_Conversion
     (Source => Iir_Delay_Mechanism, Target => Boolean);

   function Boolean_To_Iir_Force_Mode is new Ada.Unchecked_Conversion
     (Source => Boolean, Target => Iir_Force_Mode);
   function Iir_Force_Mode_To_Boolean is new Ada.Unchecked_Conversion
     (Source => Iir_Force_Mode, Target => Boolean);

   function Boolean_To_Iir_Signal_Kind is new Ada.Unchecked_Conversion
     (Source => Boolean, Target => Iir_Signal_Kind);
   function Iir_Signal_Kind_To_Boolean is new Ada.Unchecked_Conversion
     (Source => Iir_Signal_Kind, Target => Boolean);

   function Boolean_To_Direction_Type is new Ada.Unchecked_Conversion
     (Source => Boolean, Target => Direction_Type);
   function Direction_Type_To_Boolean is new Ada.Unchecked_Conversion
     (Source => Direction_Type, Target => Boolean);

   function Iir_To_String8_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => String8_Id);
   function String8_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => String8_Id, Target => Iir);

   function Iir_To_Int32 is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Int32);
   function Int32_To_Iir is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Iir);

   function Iir_To_PSL_Node is new Ada.Unchecked_Conversion
     (Source => Iir, Target => PSL_Node);

   function PSL_Node_To_Iir is new Ada.Unchecked_Conversion
     (Source => PSL_Node, Target => Iir);

   function Iir_To_PSL_NFA is new Ada.Unchecked_Conversion
     (Source => Iir, Target => PSL_NFA);

   function PSL_NFA_To_Iir is new Ada.Unchecked_Conversion
     (Source => PSL_NFA, Target => Iir);

   --  Subprograms
   function Get_Format (Kind : Iir_Kind) return Format_Type is
   begin
      case Kind is
         when Iir_Kind_Unused
           | Iir_Kind_Error
           | Iir_Kind_Library_Clause
           | Iir_Kind_Use_Clause
           | Iir_Kind_Context_Reference
           | Iir_Kind_PSL_Inherit_Spec
           | Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Null_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Overflow_Literal
           | Iir_Kind_Unaffected_Waveform
           | Iir_Kind_Waveform_Element
           | Iir_Kind_Conditional_Waveform
           | Iir_Kind_Conditional_Expression
           | Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Entity_Aspect_Configuration
           | Iir_Kind_Entity_Aspect_Open
           | Iir_Kind_Psl_Hierarchical_Name
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Entity_Class
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Aggregate_Info
           | Iir_Kind_Procedure_Call
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Array_Element_Resolution
           | Iir_Kind_Record_Resolution
           | Iir_Kind_Record_Element_Resolution
           | Iir_Kind_Break_Element
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Step_Limit_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Interface_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_File_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Range_Expression
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Wildcard_Type_Definition
           | Iir_Kind_Overload_List
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Boolean_Parameter
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Quantity_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Attribute_Implicit_Declaration
           | Iir_Kind_Suspend_State_Declaration
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Implicit_Condition_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator
           | Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Match_Equality_Operator
           | Iir_Kind_Match_Inequality_Operator
           | Iir_Kind_Match_Less_Than_Operator
           | Iir_Kind_Match_Less_Than_Or_Equal_Operator
           | Iir_Kind_Match_Greater_Than_Operator
           | Iir_Kind_Match_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call
           | Iir_Kind_Aggregate
           | Iir_Kind_Parenthesis_Expression
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Psl_Prev
           | Iir_Kind_Psl_Stable
           | Iir_Kind_Psl_Rose
           | Iir_Kind_Psl_Fell
           | Iir_Kind_Psl_Onehot
           | Iir_Kind_Psl_Onehot0
           | Iir_Kind_Psl_Expression
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_If_Generate_Else_Clause
           | Iir_Kind_Simultaneous_Null_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Suspend_State_Statement
           | Iir_Kind_Elsif
           | Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Reference_Name
           | Iir_Kind_External_Constant_Name
           | Iir_Kind_External_Signal_Name
           | Iir_Kind_External_Variable_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Package_Pathname
           | Iir_Kind_Absolute_Pathname
           | Iir_Kind_Relative_Pathname
           | Iir_Kind_Pathname_Element
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute
           | Iir_Kind_Nature_Reference_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Behavior_Attribute
           | Iir_Kind_Structure_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return Format_Short;
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Block_Header
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Signature
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Foreign_Vector_Type_Definition
           | Iir_Kind_Subtype_Definition
           | Iir_Kind_Scalar_Nature_Definition
           | Iir_Kind_Record_Nature_Definition
           | Iir_Kind_Array_Nature_Definition
           | Iir_Kind_Array_Subnature_Definition
           | Iir_Kind_Foreign_Module
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration
           | Iir_Kind_Vunit_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Package_Header
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Instantiation_Declaration
           | Iir_Kind_Procedure_Instantiation_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Block_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute =>
            return Format_Medium;
      end case;
   end Get_Format;

   function Get_First_Design_Unit (Design : Iir) return Iir is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_First_Design_Unit (Get_Kind (Design)),
                     "no field First_Design_Unit");
      return Get_Field5 (Design);
   end Get_First_Design_Unit;

   procedure Set_First_Design_Unit (Design : Iir; Chain : Iir) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_First_Design_Unit (Get_Kind (Design)),
                     "no field First_Design_Unit");
      Set_Field5 (Design, Chain);
   end Set_First_Design_Unit;

   function Get_Last_Design_Unit (Design : Iir) return Iir is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Last_Design_Unit (Get_Kind (Design)),
                     "no field Last_Design_Unit");
      return Get_Field6 (Design);
   end Get_Last_Design_Unit;

   procedure Set_Last_Design_Unit (Design : Iir; Chain : Iir) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Last_Design_Unit (Get_Kind (Design)),
                     "no field Last_Design_Unit");
      Set_Field6 (Design, Chain);
   end Set_Last_Design_Unit;

   function Get_Library_Declaration (Design : Iir) return Iir is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Library_Declaration (Get_Kind (Design)),
                     "no field Library_Declaration");
      return Get_Field1 (Design);
   end Get_Library_Declaration;

   procedure Set_Library_Declaration (Design : Iir; Library : Iir) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Library_Declaration (Get_Kind (Design)),
                     "no field Library_Declaration");
      Set_Field1 (Design, Library);
   end Set_Library_Declaration;

   function Get_File_Checksum (Design : Iir) return File_Checksum_Id is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_File_Checksum (Get_Kind (Design)),
                     "no field File_Checksum");
      return Iir_To_File_Checksum_Id (Get_Field4 (Design));
   end Get_File_Checksum;

   procedure Set_File_Checksum (Design : Iir; Checksum : File_Checksum_Id) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_File_Checksum (Get_Kind (Design)),
                     "no field File_Checksum");
      Set_Field4 (Design, File_Checksum_Id_To_Iir (Checksum));
   end Set_File_Checksum;

   function Get_Analysis_Time_Stamp (Design : Iir) return Time_Stamp_Id is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Analysis_Time_Stamp (Get_Kind (Design)),
                     "no field Analysis_Time_Stamp");
      return Iir_To_Time_Stamp_Id (Get_Field3 (Design));
   end Get_Analysis_Time_Stamp;

   procedure Set_Analysis_Time_Stamp (Design : Iir; Stamp : Time_Stamp_Id) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Analysis_Time_Stamp (Get_Kind (Design)),
                     "no field Analysis_Time_Stamp");
      Set_Field3 (Design, Time_Stamp_Id_To_Iir (Stamp));
   end Set_Analysis_Time_Stamp;

   function Get_Design_File_Source (Design : Iir) return Source_File_Entry is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Design_File_Source (Get_Kind (Design)),
                     "no field Design_File_Source");
      return Iir_To_Source_File_Entry (Get_Field7 (Design));
   end Get_Design_File_Source;

   procedure Set_Design_File_Source (Design : Iir; Sfe : Source_File_Entry) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Design_File_Source (Get_Kind (Design)),
                     "no field Design_File_Source");
      Set_Field7 (Design, Source_File_Entry_To_Iir (Sfe));
   end Set_Design_File_Source;

   function Get_Library (File : Iir_Design_File) return Iir is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Library (Get_Kind (File)),
                     "no field Library");
      return Get_Field0 (File);
   end Get_Library;

   procedure Set_Library (File : Iir_Design_File; Lib : Iir) is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Library (Get_Kind (File)),
                     "no field Library");
      Set_Field0 (File, Lib);
   end Set_Library;

   function Get_File_Dependence_List (File : Iir_Design_File) return Iir_List
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_File_Dependence_List (Get_Kind (File)),
                     "no field File_Dependence_List");
      return Iir_To_Iir_List (Get_Field1 (File));
   end Get_File_Dependence_List;

   procedure Set_File_Dependence_List (File : Iir_Design_File; Lst : Iir_List)
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_File_Dependence_List (Get_Kind (File)),
                     "no field File_Dependence_List");
      Set_Field1 (File, Iir_List_To_Iir (Lst));
   end Set_File_Dependence_List;

   function Get_Design_File_Filename (File : Iir_Design_File) return Name_Id
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Filename (Get_Kind (File)),
                     "no field Design_File_Filename");
      return Name_Id'Val (Get_Field12 (File));
   end Get_Design_File_Filename;

   procedure Set_Design_File_Filename (File : Iir_Design_File; Name : Name_Id)
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Filename (Get_Kind (File)),
                     "no field Design_File_Filename");
      Set_Field12 (File, Name_Id'Pos (Name));
   end Set_Design_File_Filename;

   function Get_Design_File_Directory (File : Iir_Design_File) return Name_Id
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Directory (Get_Kind (File)),
                     "no field Design_File_Directory");
      return Name_Id'Val (Get_Field11 (File));
   end Get_Design_File_Directory;

   procedure Set_Design_File_Directory (File : Iir_Design_File; Dir : Name_Id)
   is
   begin
      pragma Assert (File /= Null_Iir);
      pragma Assert (Has_Design_File_Directory (Get_Kind (File)),
                     "no field Design_File_Directory");
      Set_Field11 (File, Name_Id'Pos (Dir));
   end Set_Design_File_Directory;

   function Get_Design_File (Unit : Iir_Design_Unit) return Iir is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Design_File (Get_Kind (Unit)),
                     "no field Design_File");
      return Get_Field0 (Unit);
   end Get_Design_File;

   procedure Set_Design_File (Unit : Iir_Design_Unit; File : Iir) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Design_File (Get_Kind (Unit)),
                     "no field Design_File");
      Set_Field0 (Unit, File);
   end Set_Design_File;

   function Get_Design_File_Chain (Library : Iir) return Iir is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Design_File_Chain (Get_Kind (Library)),
                     "no field Design_File_Chain");
      return Get_Field1 (Library);
   end Get_Design_File_Chain;

   procedure Set_Design_File_Chain (Library : Iir; Chain : Iir) is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Design_File_Chain (Get_Kind (Library)),
                     "no field Design_File_Chain");
      Set_Field1 (Library, Chain);
   end Set_Design_File_Chain;

   function Get_Library_Directory (Library : Iir) return Name_Id is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Library_Directory (Get_Kind (Library)),
                     "no field Library_Directory");
      return Name_Id'Val (Get_Field5 (Library));
   end Get_Library_Directory;

   procedure Set_Library_Directory (Library : Iir; Dir : Name_Id) is
   begin
      pragma Assert (Library /= Null_Iir);
      pragma Assert (Has_Library_Directory (Get_Kind (Library)),
                     "no field Library_Directory");
      Set_Field5 (Library, Name_Id'Pos (Dir));
   end Set_Library_Directory;

   function Get_Date (Target : Iir) return Date_Type is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Date (Get_Kind (Target)),
                     "no field Date");
      return Date_Type'Val (Get_Field4 (Target));
   end Get_Date;

   procedure Set_Date (Target : Iir; Date : Date_Type) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Date (Get_Kind (Target)),
                     "no field Date");
      Set_Field4 (Target, Date_Type'Pos (Date));
   end Set_Date;

   function Get_Context_Items (Design_Unit : Iir) return Iir is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Context_Items (Get_Kind (Design_Unit)),
                     "no field Context_Items");
      return Get_Field1 (Design_Unit);
   end Get_Context_Items;

   procedure Set_Context_Items (Design_Unit : Iir; Items_Chain : Iir) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Context_Items (Get_Kind (Design_Unit)),
                     "no field Context_Items");
      Set_Field1 (Design_Unit, Items_Chain);
   end Set_Context_Items;

   function Get_Dependence_List (Unit : Iir) return Iir_List is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Dependence_List (Get_Kind (Unit)),
                     "no field Dependence_List");
      return Iir_To_Iir_List (Get_Field8 (Unit));
   end Get_Dependence_List;

   procedure Set_Dependence_List (Unit : Iir; List : Iir_List) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Dependence_List (Get_Kind (Unit)),
                     "no field Dependence_List");
      Set_Field8 (Unit, Iir_List_To_Iir (List));
   end Set_Dependence_List;

   function Get_Analysis_Checks_List (Unit : Iir) return Iir_List is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Analysis_Checks_List (Get_Kind (Unit)),
                     "no field Analysis_Checks_List");
      return Iir_To_Iir_List (Get_Field9 (Unit));
   end Get_Analysis_Checks_List;

   procedure Set_Analysis_Checks_List (Unit : Iir; List : Iir_List) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Analysis_Checks_List (Get_Kind (Unit)),
                     "no field Analysis_Checks_List");
      Set_Field9 (Unit, Iir_List_To_Iir (List));
   end Set_Analysis_Checks_List;

   function Get_Date_State (Unit : Iir_Design_Unit) return Date_State_Type is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Date_State (Get_Kind (Unit)),
                     "no field Date_State");
      return Date_State_Type'Val (Get_State1 (Unit));
   end Get_Date_State;

   procedure Set_Date_State (Unit : Iir_Design_Unit; State : Date_State_Type)
   is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Date_State (Get_Kind (Unit)),
                     "no field Date_State");
      Set_State1 (Unit, Date_State_Type'Pos (State));
   end Set_Date_State;

   function Get_Guarded_Target_State (Stmt : Iir) return Tri_State_Type is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Guarded_Target_State (Get_Kind (Stmt)),
                     "no field Guarded_Target_State");
      return Tri_State_Type'Val (Get_State1 (Stmt));
   end Get_Guarded_Target_State;

   procedure Set_Guarded_Target_State (Stmt : Iir; State : Tri_State_Type) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Guarded_Target_State (Get_Kind (Stmt)),
                     "no field Guarded_Target_State");
      Set_State1 (Stmt, Tri_State_Type'Pos (State));
   end Set_Guarded_Target_State;

   function Get_Library_Unit (Design_Unit : Iir_Design_Unit) return Iir is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Library_Unit (Get_Kind (Design_Unit)),
                     "no field Library_Unit");
      return Get_Field7 (Design_Unit);
   end Get_Library_Unit;

   procedure Set_Library_Unit (Design_Unit : Iir_Design_Unit; Lib_Unit : Iir)
   is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Library_Unit (Get_Kind (Design_Unit)),
                     "no field Library_Unit");
      Set_Field7 (Design_Unit, Lib_Unit);
   end Set_Library_Unit;

   function Get_Hash_Chain (Design_Unit : Iir_Design_Unit) return Iir is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Hash_Chain (Get_Kind (Design_Unit)),
                     "no field Hash_Chain");
      return Get_Field5 (Design_Unit);
   end Get_Hash_Chain;

   procedure Set_Hash_Chain (Design_Unit : Iir_Design_Unit; Chain : Iir) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Hash_Chain (Get_Kind (Design_Unit)),
                     "no field Hash_Chain");
      Set_Field5 (Design_Unit, Chain);
   end Set_Hash_Chain;

   function Get_Design_Unit_Source_Pos (Design_Unit : Iir) return Source_Ptr
   is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Pos (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Pos");
      return Iir_To_Source_Ptr (Get_Field10 (Design_Unit));
   end Get_Design_Unit_Source_Pos;

   procedure Set_Design_Unit_Source_Pos (Design_Unit : Iir; Pos : Source_Ptr)
   is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Pos (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Pos");
      Set_Field10 (Design_Unit, Source_Ptr_To_Iir (Pos));
   end Set_Design_Unit_Source_Pos;

   function Get_Design_Unit_Source_Line (Design_Unit : Iir) return Int32 is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Line (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Line");
      return Iir_To_Int32 (Get_Field11 (Design_Unit));
   end Get_Design_Unit_Source_Line;

   procedure Set_Design_Unit_Source_Line (Design_Unit : Iir; Line : Int32) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Line (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Line");
      Set_Field11 (Design_Unit, Int32_To_Iir (Line));
   end Set_Design_Unit_Source_Line;

   function Get_Design_Unit_Source_Col (Design_Unit : Iir) return Int32 is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Col (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Col");
      return Iir_To_Int32 (Get_Field12 (Design_Unit));
   end Get_Design_Unit_Source_Col;

   procedure Set_Design_Unit_Source_Col (Design_Unit : Iir; Line : Int32) is
   begin
      pragma Assert (Design_Unit /= Null_Iir);
      pragma Assert (Has_Design_Unit_Source_Col (Get_Kind (Design_Unit)),
                     "no field Design_Unit_Source_Col");
      Set_Field12 (Design_Unit, Int32_To_Iir (Line));
   end Set_Design_Unit_Source_Col;

   type Int64_Conv is record
      Field4: Iir;
      Field5: Iir;
   end record;
   pragma Pack (Int64_Conv);
   pragma Assert (Int64_Conv'Size = Int64'Size);

   function Get_Value (Lit : Iir) return Int64
   is
      function To_Int64 is new Ada.Unchecked_Conversion
         (Int64_Conv, Int64);
      Conv : Int64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Value (Get_Kind (Lit)),
                     "no field Value");
      Conv.Field4 := Get_Field4 (Lit);
      Conv.Field5 := Get_Field5 (Lit);
      return To_Int64 (Conv);
   end Get_Value;

   procedure Set_Value (Lit : Iir; Val : Int64)
   is
      function To_Int64_Conv is new Ada.Unchecked_Conversion
         (Int64, Int64_Conv);
      Conv : Int64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Value (Get_Kind (Lit)),
                     "no field Value");
      Conv := To_Int64_Conv (Val);
      Set_Field4 (Lit, Conv.Field4);
      Set_Field5 (Lit, Conv.Field5);
   end Set_Value;

   function Get_Enum_Pos (Lit : Iir) return Iir_Int32 is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Enum_Pos (Get_Kind (Lit)),
                     "no field Enum_Pos");
      return Iir_Int32'Val (Get_Field5 (Lit));
   end Get_Enum_Pos;

   procedure Set_Enum_Pos (Lit : Iir; Val : Iir_Int32) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Enum_Pos (Get_Kind (Lit)),
                     "no field Enum_Pos");
      Set_Field5 (Lit, Iir_Int32'Pos (Val));
   end Set_Enum_Pos;

   function Get_Physical_Literal (Unit : Iir) return Iir is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Physical_Literal (Get_Kind (Unit)),
                     "no field Physical_Literal");
      return Get_Field4 (Unit);
   end Get_Physical_Literal;

   procedure Set_Physical_Literal (Unit : Iir; Lit : Iir) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Physical_Literal (Get_Kind (Unit)),
                     "no field Physical_Literal");
      Set_Field4 (Unit, Lit);
   end Set_Physical_Literal;

   type Fp64_Conv is record
      Field4: Iir;
      Field5: Iir;
   end record;
   pragma Pack (Fp64_Conv);
   pragma Assert (Fp64_Conv'Size = Fp64'Size);

   function Get_Fp_Value (Lit : Iir) return Fp64
   is
      function To_Fp64 is new Ada.Unchecked_Conversion
         (Fp64_Conv, Fp64);
      Conv : Fp64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Fp_Value (Get_Kind (Lit)),
                     "no field Fp_Value");
      Conv.Field4 := Get_Field4 (Lit);
      Conv.Field5 := Get_Field5 (Lit);
      return To_Fp64 (Conv);
   end Get_Fp_Value;

   procedure Set_Fp_Value (Lit : Iir; Val : Fp64)
   is
      function To_Fp64_Conv is new Ada.Unchecked_Conversion
         (Fp64, Fp64_Conv);
      Conv : Fp64_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Fp_Value (Get_Kind (Lit)),
                     "no field Fp_Value");
      Conv := To_Fp64_Conv (Val);
      Set_Field4 (Lit, Conv.Field4);
      Set_Field5 (Lit, Conv.Field5);
   end Set_Fp_Value;

   function Get_Simple_Aggregate_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Aggregate_List (Get_Kind (Target)),
                     "no field Simple_Aggregate_List");
      return Iir_To_Iir_Flist (Get_Field4 (Target));
   end Get_Simple_Aggregate_List;

   procedure Set_Simple_Aggregate_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Aggregate_List (Get_Kind (Target)),
                     "no field Simple_Aggregate_List");
      Set_Field4 (Target, Iir_Flist_To_Iir (List));
   end Set_Simple_Aggregate_List;

   function Get_String8_Id (Lit : Iir) return String8_Id is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String8_Id (Get_Kind (Lit)),
                     "no field String8_Id");
      return Iir_To_String8_Id (Get_Field5 (Lit));
   end Get_String8_Id;

   procedure Set_String8_Id (Lit : Iir; Id : String8_Id) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String8_Id (Get_Kind (Lit)),
                     "no field String8_Id");
      Set_Field5 (Lit, String8_Id_To_Iir (Id));
   end Set_String8_Id;

   function Get_String_Length (Lit : Iir) return Int32 is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String_Length (Get_Kind (Lit)),
                     "no field String_Length");
      return Iir_To_Int32 (Get_Field4 (Lit));
   end Get_String_Length;

   procedure Set_String_Length (Lit : Iir; Len : Int32) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_String_Length (Get_Kind (Lit)),
                     "no field String_Length");
      Set_Field4 (Lit, Int32_To_Iir (Len));
   end Set_String_Length;

   type Number_Base_Type_Conv is record
      Flag12: Boolean;
      Flag13: Boolean;
      Flag14: Boolean;
   end record;
   pragma Pack (Number_Base_Type_Conv);
   pragma Assert (Number_Base_Type_Conv'Size = Number_Base_Type'Size);

   function Get_Bit_String_Base (Lit : Iir) return Number_Base_Type
   is
      function To_Number_Base_Type is new Ada.Unchecked_Conversion
         (Number_Base_Type_Conv, Number_Base_Type);
      Conv : Number_Base_Type_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Bit_String_Base (Get_Kind (Lit)),
                     "no field Bit_String_Base");
      Conv.Flag12 := Get_Flag12 (Lit);
      Conv.Flag13 := Get_Flag13 (Lit);
      Conv.Flag14 := Get_Flag14 (Lit);
      return To_Number_Base_Type (Conv);
   end Get_Bit_String_Base;

   procedure Set_Bit_String_Base (Lit : Iir; Base : Number_Base_Type)
   is
      function To_Number_Base_Type_Conv is new Ada.Unchecked_Conversion
         (Number_Base_Type, Number_Base_Type_Conv);
      Conv : Number_Base_Type_Conv;
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Bit_String_Base (Get_Kind (Lit)),
                     "no field Bit_String_Base");
      Conv := To_Number_Base_Type_Conv (Base);
      Set_Flag12 (Lit, Conv.Flag12);
      Set_Flag13 (Lit, Conv.Flag13);
      Set_Flag14 (Lit, Conv.Flag14);
   end Set_Bit_String_Base;

   function Get_Has_Signed (Lit : Iir) return Boolean is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Signed (Get_Kind (Lit)),
                     "no field Has_Signed");
      return Get_Flag1 (Lit);
   end Get_Has_Signed;

   procedure Set_Has_Signed (Lit : Iir; Flag : Boolean) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Signed (Get_Kind (Lit)),
                     "no field Has_Signed");
      Set_Flag1 (Lit, Flag);
   end Set_Has_Signed;

   function Get_Has_Sign (Lit : Iir) return Boolean is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Sign (Get_Kind (Lit)),
                     "no field Has_Sign");
      return Get_Flag2 (Lit);
   end Get_Has_Sign;

   procedure Set_Has_Sign (Lit : Iir; Flag : Boolean) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Sign (Get_Kind (Lit)),
                     "no field Has_Sign");
      Set_Flag2 (Lit, Flag);
   end Set_Has_Sign;

   function Get_Has_Length (Lit : Iir) return Boolean is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Length (Get_Kind (Lit)),
                     "no field Has_Length");
      return Get_Flag3 (Lit);
   end Get_Has_Length;

   procedure Set_Has_Length (Lit : Iir; Flag : Boolean) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Has_Length (Get_Kind (Lit)),
                     "no field Has_Length");
      Set_Flag3 (Lit, Flag);
   end Set_Has_Length;

   function Get_Literal_Length (Lit : Iir) return Int32 is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Length (Get_Kind (Lit)),
                     "no field Literal_Length");
      return Iir_To_Int32 (Get_Field0 (Lit));
   end Get_Literal_Length;

   procedure Set_Literal_Length (Lit : Iir; Len : Int32) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Length (Get_Kind (Lit)),
                     "no field Literal_Length");
      Set_Field0 (Lit, Int32_To_Iir (Len));
   end Set_Literal_Length;

   function Get_Literal_Origin (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Origin (Get_Kind (Lit)),
                     "no field Literal_Origin");
      return Get_Field2 (Lit);
   end Get_Literal_Origin;

   procedure Set_Literal_Origin (Lit : Iir; Orig : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Origin (Get_Kind (Lit)),
                     "no field Literal_Origin");
      Set_Field2 (Lit, Orig);
   end Set_Literal_Origin;

   function Get_Range_Origin (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Range_Origin (Get_Kind (Lit)),
                     "no field Range_Origin");
      return Get_Field0 (Lit);
   end Get_Range_Origin;

   procedure Set_Range_Origin (Lit : Iir; Orig : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Range_Origin (Get_Kind (Lit)),
                     "no field Range_Origin");
      Set_Field0 (Lit, Orig);
   end Set_Range_Origin;

   function Get_Literal_Subtype (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Subtype (Get_Kind (Lit)),
                     "no field Literal_Subtype");
      return Get_Field3 (Lit);
   end Get_Literal_Subtype;

   procedure Set_Literal_Subtype (Lit : Iir; Atype : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Literal_Subtype (Get_Kind (Lit)),
                     "no field Literal_Subtype");
      Set_Field3 (Lit, Atype);
   end Set_Literal_Subtype;

   function Get_Allocator_Subtype (Lit : Iir) return Iir is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Allocator_Subtype (Get_Kind (Lit)),
                     "no field Allocator_Subtype");
      return Get_Field3 (Lit);
   end Get_Allocator_Subtype;

   procedure Set_Allocator_Subtype (Lit : Iir; Atype : Iir) is
   begin
      pragma Assert (Lit /= Null_Iir);
      pragma Assert (Has_Allocator_Subtype (Get_Kind (Lit)),
                     "no field Allocator_Subtype");
      Set_Field3 (Lit, Atype);
   end Set_Allocator_Subtype;

   function Get_Entity_Class (Target : Iir) return Token_Type is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class (Get_Kind (Target)),
                     "no field Entity_Class");
      return Iir_To_Token_Type (Get_Field3 (Target));
   end Get_Entity_Class;

   procedure Set_Entity_Class (Target : Iir; Kind : Token_Type) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class (Get_Kind (Target)),
                     "no field Entity_Class");
      Set_Field3 (Target, Token_Type_To_Iir (Kind));
   end Set_Entity_Class;

   function Get_Entity_Name_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Name_List (Get_Kind (Target)),
                     "no field Entity_Name_List");
      return Iir_To_Iir_Flist (Get_Field8 (Target));
   end Get_Entity_Name_List;

   procedure Set_Entity_Name_List (Target : Iir; Names : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Name_List (Get_Kind (Target)),
                     "no field Entity_Name_List");
      Set_Field8 (Target, Iir_Flist_To_Iir (Names));
   end Set_Entity_Name_List;

   function Get_Attribute_Designator (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Designator (Get_Kind (Target)),
                     "no field Attribute_Designator");
      return Get_Field6 (Target);
   end Get_Attribute_Designator;

   procedure Set_Attribute_Designator (Target : Iir; Designator : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Designator (Get_Kind (Target)),
                     "no field Attribute_Designator");
      Set_Field6 (Target, Designator);
   end Set_Attribute_Designator;

   function Get_Attribute_Specification_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Specification_Chain (Get_Kind (Target)),
                     "no field Attribute_Specification_Chain");
      return Get_Field7 (Target);
   end Get_Attribute_Specification_Chain;

   procedure Set_Attribute_Specification_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Specification_Chain (Get_Kind (Target)),
                     "no field Attribute_Specification_Chain");
      Set_Field7 (Target, Chain);
   end Set_Attribute_Specification_Chain;

   function Get_Attribute_Specification (Val : Iir) return Iir is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Attribute_Specification (Get_Kind (Val)),
                     "no field Attribute_Specification");
      return Get_Field4 (Val);
   end Get_Attribute_Specification;

   procedure Set_Attribute_Specification (Val : Iir; Attr : Iir) is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Attribute_Specification (Get_Kind (Val)),
                     "no field Attribute_Specification");
      Set_Field4 (Val, Attr);
   end Set_Attribute_Specification;

   function Get_Static_Attribute_Flag (Attr : Iir) return Boolean is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Static_Attribute_Flag (Get_Kind (Attr)),
                     "no field Static_Attribute_Flag");
      return Get_Flag2 (Attr);
   end Get_Static_Attribute_Flag;

   procedure Set_Static_Attribute_Flag (Attr : Iir; Flag : Boolean) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Static_Attribute_Flag (Get_Kind (Attr)),
                     "no field Static_Attribute_Flag");
      Set_Flag2 (Attr, Flag);
   end Set_Static_Attribute_Flag;

   function Get_Signal_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_List (Get_Kind (Target)),
                     "no field Signal_List");
      return Iir_To_Iir_Flist (Get_Field3 (Target));
   end Get_Signal_List;

   procedure Set_Signal_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_List (Get_Kind (Target)),
                     "no field Signal_List");
      Set_Field3 (Target, Iir_Flist_To_Iir (List));
   end Set_Signal_List;

   function Get_Quantity_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Quantity_List (Get_Kind (Target)),
                     "no field Quantity_List");
      return Iir_To_Iir_Flist (Get_Field3 (Target));
   end Get_Quantity_List;

   procedure Set_Quantity_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Quantity_List (Get_Kind (Target)),
                     "no field Quantity_List");
      Set_Field3 (Target, Iir_Flist_To_Iir (List));
   end Set_Quantity_List;

   function Get_Designated_Entity (Val : Iir_Attribute_Value) return Iir is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Designated_Entity (Get_Kind (Val)),
                     "no field Designated_Entity");
      return Get_Field3 (Val);
   end Get_Designated_Entity;

   procedure Set_Designated_Entity (Val : Iir_Attribute_Value; Entity : Iir)
   is
   begin
      pragma Assert (Val /= Null_Iir);
      pragma Assert (Has_Designated_Entity (Get_Kind (Val)),
                     "no field Designated_Entity");
      Set_Field3 (Val, Entity);
   end Set_Designated_Entity;

   function Get_Formal (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal (Get_Kind (Target)),
                     "no field Formal");
      return Get_Field1 (Target);
   end Get_Formal;

   procedure Set_Formal (Target : Iir; Formal : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal (Get_Kind (Target)),
                     "no field Formal");
      Set_Field1 (Target, Formal);
   end Set_Formal;

   function Get_Actual (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual (Get_Kind (Target)),
                     "no field Actual");
      return Get_Field3 (Target);
   end Get_Actual;

   procedure Set_Actual (Target : Iir; Actual : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual (Get_Kind (Target)),
                     "no field Actual");
      Set_Field3 (Target, Actual);
   end Set_Actual;

   function Get_Open_Actual (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Open_Actual (Get_Kind (Target)),
                     "no field Open_Actual");
      return Get_Field3 (Target);
   end Get_Open_Actual;

   procedure Set_Open_Actual (Target : Iir; Actual : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Open_Actual (Get_Kind (Target)),
                     "no field Open_Actual");
      Set_Field3 (Target, Actual);
   end Set_Open_Actual;

   function Get_Actual_Conversion (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Conversion (Get_Kind (Target)),
                     "no field Actual_Conversion");
      return Get_Field4 (Target);
   end Get_Actual_Conversion;

   procedure Set_Actual_Conversion (Target : Iir; Conv : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Conversion (Get_Kind (Target)),
                     "no field Actual_Conversion");
      Set_Field4 (Target, Conv);
   end Set_Actual_Conversion;

   function Get_Formal_Conversion (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal_Conversion (Get_Kind (Target)),
                     "no field Formal_Conversion");
      return Get_Field5 (Target);
   end Get_Formal_Conversion;

   procedure Set_Formal_Conversion (Target : Iir; Conv : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Formal_Conversion (Get_Kind (Target)),
                     "no field Formal_Conversion");
      Set_Field5 (Target, Conv);
   end Set_Formal_Conversion;

   function Get_Whole_Association_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Whole_Association_Flag (Get_Kind (Target)),
                     "no field Whole_Association_Flag");
      return Get_Flag1 (Target);
   end Get_Whole_Association_Flag;

   procedure Set_Whole_Association_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Whole_Association_Flag (Get_Kind (Target)),
                     "no field Whole_Association_Flag");
      Set_Flag1 (Target, Flag);
   end Set_Whole_Association_Flag;

   function Get_Collapse_Signal_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Collapse_Signal_Flag (Get_Kind (Target)),
                     "no field Collapse_Signal_Flag");
      return Get_Flag2 (Target);
   end Get_Collapse_Signal_Flag;

   procedure Set_Collapse_Signal_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Collapse_Signal_Flag (Get_Kind (Target)),
                     "no field Collapse_Signal_Flag");
      Set_Flag2 (Target, Flag);
   end Set_Collapse_Signal_Flag;

   function Get_Artificial_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Artificial_Flag (Get_Kind (Target)),
                     "no field Artificial_Flag");
      return Get_Flag3 (Target);
   end Get_Artificial_Flag;

   procedure Set_Artificial_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Artificial_Flag (Get_Kind (Target)),
                     "no field Artificial_Flag");
      Set_Flag3 (Target, Flag);
   end Set_Artificial_Flag;

   function Get_Open_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Open_Flag (Get_Kind (Target)),
                     "no field Open_Flag");
      return Get_Flag7 (Target);
   end Get_Open_Flag;

   procedure Set_Open_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Open_Flag (Get_Kind (Target)),
                     "no field Open_Flag");
      Set_Flag7 (Target, Flag);
   end Set_Open_Flag;

   function Get_After_Drivers_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_After_Drivers_Flag (Get_Kind (Target)),
                     "no field After_Drivers_Flag");
      return Get_Flag5 (Target);
   end Get_After_Drivers_Flag;

   procedure Set_After_Drivers_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_After_Drivers_Flag (Get_Kind (Target)),
                     "no field After_Drivers_Flag");
      Set_Flag5 (Target, Flag);
   end Set_After_Drivers_Flag;

   function Get_We_Value (We : Iir_Waveform_Element) return Iir is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_We_Value (Get_Kind (We)),
                     "no field We_Value");
      return Get_Field1 (We);
   end Get_We_Value;

   procedure Set_We_Value (We : Iir_Waveform_Element; An_Iir : Iir) is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_We_Value (Get_Kind (We)),
                     "no field We_Value");
      Set_Field1 (We, An_Iir);
   end Set_We_Value;

   function Get_Time (We : Iir_Waveform_Element) return Iir is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_Time (Get_Kind (We)),
                     "no field Time");
      return Get_Field3 (We);
   end Get_Time;

   procedure Set_Time (We : Iir_Waveform_Element; An_Iir : Iir) is
   begin
      pragma Assert (We /= Null_Iir);
      pragma Assert (Has_Time (Get_Kind (We)),
                     "no field Time");
      Set_Field3 (We, An_Iir);
   end Set_Time;

   function Get_Associated_Expr (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Expr (Get_Kind (Target)),
                     "no field Associated_Expr");
      return Get_Field3 (Target);
   end Get_Associated_Expr;

   procedure Set_Associated_Expr (Target : Iir; Associated : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Expr (Get_Kind (Target)),
                     "no field Associated_Expr");
      Set_Field3 (Target, Associated);
   end Set_Associated_Expr;

   function Get_Associated_Block (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Block (Get_Kind (Target)),
                     "no field Associated_Block");
      return Get_Field3 (Target);
   end Get_Associated_Block;

   procedure Set_Associated_Block (Target : Iir; Associated : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Block (Get_Kind (Target)),
                     "no field Associated_Block");
      Set_Field3 (Target, Associated);
   end Set_Associated_Block;

   function Get_Associated_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Chain (Get_Kind (Target)),
                     "no field Associated_Chain");
      return Get_Field4 (Target);
   end Get_Associated_Chain;

   procedure Set_Associated_Chain (Target : Iir; Associated : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Associated_Chain (Get_Kind (Target)),
                     "no field Associated_Chain");
      Set_Field4 (Target, Associated);
   end Set_Associated_Chain;

   function Get_Choice_Name (Choice : Iir) return Iir is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Name (Get_Kind (Choice)),
                     "no field Choice_Name");
      return Get_Field5 (Choice);
   end Get_Choice_Name;

   procedure Set_Choice_Name (Choice : Iir; Name : Iir) is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Name (Get_Kind (Choice)),
                     "no field Choice_Name");
      Set_Field5 (Choice, Name);
   end Set_Choice_Name;

   function Get_Choice_Expression (Choice : Iir) return Iir is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Expression (Get_Kind (Choice)),
                     "no field Choice_Expression");
      return Get_Field5 (Choice);
   end Get_Choice_Expression;

   procedure Set_Choice_Expression (Choice : Iir; Name : Iir) is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Expression (Get_Kind (Choice)),
                     "no field Choice_Expression");
      Set_Field5 (Choice, Name);
   end Set_Choice_Expression;

   function Get_Choice_Range (Choice : Iir) return Iir is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Range (Get_Kind (Choice)),
                     "no field Choice_Range");
      return Get_Field5 (Choice);
   end Get_Choice_Range;

   procedure Set_Choice_Range (Choice : Iir; Name : Iir) is
   begin
      pragma Assert (Choice /= Null_Iir);
      pragma Assert (Has_Choice_Range (Get_Kind (Choice)),
                     "no field Choice_Range");
      Set_Field5 (Choice, Name);
   end Set_Choice_Range;

   function Get_Same_Alternative_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Same_Alternative_Flag (Get_Kind (Target)),
                     "no field Same_Alternative_Flag");
      return Get_Flag1 (Target);
   end Get_Same_Alternative_Flag;

   procedure Set_Same_Alternative_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Same_Alternative_Flag (Get_Kind (Target)),
                     "no field Same_Alternative_Flag");
      Set_Flag1 (Target, Val);
   end Set_Same_Alternative_Flag;

   function Get_Element_Type_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Type_Flag (Get_Kind (Target)),
                     "no field Element_Type_Flag");
      return Get_Flag2 (Target);
   end Get_Element_Type_Flag;

   procedure Set_Element_Type_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Type_Flag (Get_Kind (Target)),
                     "no field Element_Type_Flag");
      Set_Flag2 (Target, Val);
   end Set_Element_Type_Flag;

   function Get_Architecture (Target : Iir_Entity_Aspect_Entity) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Architecture (Get_Kind (Target)),
                     "no field Architecture");
      return Get_Field3 (Target);
   end Get_Architecture;

   procedure Set_Architecture (Target : Iir_Entity_Aspect_Entity; Arch : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Architecture (Get_Kind (Target)),
                     "no field Architecture");
      Set_Field3 (Target, Arch);
   end Set_Architecture;

   function Get_Block_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Specification (Get_Kind (Target)),
                     "no field Block_Specification");
      return Get_Field5 (Target);
   end Get_Block_Specification;

   procedure Set_Block_Specification (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Specification (Get_Kind (Target)),
                     "no field Block_Specification");
      Set_Field5 (Target, Block);
   end Set_Block_Specification;

   function Get_Prev_Block_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prev_Block_Configuration (Get_Kind (Target)),
                     "no field Prev_Block_Configuration");
      return Get_Field4 (Target);
   end Get_Prev_Block_Configuration;

   procedure Set_Prev_Block_Configuration (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prev_Block_Configuration (Get_Kind (Target)),
                     "no field Prev_Block_Configuration");
      Set_Field4 (Target, Block);
   end Set_Prev_Block_Configuration;

   function Get_Configuration_Item_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Item_Chain (Get_Kind (Target)),
                     "no field Configuration_Item_Chain");
      return Get_Field3 (Target);
   end Get_Configuration_Item_Chain;

   procedure Set_Configuration_Item_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Item_Chain (Get_Kind (Target)),
                     "no field Configuration_Item_Chain");
      Set_Field3 (Target, Chain);
   end Set_Configuration_Item_Chain;

   function Get_Attribute_Value_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Chain");
      return Get_Field5 (Target);
   end Get_Attribute_Value_Chain;

   procedure Set_Attribute_Value_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Chain");
      Set_Field5 (Target, Chain);
   end Set_Attribute_Value_Chain;

   function Get_Spec_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Spec_Chain (Get_Kind (Target)),
                     "no field Spec_Chain");
      return Get_Field2 (Target);
   end Get_Spec_Chain;

   procedure Set_Spec_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Spec_Chain (Get_Kind (Target)),
                     "no field Spec_Chain");
      Set_Field2 (Target, Chain);
   end Set_Spec_Chain;

   function Get_Value_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Value_Chain (Get_Kind (Target)),
                     "no field Value_Chain");
      return Get_Field0 (Target);
   end Get_Value_Chain;

   procedure Set_Value_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Value_Chain (Get_Kind (Target)),
                     "no field Value_Chain");
      Set_Field0 (Target, Chain);
   end Set_Value_Chain;

   function Get_Attribute_Value_Spec_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Spec_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Spec_Chain");
      return Get_Field4 (Target);
   end Get_Attribute_Value_Spec_Chain;

   procedure Set_Attribute_Value_Spec_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Attribute_Value_Spec_Chain (Get_Kind (Target)),
                     "no field Attribute_Value_Spec_Chain");
      Set_Field4 (Target, Chain);
   end Set_Attribute_Value_Spec_Chain;

   function Get_Entity_Name (Arch : Iir) return Iir is
   begin
      pragma Assert (Arch /= Null_Iir);
      pragma Assert (Has_Entity_Name (Get_Kind (Arch)),
                     "no field Entity_Name");
      return Get_Field2 (Arch);
   end Get_Entity_Name;

   procedure Set_Entity_Name (Arch : Iir; Entity : Iir) is
   begin
      pragma Assert (Arch /= Null_Iir);
      pragma Assert (Has_Entity_Name (Get_Kind (Arch)),
                     "no field Entity_Name");
      Set_Field2 (Arch, Entity);
   end Set_Entity_Name;

   function Get_Package (Package_Body : Iir) return Iir is
   begin
      pragma Assert (Package_Body /= Null_Iir);
      pragma Assert (Has_Package (Get_Kind (Package_Body)),
                     "no field Package");
      return Get_Field4 (Package_Body);
   end Get_Package;

   procedure Set_Package (Package_Body : Iir; Decl : Iir) is
   begin
      pragma Assert (Package_Body /= Null_Iir);
      pragma Assert (Has_Package (Get_Kind (Package_Body)),
                     "no field Package");
      Set_Field4 (Package_Body, Decl);
   end Set_Package;

   function Get_Package_Body (Pkg : Iir) return Iir is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Body (Get_Kind (Pkg)),
                     "no field Package_Body");
      return Get_Field4 (Pkg);
   end Get_Package_Body;

   procedure Set_Package_Body (Pkg : Iir; Decl : Iir) is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Body (Get_Kind (Pkg)),
                     "no field Package_Body");
      Set_Field4 (Pkg, Decl);
   end Set_Package_Body;

   function Get_Instance_Package_Body (Pkg : Iir) return Iir is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Instance_Package_Body (Get_Kind (Pkg)),
                     "no field Instance_Package_Body");
      return Get_Field4 (Pkg);
   end Get_Instance_Package_Body;

   procedure Set_Instance_Package_Body (Pkg : Iir; Decl : Iir) is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Instance_Package_Body (Get_Kind (Pkg)),
                     "no field Instance_Package_Body");
      Set_Field4 (Pkg, Decl);
   end Set_Instance_Package_Body;

   function Get_Need_Body (Decl : Iir_Package_Declaration) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Body (Get_Kind (Decl)),
                     "no field Need_Body");
      return Get_Flag1 (Decl);
   end Get_Need_Body;

   procedure Set_Need_Body (Decl : Iir_Package_Declaration; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Body (Get_Kind (Decl)),
                     "no field Need_Body");
      Set_Flag1 (Decl, Flag);
   end Set_Need_Body;

   function Get_Macro_Expanded_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Macro_Expanded_Flag (Get_Kind (Decl)),
                     "no field Macro_Expanded_Flag");
      return Get_Flag2 (Decl);
   end Get_Macro_Expanded_Flag;

   procedure Set_Macro_Expanded_Flag (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Macro_Expanded_Flag (Get_Kind (Decl)),
                     "no field Macro_Expanded_Flag");
      Set_Flag2 (Decl, Flag);
   end Set_Macro_Expanded_Flag;

   function Get_Need_Instance_Bodies (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Instance_Bodies (Get_Kind (Decl)),
                     "no field Need_Instance_Bodies");
      return Get_Flag3 (Decl);
   end Get_Need_Instance_Bodies;

   procedure Set_Need_Instance_Bodies (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Need_Instance_Bodies (Get_Kind (Decl)),
                     "no field Need_Instance_Bodies");
      Set_Flag3 (Decl, Flag);
   end Set_Need_Instance_Bodies;

   function Get_Hierarchical_Name (Vunit : Iir) return Iir is
   begin
      pragma Assert (Vunit /= Null_Iir);
      pragma Assert (Has_Hierarchical_Name (Get_Kind (Vunit)),
                     "no field Hierarchical_Name");
      return Get_Field1 (Vunit);
   end Get_Hierarchical_Name;

   procedure Set_Hierarchical_Name (Vunit : Iir; Name : Iir) is
   begin
      pragma Assert (Vunit /= Null_Iir);
      pragma Assert (Has_Hierarchical_Name (Get_Kind (Vunit)),
                     "no field Hierarchical_Name");
      Set_Field1 (Vunit, Name);
   end Set_Hierarchical_Name;

   function Get_Vunit_Item_Chain (Vunit : Iir) return Iir is
   begin
      pragma Assert (Vunit /= Null_Iir);
      pragma Assert (Has_Vunit_Item_Chain (Get_Kind (Vunit)),
                     "no field Vunit_Item_Chain");
      return Get_Field6 (Vunit);
   end Get_Vunit_Item_Chain;

   procedure Set_Vunit_Item_Chain (Vunit : Iir; Chain : Iir) is
   begin
      pragma Assert (Vunit /= Null_Iir);
      pragma Assert (Has_Vunit_Item_Chain (Get_Kind (Vunit)),
                     "no field Vunit_Item_Chain");
      Set_Field6 (Vunit, Chain);
   end Set_Vunit_Item_Chain;

   function Get_Bound_Vunit_Chain (Unit : Iir) return Iir is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Bound_Vunit_Chain (Get_Kind (Unit)),
                     "no field Bound_Vunit_Chain");
      return Get_Field8 (Unit);
   end Get_Bound_Vunit_Chain;

   procedure Set_Bound_Vunit_Chain (Unit : Iir; Vunit : Iir) is
   begin
      pragma Assert (Unit /= Null_Iir);
      pragma Assert (Has_Bound_Vunit_Chain (Get_Kind (Unit)),
                     "no field Bound_Vunit_Chain");
      Set_Field8 (Unit, Vunit);
   end Set_Bound_Vunit_Chain;

   function Get_Verification_Block_Configuration (Vunit : Iir) return Iir is
   begin
      pragma Assert (Vunit /= Null_Iir);
      pragma Assert (Has_Verification_Block_Configuration (Get_Kind (Vunit)),
                     "no field Verification_Block_Configuration");
      return Get_Field4 (Vunit);
   end Get_Verification_Block_Configuration;

   procedure Set_Verification_Block_Configuration (Vunit : Iir; Conf : Iir) is
   begin
      pragma Assert (Vunit /= Null_Iir);
      pragma Assert (Has_Verification_Block_Configuration (Get_Kind (Vunit)),
                     "no field Verification_Block_Configuration");
      Set_Field4 (Vunit, Conf);
   end Set_Verification_Block_Configuration;

   function Get_Block_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Configuration (Get_Kind (Target)),
                     "no field Block_Configuration");
      return Get_Field4 (Target);
   end Get_Block_Configuration;

   procedure Set_Block_Configuration (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Configuration (Get_Kind (Target)),
                     "no field Block_Configuration");
      Set_Field4 (Target, Block);
   end Set_Block_Configuration;

   function Get_Concurrent_Statement_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Concurrent_Statement_Chain (Get_Kind (Target)),
                     "no field Concurrent_Statement_Chain");
      return Get_Field4 (Target);
   end Get_Concurrent_Statement_Chain;

   procedure Set_Concurrent_Statement_Chain (Target : Iir; First : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Concurrent_Statement_Chain (Get_Kind (Target)),
                     "no field Concurrent_Statement_Chain");
      Set_Field4 (Target, First);
   end Set_Concurrent_Statement_Chain;

   function Get_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Chain (Get_Kind (Target)),
                     "no field Chain");
      return Get_Field2 (Target);
   end Get_Chain;

   procedure Set_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Chain (Get_Kind (Target)),
                     "no field Chain");
      Set_Field2 (Target, Chain);
   end Set_Chain;

   function Get_Port_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Chain (Get_Kind (Target)),
                     "no field Port_Chain");
      return Get_Field7 (Target);
   end Get_Port_Chain;

   procedure Set_Port_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Chain (Get_Kind (Target)),
                     "no field Port_Chain");
      Set_Field7 (Target, Chain);
   end Set_Port_Chain;

   function Get_Generic_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Chain (Get_Kind (Target)),
                     "no field Generic_Chain");
      return Get_Field6 (Target);
   end Get_Generic_Chain;

   procedure Set_Generic_Chain (Target : Iir; Generics : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Chain (Get_Kind (Target)),
                     "no field Generic_Chain");
      Set_Field6 (Target, Generics);
   end Set_Generic_Chain;

   function Get_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type (Get_Kind (Target)),
                     "no field Type");
      return Get_Field1 (Target);
   end Get_Type;

   procedure Set_Type (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type (Get_Kind (Target)),
                     "no field Type");
      Set_Field1 (Target, Atype);
   end Set_Type;

   function Get_Subtype_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Indication (Get_Kind (Target)),
                     "no field Subtype_Indication");
      return Get_Field5 (Target);
   end Get_Subtype_Indication;

   procedure Set_Subtype_Indication (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Indication (Get_Kind (Target)),
                     "no field Subtype_Indication");
      Set_Field5 (Target, Atype);
   end Set_Subtype_Indication;

   function Get_Discrete_Range (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Discrete_Range (Get_Kind (Target)),
                     "no field Discrete_Range");
      return Get_Field4 (Target);
   end Get_Discrete_Range;

   procedure Set_Discrete_Range (Target : Iir; Rng : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Discrete_Range (Get_Kind (Target)),
                     "no field Discrete_Range");
      Set_Field4 (Target, Rng);
   end Set_Discrete_Range;

   function Get_Type_Definition (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Type_Definition (Get_Kind (Decl)),
                     "no field Type_Definition");
      return Get_Field1 (Decl);
   end Get_Type_Definition;

   procedure Set_Type_Definition (Decl : Iir; Atype : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Type_Definition (Get_Kind (Decl)),
                     "no field Type_Definition");
      Set_Field1 (Decl, Atype);
   end Set_Type_Definition;

   function Get_Subtype_Definition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Definition (Get_Kind (Target)),
                     "no field Subtype_Definition");
      return Get_Field4 (Target);
   end Get_Subtype_Definition;

   procedure Set_Subtype_Definition (Target : Iir; Def : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Definition (Get_Kind (Target)),
                     "no field Subtype_Definition");
      Set_Field4 (Target, Def);
   end Set_Subtype_Definition;

   function Get_Incomplete_Type_Declaration (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Declaration (Get_Kind (N)),
                     "no field Incomplete_Type_Declaration");
      return Get_Field5 (N);
   end Get_Incomplete_Type_Declaration;

   procedure Set_Incomplete_Type_Declaration (N : Iir; Decl : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Declaration (Get_Kind (N)),
                     "no field Incomplete_Type_Declaration");
      Set_Field5 (N, Decl);
   end Set_Incomplete_Type_Declaration;

   function Get_Interface_Type_Subprograms (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Type_Subprograms (Get_Kind (Target)),
                     "no field Interface_Type_Subprograms");
      return Get_Field4 (Target);
   end Get_Interface_Type_Subprograms;

   procedure Set_Interface_Type_Subprograms (Target : Iir; Subprg : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Type_Subprograms (Get_Kind (Target)),
                     "no field Interface_Type_Subprograms");
      Set_Field4 (Target, Subprg);
   end Set_Interface_Type_Subprograms;

   function Get_Interface_Type_Definition (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Interface_Type_Definition (Get_Kind (N)),
                     "no field Interface_Type_Definition");
      return Get_Field5 (N);
   end Get_Interface_Type_Definition;

   procedure Set_Interface_Type_Definition (N : Iir; Atype : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Interface_Type_Definition (Get_Kind (N)),
                     "no field Interface_Type_Definition");
      Set_Field5 (N, Atype);
   end Set_Interface_Type_Definition;

   function Get_Nature_Definition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Nature_Definition (Get_Kind (Target)),
                     "no field Nature_Definition");
      return Get_Field1 (Target);
   end Get_Nature_Definition;

   procedure Set_Nature_Definition (Target : Iir; Def : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Nature_Definition (Get_Kind (Target)),
                     "no field Nature_Definition");
      Set_Field1 (Target, Def);
   end Set_Nature_Definition;

   function Get_Nature (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Nature (Get_Kind (Target)),
                     "no field Nature");
      return Get_Field1 (Target);
   end Get_Nature;

   procedure Set_Nature (Target : Iir; Nature : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Nature (Get_Kind (Target)),
                     "no field Nature");
      Set_Field1 (Target, Nature);
   end Set_Nature;

   function Get_Subnature_Indication (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Subnature_Indication (Get_Kind (Decl)),
                     "no field Subnature_Indication");
      return Get_Field5 (Decl);
   end Get_Subnature_Indication;

   procedure Set_Subnature_Indication (Decl : Iir; Sub_Nature : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Subnature_Indication (Get_Kind (Decl)),
                     "no field Subnature_Indication");
      Set_Field5 (Decl, Sub_Nature);
   end Set_Subnature_Indication;

   function Get_Reference_Terminal_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Reference_Terminal_Flag (Get_Kind (Decl)),
                     "no field Reference_Terminal_Flag");
      return Get_Flag1 (Decl);
   end Get_Reference_Terminal_Flag;

   procedure Set_Reference_Terminal_Flag (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Reference_Terminal_Flag (Get_Kind (Decl)),
                     "no field Reference_Terminal_Flag");
      Set_Flag1 (Decl, Flag);
   end Set_Reference_Terminal_Flag;

   type Iir_Mode_Conv is record
      Flag13: Boolean;
      Flag14: Boolean;
      Flag15: Boolean;
   end record;
   pragma Pack (Iir_Mode_Conv);
   pragma Assert (Iir_Mode_Conv'Size = Iir_Mode'Size);

   function Get_Mode (Target : Iir) return Iir_Mode
   is
      function To_Iir_Mode is new Ada.Unchecked_Conversion
         (Iir_Mode_Conv, Iir_Mode);
      Conv : Iir_Mode_Conv;
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Mode (Get_Kind (Target)),
                     "no field Mode");
      Conv.Flag13 := Get_Flag13 (Target);
      Conv.Flag14 := Get_Flag14 (Target);
      Conv.Flag15 := Get_Flag15 (Target);
      return To_Iir_Mode (Conv);
   end Get_Mode;

   procedure Set_Mode (Target : Iir; Mode : Iir_Mode)
   is
      function To_Iir_Mode_Conv is new Ada.Unchecked_Conversion
         (Iir_Mode, Iir_Mode_Conv);
      Conv : Iir_Mode_Conv;
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Mode (Get_Kind (Target)),
                     "no field Mode");
      Conv := To_Iir_Mode_Conv (Mode);
      Set_Flag13 (Target, Conv.Flag13);
      Set_Flag14 (Target, Conv.Flag14);
      Set_Flag15 (Target, Conv.Flag15);
   end Set_Mode;

   function Get_Guarded_Signal_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guarded_Signal_Flag (Get_Kind (Target)),
                     "no field Guarded_Signal_Flag");
      return Get_Flag8 (Target);
   end Get_Guarded_Signal_Flag;

   procedure Set_Guarded_Signal_Flag (Target : Iir; Guarded : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guarded_Signal_Flag (Get_Kind (Target)),
                     "no field Guarded_Signal_Flag");
      Set_Flag8 (Target, Guarded);
   end Set_Guarded_Signal_Flag;

   function Get_Signal_Kind (Target : Iir) return Iir_Signal_Kind is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Kind (Get_Kind (Target)),
                     "no field Signal_Kind");
      return Boolean_To_Iir_Signal_Kind (Get_Flag9 (Target));
   end Get_Signal_Kind;

   procedure Set_Signal_Kind (Target : Iir; Signal_Kind : Iir_Signal_Kind) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Kind (Get_Kind (Target)),
                     "no field Signal_Kind");
      Set_Flag9 (Target, Iir_Signal_Kind_To_Boolean (Signal_Kind));
   end Set_Signal_Kind;

   function Get_Base_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Base_Name (Get_Kind (Target)),
                     "no field Base_Name");
      return Get_Field5 (Target);
   end Get_Base_Name;

   procedure Set_Base_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Base_Name (Get_Kind (Target)),
                     "no field Base_Name");
      Set_Field5 (Target, Name);
   end Set_Base_Name;

   function Get_Interface_Declaration_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Declaration_Chain (Get_Kind (Target)),
                     "no field Interface_Declaration_Chain");
      return Get_Field5 (Target);
   end Get_Interface_Declaration_Chain;

   procedure Set_Interface_Declaration_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Interface_Declaration_Chain (Get_Kind (Target)),
                     "no field Interface_Declaration_Chain");
      Set_Field5 (Target, Chain);
   end Set_Interface_Declaration_Chain;

   function Get_Default_Subprogram (Inter : Iir) return Iir is
   begin
      pragma Assert (Inter /= Null_Iir);
      pragma Assert (Has_Default_Subprogram (Get_Kind (Inter)),
                     "no field Default_Subprogram");
      return Get_Field9 (Inter);
   end Get_Default_Subprogram;

   procedure Set_Default_Subprogram (Inter : Iir; Subprg : Iir) is
   begin
      pragma Assert (Inter /= Null_Iir);
      pragma Assert (Has_Default_Subprogram (Get_Kind (Inter)),
                     "no field Default_Subprogram");
      Set_Field9 (Inter, Subprg);
   end Set_Default_Subprogram;

   function Get_Associated_Subprogram (Inter : Iir) return Iir is
   begin
      pragma Assert (Inter /= Null_Iir);
      pragma Assert (Has_Associated_Subprogram (Get_Kind (Inter)),
                     "no field Associated_Subprogram");
      return Get_Field6 (Inter);
   end Get_Associated_Subprogram;

   procedure Set_Associated_Subprogram (Inter : Iir; Subprg : Iir) is
   begin
      pragma Assert (Inter /= Null_Iir);
      pragma Assert (Has_Associated_Subprogram (Get_Kind (Inter)),
                     "no field Associated_Subprogram");
      Set_Field6 (Inter, Subprg);
   end Set_Associated_Subprogram;

   function Get_Subprogram_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Specification (Get_Kind (Target)),
                     "no field Subprogram_Specification");
      return Get_Field6 (Target);
   end Get_Subprogram_Specification;

   procedure Set_Subprogram_Specification (Target : Iir; Spec : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Specification (Get_Kind (Target)),
                     "no field Subprogram_Specification");
      Set_Field6 (Target, Spec);
   end Set_Subprogram_Specification;

   function Get_Sequential_Statement_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sequential_Statement_Chain (Get_Kind (Target)),
                     "no field Sequential_Statement_Chain");
      return Get_Field4 (Target);
   end Get_Sequential_Statement_Chain;

   procedure Set_Sequential_Statement_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sequential_Statement_Chain (Get_Kind (Target)),
                     "no field Sequential_Statement_Chain");
      Set_Field4 (Target, Chain);
   end Set_Sequential_Statement_Chain;

   function Get_Simultaneous_Statement_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simultaneous_Statement_Chain (Get_Kind (Target)),
                     "no field Simultaneous_Statement_Chain");
      return Get_Field4 (Target);
   end Get_Simultaneous_Statement_Chain;

   procedure Set_Simultaneous_Statement_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simultaneous_Statement_Chain (Get_Kind (Target)),
                     "no field Simultaneous_Statement_Chain");
      Set_Field4 (Target, Chain);
   end Set_Simultaneous_Statement_Chain;

   function Get_Subprogram_Body (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Body (Get_Kind (Target)),
                     "no field Subprogram_Body");
      return Get_Field9 (Target);
   end Get_Subprogram_Body;

   procedure Set_Subprogram_Body (Target : Iir; A_Body : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Body (Get_Kind (Target)),
                     "no field Subprogram_Body");
      Set_Field9 (Target, A_Body);
   end Set_Subprogram_Body;

   function Get_Overload_Number (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_Number (Get_Kind (Target)),
                     "no field Overload_Number");
      return Iir_Int32'Val (Get_Field12 (Target));
   end Get_Overload_Number;

   procedure Set_Overload_Number (Target : Iir; Val : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_Number (Get_Kind (Target)),
                     "no field Overload_Number");
      Set_Field12 (Target, Iir_Int32'Pos (Val));
   end Set_Overload_Number;

   function Get_Subprogram_Depth (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Depth (Get_Kind (Target)),
                     "no field Subprogram_Depth");
      return Iir_Int32'Val (Get_Field10 (Target));
   end Get_Subprogram_Depth;

   procedure Set_Subprogram_Depth (Target : Iir; Depth : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Depth (Get_Kind (Target)),
                     "no field Subprogram_Depth");
      Set_Field10 (Target, Iir_Int32'Pos (Depth));
   end Set_Subprogram_Depth;

   function Get_Subprogram_Hash (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Hash (Get_Kind (Target)),
                     "no field Subprogram_Hash");
      return Iir_Int32'Val (Get_Field4 (Target));
   end Get_Subprogram_Hash;

   procedure Set_Subprogram_Hash (Target : Iir; Val : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Hash (Get_Kind (Target)),
                     "no field Subprogram_Hash");
      Set_Field4 (Target, Iir_Int32'Pos (Val));
   end Set_Subprogram_Hash;

   function Get_Impure_Depth (Target : Iir) return Iir_Int32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Impure_Depth (Get_Kind (Target)),
                     "no field Impure_Depth");
      return Iir_To_Iir_Int32 (Get_Field3 (Target));
   end Get_Impure_Depth;

   procedure Set_Impure_Depth (Target : Iir; Depth : Iir_Int32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Impure_Depth (Get_Kind (Target)),
                     "no field Impure_Depth");
      Set_Field3 (Target, Iir_Int32_To_Iir (Depth));
   end Set_Impure_Depth;

   function Get_Return_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type (Get_Kind (Target)),
                     "no field Return_Type");
      return Get_Field1 (Target);
   end Get_Return_Type;

   procedure Set_Return_Type (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type (Get_Kind (Target)),
                     "no field Return_Type");
      Set_Field1 (Target, Decl);
   end Set_Return_Type;

   function Get_Implicit_Definition (D : Iir) return Iir_Predefined_Functions
   is
   begin
      pragma Assert (D /= Null_Iir);
      pragma Assert (Has_Implicit_Definition (Get_Kind (D)),
                     "no field Implicit_Definition");
      return Iir_Predefined_Functions'Val (Get_Field7 (D));
   end Get_Implicit_Definition;

   procedure Set_Implicit_Definition (D : Iir; Def : Iir_Predefined_Functions)
   is
   begin
      pragma Assert (D /= Null_Iir);
      pragma Assert (Has_Implicit_Definition (Get_Kind (D)),
                     "no field Implicit_Definition");
      Set_Field7 (D, Iir_Predefined_Functions'Pos (Def));
   end Set_Implicit_Definition;

   function Get_Uninstantiated_Subprogram_Name (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Subprogram_Name (Get_Kind (N)),
                     "no field Uninstantiated_Subprogram_Name");
      return Get_Field7 (N);
   end Get_Uninstantiated_Subprogram_Name;

   procedure Set_Uninstantiated_Subprogram_Name (N : Iir; Name : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Subprogram_Name (Get_Kind (N)),
                     "no field Uninstantiated_Subprogram_Name");
      Set_Field7 (N, Name);
   end Set_Uninstantiated_Subprogram_Name;

   function Get_Default_Value (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Value (Get_Kind (Target)),
                     "no field Default_Value");
      return Get_Field4 (Target);
   end Get_Default_Value;

   procedure Set_Default_Value (Target : Iir; Value : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Value (Get_Kind (Target)),
                     "no field Default_Value");
      Set_Field4 (Target, Value);
   end Set_Default_Value;

   function Get_Deferred_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration (Get_Kind (Target)),
                     "no field Deferred_Declaration");
      return Get_Field6 (Target);
   end Get_Deferred_Declaration;

   procedure Set_Deferred_Declaration (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration (Get_Kind (Target)),
                     "no field Deferred_Declaration");
      Set_Field6 (Target, Decl);
   end Set_Deferred_Declaration;

   function Get_Deferred_Declaration_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration_Flag (Get_Kind (Target)),
                     "no field Deferred_Declaration_Flag");
      return Get_Flag1 (Target);
   end Get_Deferred_Declaration_Flag;

   procedure Set_Deferred_Declaration_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Deferred_Declaration_Flag (Get_Kind (Target)),
                     "no field Deferred_Declaration_Flag");
      Set_Flag1 (Target, Flag);
   end Set_Deferred_Declaration_Flag;

   function Get_Shared_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Shared_Flag (Get_Kind (Target)),
                     "no field Shared_Flag");
      return Get_Flag2 (Target);
   end Get_Shared_Flag;

   procedure Set_Shared_Flag (Target : Iir; Shared : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Shared_Flag (Get_Kind (Target)),
                     "no field Shared_Flag");
      Set_Flag2 (Target, Shared);
   end Set_Shared_Flag;

   function Get_Design_Unit (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Design_Unit (Get_Kind (Target)),
                     "no field Design_Unit");
      return Get_Field0 (Target);
   end Get_Design_Unit;

   procedure Set_Design_Unit (Target : Iir; Unit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Design_Unit (Get_Kind (Target)),
                     "no field Design_Unit");
      Set_Field0 (Target, Unit);
   end Set_Design_Unit;

   function Get_Block_Statement (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Statement (Get_Kind (Target)),
                     "no field Block_Statement");
      return Get_Field5 (Target);
   end Get_Block_Statement;

   procedure Set_Block_Statement (Target : Iir; Block : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Statement (Get_Kind (Target)),
                     "no field Block_Statement");
      Set_Field5 (Target, Block);
   end Set_Block_Statement;

   function Get_Signal_Driver (Target : Iir_Signal_Declaration) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Driver (Get_Kind (Target)),
                     "no field Signal_Driver");
      return Get_Field7 (Target);
   end Get_Signal_Driver;

   procedure Set_Signal_Driver (Target : Iir_Signal_Declaration; Driver : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Signal_Driver (Get_Kind (Target)),
                     "no field Signal_Driver");
      Set_Field7 (Target, Driver);
   end Set_Signal_Driver;

   function Get_Declaration_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Declaration_Chain (Get_Kind (Target)),
                     "no field Declaration_Chain");
      return Get_Field1 (Target);
   end Get_Declaration_Chain;

   procedure Set_Declaration_Chain (Target : Iir; Decls : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Declaration_Chain (Get_Kind (Target)),
                     "no field Declaration_Chain");
      Set_Field1 (Target, Decls);
   end Set_Declaration_Chain;

   function Get_File_Logical_Name (Target : Iir_File_Declaration) return Iir
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Logical_Name (Get_Kind (Target)),
                     "no field File_Logical_Name");
      return Get_Field6 (Target);
   end Get_File_Logical_Name;

   procedure Set_File_Logical_Name (Target : Iir_File_Declaration; Name : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Logical_Name (Get_Kind (Target)),
                     "no field File_Logical_Name");
      Set_Field6 (Target, Name);
   end Set_File_Logical_Name;

   function Get_File_Open_Kind (Target : Iir_File_Declaration) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Open_Kind (Get_Kind (Target)),
                     "no field File_Open_Kind");
      return Get_Field7 (Target);
   end Get_File_Open_Kind;

   procedure Set_File_Open_Kind (Target : Iir_File_Declaration; Kind : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Open_Kind (Get_Kind (Target)),
                     "no field File_Open_Kind");
      Set_Field7 (Target, Kind);
   end Set_File_Open_Kind;

   function Get_Element_Position (Target : Iir) return Iir_Index32 is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Position (Get_Kind (Target)),
                     "no field Element_Position");
      return Iir_Index32'Val (Get_Field4 (Target));
   end Get_Element_Position;

   procedure Set_Element_Position (Target : Iir; Pos : Iir_Index32) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Element_Position (Get_Kind (Target)),
                     "no field Element_Position");
      Set_Field4 (Target, Iir_Index32'Pos (Pos));
   end Set_Element_Position;

   function Get_Use_Clause_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Use_Clause_Chain (Get_Kind (Target)),
                     "no field Use_Clause_Chain");
      return Get_Field3 (Target);
   end Get_Use_Clause_Chain;

   procedure Set_Use_Clause_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Use_Clause_Chain (Get_Kind (Target)),
                     "no field Use_Clause_Chain");
      Set_Field3 (Target, Chain);
   end Set_Use_Clause_Chain;

   function Get_Context_Reference_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Context_Reference_Chain (Get_Kind (Target)),
                     "no field Context_Reference_Chain");
      return Get_Field3 (Target);
   end Get_Context_Reference_Chain;

   procedure Set_Context_Reference_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Context_Reference_Chain (Get_Kind (Target)),
                     "no field Context_Reference_Chain");
      Set_Field3 (Target, Chain);
   end Set_Context_Reference_Chain;

   function Get_Inherit_Spec_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Inherit_Spec_Chain (Get_Kind (Target)),
                     "no field Inherit_Spec_Chain");
      return Get_Field3 (Target);
   end Get_Inherit_Spec_Chain;

   procedure Set_Inherit_Spec_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Inherit_Spec_Chain (Get_Kind (Target)),
                     "no field Inherit_Spec_Chain");
      Set_Field3 (Target, Chain);
   end Set_Inherit_Spec_Chain;

   function Get_Selected_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Name (Get_Kind (Target)),
                     "no field Selected_Name");
      return Get_Field1 (Target);
   end Get_Selected_Name;

   procedure Set_Selected_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Name (Get_Kind (Target)),
                     "no field Selected_Name");
      Set_Field1 (Target, Name);
   end Set_Selected_Name;

   function Get_Type_Declarator (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Type_Declarator (Get_Kind (Def)),
                     "no field Type_Declarator");
      return Get_Field3 (Def);
   end Get_Type_Declarator;

   procedure Set_Type_Declarator (Def : Iir; Decl : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Type_Declarator (Get_Kind (Def)),
                     "no field Type_Declarator");
      Set_Field3 (Def, Decl);
   end Set_Type_Declarator;

   function Get_Complete_Type_Definition (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Complete_Type_Definition (Get_Kind (N)),
                     "no field Complete_Type_Definition");
      return Get_Field5 (N);
   end Get_Complete_Type_Definition;

   procedure Set_Complete_Type_Definition (N : Iir; Def : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Complete_Type_Definition (Get_Kind (N)),
                     "no field Complete_Type_Definition");
      Set_Field5 (N, Def);
   end Set_Complete_Type_Definition;

   function Get_Incomplete_Type_Ref_Chain (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Ref_Chain (Get_Kind (N)),
                     "no field Incomplete_Type_Ref_Chain");
      return Get_Field0 (N);
   end Get_Incomplete_Type_Ref_Chain;

   procedure Set_Incomplete_Type_Ref_Chain (N : Iir; Def : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Incomplete_Type_Ref_Chain (Get_Kind (N)),
                     "no field Incomplete_Type_Ref_Chain");
      Set_Field0 (N, Def);
   end Set_Incomplete_Type_Ref_Chain;

   function Get_Associated_Type (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Associated_Type (Get_Kind (Def)),
                     "no field Associated_Type");
      return Get_Field5 (Def);
   end Get_Associated_Type;

   procedure Set_Associated_Type (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Associated_Type (Get_Kind (Def)),
                     "no field Associated_Type");
      Set_Field5 (Def, Atype);
   end Set_Associated_Type;

   function Get_Enumeration_Literal_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Enumeration_Literal_List (Get_Kind (Target)),
                     "no field Enumeration_Literal_List");
      return Iir_To_Iir_Flist (Get_Field2 (Target));
   end Get_Enumeration_Literal_List;

   procedure Set_Enumeration_Literal_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Enumeration_Literal_List (Get_Kind (Target)),
                     "no field Enumeration_Literal_List");
      Set_Field2 (Target, Iir_Flist_To_Iir (List));
   end Set_Enumeration_Literal_List;

   function Get_Entity_Class_Entry_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class_Entry_Chain (Get_Kind (Target)),
                     "no field Entity_Class_Entry_Chain");
      return Get_Field1 (Target);
   end Get_Entity_Class_Entry_Chain;

   procedure Set_Entity_Class_Entry_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Class_Entry_Chain (Get_Kind (Target)),
                     "no field Entity_Class_Entry_Chain");
      Set_Field1 (Target, Chain);
   end Set_Entity_Class_Entry_Chain;

   function Get_Group_Constituent_List (Group : Iir) return Iir_Flist is
   begin
      pragma Assert (Group /= Null_Iir);
      pragma Assert (Has_Group_Constituent_List (Get_Kind (Group)),
                     "no field Group_Constituent_List");
      return Iir_To_Iir_Flist (Get_Field1 (Group));
   end Get_Group_Constituent_List;

   procedure Set_Group_Constituent_List (Group : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Group /= Null_Iir);
      pragma Assert (Has_Group_Constituent_List (Get_Kind (Group)),
                     "no field Group_Constituent_List");
      Set_Field1 (Group, Iir_Flist_To_Iir (List));
   end Set_Group_Constituent_List;

   function Get_Unit_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Chain (Get_Kind (Target)),
                     "no field Unit_Chain");
      return Get_Field2 (Target);
   end Get_Unit_Chain;

   procedure Set_Unit_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Chain (Get_Kind (Target)),
                     "no field Unit_Chain");
      Set_Field2 (Target, Chain);
   end Set_Unit_Chain;

   function Get_Primary_Unit (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Primary_Unit (Get_Kind (Target)),
                     "no field Primary_Unit");
      return Get_Field2 (Target);
   end Get_Primary_Unit;

   procedure Set_Primary_Unit (Target : Iir; Unit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Primary_Unit (Get_Kind (Target)),
                     "no field Primary_Unit");
      Set_Field2 (Target, Unit);
   end Set_Primary_Unit;

   function Get_Identifier (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Identifier (Get_Kind (Target)),
                     "no field Identifier");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Identifier;

   procedure Set_Identifier (Target : Iir; Identifier : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Identifier (Get_Kind (Target)),
                     "no field Identifier");
      Set_Field3 (Target, Name_Id_To_Iir (Identifier));
   end Set_Identifier;

   function Get_Label (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Label (Get_Kind (Target)),
                     "no field Label");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Label;

   procedure Set_Label (Target : Iir; Label : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Label (Get_Kind (Target)),
                     "no field Label");
      Set_Field3 (Target, Name_Id_To_Iir (Label));
   end Set_Label;

   function Get_Return_Identifier (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Identifier (Get_Kind (Target)),
                     "no field Return_Identifier");
      return Get_Field11 (Target);
   end Get_Return_Identifier;

   procedure Set_Return_Identifier (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Identifier (Get_Kind (Target)),
                     "no field Return_Identifier");
      Set_Field11 (Target, Decl);
   end Set_Return_Identifier;

   function Get_Visible_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Visible_Flag (Get_Kind (Target)),
                     "no field Visible_Flag");
      return Get_Flag4 (Target);
   end Get_Visible_Flag;

   procedure Set_Visible_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Visible_Flag (Get_Kind (Target)),
                     "no field Visible_Flag");
      Set_Flag4 (Target, Flag);
   end Set_Visible_Flag;

   function Get_Range_Constraint (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Range_Constraint (Get_Kind (Target)),
                     "no field Range_Constraint");
      return Get_Field1 (Target);
   end Get_Range_Constraint;

   procedure Set_Range_Constraint (Target : Iir; Constraint : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Range_Constraint (Get_Kind (Target)),
                     "no field Range_Constraint");
      Set_Field1 (Target, Constraint);
   end Set_Range_Constraint;

   function Get_Direction (Decl : Iir) return Direction_Type is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Direction (Get_Kind (Decl)),
                     "no field Direction");
      return Boolean_To_Direction_Type (Get_Flag1 (Decl));
   end Get_Direction;

   procedure Set_Direction (Decl : Iir; Dir : Direction_Type) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Direction (Get_Kind (Decl)),
                     "no field Direction");
      Set_Flag1 (Decl, Direction_Type_To_Boolean (Dir));
   end Set_Direction;

   function Get_Left_Limit (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit (Get_Kind (Decl)),
                     "no field Left_Limit");
      return Get_Field4 (Decl);
   end Get_Left_Limit;

   procedure Set_Left_Limit (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit (Get_Kind (Decl)),
                     "no field Left_Limit");
      Set_Field4 (Decl, Limit);
   end Set_Left_Limit;

   function Get_Right_Limit (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit (Get_Kind (Decl)),
                     "no field Right_Limit");
      return Get_Field5 (Decl);
   end Get_Right_Limit;

   procedure Set_Right_Limit (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit (Get_Kind (Decl)),
                     "no field Right_Limit");
      Set_Field5 (Decl, Limit);
   end Set_Right_Limit;

   function Get_Left_Limit_Expr (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit_Expr (Get_Kind (Decl)),
                     "no field Left_Limit_Expr");
      return Get_Field2 (Decl);
   end Get_Left_Limit_Expr;

   procedure Set_Left_Limit_Expr (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Left_Limit_Expr (Get_Kind (Decl)),
                     "no field Left_Limit_Expr");
      Set_Field2 (Decl, Limit);
   end Set_Left_Limit_Expr;

   function Get_Right_Limit_Expr (Decl : Iir_Range_Expression) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit_Expr (Get_Kind (Decl)),
                     "no field Right_Limit_Expr");
      return Get_Field3 (Decl);
   end Get_Right_Limit_Expr;

   procedure Set_Right_Limit_Expr (Decl : Iir_Range_Expression; Limit : Iir)
   is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Right_Limit_Expr (Get_Kind (Decl)),
                     "no field Right_Limit_Expr");
      Set_Field3 (Decl, Limit);
   end Set_Right_Limit_Expr;

   function Get_Parent_Type (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Parent_Type (Get_Kind (Decl)),
                     "no field Parent_Type");
      return Get_Field4 (Decl);
   end Get_Parent_Type;

   procedure Set_Parent_Type (Decl : Iir; Base_Type : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Parent_Type (Get_Kind (Decl)),
                     "no field Parent_Type");
      Set_Field4 (Decl, Base_Type);
   end Set_Parent_Type;

   function Get_Simple_Nature (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simple_Nature (Get_Kind (Def)),
                     "no field Simple_Nature");
      return Get_Field7 (Def);
   end Get_Simple_Nature;

   procedure Set_Simple_Nature (Def : Iir; Nature : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simple_Nature (Get_Kind (Def)),
                     "no field Simple_Nature");
      Set_Field7 (Def, Nature);
   end Set_Simple_Nature;

   function Get_Base_Nature (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Base_Nature (Get_Kind (Decl)),
                     "no field Base_Nature");
      return Get_Field4 (Decl);
   end Get_Base_Nature;

   procedure Set_Base_Nature (Decl : Iir; Base_Nature : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Base_Nature (Get_Kind (Decl)),
                     "no field Base_Nature");
      Set_Field4 (Decl, Base_Nature);
   end Set_Base_Nature;

   function Get_Resolution_Indication (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Resolution_Indication (Get_Kind (Decl)),
                     "no field Resolution_Indication");
      return Get_Field5 (Decl);
   end Get_Resolution_Indication;

   procedure Set_Resolution_Indication (Decl : Iir; Ind : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Resolution_Indication (Get_Kind (Decl)),
                     "no field Resolution_Indication");
      Set_Field5 (Decl, Ind);
   end Set_Resolution_Indication;

   function Get_Record_Element_Resolution_Chain (Res : Iir) return Iir is
   begin
      pragma Assert (Res /= Null_Iir);
      pragma Assert (Has_Record_Element_Resolution_Chain (Get_Kind (Res)),
                     "no field Record_Element_Resolution_Chain");
      return Get_Field1 (Res);
   end Get_Record_Element_Resolution_Chain;

   procedure Set_Record_Element_Resolution_Chain (Res : Iir; Chain : Iir) is
   begin
      pragma Assert (Res /= Null_Iir);
      pragma Assert (Has_Record_Element_Resolution_Chain (Get_Kind (Res)),
                     "no field Record_Element_Resolution_Chain");
      Set_Field1 (Res, Chain);
   end Set_Record_Element_Resolution_Chain;

   function Get_Tolerance (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Tolerance (Get_Kind (Def)),
                     "no field Tolerance");
      return Get_Field7 (Def);
   end Get_Tolerance;

   procedure Set_Tolerance (Def : Iir; Tol : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Tolerance (Get_Kind (Def)),
                     "no field Tolerance");
      Set_Field7 (Def, Tol);
   end Set_Tolerance;

   function Get_Plus_Terminal_Name (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Plus_Terminal_Name (Get_Kind (Def)),
                     "no field Plus_Terminal_Name");
      return Get_Field8 (Def);
   end Get_Plus_Terminal_Name;

   procedure Set_Plus_Terminal_Name (Def : Iir; Name : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Plus_Terminal_Name (Get_Kind (Def)),
                     "no field Plus_Terminal_Name");
      Set_Field8 (Def, Name);
   end Set_Plus_Terminal_Name;

   function Get_Minus_Terminal_Name (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Minus_Terminal_Name (Get_Kind (Def)),
                     "no field Minus_Terminal_Name");
      return Get_Field9 (Def);
   end Get_Minus_Terminal_Name;

   procedure Set_Minus_Terminal_Name (Def : Iir; Name : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Minus_Terminal_Name (Get_Kind (Def)),
                     "no field Minus_Terminal_Name");
      Set_Field9 (Def, Name);
   end Set_Minus_Terminal_Name;

   function Get_Plus_Terminal (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Plus_Terminal (Get_Kind (Def)),
                     "no field Plus_Terminal");
      return Get_Field10 (Def);
   end Get_Plus_Terminal;

   procedure Set_Plus_Terminal (Def : Iir; Terminal : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Plus_Terminal (Get_Kind (Def)),
                     "no field Plus_Terminal");
      Set_Field10 (Def, Terminal);
   end Set_Plus_Terminal;

   function Get_Minus_Terminal (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Minus_Terminal (Get_Kind (Def)),
                     "no field Minus_Terminal");
      return Get_Field11 (Def);
   end Get_Minus_Terminal;

   procedure Set_Minus_Terminal (Def : Iir; Terminal : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Minus_Terminal (Get_Kind (Def)),
                     "no field Minus_Terminal");
      Set_Field11 (Def, Terminal);
   end Set_Minus_Terminal;

   function Get_Magnitude_Expression (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Magnitude_Expression (Get_Kind (Decl)),
                     "no field Magnitude_Expression");
      return Get_Field6 (Decl);
   end Get_Magnitude_Expression;

   procedure Set_Magnitude_Expression (Decl : Iir; Expr : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Magnitude_Expression (Get_Kind (Decl)),
                     "no field Magnitude_Expression");
      Set_Field6 (Decl, Expr);
   end Set_Magnitude_Expression;

   function Get_Phase_Expression (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Phase_Expression (Get_Kind (Decl)),
                     "no field Phase_Expression");
      return Get_Field7 (Decl);
   end Get_Phase_Expression;

   procedure Set_Phase_Expression (Decl : Iir; Expr : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Phase_Expression (Get_Kind (Decl)),
                     "no field Phase_Expression");
      Set_Field7 (Decl, Expr);
   end Set_Phase_Expression;

   function Get_Power_Expression (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Power_Expression (Get_Kind (Decl)),
                     "no field Power_Expression");
      return Get_Field4 (Decl);
   end Get_Power_Expression;

   procedure Set_Power_Expression (Decl : Iir; Expr : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Power_Expression (Get_Kind (Decl)),
                     "no field Power_Expression");
      Set_Field4 (Decl, Expr);
   end Set_Power_Expression;

   function Get_Simultaneous_Left (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Left (Get_Kind (Def)),
                     "no field Simultaneous_Left");
      return Get_Field5 (Def);
   end Get_Simultaneous_Left;

   procedure Set_Simultaneous_Left (Def : Iir; Expr : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Left (Get_Kind (Def)),
                     "no field Simultaneous_Left");
      Set_Field5 (Def, Expr);
   end Set_Simultaneous_Left;

   function Get_Simultaneous_Right (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Right (Get_Kind (Def)),
                     "no field Simultaneous_Right");
      return Get_Field6 (Def);
   end Get_Simultaneous_Right;

   procedure Set_Simultaneous_Right (Def : Iir; Expr : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Simultaneous_Right (Get_Kind (Def)),
                     "no field Simultaneous_Right");
      Set_Field6 (Def, Expr);
   end Set_Simultaneous_Right;

   function Get_Text_File_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Text_File_Flag (Get_Kind (Atype)),
                     "no field Text_File_Flag");
      return Get_Flag4 (Atype);
   end Get_Text_File_Flag;

   procedure Set_Text_File_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Text_File_Flag (Get_Kind (Atype)),
                     "no field Text_File_Flag");
      Set_Flag4 (Atype, Flag);
   end Set_Text_File_Flag;

   function Get_Only_Characters_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Only_Characters_Flag (Get_Kind (Atype)),
                     "no field Only_Characters_Flag");
      return Get_Flag4 (Atype);
   end Get_Only_Characters_Flag;

   procedure Set_Only_Characters_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Only_Characters_Flag (Get_Kind (Atype)),
                     "no field Only_Characters_Flag");
      Set_Flag4 (Atype, Flag);
   end Set_Only_Characters_Flag;

   function Get_Is_Character_Type (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Is_Character_Type (Get_Kind (Atype)),
                     "no field Is_Character_Type");
      return Get_Flag5 (Atype);
   end Get_Is_Character_Type;

   procedure Set_Is_Character_Type (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Is_Character_Type (Get_Kind (Atype)),
                     "no field Is_Character_Type");
      Set_Flag5 (Atype, Flag);
   end Set_Is_Character_Type;

   function Get_Nature_Staticness (Anat : Iir) return Iir_Staticness is
   begin
      pragma Assert (Anat /= Null_Iir);
      pragma Assert (Has_Nature_Staticness (Get_Kind (Anat)),
                     "no field Nature_Staticness");
      return Iir_Staticness'Val (Get_State1 (Anat));
   end Get_Nature_Staticness;

   procedure Set_Nature_Staticness (Anat : Iir; Static : Iir_Staticness) is
   begin
      pragma Assert (Anat /= Null_Iir);
      pragma Assert (Has_Nature_Staticness (Get_Kind (Anat)),
                     "no field Nature_Staticness");
      Set_State1 (Anat, Iir_Staticness'Pos (Static));
   end Set_Nature_Staticness;

   function Get_Type_Staticness (Atype : Iir) return Iir_Staticness is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Type_Staticness (Get_Kind (Atype)),
                     "no field Type_Staticness");
      return Iir_Staticness'Val (Get_State1 (Atype));
   end Get_Type_Staticness;

   procedure Set_Type_Staticness (Atype : Iir; Static : Iir_Staticness) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Type_Staticness (Get_Kind (Atype)),
                     "no field Type_Staticness");
      Set_State1 (Atype, Iir_Staticness'Pos (Static));
   end Set_Type_Staticness;

   function Get_Constraint_State (Atype : Iir) return Iir_Constraint is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Constraint_State (Get_Kind (Atype)),
                     "no field Constraint_State");
      return Iir_Constraint'Val (Get_State2 (Atype));
   end Get_Constraint_State;

   procedure Set_Constraint_State (Atype : Iir; State : Iir_Constraint) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Constraint_State (Get_Kind (Atype)),
                     "no field Constraint_State");
      Set_State2 (Atype, Iir_Constraint'Pos (State));
   end Set_Constraint_State;

   function Get_Index_Subtype_List (Decl : Iir) return Iir_Flist is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_Subtype_List (Get_Kind (Decl)),
                     "no field Index_Subtype_List");
      return Iir_To_Iir_Flist (Get_Field9 (Decl));
   end Get_Index_Subtype_List;

   procedure Set_Index_Subtype_List (Decl : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_Subtype_List (Get_Kind (Decl)),
                     "no field Index_Subtype_List");
      Set_Field9 (Decl, Iir_Flist_To_Iir (List));
   end Set_Index_Subtype_List;

   function Get_Index_Subtype_Definition_List (Def : Iir) return Iir_Flist is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Subtype_Definition_List (Get_Kind (Def)),
                     "no field Index_Subtype_Definition_List");
      return Iir_To_Iir_Flist (Get_Field6 (Def));
   end Get_Index_Subtype_Definition_List;

   procedure Set_Index_Subtype_Definition_List (Def : Iir; Idx : Iir_Flist) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Subtype_Definition_List (Get_Kind (Def)),
                     "no field Index_Subtype_Definition_List");
      Set_Field6 (Def, Iir_Flist_To_Iir (Idx));
   end Set_Index_Subtype_Definition_List;

   function Get_Element_Subtype_Indication (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype_Indication (Get_Kind (Decl)),
                     "no field Element_Subtype_Indication");
      return Get_Field2 (Decl);
   end Get_Element_Subtype_Indication;

   procedure Set_Element_Subtype_Indication (Decl : Iir; Sub_Type : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype_Indication (Get_Kind (Decl)),
                     "no field Element_Subtype_Indication");
      Set_Field2 (Decl, Sub_Type);
   end Set_Element_Subtype_Indication;

   function Get_Element_Subtype (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype (Get_Kind (Decl)),
                     "no field Element_Subtype");
      return Get_Field1 (Decl);
   end Get_Element_Subtype;

   procedure Set_Element_Subtype (Decl : Iir; Sub_Type : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subtype (Get_Kind (Decl)),
                     "no field Element_Subtype");
      Set_Field1 (Decl, Sub_Type);
   end Set_Element_Subtype;

   function Get_Element_Subnature_Indication (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subnature_Indication (Get_Kind (Decl)),
                     "no field Element_Subnature_Indication");
      return Get_Field2 (Decl);
   end Get_Element_Subnature_Indication;

   procedure Set_Element_Subnature_Indication (Decl : Iir; Sub_Nature : Iir)
   is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subnature_Indication (Get_Kind (Decl)),
                     "no field Element_Subnature_Indication");
      Set_Field2 (Decl, Sub_Nature);
   end Set_Element_Subnature_Indication;

   function Get_Element_Subnature (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subnature (Get_Kind (Decl)),
                     "no field Element_Subnature");
      return Get_Field1 (Decl);
   end Get_Element_Subnature;

   procedure Set_Element_Subnature (Decl : Iir; Sub_Nature : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Element_Subnature (Get_Kind (Decl)),
                     "no field Element_Subnature");
      Set_Field1 (Decl, Sub_Nature);
   end Set_Element_Subnature;

   function Get_Index_Constraint_List (Def : Iir) return Iir_Flist is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Constraint_List (Get_Kind (Def)),
                     "no field Index_Constraint_List");
      return Iir_To_Iir_Flist (Get_Field6 (Def));
   end Get_Index_Constraint_List;

   procedure Set_Index_Constraint_List (Def : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Index_Constraint_List (Get_Kind (Def)),
                     "no field Index_Constraint_List");
      Set_Field6 (Def, Iir_Flist_To_Iir (List));
   end Set_Index_Constraint_List;

   function Get_Array_Element_Constraint (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Array_Element_Constraint (Get_Kind (Def)),
                     "no field Array_Element_Constraint");
      return Get_Field8 (Def);
   end Get_Array_Element_Constraint;

   procedure Set_Array_Element_Constraint (Def : Iir; El : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Array_Element_Constraint (Get_Kind (Def)),
                     "no field Array_Element_Constraint");
      Set_Field8 (Def, El);
   end Set_Array_Element_Constraint;

   function Get_Has_Array_Constraint_Flag (Def : Iir) return Boolean is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Has_Array_Constraint_Flag (Get_Kind (Def)),
                     "no field Has_Array_Constraint_Flag");
      return Get_Flag5 (Def);
   end Get_Has_Array_Constraint_Flag;

   procedure Set_Has_Array_Constraint_Flag (Def : Iir; Flag : Boolean) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Has_Array_Constraint_Flag (Get_Kind (Def)),
                     "no field Has_Array_Constraint_Flag");
      Set_Flag5 (Def, Flag);
   end Set_Has_Array_Constraint_Flag;

   function Get_Has_Element_Constraint_Flag (Def : Iir) return Boolean is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Has_Element_Constraint_Flag (Get_Kind (Def)),
                     "no field Has_Element_Constraint_Flag");
      return Get_Flag6 (Def);
   end Get_Has_Element_Constraint_Flag;

   procedure Set_Has_Element_Constraint_Flag (Def : Iir; Flag : Boolean) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Has_Element_Constraint_Flag (Get_Kind (Def)),
                     "no field Has_Element_Constraint_Flag");
      Set_Flag6 (Def, Flag);
   end Set_Has_Element_Constraint_Flag;

   function Get_Elements_Declaration_List (Decl : Iir) return Iir_Flist is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Elements_Declaration_List (Get_Kind (Decl)),
                     "no field Elements_Declaration_List");
      return Iir_To_Iir_Flist (Get_Field1 (Decl));
   end Get_Elements_Declaration_List;

   procedure Set_Elements_Declaration_List (Decl : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Elements_Declaration_List (Get_Kind (Decl)),
                     "no field Elements_Declaration_List");
      Set_Field1 (Decl, Iir_Flist_To_Iir (List));
   end Set_Elements_Declaration_List;

   function Get_Owned_Elements_Chain (Atype : Iir) return Iir is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Owned_Elements_Chain (Get_Kind (Atype)),
                     "no field Owned_Elements_Chain");
      return Get_Field6 (Atype);
   end Get_Owned_Elements_Chain;

   procedure Set_Owned_Elements_Chain (Atype : Iir; Chain : Iir) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Owned_Elements_Chain (Get_Kind (Atype)),
                     "no field Owned_Elements_Chain");
      Set_Field6 (Atype, Chain);
   end Set_Owned_Elements_Chain;

   function Get_Designated_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Type (Get_Kind (Target)),
                     "no field Designated_Type");
      return Get_Field1 (Target);
   end Get_Designated_Type;

   procedure Set_Designated_Type (Target : Iir; Dtype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Type (Get_Kind (Target)),
                     "no field Designated_Type");
      Set_Field1 (Target, Dtype);
   end Set_Designated_Type;

   function Get_Designated_Subtype_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Subtype_Indication (Get_Kind (Target)),
                     "no field Designated_Subtype_Indication");
      return Get_Field5 (Target);
   end Get_Designated_Subtype_Indication;

   procedure Set_Designated_Subtype_Indication (Target : Iir; Dtype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Designated_Subtype_Indication (Get_Kind (Target)),
                     "no field Designated_Subtype_Indication");
      Set_Field5 (Target, Dtype);
   end Set_Designated_Subtype_Indication;

   function Get_Index_List (Decl : Iir) return Iir_Flist is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_List (Get_Kind (Decl)),
                     "no field Index_List");
      return Iir_To_Iir_Flist (Get_Field2 (Decl));
   end Get_Index_List;

   procedure Set_Index_List (Decl : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Index_List (Get_Kind (Decl)),
                     "no field Index_List");
      Set_Field2 (Decl, Iir_Flist_To_Iir (List));
   end Set_Index_List;

   function Get_Reference (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Reference (Get_Kind (Def)),
                     "no field Reference");
      return Get_Field2 (Def);
   end Get_Reference;

   procedure Set_Reference (Def : Iir; Ref : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Reference (Get_Kind (Def)),
                     "no field Reference");
      Set_Field2 (Def, Ref);
   end Set_Reference;

   function Get_Nature_Declarator (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Nature_Declarator (Get_Kind (Def)),
                     "no field Nature_Declarator");
      return Get_Field3 (Def);
   end Get_Nature_Declarator;

   procedure Set_Nature_Declarator (Def : Iir; Decl : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Nature_Declarator (Get_Kind (Def)),
                     "no field Nature_Declarator");
      Set_Field3 (Def, Decl);
   end Set_Nature_Declarator;

   function Get_Across_Type_Mark (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type_Mark (Get_Kind (Def)),
                     "no field Across_Type_Mark");
      return Get_Field9 (Def);
   end Get_Across_Type_Mark;

   procedure Set_Across_Type_Mark (Def : Iir; Name : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type_Mark (Get_Kind (Def)),
                     "no field Across_Type_Mark");
      Set_Field9 (Def, Name);
   end Set_Across_Type_Mark;

   function Get_Through_Type_Mark (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type_Mark (Get_Kind (Def)),
                     "no field Through_Type_Mark");
      return Get_Field10 (Def);
   end Get_Through_Type_Mark;

   procedure Set_Through_Type_Mark (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type_Mark (Get_Kind (Def)),
                     "no field Through_Type_Mark");
      Set_Field10 (Def, Atype);
   end Set_Through_Type_Mark;

   function Get_Across_Type_Definition (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type_Definition (Get_Kind (Def)),
                     "no field Across_Type_Definition");
      return Get_Field10 (Def);
   end Get_Across_Type_Definition;

   procedure Set_Across_Type_Definition (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type_Definition (Get_Kind (Def)),
                     "no field Across_Type_Definition");
      Set_Field10 (Def, Atype);
   end Set_Across_Type_Definition;

   function Get_Through_Type_Definition (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type_Definition (Get_Kind (Def)),
                     "no field Through_Type_Definition");
      return Get_Field5 (Def);
   end Get_Through_Type_Definition;

   procedure Set_Through_Type_Definition (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type_Definition (Get_Kind (Def)),
                     "no field Through_Type_Definition");
      Set_Field5 (Def, Atype);
   end Set_Through_Type_Definition;

   function Get_Across_Type (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type (Get_Kind (Def)),
                     "no field Across_Type");
      return Get_Field11 (Def);
   end Get_Across_Type;

   procedure Set_Across_Type (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Across_Type (Get_Kind (Def)),
                     "no field Across_Type");
      Set_Field11 (Def, Atype);
   end Set_Across_Type;

   function Get_Through_Type (Def : Iir) return Iir is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type (Get_Kind (Def)),
                     "no field Through_Type");
      return Get_Field12 (Def);
   end Get_Through_Type;

   procedure Set_Through_Type (Def : Iir; Atype : Iir) is
   begin
      pragma Assert (Def /= Null_Iir);
      pragma Assert (Has_Through_Type (Get_Kind (Def)),
                     "no field Through_Type");
      Set_Field12 (Def, Atype);
   end Set_Through_Type;

   function Get_Target (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Target (Get_Kind (Target)),
                     "no field Target");
      return Get_Field1 (Target);
   end Get_Target;

   procedure Set_Target (Target : Iir; Atarget : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Target (Get_Kind (Target)),
                     "no field Target");
      Set_Field1 (Target, Atarget);
   end Set_Target;

   function Get_Waveform_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Waveform_Chain (Get_Kind (Target)),
                     "no field Waveform_Chain");
      return Get_Field5 (Target);
   end Get_Waveform_Chain;

   procedure Set_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Waveform_Chain (Get_Kind (Target)),
                     "no field Waveform_Chain");
      Set_Field5 (Target, Chain);
   end Set_Waveform_Chain;

   function Get_Guard (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard (Get_Kind (Target)),
                     "no field Guard");
      return Get_Field8 (Target);
   end Get_Guard;

   procedure Set_Guard (Target : Iir; Guard : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard (Get_Kind (Target)),
                     "no field Guard");
      Set_Field8 (Target, Guard);
   end Set_Guard;

   function Get_Delay_Mechanism (Target : Iir) return Iir_Delay_Mechanism is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Delay_Mechanism (Get_Kind (Target)),
                     "no field Delay_Mechanism");
      return Boolean_To_Iir_Delay_Mechanism (Get_Flag1 (Target));
   end Get_Delay_Mechanism;

   procedure Set_Delay_Mechanism (Target : Iir; Kind : Iir_Delay_Mechanism) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Delay_Mechanism (Get_Kind (Target)),
                     "no field Delay_Mechanism");
      Set_Flag1 (Target, Iir_Delay_Mechanism_To_Boolean (Kind));
   end Set_Delay_Mechanism;

   function Get_Reject_Time_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Reject_Time_Expression (Get_Kind (Target)),
                     "no field Reject_Time_Expression");
      return Get_Field4 (Target);
   end Get_Reject_Time_Expression;

   procedure Set_Reject_Time_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Reject_Time_Expression (Get_Kind (Target)),
                     "no field Reject_Time_Expression");
      Set_Field4 (Target, Expr);
   end Set_Reject_Time_Expression;

   function Get_Force_Mode (Stmt : Iir) return Iir_Force_Mode is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Force_Mode (Get_Kind (Stmt)),
                     "no field Force_Mode");
      return Boolean_To_Iir_Force_Mode (Get_Flag1 (Stmt));
   end Get_Force_Mode;

   procedure Set_Force_Mode (Stmt : Iir; Mode : Iir_Force_Mode) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Force_Mode (Get_Kind (Stmt)),
                     "no field Force_Mode");
      Set_Flag1 (Stmt, Iir_Force_Mode_To_Boolean (Mode));
   end Set_Force_Mode;

   function Get_Has_Force_Mode (Stmt : Iir) return Boolean is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Has_Force_Mode (Get_Kind (Stmt)),
                     "no field Has_Force_Mode");
      return Get_Flag2 (Stmt);
   end Get_Has_Force_Mode;

   procedure Set_Has_Force_Mode (Stmt : Iir; Flag : Boolean) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Has_Force_Mode (Get_Kind (Stmt)),
                     "no field Has_Force_Mode");
      Set_Flag2 (Stmt, Flag);
   end Set_Has_Force_Mode;

   function Get_Sensitivity_List (Wait : Iir) return Iir_List is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Sensitivity_List (Get_Kind (Wait)),
                     "no field Sensitivity_List");
      return Iir_To_Iir_List (Get_Field6 (Wait));
   end Get_Sensitivity_List;

   procedure Set_Sensitivity_List (Wait : Iir; List : Iir_List) is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Sensitivity_List (Get_Kind (Wait)),
                     "no field Sensitivity_List");
      Set_Field6 (Wait, Iir_List_To_Iir (List));
   end Set_Sensitivity_List;

   function Get_Process_Origin (Proc : Iir) return Iir is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Process_Origin (Get_Kind (Proc)),
                     "no field Process_Origin");
      return Get_Field8 (Proc);
   end Get_Process_Origin;

   procedure Set_Process_Origin (Proc : Iir; Orig : Iir) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Process_Origin (Get_Kind (Proc)),
                     "no field Process_Origin");
      Set_Field8 (Proc, Orig);
   end Set_Process_Origin;

   function Get_Package_Origin (Pkg : Iir) return Iir is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Origin (Get_Kind (Pkg)),
                     "no field Package_Origin");
      return Get_Field7 (Pkg);
   end Get_Package_Origin;

   procedure Set_Package_Origin (Pkg : Iir; Orig : Iir) is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Origin (Get_Kind (Pkg)),
                     "no field Package_Origin");
      Set_Field7 (Pkg, Orig);
   end Set_Package_Origin;

   function Get_Condition_Clause (Wait : Iir_Wait_Statement) return Iir is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Condition_Clause (Get_Kind (Wait)),
                     "no field Condition_Clause");
      return Get_Field5 (Wait);
   end Get_Condition_Clause;

   procedure Set_Condition_Clause (Wait : Iir_Wait_Statement; Cond : Iir) is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Condition_Clause (Get_Kind (Wait)),
                     "no field Condition_Clause");
      Set_Field5 (Wait, Cond);
   end Set_Condition_Clause;

   function Get_Break_Element (Stmt : Iir) return Iir is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Break_Element (Get_Kind (Stmt)),
                     "no field Break_Element");
      return Get_Field4 (Stmt);
   end Get_Break_Element;

   procedure Set_Break_Element (Stmt : Iir; El : Iir) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Break_Element (Get_Kind (Stmt)),
                     "no field Break_Element");
      Set_Field4 (Stmt, El);
   end Set_Break_Element;

   function Get_Selector_Quantity (Stmt : Iir) return Iir is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Selector_Quantity (Get_Kind (Stmt)),
                     "no field Selector_Quantity");
      return Get_Field3 (Stmt);
   end Get_Selector_Quantity;

   procedure Set_Selector_Quantity (Stmt : Iir; Sel : Iir) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Selector_Quantity (Get_Kind (Stmt)),
                     "no field Selector_Quantity");
      Set_Field3 (Stmt, Sel);
   end Set_Selector_Quantity;

   function Get_Break_Quantity (Stmt : Iir) return Iir is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Break_Quantity (Get_Kind (Stmt)),
                     "no field Break_Quantity");
      return Get_Field4 (Stmt);
   end Get_Break_Quantity;

   procedure Set_Break_Quantity (Stmt : Iir; Sel : Iir) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Break_Quantity (Get_Kind (Stmt)),
                     "no field Break_Quantity");
      Set_Field4 (Stmt, Sel);
   end Set_Break_Quantity;

   function Get_Timeout_Clause (Wait : Iir_Wait_Statement) return Iir is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Timeout_Clause (Get_Kind (Wait)),
                     "no field Timeout_Clause");
      return Get_Field1 (Wait);
   end Get_Timeout_Clause;

   procedure Set_Timeout_Clause (Wait : Iir_Wait_Statement; Timeout : Iir) is
   begin
      pragma Assert (Wait /= Null_Iir);
      pragma Assert (Has_Timeout_Clause (Get_Kind (Wait)),
                     "no field Timeout_Clause");
      Set_Field1 (Wait, Timeout);
   end Set_Timeout_Clause;

   function Get_Postponed_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Postponed_Flag (Get_Kind (Target)),
                     "no field Postponed_Flag");
      return Get_Flag3 (Target);
   end Get_Postponed_Flag;

   procedure Set_Postponed_Flag (Target : Iir; Value : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Postponed_Flag (Get_Kind (Target)),
                     "no field Postponed_Flag");
      Set_Flag3 (Target, Value);
   end Set_Postponed_Flag;

   function Get_Callees_List (Proc : Iir) return Iir_List is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Callees_List (Get_Kind (Proc)),
                     "no field Callees_List");
      return Iir_To_Iir_List (Get_Field7 (Proc));
   end Get_Callees_List;

   procedure Set_Callees_List (Proc : Iir; List : Iir_List) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Callees_List (Get_Kind (Proc)),
                     "no field Callees_List");
      Set_Field7 (Proc, Iir_List_To_Iir (List));
   end Set_Callees_List;

   function Get_Passive_Flag (Proc : Iir) return Boolean is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Passive_Flag (Get_Kind (Proc)),
                     "no field Passive_Flag");
      return Get_Flag2 (Proc);
   end Get_Passive_Flag;

   procedure Set_Passive_Flag (Proc : Iir; Flag : Boolean) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Passive_Flag (Get_Kind (Proc)),
                     "no field Passive_Flag");
      Set_Flag2 (Proc, Flag);
   end Set_Passive_Flag;

   function Get_Resolution_Function_Flag (Func : Iir) return Boolean is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Resolution_Function_Flag (Get_Kind (Func)),
                     "no field Resolution_Function_Flag");
      return Get_Flag13 (Func);
   end Get_Resolution_Function_Flag;

   procedure Set_Resolution_Function_Flag (Func : Iir; Flag : Boolean) is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Resolution_Function_Flag (Get_Kind (Func)),
                     "no field Resolution_Function_Flag");
      Set_Flag13 (Func, Flag);
   end Set_Resolution_Function_Flag;

   function Get_Wait_State (Proc : Iir) return Tri_State_Type is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Wait_State (Get_Kind (Proc)),
                     "no field Wait_State");
      return Tri_State_Type'Val (Get_State1 (Proc));
   end Get_Wait_State;

   procedure Set_Wait_State (Proc : Iir; State : Tri_State_Type) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Wait_State (Get_Kind (Proc)),
                     "no field Wait_State");
      Set_State1 (Proc, Tri_State_Type'Pos (State));
   end Set_Wait_State;

   function Get_All_Sensitized_State (Proc : Iir) return Iir_All_Sensitized is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_All_Sensitized_State (Get_Kind (Proc)),
                     "no field All_Sensitized_State");
      return Iir_All_Sensitized'Val (Get_State3 (Proc));
   end Get_All_Sensitized_State;

   procedure Set_All_Sensitized_State (Proc : Iir; State : Iir_All_Sensitized)
   is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_All_Sensitized_State (Get_Kind (Proc)),
                     "no field All_Sensitized_State");
      Set_State3 (Proc, Iir_All_Sensitized'Pos (State));
   end Set_All_Sensitized_State;

   function Get_Seen_Flag (Proc : Iir) return Boolean is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Seen_Flag (Get_Kind (Proc)),
                     "no field Seen_Flag");
      return Get_Flag1 (Proc);
   end Get_Seen_Flag;

   procedure Set_Seen_Flag (Proc : Iir; Flag : Boolean) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Seen_Flag (Get_Kind (Proc)),
                     "no field Seen_Flag");
      Set_Flag1 (Proc, Flag);
   end Set_Seen_Flag;

   function Get_Pure_Flag (Func : Iir) return Boolean is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Pure_Flag (Get_Kind (Func)),
                     "no field Pure_Flag");
      return Get_Flag2 (Func);
   end Get_Pure_Flag;

   procedure Set_Pure_Flag (Func : Iir; Flag : Boolean) is
   begin
      pragma Assert (Func /= Null_Iir);
      pragma Assert (Has_Pure_Flag (Get_Kind (Func)),
                     "no field Pure_Flag");
      Set_Flag2 (Func, Flag);
   end Set_Pure_Flag;

   function Get_Foreign_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Foreign_Flag (Get_Kind (Decl)),
                     "no field Foreign_Flag");
      return Get_Flag3 (Decl);
   end Get_Foreign_Flag;

   procedure Set_Foreign_Flag (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Foreign_Flag (Get_Kind (Decl)),
                     "no field Foreign_Flag");
      Set_Flag3 (Decl, Flag);
   end Set_Foreign_Flag;

   function Get_Resolved_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Resolved_Flag (Get_Kind (Atype)),
                     "no field Resolved_Flag");
      return Get_Flag1 (Atype);
   end Get_Resolved_Flag;

   procedure Set_Resolved_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Resolved_Flag (Get_Kind (Atype)),
                     "no field Resolved_Flag");
      Set_Flag1 (Atype, Flag);
   end Set_Resolved_Flag;

   function Get_Signal_Type_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Signal_Type_Flag (Get_Kind (Atype)),
                     "no field Signal_Type_Flag");
      return Get_Flag2 (Atype);
   end Get_Signal_Type_Flag;

   procedure Set_Signal_Type_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Signal_Type_Flag (Get_Kind (Atype)),
                     "no field Signal_Type_Flag");
      Set_Flag2 (Atype, Flag);
   end Set_Signal_Type_Flag;

   function Get_Has_Signal_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Has_Signal_Flag (Get_Kind (Atype)),
                     "no field Has_Signal_Flag");
      return Get_Flag3 (Atype);
   end Get_Has_Signal_Flag;

   procedure Set_Has_Signal_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Has_Signal_Flag (Get_Kind (Atype)),
                     "no field Has_Signal_Flag");
      Set_Flag3 (Atype, Flag);
   end Set_Has_Signal_Flag;

   function Get_Purity_State (Proc : Iir) return Iir_Pure_State is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Purity_State (Get_Kind (Proc)),
                     "no field Purity_State");
      return Iir_Pure_State'Val (Get_State2 (Proc));
   end Get_Purity_State;

   procedure Set_Purity_State (Proc : Iir; State : Iir_Pure_State) is
   begin
      pragma Assert (Proc /= Null_Iir);
      pragma Assert (Has_Purity_State (Get_Kind (Proc)),
                     "no field Purity_State");
      Set_State2 (Proc, Iir_Pure_State'Pos (State));
   end Set_Purity_State;

   function Get_Elab_Flag (Design : Iir) return Boolean is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Elab_Flag (Get_Kind (Design)),
                     "no field Elab_Flag");
      return Get_Flag3 (Design);
   end Get_Elab_Flag;

   procedure Set_Elab_Flag (Design : Iir; Flag : Boolean) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Elab_Flag (Get_Kind (Design)),
                     "no field Elab_Flag");
      Set_Flag3 (Design, Flag);
   end Set_Elab_Flag;

   function Get_Vendor_Library_Flag (Lib : Iir) return Boolean is
   begin
      pragma Assert (Lib /= Null_Iir);
      pragma Assert (Has_Vendor_Library_Flag (Get_Kind (Lib)),
                     "no field Vendor_Library_Flag");
      return Get_Flag1 (Lib);
   end Get_Vendor_Library_Flag;

   procedure Set_Vendor_Library_Flag (Lib : Iir; Flag : Boolean) is
   begin
      pragma Assert (Lib /= Null_Iir);
      pragma Assert (Has_Vendor_Library_Flag (Get_Kind (Lib)),
                     "no field Vendor_Library_Flag");
      Set_Flag1 (Lib, Flag);
   end Set_Vendor_Library_Flag;

   function Get_Configuration_Mark_Flag (Design : Iir) return Boolean is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Mark_Flag (Get_Kind (Design)),
                     "no field Configuration_Mark_Flag");
      return Get_Flag4 (Design);
   end Get_Configuration_Mark_Flag;

   procedure Set_Configuration_Mark_Flag (Design : Iir; Flag : Boolean) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Mark_Flag (Get_Kind (Design)),
                     "no field Configuration_Mark_Flag");
      Set_Flag4 (Design, Flag);
   end Set_Configuration_Mark_Flag;

   function Get_Configuration_Done_Flag (Design : Iir) return Boolean is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Done_Flag (Get_Kind (Design)),
                     "no field Configuration_Done_Flag");
      return Get_Flag5 (Design);
   end Get_Configuration_Done_Flag;

   procedure Set_Configuration_Done_Flag (Design : Iir; Flag : Boolean) is
   begin
      pragma Assert (Design /= Null_Iir);
      pragma Assert (Has_Configuration_Done_Flag (Get_Kind (Design)),
                     "no field Configuration_Done_Flag");
      Set_Flag5 (Design, Flag);
   end Set_Configuration_Done_Flag;

   function Get_Index_Constraint_Flag (Atype : Iir) return Boolean is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Index_Constraint_Flag (Get_Kind (Atype)),
                     "no field Index_Constraint_Flag");
      return Get_Flag4 (Atype);
   end Get_Index_Constraint_Flag;

   procedure Set_Index_Constraint_Flag (Atype : Iir; Flag : Boolean) is
   begin
      pragma Assert (Atype /= Null_Iir);
      pragma Assert (Has_Index_Constraint_Flag (Get_Kind (Atype)),
                     "no field Index_Constraint_Flag");
      Set_Flag4 (Atype, Flag);
   end Set_Index_Constraint_Flag;

   function Get_Hide_Implicit_Flag (Subprg : Iir) return Boolean is
   begin
      pragma Assert (Subprg /= Null_Iir);
      pragma Assert (Has_Hide_Implicit_Flag (Get_Kind (Subprg)),
                     "no field Hide_Implicit_Flag");
      return Get_Flag12 (Subprg);
   end Get_Hide_Implicit_Flag;

   procedure Set_Hide_Implicit_Flag (Subprg : Iir; Flag : Boolean) is
   begin
      pragma Assert (Subprg /= Null_Iir);
      pragma Assert (Has_Hide_Implicit_Flag (Get_Kind (Subprg)),
                     "no field Hide_Implicit_Flag");
      Set_Flag12 (Subprg, Flag);
   end Set_Hide_Implicit_Flag;

   function Get_Assertion_Condition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Assertion_Condition (Get_Kind (Target)),
                     "no field Assertion_Condition");
      return Get_Field1 (Target);
   end Get_Assertion_Condition;

   procedure Set_Assertion_Condition (Target : Iir; Cond : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Assertion_Condition (Get_Kind (Target)),
                     "no field Assertion_Condition");
      Set_Field1 (Target, Cond);
   end Set_Assertion_Condition;

   function Get_Report_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Report_Expression (Get_Kind (Target)),
                     "no field Report_Expression");
      return Get_Field5 (Target);
   end Get_Report_Expression;

   procedure Set_Report_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Report_Expression (Get_Kind (Target)),
                     "no field Report_Expression");
      Set_Field5 (Target, Expr);
   end Set_Report_Expression;

   function Get_Severity_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Severity_Expression (Get_Kind (Target)),
                     "no field Severity_Expression");
      return Get_Field4 (Target);
   end Get_Severity_Expression;

   procedure Set_Severity_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Severity_Expression (Get_Kind (Target)),
                     "no field Severity_Expression");
      Set_Field4 (Target, Expr);
   end Set_Severity_Expression;

   function Get_Instantiated_Unit (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiated_Unit (Get_Kind (Target)),
                     "no field Instantiated_Unit");
      return Get_Field1 (Target);
   end Get_Instantiated_Unit;

   procedure Set_Instantiated_Unit (Target : Iir; Unit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiated_Unit (Get_Kind (Target)),
                     "no field Instantiated_Unit");
      Set_Field1 (Target, Unit);
   end Set_Instantiated_Unit;

   function Get_Instantiated_Header (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiated_Header (Get_Kind (Target)),
                     "no field Instantiated_Header");
      return Get_Field4 (Target);
   end Get_Instantiated_Header;

   procedure Set_Instantiated_Header (Target : Iir; Hdr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiated_Header (Get_Kind (Target)),
                     "no field Instantiated_Header");
      Set_Field4 (Target, Hdr);
   end Set_Instantiated_Header;

   function Get_Generic_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Generic_Map_Aspect_Chain");
      return Get_Field8 (Target);
   end Get_Generic_Map_Aspect_Chain;

   procedure Set_Generic_Map_Aspect_Chain (Target : Iir; Generics : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generic_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Generic_Map_Aspect_Chain");
      Set_Field8 (Target, Generics);
   end Set_Generic_Map_Aspect_Chain;

   function Get_Port_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Port_Map_Aspect_Chain");
      return Get_Field9 (Target);
   end Get_Port_Map_Aspect_Chain;

   procedure Set_Port_Map_Aspect_Chain (Target : Iir; Port : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Port_Map_Aspect_Chain (Get_Kind (Target)),
                     "no field Port_Map_Aspect_Chain");
      Set_Field9 (Target, Port);
   end Set_Port_Map_Aspect_Chain;

   function Get_Configuration_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Name (Get_Kind (Target)),
                     "no field Configuration_Name");
      return Get_Field1 (Target);
   end Get_Configuration_Name;

   procedure Set_Configuration_Name (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Name (Get_Kind (Target)),
                     "no field Configuration_Name");
      Set_Field1 (Target, Conf);
   end Set_Configuration_Name;

   function Get_Component_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Configuration (Get_Kind (Target)),
                     "no field Component_Configuration");
      return Get_Field6 (Target);
   end Get_Component_Configuration;

   procedure Set_Component_Configuration (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Configuration (Get_Kind (Target)),
                     "no field Component_Configuration");
      Set_Field6 (Target, Conf);
   end Set_Component_Configuration;

   function Get_Configuration_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Specification (Get_Kind (Target)),
                     "no field Configuration_Specification");
      return Get_Field7 (Target);
   end Get_Configuration_Specification;

   procedure Set_Configuration_Specification (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Configuration_Specification (Get_Kind (Target)),
                     "no field Configuration_Specification");
      Set_Field7 (Target, Conf);
   end Set_Configuration_Specification;

   function Get_Default_Binding_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Binding_Indication (Get_Kind (Target)),
                     "no field Default_Binding_Indication");
      return Get_Field5 (Target);
   end Get_Default_Binding_Indication;

   procedure Set_Default_Binding_Indication (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Binding_Indication (Get_Kind (Target)),
                     "no field Default_Binding_Indication");
      Set_Field5 (Target, Conf);
   end Set_Default_Binding_Indication;

   function Get_Default_Configuration_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Configuration_Declaration (Get_Kind (Target)),
                     "no field Default_Configuration_Declaration");
      return Get_Field6 (Target);
   end Get_Default_Configuration_Declaration;

   procedure Set_Default_Configuration_Declaration (Target : Iir; Conf : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Configuration_Declaration (Get_Kind (Target)),
                     "no field Default_Configuration_Declaration");
      Set_Field6 (Target, Conf);
   end Set_Default_Configuration_Declaration;

   function Get_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expression (Get_Kind (Target)),
                     "no field Expression");
      return Get_Field5 (Target);
   end Get_Expression;

   procedure Set_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expression (Get_Kind (Target)),
                     "no field Expression");
      Set_Field5 (Target, Expr);
   end Set_Expression;

   function Get_Conditional_Expression_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Expression_Chain (Get_Kind (Target)),
                     "no field Conditional_Expression_Chain");
      return Get_Field5 (Target);
   end Get_Conditional_Expression_Chain;

   procedure Set_Conditional_Expression_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Expression_Chain (Get_Kind (Target)),
                     "no field Conditional_Expression_Chain");
      Set_Field5 (Target, Chain);
   end Set_Conditional_Expression_Chain;

   function Get_Allocator_Designated_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Allocator_Designated_Type (Get_Kind (Target)),
                     "no field Allocator_Designated_Type");
      return Get_Field2 (Target);
   end Get_Allocator_Designated_Type;

   procedure Set_Allocator_Designated_Type (Target : Iir; A_Type : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Allocator_Designated_Type (Get_Kind (Target)),
                     "no field Allocator_Designated_Type");
      Set_Field2 (Target, A_Type);
   end Set_Allocator_Designated_Type;

   function Get_Selected_Waveform_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Waveform_Chain (Get_Kind (Target)),
                     "no field Selected_Waveform_Chain");
      return Get_Field7 (Target);
   end Get_Selected_Waveform_Chain;

   procedure Set_Selected_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Selected_Waveform_Chain (Get_Kind (Target)),
                     "no field Selected_Waveform_Chain");
      Set_Field7 (Target, Chain);
   end Set_Selected_Waveform_Chain;

   function Get_Conditional_Waveform_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Waveform_Chain (Get_Kind (Target)),
                     "no field Conditional_Waveform_Chain");
      return Get_Field5 (Target);
   end Get_Conditional_Waveform_Chain;

   procedure Set_Conditional_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Conditional_Waveform_Chain (Get_Kind (Target)),
                     "no field Conditional_Waveform_Chain");
      Set_Field5 (Target, Chain);
   end Set_Conditional_Waveform_Chain;

   function Get_Guard_Expression (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Expression (Get_Kind (Target)),
                     "no field Guard_Expression");
      return Get_Field2 (Target);
   end Get_Guard_Expression;

   procedure Set_Guard_Expression (Target : Iir; Expr : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Expression (Get_Kind (Target)),
                     "no field Guard_Expression");
      Set_Field2 (Target, Expr);
   end Set_Guard_Expression;

   function Get_Guard_Decl (Target : Iir_Block_Statement) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Decl (Get_Kind (Target)),
                     "no field Guard_Decl");
      return Get_Field8 (Target);
   end Get_Guard_Decl;

   procedure Set_Guard_Decl (Target : Iir_Block_Statement; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Guard_Decl (Get_Kind (Target)),
                     "no field Guard_Decl");
      Set_Field8 (Target, Decl);
   end Set_Guard_Decl;

   function Get_Guard_Sensitivity_List (Guard : Iir) return Iir_List is
   begin
      pragma Assert (Guard /= Null_Iir);
      pragma Assert (Has_Guard_Sensitivity_List (Get_Kind (Guard)),
                     "no field Guard_Sensitivity_List");
      return Iir_To_Iir_List (Get_Field4 (Guard));
   end Get_Guard_Sensitivity_List;

   procedure Set_Guard_Sensitivity_List (Guard : Iir; List : Iir_List) is
   begin
      pragma Assert (Guard /= Null_Iir);
      pragma Assert (Has_Guard_Sensitivity_List (Get_Kind (Guard)),
                     "no field Guard_Sensitivity_List");
      Set_Field4 (Guard, Iir_List_To_Iir (List));
   end Set_Guard_Sensitivity_List;

   function Get_Attribute_Implicit_Chain (Decl : Iir) return Iir is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Attribute_Implicit_Chain (Get_Kind (Decl)),
                     "no field Attribute_Implicit_Chain");
      return Get_Field3 (Decl);
   end Get_Attribute_Implicit_Chain;

   procedure Set_Attribute_Implicit_Chain (Decl : Iir; Chain : Iir) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Attribute_Implicit_Chain (Get_Kind (Decl)),
                     "no field Attribute_Implicit_Chain");
      Set_Field3 (Decl, Chain);
   end Set_Attribute_Implicit_Chain;

   function Get_Block_Block_Configuration (Block : Iir) return Iir is
   begin
      pragma Assert (Block /= Null_Iir);
      pragma Assert (Has_Block_Block_Configuration (Get_Kind (Block)),
                     "no field Block_Block_Configuration");
      return Get_Field6 (Block);
   end Get_Block_Block_Configuration;

   procedure Set_Block_Block_Configuration (Block : Iir; Conf : Iir) is
   begin
      pragma Assert (Block /= Null_Iir);
      pragma Assert (Has_Block_Block_Configuration (Get_Kind (Block)),
                     "no field Block_Block_Configuration");
      Set_Field6 (Block, Conf);
   end Set_Block_Block_Configuration;

   function Get_Package_Header (Pkg : Iir) return Iir is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Header (Get_Kind (Pkg)),
                     "no field Package_Header");
      return Get_Field6 (Pkg);
   end Get_Package_Header;

   procedure Set_Package_Header (Pkg : Iir; Header : Iir) is
   begin
      pragma Assert (Pkg /= Null_Iir);
      pragma Assert (Has_Package_Header (Get_Kind (Pkg)),
                     "no field Package_Header");
      Set_Field6 (Pkg, Header);
   end Set_Package_Header;

   function Get_Block_Header (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Header (Get_Kind (Target)),
                     "no field Block_Header");
      return Get_Field7 (Target);
   end Get_Block_Header;

   procedure Set_Block_Header (Target : Iir; Header : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Block_Header (Get_Kind (Target)),
                     "no field Block_Header");
      Set_Field7 (Target, Header);
   end Set_Block_Header;

   function Get_Uninstantiated_Package_Name (Inst : Iir) return Iir is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Name (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Name");
      return Get_Field7 (Inst);
   end Get_Uninstantiated_Package_Name;

   procedure Set_Uninstantiated_Package_Name (Inst : Iir; Name : Iir) is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Name (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Name");
      Set_Field7 (Inst, Name);
   end Set_Uninstantiated_Package_Name;

   function Get_Uninstantiated_Package_Decl (Inst : Iir) return Iir is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Decl (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Decl");
      return Get_Field9 (Inst);
   end Get_Uninstantiated_Package_Decl;

   procedure Set_Uninstantiated_Package_Decl (Inst : Iir; Pkg : Iir) is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Uninstantiated_Package_Decl (Get_Kind (Inst)),
                     "no field Uninstantiated_Package_Decl");
      Set_Field9 (Inst, Pkg);
   end Set_Uninstantiated_Package_Decl;

   function Get_Instance_Source_File (Inst : Iir) return Source_File_Entry is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Instance_Source_File (Get_Kind (Inst)),
                     "no field Instance_Source_File");
      return Iir_To_Source_File_Entry (Get_Field10 (Inst));
   end Get_Instance_Source_File;

   procedure Set_Instance_Source_File (Inst : Iir; File : Source_File_Entry)
   is
   begin
      pragma Assert (Inst /= Null_Iir);
      pragma Assert (Has_Instance_Source_File (Get_Kind (Inst)),
                     "no field Instance_Source_File");
      Set_Field10 (Inst, Source_File_Entry_To_Iir (File));
   end Set_Instance_Source_File;

   function Get_Generate_Block_Configuration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Block_Configuration (Get_Kind (Target)),
                     "no field Generate_Block_Configuration");
      return Get_Field2 (Target);
   end Get_Generate_Block_Configuration;

   procedure Set_Generate_Block_Configuration (Target : Iir; Conf : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Block_Configuration (Get_Kind (Target)),
                     "no field Generate_Block_Configuration");
      Set_Field2 (Target, Conf);
   end Set_Generate_Block_Configuration;

   function Get_Generate_Statement_Body (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Statement_Body (Get_Kind (Target)),
                     "no field Generate_Statement_Body");
      return Get_Field4 (Target);
   end Get_Generate_Statement_Body;

   procedure Set_Generate_Statement_Body (Target : Iir; Bod : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Statement_Body (Get_Kind (Target)),
                     "no field Generate_Statement_Body");
      Set_Field4 (Target, Bod);
   end Set_Generate_Statement_Body;

   function Get_Alternative_Label (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Alternative_Label (Get_Kind (Target)),
                     "no field Alternative_Label");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Alternative_Label;

   procedure Set_Alternative_Label (Target : Iir; Label : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Alternative_Label (Get_Kind (Target)),
                     "no field Alternative_Label");
      Set_Field3 (Target, Name_Id_To_Iir (Label));
   end Set_Alternative_Label;

   function Get_Generate_Else_Clause (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Else_Clause (Get_Kind (Target)),
                     "no field Generate_Else_Clause");
      return Get_Field5 (Target);
   end Get_Generate_Else_Clause;

   procedure Set_Generate_Else_Clause (Target : Iir; Clause : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Generate_Else_Clause (Get_Kind (Target)),
                     "no field Generate_Else_Clause");
      Set_Field5 (Target, Clause);
   end Set_Generate_Else_Clause;

   function Get_Condition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Condition (Get_Kind (Target)),
                     "no field Condition");
      return Get_Field1 (Target);
   end Get_Condition;

   procedure Set_Condition (Target : Iir; Condition : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Condition (Get_Kind (Target)),
                     "no field Condition");
      Set_Field1 (Target, Condition);
   end Set_Condition;

   function Get_Else_Clause (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Else_Clause (Get_Kind (Target)),
                     "no field Else_Clause");
      return Get_Field5 (Target);
   end Get_Else_Clause;

   procedure Set_Else_Clause (Target : Iir; Clause : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Else_Clause (Get_Kind (Target)),
                     "no field Else_Clause");
      Set_Field5 (Target, Clause);
   end Set_Else_Clause;

   function Get_Parameter_Specification (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Specification (Get_Kind (Target)),
                     "no field Parameter_Specification");
      return Get_Field1 (Target);
   end Get_Parameter_Specification;

   procedure Set_Parameter_Specification (Target : Iir; Param : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Specification (Get_Kind (Target)),
                     "no field Parameter_Specification");
      Set_Field1 (Target, Param);
   end Set_Parameter_Specification;

   function Get_Parent (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parent (Get_Kind (Target)),
                     "no field Parent");
      return Get_Field0 (Target);
   end Get_Parent;

   procedure Set_Parent (Target : Iir; Parent : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parent (Get_Kind (Target)),
                     "no field Parent");
      Set_Field0 (Target, Parent);
   end Set_Parent;

   function Get_Loop_Label (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Loop_Label (Get_Kind (Target)),
                     "no field Loop_Label");
      return Get_Field5 (Target);
   end Get_Loop_Label;

   procedure Set_Loop_Label (Target : Iir; Stmt : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Loop_Label (Get_Kind (Target)),
                     "no field Loop_Label");
      Set_Field5 (Target, Stmt);
   end Set_Loop_Label;

   function Get_Exit_Flag (Stmt : Iir) return Boolean is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Exit_Flag (Get_Kind (Stmt)),
                     "no field Exit_Flag");
      return Get_Flag1 (Stmt);
   end Get_Exit_Flag;

   procedure Set_Exit_Flag (Stmt : Iir; Flag : Boolean) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Exit_Flag (Get_Kind (Stmt)),
                     "no field Exit_Flag");
      Set_Flag1 (Stmt, Flag);
   end Set_Exit_Flag;

   function Get_Next_Flag (Stmt : Iir) return Boolean is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Next_Flag (Get_Kind (Stmt)),
                     "no field Next_Flag");
      return Get_Flag2 (Stmt);
   end Get_Next_Flag;

   procedure Set_Next_Flag (Stmt : Iir; Flag : Boolean) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Next_Flag (Get_Kind (Stmt)),
                     "no field Next_Flag");
      Set_Flag2 (Stmt, Flag);
   end Set_Next_Flag;

   function Get_Component_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Name (Get_Kind (Target)),
                     "no field Component_Name");
      return Get_Field5 (Target);
   end Get_Component_Name;

   procedure Set_Component_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Component_Name (Get_Kind (Target)),
                     "no field Component_Name");
      Set_Field5 (Target, Name);
   end Set_Component_Name;

   function Get_Instantiation_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiation_List (Get_Kind (Target)),
                     "no field Instantiation_List");
      return Iir_To_Iir_Flist (Get_Field1 (Target));
   end Get_Instantiation_List;

   procedure Set_Instantiation_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Instantiation_List (Get_Kind (Target)),
                     "no field Instantiation_List");
      Set_Field1 (Target, Iir_Flist_To_Iir (List));
   end Set_Instantiation_List;

   function Get_Entity_Aspect (Target : Iir_Binding_Indication) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Aspect (Get_Kind (Target)),
                     "no field Entity_Aspect");
      return Get_Field3 (Target);
   end Get_Entity_Aspect;

   procedure Set_Entity_Aspect (Target : Iir_Binding_Indication; Entity : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Entity_Aspect (Get_Kind (Target)),
                     "no field Entity_Aspect");
      Set_Field3 (Target, Entity);
   end Set_Entity_Aspect;

   function Get_Default_Entity_Aspect (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Entity_Aspect (Get_Kind (Target)),
                     "no field Default_Entity_Aspect");
      return Get_Field1 (Target);
   end Get_Default_Entity_Aspect;

   procedure Set_Default_Entity_Aspect (Target : Iir; Aspect : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Default_Entity_Aspect (Get_Kind (Target)),
                     "no field Default_Entity_Aspect");
      Set_Field1 (Target, Aspect);
   end Set_Default_Entity_Aspect;

   function Get_Binding_Indication (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Binding_Indication (Get_Kind (Target)),
                     "no field Binding_Indication");
      return Get_Field3 (Target);
   end Get_Binding_Indication;

   procedure Set_Binding_Indication (Target : Iir; Binding : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Binding_Indication (Get_Kind (Target)),
                     "no field Binding_Indication");
      Set_Field3 (Target, Binding);
   end Set_Binding_Indication;

   function Get_Named_Entity (Name : Iir) return Iir is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Named_Entity (Get_Kind (Name)),
                     "no field Named_Entity");
      return Get_Field4 (Name);
   end Get_Named_Entity;

   procedure Set_Named_Entity (Name : Iir; Val : Iir) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Named_Entity (Get_Kind (Name)),
                     "no field Named_Entity");
      Set_Field4 (Name, Val);
   end Set_Named_Entity;

   function Get_Referenced_Name (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Referenced_Name (Get_Kind (N)),
                     "no field Referenced_Name");
      return Get_Field2 (N);
   end Get_Referenced_Name;

   procedure Set_Referenced_Name (N : Iir; Name : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Referenced_Name (Get_Kind (N)),
                     "no field Referenced_Name");
      Set_Field2 (N, Name);
   end Set_Referenced_Name;

   function Get_Expr_Staticness (Target : Iir) return Iir_Staticness is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expr_Staticness (Get_Kind (Target)),
                     "no field Expr_Staticness");
      return Iir_Staticness'Val (Get_State1 (Target));
   end Get_Expr_Staticness;

   procedure Set_Expr_Staticness (Target : Iir; Static : Iir_Staticness) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Expr_Staticness (Get_Kind (Target)),
                     "no field Expr_Staticness");
      Set_State1 (Target, Iir_Staticness'Pos (Static));
   end Set_Expr_Staticness;

   type Scalar_Size_Conv is record
      Flag6: Boolean;
      Flag7: Boolean;
   end record;
   pragma Pack (Scalar_Size_Conv);
   pragma Assert (Scalar_Size_Conv'Size = Scalar_Size'Size);

   function Get_Scalar_Size (N : Iir) return Scalar_Size
   is
      function To_Scalar_Size is new Ada.Unchecked_Conversion
         (Scalar_Size_Conv, Scalar_Size);
      Conv : Scalar_Size_Conv;
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Scalar_Size (Get_Kind (N)),
                     "no field Scalar_Size");
      Conv.Flag6 := Get_Flag6 (N);
      Conv.Flag7 := Get_Flag7 (N);
      return To_Scalar_Size (Conv);
   end Get_Scalar_Size;

   procedure Set_Scalar_Size (N : Iir; Sz : Scalar_Size)
   is
      function To_Scalar_Size_Conv is new Ada.Unchecked_Conversion
         (Scalar_Size, Scalar_Size_Conv);
      Conv : Scalar_Size_Conv;
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Scalar_Size (Get_Kind (N)),
                     "no field Scalar_Size");
      Conv := To_Scalar_Size_Conv (Sz);
      Set_Flag6 (N, Conv.Flag6);
      Set_Flag7 (N, Conv.Flag7);
   end Set_Scalar_Size;

   function Get_Error_Origin (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Error_Origin (Get_Kind (Target)),
                     "no field Error_Origin");
      return Get_Field2 (Target);
   end Get_Error_Origin;

   procedure Set_Error_Origin (Target : Iir; Origin : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Error_Origin (Get_Kind (Target)),
                     "no field Error_Origin");
      Set_Field2 (Target, Origin);
   end Set_Error_Origin;

   function Get_Operand (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Operand (Get_Kind (Target)),
                     "no field Operand");
      return Get_Field2 (Target);
   end Get_Operand;

   procedure Set_Operand (Target : Iir; An_Iir : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Operand (Get_Kind (Target)),
                     "no field Operand");
      Set_Field2 (Target, An_Iir);
   end Set_Operand;

   function Get_Left (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Left (Get_Kind (Target)),
                     "no field Left");
      return Get_Field2 (Target);
   end Get_Left;

   procedure Set_Left (Target : Iir; An_Iir : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Left (Get_Kind (Target)),
                     "no field Left");
      Set_Field2 (Target, An_Iir);
   end Set_Left;

   function Get_Right (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Right (Get_Kind (Target)),
                     "no field Right");
      return Get_Field4 (Target);
   end Get_Right;

   procedure Set_Right (Target : Iir; An_Iir : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Right (Get_Kind (Target)),
                     "no field Right");
      Set_Field4 (Target, An_Iir);
   end Set_Right;

   function Get_Unit_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Name (Get_Kind (Target)),
                     "no field Unit_Name");
      return Get_Field3 (Target);
   end Get_Unit_Name;

   procedure Set_Unit_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Unit_Name (Get_Kind (Target)),
                     "no field Unit_Name");
      Set_Field3 (Target, Name);
   end Set_Unit_Name;

   function Get_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name (Get_Kind (Target)),
                     "no field Name");
      return Get_Field4 (Target);
   end Get_Name;

   procedure Set_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name (Get_Kind (Target)),
                     "no field Name");
      Set_Field4 (Target, Name);
   end Set_Name;

   function Get_Group_Template_Name (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Group_Template_Name (Get_Kind (Target)),
                     "no field Group_Template_Name");
      return Get_Field5 (Target);
   end Get_Group_Template_Name;

   procedure Set_Group_Template_Name (Target : Iir; Name : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Group_Template_Name (Get_Kind (Target)),
                     "no field Group_Template_Name");
      Set_Field5 (Target, Name);
   end Set_Group_Template_Name;

   function Get_Name_Staticness (Target : Iir) return Iir_Staticness is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name_Staticness (Get_Kind (Target)),
                     "no field Name_Staticness");
      return Iir_Staticness'Val (Get_State2 (Target));
   end Get_Name_Staticness;

   procedure Set_Name_Staticness (Target : Iir; Static : Iir_Staticness) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Name_Staticness (Get_Kind (Target)),
                     "no field Name_Staticness");
      Set_State2 (Target, Iir_Staticness'Pos (Static));
   end Set_Name_Staticness;

   function Get_Prefix (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prefix (Get_Kind (Target)),
                     "no field Prefix");
      return Get_Field0 (Target);
   end Get_Prefix;

   procedure Set_Prefix (Target : Iir; Prefix : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Prefix (Get_Kind (Target)),
                     "no field Prefix");
      Set_Field0 (Target, Prefix);
   end Set_Prefix;

   function Get_Signature_Prefix (Sign : Iir) return Iir is
   begin
      pragma Assert (Sign /= Null_Iir);
      pragma Assert (Has_Signature_Prefix (Get_Kind (Sign)),
                     "no field Signature_Prefix");
      return Get_Field1 (Sign);
   end Get_Signature_Prefix;

   procedure Set_Signature_Prefix (Sign : Iir; Prefix : Iir) is
   begin
      pragma Assert (Sign /= Null_Iir);
      pragma Assert (Has_Signature_Prefix (Get_Kind (Sign)),
                     "no field Signature_Prefix");
      Set_Field1 (Sign, Prefix);
   end Set_Signature_Prefix;

   function Get_External_Pathname (Name : Iir) return Iir is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_External_Pathname (Get_Kind (Name)),
                     "no field External_Pathname");
      return Get_Field3 (Name);
   end Get_External_Pathname;

   procedure Set_External_Pathname (Name : Iir; Path : Iir) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_External_Pathname (Get_Kind (Name)),
                     "no field External_Pathname");
      Set_Field3 (Name, Path);
   end Set_External_Pathname;

   function Get_Pathname_Suffix (Path : Iir) return Iir is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Suffix (Get_Kind (Path)),
                     "no field Pathname_Suffix");
      return Get_Field2 (Path);
   end Get_Pathname_Suffix;

   procedure Set_Pathname_Suffix (Path : Iir; Suffix : Iir) is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Suffix (Get_Kind (Path)),
                     "no field Pathname_Suffix");
      Set_Field2 (Path, Suffix);
   end Set_Pathname_Suffix;

   function Get_Pathname_Expression (Path : Iir) return Iir is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Expression (Get_Kind (Path)),
                     "no field Pathname_Expression");
      return Get_Field5 (Path);
   end Get_Pathname_Expression;

   procedure Set_Pathname_Expression (Path : Iir; Expr : Iir) is
   begin
      pragma Assert (Path /= Null_Iir);
      pragma Assert (Has_Pathname_Expression (Get_Kind (Path)),
                     "no field Pathname_Expression");
      Set_Field5 (Path, Expr);
   end Set_Pathname_Expression;

   function Get_In_Formal_Flag (Name : Iir) return Boolean is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_In_Formal_Flag (Get_Kind (Name)),
                     "no field In_Formal_Flag");
      return Get_Flag4 (Name);
   end Get_In_Formal_Flag;

   procedure Set_In_Formal_Flag (Name : Iir; Flag : Boolean) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_In_Formal_Flag (Get_Kind (Name)),
                     "no field In_Formal_Flag");
      Set_Flag4 (Name, Flag);
   end Set_In_Formal_Flag;

   function Get_Inertial_Flag (Name : Iir) return Boolean is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Inertial_Flag (Get_Kind (Name)),
                     "no field Inertial_Flag");
      return Get_Flag5 (Name);
   end Get_Inertial_Flag;

   procedure Set_Inertial_Flag (Name : Iir; Flag : Boolean) is
   begin
      pragma Assert (Name /= Null_Iir);
      pragma Assert (Has_Inertial_Flag (Get_Kind (Name)),
                     "no field Inertial_Flag");
      Set_Flag5 (Name, Flag);
   end Set_Inertial_Flag;

   function Get_Slice_Subtype (Slice : Iir) return Iir is
   begin
      pragma Assert (Slice /= Null_Iir);
      pragma Assert (Has_Slice_Subtype (Get_Kind (Slice)),
                     "no field Slice_Subtype");
      return Get_Field3 (Slice);
   end Get_Slice_Subtype;

   procedure Set_Slice_Subtype (Slice : Iir; Atype : Iir) is
   begin
      pragma Assert (Slice /= Null_Iir);
      pragma Assert (Has_Slice_Subtype (Get_Kind (Slice)),
                     "no field Slice_Subtype");
      Set_Field3 (Slice, Atype);
   end Set_Slice_Subtype;

   function Get_Suffix (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Suffix (Get_Kind (Target)),
                     "no field Suffix");
      return Get_Field2 (Target);
   end Get_Suffix;

   procedure Set_Suffix (Target : Iir; Suffix : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Suffix (Get_Kind (Target)),
                     "no field Suffix");
      Set_Field2 (Target, Suffix);
   end Set_Suffix;

   function Get_Index_Subtype (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Index_Subtype (Get_Kind (Attr)),
                     "no field Index_Subtype");
      return Get_Field2 (Attr);
   end Get_Index_Subtype;

   procedure Set_Index_Subtype (Attr : Iir; St : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Index_Subtype (Get_Kind (Attr)),
                     "no field Index_Subtype");
      Set_Field2 (Attr, St);
   end Set_Index_Subtype;

   function Get_Parameter (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter (Get_Kind (Target)),
                     "no field Parameter");
      return Get_Field4 (Target);
   end Get_Parameter;

   procedure Set_Parameter (Target : Iir; Param : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter (Get_Kind (Target)),
                     "no field Parameter");
      Set_Field4 (Target, Param);
   end Set_Parameter;

   function Get_Parameter_2 (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_2 (Get_Kind (Target)),
                     "no field Parameter_2");
      return Get_Field6 (Target);
   end Get_Parameter_2;

   procedure Set_Parameter_2 (Target : Iir; Param : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_2 (Get_Kind (Target)),
                     "no field Parameter_2");
      Set_Field6 (Target, Param);
   end Set_Parameter_2;

   function Get_Parameter_3 (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_3 (Get_Kind (Target)),
                     "no field Parameter_3");
      return Get_Field7 (Target);
   end Get_Parameter_3;

   procedure Set_Parameter_3 (Target : Iir; Param : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_3 (Get_Kind (Target)),
                     "no field Parameter_3");
      Set_Field7 (Target, Param);
   end Set_Parameter_3;

   function Get_Parameter_4 (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_4 (Get_Kind (Target)),
                     "no field Parameter_4");
      return Get_Field8 (Target);
   end Get_Parameter_4;

   procedure Set_Parameter_4 (Target : Iir; Param : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_4 (Get_Kind (Target)),
                     "no field Parameter_4");
      Set_Field8 (Target, Param);
   end Set_Parameter_4;

   function Get_Attr_Chain (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attr_Chain (Get_Kind (Attr)),
                     "no field Attr_Chain");
      return Get_Field2 (Attr);
   end Get_Attr_Chain;

   procedure Set_Attr_Chain (Attr : Iir; Chain : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attr_Chain (Get_Kind (Attr)),
                     "no field Attr_Chain");
      Set_Field2 (Attr, Chain);
   end Set_Attr_Chain;

   function Get_Attribute_Implicit_Declaration (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attribute_Implicit_Declaration (Get_Kind (Attr)),
                     "no field Attribute_Implicit_Declaration");
      return Get_Field3 (Attr);
   end Get_Attribute_Implicit_Declaration;

   procedure Set_Attribute_Implicit_Declaration (Attr : Iir; Decl : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attribute_Implicit_Declaration (Get_Kind (Attr)),
                     "no field Attribute_Implicit_Declaration");
      Set_Field3 (Attr, Decl);
   end Set_Attribute_Implicit_Declaration;

   function Get_Actual_Type (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type (Get_Kind (Target)),
                     "no field Actual_Type");
      return Get_Field5 (Target);
   end Get_Actual_Type;

   procedure Set_Actual_Type (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type (Get_Kind (Target)),
                     "no field Actual_Type");
      Set_Field5 (Target, Atype);
   end Set_Actual_Type;

   function Get_Actual_Type_Definition (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type_Definition (Get_Kind (Target)),
                     "no field Actual_Type_Definition");
      return Get_Field3 (Target);
   end Get_Actual_Type_Definition;

   procedure Set_Actual_Type_Definition (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Actual_Type_Definition (Get_Kind (Target)),
                     "no field Actual_Type_Definition");
      Set_Field3 (Target, Atype);
   end Set_Actual_Type_Definition;

   function Get_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Chain (Get_Kind (Target)),
                     "no field Association_Chain");
      return Get_Field2 (Target);
   end Get_Association_Chain;

   procedure Set_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Chain (Get_Kind (Target)),
                     "no field Association_Chain");
      Set_Field2 (Target, Chain);
   end Set_Association_Chain;

   function Get_Individual_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Individual_Association_Chain (Get_Kind (Target)),
                     "no field Individual_Association_Chain");
      return Get_Field4 (Target);
   end Get_Individual_Association_Chain;

   procedure Set_Individual_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Individual_Association_Chain (Get_Kind (Target)),
                     "no field Individual_Association_Chain");
      Set_Field4 (Target, Chain);
   end Set_Individual_Association_Chain;

   function Get_Subprogram_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Association_Chain (Get_Kind (Target)),
                     "no field Subprogram_Association_Chain");
      return Get_Field4 (Target);
   end Get_Subprogram_Association_Chain;

   procedure Set_Subprogram_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subprogram_Association_Chain (Get_Kind (Target)),
                     "no field Subprogram_Association_Chain");
      Set_Field4 (Target, Chain);
   end Set_Subprogram_Association_Chain;

   function Get_Aggregate_Info (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggregate_Info (Get_Kind (Target)),
                     "no field Aggregate_Info");
      return Get_Field5 (Target);
   end Get_Aggregate_Info;

   procedure Set_Aggregate_Info (Target : Iir; Info : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggregate_Info (Get_Kind (Target)),
                     "no field Aggregate_Info");
      Set_Field5 (Target, Info);
   end Set_Aggregate_Info;

   function Get_Sub_Aggregate_Info (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sub_Aggregate_Info (Get_Kind (Target)),
                     "no field Sub_Aggregate_Info");
      return Get_Field1 (Target);
   end Get_Sub_Aggregate_Info;

   procedure Set_Sub_Aggregate_Info (Target : Iir; Info : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Sub_Aggregate_Info (Get_Kind (Target)),
                     "no field Sub_Aggregate_Info");
      Set_Field1 (Target, Info);
   end Set_Sub_Aggregate_Info;

   function Get_Aggr_Dynamic_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Dynamic_Flag (Get_Kind (Target)),
                     "no field Aggr_Dynamic_Flag");
      return Get_Flag3 (Target);
   end Get_Aggr_Dynamic_Flag;

   procedure Set_Aggr_Dynamic_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Dynamic_Flag (Get_Kind (Target)),
                     "no field Aggr_Dynamic_Flag");
      Set_Flag3 (Target, Val);
   end Set_Aggr_Dynamic_Flag;

   function Get_Aggr_Min_Length (Info : Iir_Aggregate_Info) return Iir_Int32
   is
   begin
      pragma Assert (Info /= Null_Iir);
      pragma Assert (Has_Aggr_Min_Length (Get_Kind (Info)),
                     "no field Aggr_Min_Length");
      return Iir_To_Iir_Int32 (Get_Field4 (Info));
   end Get_Aggr_Min_Length;

   procedure Set_Aggr_Min_Length (Info : Iir_Aggregate_Info; Nbr : Iir_Int32)
   is
   begin
      pragma Assert (Info /= Null_Iir);
      pragma Assert (Has_Aggr_Min_Length (Get_Kind (Info)),
                     "no field Aggr_Min_Length");
      Set_Field4 (Info, Iir_Int32_To_Iir (Nbr));
   end Set_Aggr_Min_Length;

   function Get_Aggr_Low_Limit (Target : Iir_Aggregate_Info) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Low_Limit (Get_Kind (Target)),
                     "no field Aggr_Low_Limit");
      return Get_Field2 (Target);
   end Get_Aggr_Low_Limit;

   procedure Set_Aggr_Low_Limit (Target : Iir_Aggregate_Info; Limit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Low_Limit (Get_Kind (Target)),
                     "no field Aggr_Low_Limit");
      Set_Field2 (Target, Limit);
   end Set_Aggr_Low_Limit;

   function Get_Aggr_High_Limit (Target : Iir_Aggregate_Info) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_High_Limit (Get_Kind (Target)),
                     "no field Aggr_High_Limit");
      return Get_Field3 (Target);
   end Get_Aggr_High_Limit;

   procedure Set_Aggr_High_Limit (Target : Iir_Aggregate_Info; Limit : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_High_Limit (Get_Kind (Target)),
                     "no field Aggr_High_Limit");
      Set_Field3 (Target, Limit);
   end Set_Aggr_High_Limit;

   function Get_Aggr_Others_Flag (Target : Iir_Aggregate_Info) return Boolean
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Others_Flag (Get_Kind (Target)),
                     "no field Aggr_Others_Flag");
      return Get_Flag2 (Target);
   end Get_Aggr_Others_Flag;

   procedure Set_Aggr_Others_Flag (Target : Iir_Aggregate_Info; Val : Boolean)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Others_Flag (Get_Kind (Target)),
                     "no field Aggr_Others_Flag");
      Set_Flag2 (Target, Val);
   end Set_Aggr_Others_Flag;

   function Get_Aggr_Named_Flag (Target : Iir_Aggregate_Info) return Boolean
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Named_Flag (Get_Kind (Target)),
                     "no field Aggr_Named_Flag");
      return Get_Flag4 (Target);
   end Get_Aggr_Named_Flag;

   procedure Set_Aggr_Named_Flag (Target : Iir_Aggregate_Info; Val : Boolean)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Aggr_Named_Flag (Get_Kind (Target)),
                     "no field Aggr_Named_Flag");
      Set_Flag4 (Target, Val);
   end Set_Aggr_Named_Flag;

   function Get_Aggregate_Expand_Flag (Aggr : Iir) return Boolean is
   begin
      pragma Assert (Aggr /= Null_Iir);
      pragma Assert (Has_Aggregate_Expand_Flag (Get_Kind (Aggr)),
                     "no field Aggregate_Expand_Flag");
      return Get_Flag1 (Aggr);
   end Get_Aggregate_Expand_Flag;

   procedure Set_Aggregate_Expand_Flag (Aggr : Iir; Flag : Boolean) is
   begin
      pragma Assert (Aggr /= Null_Iir);
      pragma Assert (Has_Aggregate_Expand_Flag (Get_Kind (Aggr)),
                     "no field Aggregate_Expand_Flag");
      Set_Flag1 (Aggr, Flag);
   end Set_Aggregate_Expand_Flag;

   function Get_Determined_Aggregate_Flag (Aggr : Iir) return Boolean is
   begin
      pragma Assert (Aggr /= Null_Iir);
      pragma Assert (Has_Determined_Aggregate_Flag (Get_Kind (Aggr)),
                     "no field Determined_Aggregate_Flag");
      return Get_Flag2 (Aggr);
   end Get_Determined_Aggregate_Flag;

   procedure Set_Determined_Aggregate_Flag (Aggr : Iir; Flag : Boolean) is
   begin
      pragma Assert (Aggr /= Null_Iir);
      pragma Assert (Has_Determined_Aggregate_Flag (Get_Kind (Aggr)),
                     "no field Determined_Aggregate_Flag");
      Set_Flag2 (Aggr, Flag);
   end Set_Determined_Aggregate_Flag;

   function Get_Association_Choices_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Choices_Chain (Get_Kind (Target)),
                     "no field Association_Choices_Chain");
      return Get_Field4 (Target);
   end Get_Association_Choices_Chain;

   procedure Set_Association_Choices_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Association_Choices_Chain (Get_Kind (Target)),
                     "no field Association_Choices_Chain");
      Set_Field4 (Target, Chain);
   end Set_Association_Choices_Chain;

   function Get_Case_Statement_Alternative_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Case_Statement_Alternative_Chain (Get_Kind (Target)),
                     "no field Case_Statement_Alternative_Chain");
      return Get_Field1 (Target);
   end Get_Case_Statement_Alternative_Chain;

   procedure Set_Case_Statement_Alternative_Chain (Target : Iir; Chain : Iir)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Case_Statement_Alternative_Chain (Get_Kind (Target)),
                     "no field Case_Statement_Alternative_Chain");
      Set_Field1 (Target, Chain);
   end Set_Case_Statement_Alternative_Chain;

   function Get_Matching_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Matching_Flag (Get_Kind (Target)),
                     "no field Matching_Flag");
      return Get_Flag1 (Target);
   end Get_Matching_Flag;

   procedure Set_Matching_Flag (Target : Iir; Flag : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Matching_Flag (Get_Kind (Target)),
                     "no field Matching_Flag");
      Set_Flag1 (Target, Flag);
   end Set_Matching_Flag;

   function Get_Choice_Staticness (Target : Iir) return Iir_Staticness is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Choice_Staticness (Get_Kind (Target)),
                     "no field Choice_Staticness");
      return Iir_Staticness'Val (Get_State1 (Target));
   end Get_Choice_Staticness;

   procedure Set_Choice_Staticness (Target : Iir; Staticness : Iir_Staticness)
   is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Choice_Staticness (Get_Kind (Target)),
                     "no field Choice_Staticness");
      Set_State1 (Target, Iir_Staticness'Pos (Staticness));
   end Set_Choice_Staticness;

   function Get_Procedure_Call (Stmt : Iir) return Iir is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Procedure_Call (Get_Kind (Stmt)),
                     "no field Procedure_Call");
      return Get_Field1 (Stmt);
   end Get_Procedure_Call;

   procedure Set_Procedure_Call (Stmt : Iir; Call : Iir) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Procedure_Call (Get_Kind (Stmt)),
                     "no field Procedure_Call");
      Set_Field1 (Stmt, Call);
   end Set_Procedure_Call;

   function Get_Implementation (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Implementation (Get_Kind (Target)),
                     "no field Implementation");
      return Get_Field3 (Target);
   end Get_Implementation;

   procedure Set_Implementation (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Implementation (Get_Kind (Target)),
                     "no field Implementation");
      Set_Field3 (Target, Decl);
   end Set_Implementation;

   function Get_Parameter_Association_Chain (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Association_Chain (Get_Kind (Target)),
                     "no field Parameter_Association_Chain");
      return Get_Field2 (Target);
   end Get_Parameter_Association_Chain;

   procedure Set_Parameter_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Parameter_Association_Chain (Get_Kind (Target)),
                     "no field Parameter_Association_Chain");
      Set_Field2 (Target, Chain);
   end Set_Parameter_Association_Chain;

   function Get_Method_Object (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Method_Object (Get_Kind (Target)),
                     "no field Method_Object");
      return Get_Field4 (Target);
   end Get_Method_Object;

   procedure Set_Method_Object (Target : Iir; Object : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Method_Object (Get_Kind (Target)),
                     "no field Method_Object");
      Set_Field4 (Target, Object);
   end Set_Method_Object;

   function Get_Subtype_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Type_Mark (Get_Kind (Target)),
                     "no field Subtype_Type_Mark");
      return Get_Field2 (Target);
   end Get_Subtype_Type_Mark;

   procedure Set_Subtype_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subtype_Type_Mark (Get_Kind (Target)),
                     "no field Subtype_Type_Mark");
      Set_Field2 (Target, Mark);
   end Set_Subtype_Type_Mark;

   function Get_Subnature_Nature_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subnature_Nature_Mark (Get_Kind (Target)),
                     "no field Subnature_Nature_Mark");
      return Get_Field2 (Target);
   end Get_Subnature_Nature_Mark;

   procedure Set_Subnature_Nature_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Subnature_Nature_Mark (Get_Kind (Target)),
                     "no field Subnature_Nature_Mark");
      Set_Field2 (Target, Mark);
   end Set_Subnature_Nature_Mark;

   function Get_Type_Conversion_Subtype (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Conversion_Subtype (Get_Kind (Target)),
                     "no field Type_Conversion_Subtype");
      return Get_Field3 (Target);
   end Get_Type_Conversion_Subtype;

   procedure Set_Type_Conversion_Subtype (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Conversion_Subtype (Get_Kind (Target)),
                     "no field Type_Conversion_Subtype");
      Set_Field3 (Target, Atype);
   end Set_Type_Conversion_Subtype;

   function Get_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Mark (Get_Kind (Target)),
                     "no field Type_Mark");
      return Get_Field4 (Target);
   end Get_Type_Mark;

   procedure Set_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Mark (Get_Kind (Target)),
                     "no field Type_Mark");
      Set_Field4 (Target, Mark);
   end Set_Type_Mark;

   function Get_File_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Type_Mark (Get_Kind (Target)),
                     "no field File_Type_Mark");
      return Get_Field2 (Target);
   end Get_File_Type_Mark;

   procedure Set_File_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_File_Type_Mark (Get_Kind (Target)),
                     "no field File_Type_Mark");
      Set_Field2 (Target, Mark);
   end Set_File_Type_Mark;

   function Get_Return_Type_Mark (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type_Mark (Get_Kind (Target)),
                     "no field Return_Type_Mark");
      return Get_Field8 (Target);
   end Get_Return_Type_Mark;

   procedure Set_Return_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Return_Type_Mark (Get_Kind (Target)),
                     "no field Return_Type_Mark");
      Set_Field8 (Target, Mark);
   end Set_Return_Type_Mark;

   function Get_Has_Disconnect_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Disconnect_Flag (Get_Kind (Target)),
                     "no field Has_Disconnect_Flag");
      return Get_Flag1 (Target);
   end Get_Has_Disconnect_Flag;

   procedure Set_Has_Disconnect_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Disconnect_Flag (Get_Kind (Target)),
                     "no field Has_Disconnect_Flag");
      Set_Flag1 (Target, Val);
   end Set_Has_Disconnect_Flag;

   function Get_Has_Active_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Active_Flag (Get_Kind (Target)),
                     "no field Has_Active_Flag");
      return Get_Flag2 (Target);
   end Get_Has_Active_Flag;

   procedure Set_Has_Active_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Has_Active_Flag (Get_Kind (Target)),
                     "no field Has_Active_Flag");
      Set_Flag2 (Target, Val);
   end Set_Has_Active_Flag;

   function Get_Is_Within_Flag (Target : Iir) return Boolean is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Is_Within_Flag (Get_Kind (Target)),
                     "no field Is_Within_Flag");
      return Get_Flag5 (Target);
   end Get_Is_Within_Flag;

   procedure Set_Is_Within_Flag (Target : Iir; Val : Boolean) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Is_Within_Flag (Get_Kind (Target)),
                     "no field Is_Within_Flag");
      Set_Flag5 (Target, Val);
   end Set_Is_Within_Flag;

   function Get_Type_Marks_List (Target : Iir) return Iir_Flist is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Marks_List (Get_Kind (Target)),
                     "no field Type_Marks_List");
      return Iir_To_Iir_Flist (Get_Field2 (Target));
   end Get_Type_Marks_List;

   procedure Set_Type_Marks_List (Target : Iir; List : Iir_Flist) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Type_Marks_List (Get_Kind (Target)),
                     "no field Type_Marks_List");
      Set_Field2 (Target, Iir_Flist_To_Iir (List));
   end Set_Type_Marks_List;

   function Get_Implicit_Alias_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Implicit_Alias_Flag (Get_Kind (Decl)),
                     "no field Implicit_Alias_Flag");
      return Get_Flag1 (Decl);
   end Get_Implicit_Alias_Flag;

   procedure Set_Implicit_Alias_Flag (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Implicit_Alias_Flag (Get_Kind (Decl)),
                     "no field Implicit_Alias_Flag");
      Set_Flag1 (Decl, Flag);
   end Set_Implicit_Alias_Flag;

   function Get_Alias_Signature (Alias : Iir) return Iir is
   begin
      pragma Assert (Alias /= Null_Iir);
      pragma Assert (Has_Alias_Signature (Get_Kind (Alias)),
                     "no field Alias_Signature");
      return Get_Field5 (Alias);
   end Get_Alias_Signature;

   procedure Set_Alias_Signature (Alias : Iir; Signature : Iir) is
   begin
      pragma Assert (Alias /= Null_Iir);
      pragma Assert (Has_Alias_Signature (Get_Kind (Alias)),
                     "no field Alias_Signature");
      Set_Field5 (Alias, Signature);
   end Set_Alias_Signature;

   function Get_Attribute_Signature (Attr : Iir) return Iir is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attribute_Signature (Get_Kind (Attr)),
                     "no field Attribute_Signature");
      return Get_Field2 (Attr);
   end Get_Attribute_Signature;

   procedure Set_Attribute_Signature (Attr : Iir; Signature : Iir) is
   begin
      pragma Assert (Attr /= Null_Iir);
      pragma Assert (Has_Attribute_Signature (Get_Kind (Attr)),
                     "no field Attribute_Signature");
      Set_Field2 (Attr, Signature);
   end Set_Attribute_Signature;

   function Get_Overload_List (Target : Iir) return Iir_List is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_List (Get_Kind (Target)),
                     "no field Overload_List");
      return Iir_To_Iir_List (Get_Field1 (Target));
   end Get_Overload_List;

   procedure Set_Overload_List (Target : Iir; List : Iir_List) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Overload_List (Get_Kind (Target)),
                     "no field Overload_List");
      Set_Field1 (Target, Iir_List_To_Iir (List));
   end Set_Overload_List;

   function Get_Simple_Name_Identifier (Target : Iir) return Name_Id is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Identifier (Get_Kind (Target)),
                     "no field Simple_Name_Identifier");
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Simple_Name_Identifier;

   procedure Set_Simple_Name_Identifier (Target : Iir; Ident : Name_Id) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Identifier (Get_Kind (Target)),
                     "no field Simple_Name_Identifier");
      Set_Field3 (Target, Name_Id_To_Iir (Ident));
   end Set_Simple_Name_Identifier;

   function Get_Simple_Name_Subtype (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Subtype (Get_Kind (Target)),
                     "no field Simple_Name_Subtype");
      return Get_Field4 (Target);
   end Get_Simple_Name_Subtype;

   procedure Set_Simple_Name_Subtype (Target : Iir; Atype : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Simple_Name_Subtype (Get_Kind (Target)),
                     "no field Simple_Name_Subtype");
      Set_Field4 (Target, Atype);
   end Set_Simple_Name_Subtype;

   function Get_Protected_Type_Body (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Body (Get_Kind (Target)),
                     "no field Protected_Type_Body");
      return Get_Field2 (Target);
   end Get_Protected_Type_Body;

   procedure Set_Protected_Type_Body (Target : Iir; Bod : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Body (Get_Kind (Target)),
                     "no field Protected_Type_Body");
      Set_Field2 (Target, Bod);
   end Set_Protected_Type_Body;

   function Get_Protected_Type_Declaration (Target : Iir) return Iir is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Declaration (Get_Kind (Target)),
                     "no field Protected_Type_Declaration");
      return Get_Field4 (Target);
   end Get_Protected_Type_Declaration;

   procedure Set_Protected_Type_Declaration (Target : Iir; Decl : Iir) is
   begin
      pragma Assert (Target /= Null_Iir);
      pragma Assert (Has_Protected_Type_Declaration (Get_Kind (Target)),
                     "no field Protected_Type_Declaration");
      Set_Field4 (Target, Decl);
   end Set_Protected_Type_Declaration;

   function Get_Use_Flag (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Use_Flag (Get_Kind (Decl)),
                     "no field Use_Flag");
      return Get_Flag6 (Decl);
   end Get_Use_Flag;

   procedure Set_Use_Flag (Decl : Iir; Val : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Use_Flag (Get_Kind (Decl)),
                     "no field Use_Flag");
      Set_Flag6 (Decl, Val);
   end Set_Use_Flag;

   function Get_End_Has_Reserved_Id (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Reserved_Id (Get_Kind (Decl)),
                     "no field End_Has_Reserved_Id");
      return Get_Flag8 (Decl);
   end Get_End_Has_Reserved_Id;

   procedure Set_End_Has_Reserved_Id (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Reserved_Id (Get_Kind (Decl)),
                     "no field End_Has_Reserved_Id");
      Set_Flag8 (Decl, Flag);
   end Set_End_Has_Reserved_Id;

   function Get_End_Has_Identifier (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Identifier (Get_Kind (Decl)),
                     "no field End_Has_Identifier");
      return Get_Flag9 (Decl);
   end Get_End_Has_Identifier;

   procedure Set_End_Has_Identifier (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Identifier (Get_Kind (Decl)),
                     "no field End_Has_Identifier");
      Set_Flag9 (Decl, Flag);
   end Set_End_Has_Identifier;

   function Get_End_Has_Postponed (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Postponed (Get_Kind (Decl)),
                     "no field End_Has_Postponed");
      return Get_Flag10 (Decl);
   end Get_End_Has_Postponed;

   procedure Set_End_Has_Postponed (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_End_Has_Postponed (Get_Kind (Decl)),
                     "no field End_Has_Postponed");
      Set_Flag10 (Decl, Flag);
   end Set_End_Has_Postponed;

   function Get_Has_Label (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Label (Get_Kind (Decl)),
                     "no field Has_Label");
      return Get_Flag6 (Decl);
   end Get_Has_Label;

   procedure Set_Has_Label (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Label (Get_Kind (Decl)),
                     "no field Has_Label");
      Set_Flag6 (Decl, Flag);
   end Set_Has_Label;

   function Get_Has_Begin (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Begin (Get_Kind (Decl)),
                     "no field Has_Begin");
      return Get_Flag10 (Decl);
   end Get_Has_Begin;

   procedure Set_Has_Begin (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Begin (Get_Kind (Decl)),
                     "no field Has_Begin");
      Set_Flag10 (Decl, Flag);
   end Set_Has_Begin;

   function Get_Has_End (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_End (Get_Kind (Decl)),
                     "no field Has_End");
      return Get_Flag11 (Decl);
   end Get_Has_End;

   procedure Set_Has_End (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_End (Get_Kind (Decl)),
                     "no field Has_End");
      Set_Flag11 (Decl, Flag);
   end Set_Has_End;

   function Get_Has_Is (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Is (Get_Kind (Decl)),
                     "no field Has_Is");
      return Get_Flag7 (Decl);
   end Get_Has_Is;

   procedure Set_Has_Is (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Is (Get_Kind (Decl)),
                     "no field Has_Is");
      Set_Flag7 (Decl, Flag);
   end Set_Has_Is;

   function Get_Has_Pure (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Pure (Get_Kind (Decl)),
                     "no field Has_Pure");
      return Get_Flag8 (Decl);
   end Get_Has_Pure;

   procedure Set_Has_Pure (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Pure (Get_Kind (Decl)),
                     "no field Has_Pure");
      Set_Flag8 (Decl, Flag);
   end Set_Has_Pure;

   function Get_Has_Body (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Body (Get_Kind (Decl)),
                     "no field Has_Body");
      return Get_Flag9 (Decl);
   end Get_Has_Body;

   procedure Set_Has_Body (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Body (Get_Kind (Decl)),
                     "no field Has_Body");
      Set_Flag9 (Decl, Flag);
   end Set_Has_Body;

   function Get_Has_Parameter (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Parameter (Get_Kind (Decl)),
                     "no field Has_Parameter");
      return Get_Flag10 (Decl);
   end Get_Has_Parameter;

   procedure Set_Has_Parameter (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Parameter (Get_Kind (Decl)),
                     "no field Has_Parameter");
      Set_Flag10 (Decl, Flag);
   end Set_Has_Parameter;

   function Get_Has_Component (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Component (Get_Kind (Decl)),
                     "no field Has_Component");
      return Get_Flag5 (Decl);
   end Get_Has_Component;

   procedure Set_Has_Component (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Component (Get_Kind (Decl)),
                     "no field Has_Component");
      Set_Flag5 (Decl, Flag);
   end Set_Has_Component;

   function Get_Has_Identifier_List (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (Decl)),
                     "no field Has_Identifier_List");
      return Get_Flag3 (Decl);
   end Get_Has_Identifier_List;

   procedure Set_Has_Identifier_List (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Identifier_List (Get_Kind (Decl)),
                     "no field Has_Identifier_List");
      Set_Flag3 (Decl, Flag);
   end Set_Has_Identifier_List;

   function Get_Has_Mode (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Mode (Get_Kind (Decl)),
                     "no field Has_Mode");
      return Get_Flag10 (Decl);
   end Get_Has_Mode;

   procedure Set_Has_Mode (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Mode (Get_Kind (Decl)),
                     "no field Has_Mode");
      Set_Flag10 (Decl, Flag);
   end Set_Has_Mode;

   function Get_Has_Class (Decl : Iir) return Boolean is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Class (Get_Kind (Decl)),
                     "no field Has_Class");
      return Get_Flag11 (Decl);
   end Get_Has_Class;

   procedure Set_Has_Class (Decl : Iir; Flag : Boolean) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Has_Class (Get_Kind (Decl)),
                     "no field Has_Class");
      Set_Flag11 (Decl, Flag);
   end Set_Has_Class;

   function Get_Has_Delay_Mechanism (Stmt : Iir) return Boolean is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Has_Delay_Mechanism (Get_Kind (Stmt)),
                     "no field Has_Delay_Mechanism");
      return Get_Flag2 (Stmt);
   end Get_Has_Delay_Mechanism;

   procedure Set_Has_Delay_Mechanism (Stmt : Iir; Flag : Boolean) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Has_Delay_Mechanism (Get_Kind (Stmt)),
                     "no field Has_Delay_Mechanism");
      Set_Flag2 (Stmt, Flag);
   end Set_Has_Delay_Mechanism;

   function Get_Suspend_Flag (Stmt : Iir) return Boolean is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Suspend_Flag (Get_Kind (Stmt)),
                     "no field Suspend_Flag");
      return Get_Flag11 (Stmt);
   end Get_Suspend_Flag;

   procedure Set_Suspend_Flag (Stmt : Iir; Flag : Boolean) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Suspend_Flag (Get_Kind (Stmt)),
                     "no field Suspend_Flag");
      Set_Flag11 (Stmt, Flag);
   end Set_Suspend_Flag;

   function Get_Stop_Flag (Stmt : Iir) return Boolean is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Stop_Flag (Get_Kind (Stmt)),
                     "no field Stop_Flag");
      return Get_Flag13 (Stmt);
   end Get_Stop_Flag;

   procedure Set_Stop_Flag (Stmt : Iir; Flag : Boolean) is
   begin
      pragma Assert (Stmt /= Null_Iir);
      pragma Assert (Has_Stop_Flag (Get_Kind (Stmt)),
                     "no field Stop_Flag");
      Set_Flag13 (Stmt, Flag);
   end Set_Stop_Flag;

   function Get_Is_Ref (N : Iir) return Boolean is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Ref (Get_Kind (N)),
                     "no field Is_Ref");
      return Get_Flag12 (N);
   end Get_Is_Ref;

   procedure Set_Is_Ref (N : Iir; Ref : Boolean) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Ref (Get_Kind (N)),
                     "no field Is_Ref");
      Set_Flag12 (N, Ref);
   end Set_Is_Ref;

   function Get_Is_Forward_Ref (N : Iir) return Boolean is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Forward_Ref (Get_Kind (N)),
                     "no field Is_Forward_Ref");
      return Get_Flag1 (N);
   end Get_Is_Forward_Ref;

   procedure Set_Is_Forward_Ref (N : Iir; Ref : Boolean) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Is_Forward_Ref (Get_Kind (N)),
                     "no field Is_Forward_Ref");
      Set_Flag1 (N, Ref);
   end Set_Is_Forward_Ref;

   function Get_Psl_Property (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Property (Get_Kind (Decl)),
                     "no field Psl_Property");
      return Iir_To_PSL_Node (Get_Field1 (Decl));
   end Get_Psl_Property;

   procedure Set_Psl_Property (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Property (Get_Kind (Decl)),
                     "no field Psl_Property");
      Set_Field1 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Property;

   function Get_Psl_Sequence (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Sequence (Get_Kind (Decl)),
                     "no field Psl_Sequence");
      return Iir_To_PSL_Node (Get_Field1 (Decl));
   end Get_Psl_Sequence;

   procedure Set_Psl_Sequence (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Sequence (Get_Kind (Decl)),
                     "no field Psl_Sequence");
      Set_Field1 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Sequence;

   function Get_Psl_Declaration (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Declaration (Get_Kind (Decl)),
                     "no field Psl_Declaration");
      return Iir_To_PSL_Node (Get_Field5 (Decl));
   end Get_Psl_Declaration;

   procedure Set_Psl_Declaration (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Declaration (Get_Kind (Decl)),
                     "no field Psl_Declaration");
      Set_Field5 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Declaration;

   function Get_Psl_Expression (Decl : Iir) return PSL_Node is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Expression (Get_Kind (Decl)),
                     "no field Psl_Expression");
      return Iir_To_PSL_Node (Get_Field3 (Decl));
   end Get_Psl_Expression;

   procedure Set_Psl_Expression (Decl : Iir; Prop : PSL_Node) is
   begin
      pragma Assert (Decl /= Null_Iir);
      pragma Assert (Has_Psl_Expression (Get_Kind (Decl)),
                     "no field Psl_Expression");
      Set_Field3 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Expression;

   function Get_Psl_Boolean (N : Iir) return PSL_Node is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Psl_Boolean (Get_Kind (N)),
                     "no field Psl_Boolean");
      return Iir_To_PSL_Node (Get_Field1 (N));
   end Get_Psl_Boolean;

   procedure Set_Psl_Boolean (N : Iir; Bool : PSL_Node) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Psl_Boolean (Get_Kind (N)),
                     "no field Psl_Boolean");
      Set_Field1 (N, PSL_Node_To_Iir (Bool));
   end Set_Psl_Boolean;

   function Get_PSL_Clock (N : Iir) return PSL_Node is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock (Get_Kind (N)),
                     "no field PSL_Clock");
      return Iir_To_PSL_Node (Get_Field7 (N));
   end Get_PSL_Clock;

   procedure Set_PSL_Clock (N : Iir; Clock : PSL_Node) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock (Get_Kind (N)),
                     "no field PSL_Clock");
      Set_Field7 (N, PSL_Node_To_Iir (Clock));
   end Set_PSL_Clock;

   function Get_PSL_NFA (N : Iir) return PSL_NFA is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_NFA (Get_Kind (N)),
                     "no field PSL_NFA");
      return Iir_To_PSL_NFA (Get_Field8 (N));
   end Get_PSL_NFA;

   procedure Set_PSL_NFA (N : Iir; Fa : PSL_NFA) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_NFA (Get_Kind (N)),
                     "no field PSL_NFA");
      Set_Field8 (N, PSL_NFA_To_Iir (Fa));
   end Set_PSL_NFA;

   function Get_PSL_Nbr_States (N : Iir) return Int32 is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Nbr_States (Get_Kind (N)),
                     "no field PSL_Nbr_States");
      return Iir_To_Int32 (Get_Field9 (N));
   end Get_PSL_Nbr_States;

   procedure Set_PSL_Nbr_States (N : Iir; Nbr : Int32) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Nbr_States (Get_Kind (N)),
                     "no field PSL_Nbr_States");
      Set_Field9 (N, Int32_To_Iir (Nbr));
   end Set_PSL_Nbr_States;

   function Get_PSL_Clock_Sensitivity (N : Iir) return Iir_List is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock_Sensitivity (Get_Kind (N)),
                     "no field PSL_Clock_Sensitivity");
      return Iir_To_Iir_List (Get_Field10 (N));
   end Get_PSL_Clock_Sensitivity;

   procedure Set_PSL_Clock_Sensitivity (N : Iir; List : Iir_List) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Clock_Sensitivity (Get_Kind (N)),
                     "no field PSL_Clock_Sensitivity");
      Set_Field10 (N, Iir_List_To_Iir (List));
   end Set_PSL_Clock_Sensitivity;

   function Get_PSL_EOS_Flag (N : Iir) return Boolean is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_EOS_Flag (Get_Kind (N)),
                     "no field PSL_EOS_Flag");
      return Get_Flag1 (N);
   end Get_PSL_EOS_Flag;

   procedure Set_PSL_EOS_Flag (N : Iir; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_EOS_Flag (Get_Kind (N)),
                     "no field PSL_EOS_Flag");
      Set_Flag1 (N, Flag);
   end Set_PSL_EOS_Flag;

   function Get_PSL_Abort_Flag (N : Iir) return Boolean is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Abort_Flag (Get_Kind (N)),
                     "no field PSL_Abort_Flag");
      return Get_Flag2 (N);
   end Get_PSL_Abort_Flag;

   procedure Set_PSL_Abort_Flag (N : Iir; Flag : Boolean) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_PSL_Abort_Flag (Get_Kind (N)),
                     "no field PSL_Abort_Flag");
      Set_Flag2 (N, Flag);
   end Set_PSL_Abort_Flag;

   function Get_Count_Expression (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Count_Expression (Get_Kind (N)),
                     "no field Count_Expression");
      return Get_Field2 (N);
   end Get_Count_Expression;

   procedure Set_Count_Expression (N : Iir; Count : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Count_Expression (Get_Kind (N)),
                     "no field Count_Expression");
      Set_Field2 (N, Count);
   end Set_Count_Expression;

   function Get_Clock_Expression (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Clock_Expression (Get_Kind (N)),
                     "no field Clock_Expression");
      return Get_Field4 (N);
   end Get_Clock_Expression;

   procedure Set_Clock_Expression (N : Iir; Clk : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Clock_Expression (Get_Kind (N)),
                     "no field Clock_Expression");
      Set_Field4 (N, Clk);
   end Set_Clock_Expression;

   function Get_Default_Clock (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Default_Clock (Get_Kind (N)),
                     "no field Default_Clock");
      return Get_Field3 (N);
   end Get_Default_Clock;

   procedure Set_Default_Clock (N : Iir; Clk : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Default_Clock (Get_Kind (N)),
                     "no field Default_Clock");
      Set_Field3 (N, Clk);
   end Set_Default_Clock;

   function Get_Foreign_Node (N : Iir) return Int32 is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Foreign_Node (Get_Kind (N)),
                     "no field Foreign_Node");
      return Iir_To_Int32 (Get_Field1 (N));
   end Get_Foreign_Node;

   procedure Set_Foreign_Node (N : Iir; En : Int32) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Foreign_Node (Get_Kind (N)),
                     "no field Foreign_Node");
      Set_Field1 (N, Int32_To_Iir (En));
   end Set_Foreign_Node;

   function Get_Suspend_State_Index (N : Iir) return Int32 is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Suspend_State_Index (Get_Kind (N)),
                     "no field Suspend_State_Index");
      return Iir_To_Int32 (Get_Field3 (N));
   end Get_Suspend_State_Index;

   procedure Set_Suspend_State_Index (N : Iir; Num : Int32) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Suspend_State_Index (Get_Kind (N)),
                     "no field Suspend_State_Index");
      Set_Field3 (N, Int32_To_Iir (Num));
   end Set_Suspend_State_Index;

   function Get_Suspend_State_Chain (N : Iir) return Iir is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Suspend_State_Chain (Get_Kind (N)),
                     "no field Suspend_State_Chain");
      return Get_Field4 (N);
   end Get_Suspend_State_Chain;

   procedure Set_Suspend_State_Chain (N : Iir; Chain : Iir) is
   begin
      pragma Assert (N /= Null_Iir);
      pragma Assert (Has_Suspend_State_Chain (Get_Kind (N)),
                     "no field Suspend_State_Chain");
      Set_Field4 (N, Chain);
   end Set_Suspend_State_Chain;

end Vhdl.Nodes;
