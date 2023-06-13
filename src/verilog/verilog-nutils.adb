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
package body Verilog.Nutils is
   procedure Location_Copy (Dest : Node; Src : Node) is
   begin
      Set_Location (Dest, Get_Location (Src));
   end Location_Copy;

   function Nkind_In (K : Nkind; V1 : Nkind; V2 : Nkind) return Boolean is
   begin
      return K = V1 or K = V2;
   end Nkind_In;

   procedure Append_Chain (First : in out Node; Last : in out Node; El : Node)
   is
   begin
      pragma Assert (El /= Null_Node);

      if First = Null_Node then
         pragma Assert (Last = Null_Node);
         First := El;
      else
         pragma Assert (Last /= Null_Node);
         Set_Chain (Last, El);
      end if;
      Last := El;
   end Append_Chain;

   procedure Init_Chain (First, Last : out Node) is
   begin
      First := Null_Node;
      Last := Null_Node;
   end Init_Chain;

   procedure Init_Constr (Constr : out Items_Constr; Parent : Node) is
   begin
      Constr := (Parent, Null_Node, Null_Node);
   end Init_Constr;

   procedure Append_Node (Constr : in out Items_Constr; Item : Node) is
   begin
      pragma Assert (Get_Chain (Item) = Null_Node);
      if Constr.Last_Item = Null_Node then
         Constr.First_Item := Item;
      else
         Set_Chain (Constr.Last_Item, Item);
      end if;
      Constr.Last_Item := Item;
      pragma Assert (Get_Parent (Item) = Null_Node);
      Set_Parent (Item, Constr.Parent);
   end Append_Node;

   procedure Append_Constr (Constr : in out Items_Constr; Els : Items_Constr)
   is
      pragma Assert (Constr.Parent = Els.Parent);
   begin
      --  Nothing to do if ELS is empty.
      if Els.First_Item = Null_Node then
         return;
      end if;

      if Constr.Last_Item = Null_Node then
         Constr.First_Item := Els.First_Item;
      else
         Set_Chain (Constr.Last_Item, Els.First_Item);
      end if;
      Constr.Last_Item := Els.Last_Item;
   end Append_Constr;

   procedure Update_Constr (Constr : in out Items_Constr)
   is
      N : Node;
   begin
      if Constr.Last_Item = Null_Node then
         return;
      end if;

      loop
         N := Get_Chain (Constr.Last_Item);
         exit when N = Null_Node;
         Constr.Last_Item := N;
      end loop;
   end Update_Constr;

   function Get_Parent (Constr : Items_Constr) return Node is
   begin
      return Constr.Parent;
   end Get_Parent;

   function Get_Constr_Chain (Constr : Items_Constr) return Node is
   begin
      return Constr.First_Item;
   end Get_Constr_Chain;

   function Get_Chain_Length (Head : Node) return Natural
   is
      Res : Natural;
      El : Node;
   begin
      Res := 0;
      El := Head;
      while El /= Null_Node loop
         Res := Res + 1;
         El := Get_Chain (El);
      end loop;
      return Res;
   end Get_Chain_Length;

   function Get_Type_Data_Type (Decl : Node) return Node
   is
      Data_Type : constant Node := Get_Data_Type (Decl);
      Res : constant Node := Get_Expr_Type (Data_Type);
   begin
      return Res;
   end Get_Type_Data_Type;

   function Get_Type_Base_Class_Type (Klass : Node) return Node is
      Base : constant Node := Get_Base_Class_Type (Klass);
      Res : Node;
   begin
      if Base = Null_Node then
         return Null_Node;
      else
         Res := Get_Expr_Type (Base);
         return Res;
      end if;
   end Get_Type_Base_Class_Type;

end Verilog.Nutils;
