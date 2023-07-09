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

with Std_Names;

with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Standard; use Verilog.Standard;

package body Verilog.Sem_Utils is
   function Get_Base_Lvalue (Lvalue : Node) return Node
   is
      Res : Node;
   begin
      Res := Lvalue;
      loop
         case Get_Kind (Res) is
            when N_Var
              | Nkinds_Nets
              | Nkinds_Net_Port
              | N_This
              | N_This_Var =>
               return Res;
            when N_Interface_Item
               | N_Modport_Item =>
               return Res;
            when N_Name
               | N_This_Name
               | N_Scoped_Name
               | N_Hierarchical =>
               return Get_Declaration (Res);
            when N_Bit_Select
              | N_Part_Select
              | N_Part_Select_Cst
              | N_Plus_Part_Select
              | N_Minus_Part_Select
              | N_Plus_Part_Select_Cst
              | N_Minus_Part_Select_Cst
              | N_Indexed_Name
              | N_Slice_Name
              | N_Slice_Name_Cst
              | N_Member_Name
              | N_Property_Name
              | N_Associative_Index
              | N_String_Index =>
               Res := Get_Name (Res);
            when others =>
               Error_Kind ("get_base_lvalue", Res);
         end case;
      end loop;
   end Get_Base_Lvalue;

   function Get_Attribute_Parent (Parent : Node) return Node
   is
      Res : Node;
   begin
      Res := Parent;
      loop
         case Get_Kind (Res) is
            when N_Module
              | N_Seq_Block
              | N_Par_Block =>
               return Res;
            when N_If
               | N_For
               | Nkinds_Net_Port =>
               Res := Get_Parent (Res);
            when others =>
               Error_Kind ("get_attribute_parent", Res);
         end case;
      end loop;
   end Get_Attribute_Parent;

   function Strip_Names_And_Ports (N : Node) return Node
   is
      Res : Node;
   begin
      Res := N;
      loop
         case Get_Kind (Res) is
            when N_Var
              | Nkinds_Nets
              | N_Modport_Item =>
               return Res;
            when N_Name =>
               Res := Get_Declaration (Res);
            when others =>
               Error_Kind ("strip_names_and_ports", Res);
         end case;
      end loop;
   end Strip_Names_And_Ports;

   function Is_Method (Rtn : Node) return Boolean is
   begin
      return Get_Kind (Get_Parent (Rtn)) in Nkinds_Any_Class;
--        or else Get_Kind (Rtn) in Nkinds_OOB_Tf;
   end Is_Method;

   function Iterate_Base_Class_Type (Klass : Node) return Node
   is
      Base : constant Node := Get_Base_Class_Type (Klass);
      Base_Type : Node;
   begin
      if Base = Null_Node then
         if Klass /= Base_Root_Class then
            return Base_Root_Class;
         else
            return Null_Node;
         end if;
      else
         Base_Type := Get_Expr_Type (Base);
         pragma Assert (Get_Kind (Base_Type) in Nkinds_Class);
         return Base_Type;
      end if;
   end Iterate_Base_Class_Type;

   function Is_Call_To_Super_New (Stmt : Node) return Boolean
   is
      use Std_Names;
      Name : Node;
      Call : Node;
   begin
      if Stmt = Null_Node
        or else Get_Kind (Stmt) /= N_Subroutine_Call_Stmt
      then
         --  No statements or not a call.
         return False;
      end if;

      Call := Get_Call (Stmt);
      if Get_Kind (Call) /= N_Call then
         --  Not a call to a subroutine (could be a systf call).
         return False;
      end if;

      --  Check form 'super.new'.
      Name := Get_Subroutine (Call);
      if not Nkind_In (Get_Kind (Name), N_Dotted_Name, N_Method_Name) then
         return False;
      end if;
      if Get_Identifier (Name) /= Name_New then
         return False;
      end if;

      Name := Get_Name (Name);
      if Get_Kind (Name) /= N_Super then
         return False;
      end if;

      return True;
   end Is_Call_To_Super_New;

   function Compute_Length (Msb : Int32; Lsb : Int32) return Int32 is
   begin
      if Msb > Lsb then
         return Msb - Lsb + 1;
      else
         return Lsb - Msb + 1;
      end if;
   end Compute_Length;

   function Compute_Length (Rng : Node) return Int32 is
   begin
      return Compute_Length (Get_Msb_Cst (Rng), Get_Lsb_Cst (Rng));
   end Compute_Length;

   function Is_Null_Replication (N : Node) return Boolean is
   begin
      return Get_Kind (N) = N_Replication_Cst
        and then Get_Replication_Cst (N) = 0;
   end Is_Null_Replication;

   function Has_Number_X_Z (Num : Node) return Boolean is
   begin
      return Get_Number_Hi_Zx (Num) /= 0 or Get_Number_Lo_Zx (Num) /= 0;
   end Has_Number_X_Z;

   function Build_Number (Val : Uns32;
                          Ntype : Node := Null_Node;
                          Loc : Location_Type := No_Location)
                         return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Number);
      Set_Expr_Type (Res, Ntype);
      Set_Location (Res, Loc);
      Set_Number_Lo_Val (Res, Val);
      return Res;
   end Build_Number;

   function Build_Add (Num : Node;
                       Val : Uns32;
                       Loc : Location_Type := No_Location)
                      return Node
   is
      Res : Node;
      T : Uns64;
   begin
      Res := Create_Node (N_Number);
      Set_Expr_Type (Res, Get_Expr_Type (Num));
      Set_Location (Res, Loc);

      if Has_Number_X_Z (Num) then
         Set_Number_Lo_Zx (Res, not 0);
         Set_Number_Hi_Zx (Res, not 0);
      else
         T := Uns64 (Get_Number_Lo_Val (Num) + Val);
         Set_Number_Lo_Val (Res, Uns64_Lo (T));
         T := Uns64 (Uns64_Hi (T) + Get_Number_Hi_Val (Num));
         Set_Number_Hi_Val (Res, Uns64_Lo (T));
      end if;

      return Res;
   end Build_Add;

   function Find_Member_By_Id (Id : Name_Id; Chain : Node) return Node
   is
      Mb : Node;
   begin
      Mb := Chain;
      while Mb /= Null_Node loop
         if Get_Identifier (Mb) = Id then
            return Mb;
         end if;
         Mb := Get_Chain (Mb);
      end loop;
      return Null_Node;
   end Find_Member_By_Id;
end Verilog.Sem_Utils;
