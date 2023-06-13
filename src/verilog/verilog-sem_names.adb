--  Verilog semantic analyzer (names)
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

with Std_Names; use Std_Names;
with Errorout; use Errorout;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Types; use Verilog.Types;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Sem_Utils; use Verilog.Sem_Utils;
with Verilog.Sem_Types; use Verilog.Sem_Types;
with Verilog.Sem_Upwards;
with Verilog.Sem_Expr; use Verilog.Sem_Expr;
with Verilog.Sem_Eval; use Verilog.Sem_Eval;

package body Verilog.Sem_Names is
   function Find_Id_In_Chain (Items : Node; Id : Name_Id) return Node
   is
      N : Node;
   begin
      N := Items;
      while N /= Null_Node loop
         case Get_Kind (N) is
            when Nkinds_Nets
              | N_Typedef
              | N_Var
              | Nkinds_Net_Port
              | N_Parameter
              | N_Localparam
              | N_Function =>
               if Get_Identifier (N) = Id then
                  return N;
               end if;

               --  The declaration can declare its own enumeration type.
               --  So also look at the enumeration names.
               if Get_Type_Owner (N) then
                  declare
                     Atype : constant Node := Get_Data_Type (N);
                     El : Node;
                  begin
                     if Get_Kind (Atype) = N_Enum_Type then
                        El := Get_Enum_Names (Atype);
                        while El /= Null_Node loop
                           if Get_Identifier (El) = Id then
                              return El;
                           end if;
                           El := Get_Chain (El);
                        end loop;
                     end if;
                  end;
               end if;
            when N_Task
              | N_Extern_Function
              | N_Extern_Task
              | N_Import_DPI_Function
              | N_Typedef_Class
              | N_Class
              | N_Constraint
              | N_Module_Instance
              | N_Clocking
              | N_Modport
              | N_Modport_Input
              | N_Modport_Output
              | N_Modport_Inout
              | N_Type_Parameter =>
               if Get_Identifier (N) = Id then
                  return N;
               end if;
            when N_Export_DPI_Function
              | N_Assign
              | Nkinds_Process
              | N_Package_Import =>
               null;
            when others =>
               Error_Kind ("find_id_in_chain", N);
         end case;
         N := Get_Chain (N);
      end loop;
      return Null_Node;
   end Find_Id_In_Chain;

   function Find_Name_In_Decls (Items : Node; Name : Node) return Node is
   begin
      return Find_Id_In_Chain (Items, Get_Identifier (Name));
   end Find_Name_In_Decls;

   function Find_Name_In_Scope (Scope : Node; Name : Node) return Node
   is
      Id : constant Name_Id := Get_Identifier (Name);
   begin
      case Get_Kind (Scope) is
         when N_Module_Instance =>
            declare
               Inst : constant Node := Get_Instance (Scope);
               Res : Node;
            begin
               --  FIXME: search params, ports ?
               Res := Find_Id_In_Chain (Get_Items_Chain (Inst), Id);
               return Res;
            end;
         when N_Package =>
            return Find_Id_In_Chain (Get_Package_Item_Chain (Scope), Id);
         when N_Class
           | N_Generic_Class
           | N_Instantiated_Class =>
            declare
               Klass : Node;
               Res : Node;
            begin
               Klass := Scope;
               loop
                  Res := Find_Id_In_Chain
                    (Get_Parameter_Port_Chain (Klass), Id);
                  if Res /= Null_Node then
                     return Res;
                  end if;
                  Res := Find_Id_In_Chain (Get_Class_Item_Chain (Klass), Id);
                  if Res /= Null_Node then
                     return Res;
                  end if;
                  Klass := Iterate_Base_Class_Type (Klass);
                  exit when Klass = Null_Node;
               end loop;
               return Null_Node;
            end;
         when N_Type_Parameter =>
            declare
               Param : constant Node := Get_Parameter_Type (Scope);
               pragma Assert (Param /= Null_Node);
            begin
               return Find_Name_In_Scope (Param, Name);
            end;
         when N_Typedef =>
            declare
               Dtype : constant Node := Get_Type_Data_Type (Scope);
               pragma Assert (Dtype /= Null_Node);
            begin
               return Find_Name_In_Scope (Dtype, Name);
            end;
         when others =>
            Error_Kind ("find_name_in_scope", Scope);
      end case;
   end Find_Name_In_Scope;

   --  1800-2017 8.18 Data hiding and encapsulation
   procedure Check_Visibility (Decl : Node; Loc : Node)
   is
      Parent : constant Node := Get_Parent (Decl);
   begin
      --  We are only concerned by class properties and methods.
      if Get_Kind (Parent) not in Nkinds_Class then
         return;
      end if;

      case Get_Visibility (Decl) is
         when Visibility_None =>
            --  1800-2017 8.18 Data hiding and encapsulation.
            --  In systemVerilog, unqualified class properties and methods are
            --  public, available to anyone who has access to the object's
            --  name.
            return;
         when Visibility_Public =>
            --  Always ok.
            return;
         when Visibility_Local =>
            if Get_Class_Visibility (Parent) = Visibility_Local then
               return;
            end if;
         when Visibility_Protected =>
            if Get_Class_Visibility (Parent) >= Visibility_Protected then
               return;
            end if;
      end case;
      Error_Msg_Sem (+Loc, "member %i is not visible", +Decl);
   end Check_Visibility;

   procedure Sem_Member_Select (Name : Node)
   is
      Id : constant Name_Id := Get_Identifier (Name);
      Pfx : constant Node := Get_Name (Name);
      Ptype : constant Node := Get_Expr_Type (Pfx);
      Decl : Node;
   begin
      if Ptype = Null_Node then
         --  Error.
         return;
      end if;

      --  1800-2017 18.8 Disabling random variables with rand_mode()
      if Id = Name_Rand_Mode then
         --  TODO: allow only on random variable or object
         Set_Declaration (Name, Rand_Mode_Func_Method);
         return;
      end if;

      --  1800-2017 7.12.1 Array locator methods
      --  Array locator methods operate on any unpacked array, including
      --  queues [...]
      case Get_Kind (Ptype) is
         when N_Array_Cst
           | N_Queue_Cst
           | N_Associative_Array_Cst
           | N_Dynamic_Array_Cst =>
            case Id is
               when Name_Find_Index =>
                  Decl := Array_Find_Index_Method;
               when Name_Min =>
                  Decl := Array_Min_Method;
               when Name_Max =>
                  Decl := Array_Max_Method;
               when others =>
                  Decl := Null_Node;
            end case;
            if Decl /= Null_Node then
               Set_Declaration (Name, Decl);
               return;
            end if;
         when others =>
            null;
      end case;

      case Get_Kind (Ptype) is
         when N_String_Type =>
            case Id is
               when Name_Len =>
                  Decl := String_Len_Method;
               when Name_Toupper =>
                  Decl := String_Toupper_Method;
               when Name_Tolower =>
                  Decl := String_Tolower_Method;
               when Name_Substr =>
                  Decl := String_Substr_Method;
               when Name_Atoi =>
                  Decl := String_Atoi_Method;
               when Name_Atohex =>
                  Decl := String_Atohex_Method;
               when Name_Atooct =>
                  Decl := String_Atooct_Method;
               when Name_Atobin =>
                  Decl := String_Atobin_Method;
               when Name_Itoa =>
                  Decl := String_Itoa_Method;
               when others =>
                  Error_Msg_Sem (+Name, "unknown string method %i", +Id);
                  Decl := Null_Node;
            end case;
            Set_Declaration (Name, Decl);
            return;
         when N_Queue_Cst =>
            case Id is
               when Name_Size =>
                  Decl := Queue_Size_Method;
               when Name_Insert =>
                  Decl := Queue_Insert_Method;
               when Name_Delete =>
                  Decl := Queue_Delete_Method;
               when Name_Pop_Front =>
                  Decl := Queue_Pop_Front_Method;
               when Name_Pop_Back =>
                  Decl := Queue_Pop_Back_Method;
               when Name_Push_Front =>
                  Decl := Queue_Push_Front_Method;
               when Name_Push_Back =>
                  Decl := Queue_Push_Back_Method;
               when others =>
                  Error_Msg_Sem (+Name, "unknown queue method %i", +Id);
                  Decl := Null_Node;
            end case;
            Set_Declaration (Name, Decl);
            return;
         when N_Associative_Array_Cst =>
            case Id is
               when Name_Num =>
                  Decl := Associative_Num_Method;
               when Name_Size =>
                  Decl := Associative_Size_Method;
               when Name_Exists =>
                  Decl := Associative_Exists_Method;
               when Name_Delete =>
                  Decl := Associative_Delete_Method;
               when Name_First =>
                  Decl := Associative_First_Method;
               when Name_Last =>
                  Decl := Associative_Last_Method;
               when Name_Next =>
                  Decl := Associative_Next_Method;
               when Name_Prev =>
                  Decl := Associative_Prev_Method;
               when others =>
                  Error_Msg_Sem
                    (+Name, "unknown associative array method %i", +Id);
                  Decl := Null_Node;
            end case;
            Set_Declaration (Name, Decl);
            return;
         when N_Dynamic_Array_Cst =>
            case Id is
               when Name_Size =>
                  Decl := Dynamic_Size_Method;
               when Name_Delete =>
                  Decl := Dynamic_Delete_Method;
               when others =>
                  Error_Msg_Sem
                    (+Name, "unknown dynamic array method %i", +Id);
                  Decl := Null_Node;
            end case;
            Set_Declaration (Name, Decl);
            return;
         when N_Enum_Type =>
            case Id is
               when Name_First =>
                  Decl := Enum_First_Method;
               when Name_Last =>
                  Decl := Enum_Last_Method;
               when Name_Next =>
                  Decl := Enum_Next_Method;
               when Name_Prev =>
                  Decl := Enum_Prev_Method;
               when Name_Name =>
                  Decl := Enum_Name_Method;
               when others =>
                  Error_Msg_Sem (+Name, "unknown enum method %i", +Id);
                  Decl := Null_Node;
            end case;
            Set_Declaration (Name, Decl);
            return;
         when N_Event_Type =>
            case Id is
               when Name_Triggered =>
                  Decl := Event_Triggered_Method;
               when others =>
                  Error_Msg_Sem (+Name, "unknown event method %i", +Id);
                  Decl := Null_Node;
            end case;
            Set_Declaration (Name, Decl);
            return;
         when N_Struct_Type
            | N_Packed_Struct_Type =>
            declare
               Member : Node;
            begin
               Mutate_Dotted_Name (Name, N_Member_Name);
               Member := Get_Members (Ptype);
               while Member /= Null_Node loop
                  if Get_Identifier (Member) = Id then
                     Set_Declaration (Name, Member);
                     Set_Expr_Type (Name, Get_Type_Data_Type (Member));
                     return;
                  end if;
                  Member := Get_Chain (Member);
               end loop;
               Error_Msg_Sem
                 (+Name, "%i is not a member of the structure", +Id);
               return;
            end;
         when N_Class
           | N_Instantiated_Class =>
            declare
               Item : Node;
            begin
               Item := Find_Name_In_Scope (Ptype, Name);
               if Item /= Null_Node then
                  --  FIXME: visibility.
                  --  FIXME: static
                  Set_Declaration (Name, Item);
                  case Get_Kind (Item) is
                     when N_Var =>
                        Mutate_Dotted_Name (Name, N_Property_Name);
                        Set_Expr_Type (Name, Get_Type_Data_Type (Item));
                        Check_Visibility (Item, Name);
                     when N_Enum_Name =>
                        Mutate_Dotted_Name (Name, N_Class_Qualified_Name);
                        Set_Expr_Type (Name, Get_Expr_Type (Item));
                     when N_Parameter =>
                        Mutate_Dotted_Name (Name, N_Class_Qualified_Name);
                        Set_Expr_Type (Name, Get_Param_Type (Item));
                     when N_Function
                       | N_Extern_Function =>
                        Mutate_Dotted_Name (Name, N_Method_Name);
                        Set_Expr_Type (Name, Get_Type_Data_Type (Item));
                        Check_Visibility (Item, Name);
                     when N_Task
                       | N_Extern_Task =>
                        Mutate_Dotted_Name (Name, N_Method_Name);
                        Check_Visibility (Item, Name);
                     when others =>
                        Error_Kind ("sem_member_select(class)", Item);
                  end case;
                  return;
               end if;
               Error_Msg_Sem
                 (+Name, "%i is not an attribute of the class", +Id);
               return;
            end;
         when N_Modport =>
            declare
               Port : Node;
            begin
               Mutate_Dotted_Name (Name, N_Modport_Item);
               Port := Find_Id_In_Chain (Get_Modport_Ports_Chain (Ptype), Id);
               if Port = Null_Node then
                  Error_Msg_Sem
                    (+Name, "%i is not a port of %n", +Ptype);
               else
                  Set_Declaration (Name, Port);
                  Set_Expr_Type (Name, Get_Data_Type (Port));
               end if;
               return;
            end;
         when others =>
            Error_Kind ("sem_member_select", Ptype);
      end case;
   end Sem_Member_Select;

   procedure Sem_Hierarchical_Name (Name : Node)
   is
      pragma Assert (Get_Kind (Name) = N_Hierarchical);
      Pfx : constant Node := Get_Name (Name);
      Pfx_Decl : Node;
      Decl : Node;
   begin
      --  The prefix.
      Pfx_Decl := Get_Declaration (Pfx);
      if Pfx_Decl = Null_Node then
         if Get_Kind (Pfx) = N_Name then
            --  Resolve the root name.
            declare
               Id : constant Name_Id := Get_Identifier (Pfx);
            begin
               Pfx_Decl := Sem_Upwards.Find_Scope (Id);
               Set_Declaration (Pfx, Pfx_Decl);

               if Pfx_Decl = Null_Node then
                  Error_Msg_Sem (+Pfx, "no declaration for %i", +Id);
                  return;
               end if;
            end;
         else
            Sem_Hierarchical_Name (Pfx);
            Pfx_Decl := Get_Declaration (Pfx);
            if Pfx_Decl = Null_Node then
               return;
            end if;
         end if;
      end if;

      case Get_Kind (Pfx_Decl) is
         when N_Module_Instance =>
            Decl := Find_Name_In_Scope (Pfx_Decl, Name);
         when N_Interface_Instance =>
            Mutate_Dotted_Name (Name, N_Interface_Item);
            declare
               Dinter : constant Node :=
                 Get_Declaration (Get_Interface_Name (Pfx_Decl));
            begin
               Decl := Find_Name_In_Decls (Get_Items_Chain (Dinter), Name);
            end;
         when N_Interface_Port =>
            Mutate_Dotted_Name (Name, N_Interface_Item);
            declare
               Pfx_Type : constant Node := Get_Data_Type (Pfx_Decl);
            begin
               if Pfx_Type = Null_Node then
                  --  There has been already an error message for the prefix.
                  return;
               end if;
               Decl := Find_Name_In_Decls (Get_Items_Chain (Pfx_Type), Name);
            end;
         when others =>
            Error_Kind ("sem_hierarchical_name", Pfx_Decl);
      end case;
      Set_Declaration (Name, Decl);

      if Decl = Null_Node then
         Error_Msg_Sem (+Name, "%i not declared in the scope of %i",
                        (+Get_Identifier (Name), +Get_Identifier (Pfx_Decl)));
      end if;
   end Sem_Hierarchical_Name;

   procedure Sem_Dotted_Name (Name : Node)
   is
      Pfx : Node;
   begin
      Pfx := Sem_Name (Get_Name (Name));
      Set_Name (Name, Pfx);

      case Get_Kind (Pfx) is
         when N_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_This =>
            Pfx := Get_Declaration (Pfx);
            if Pfx = Null_Node then
               --  Error.
               return;
            end if;
         when others =>
            null;
      end case;

      case Get_Kind (Pfx) is
         when N_Var
           | Nkinds_Nets
           | N_Return_Var
           | N_This_Var
           | N_This_Name
           | N_Super
           | N_Foreach_Variable
           | N_Iterator_Argument
           | Nkinds_Tf_Port
           | N_Member
           | N_Indexed_Name
           | N_Associative_Index
           | N_Member_Name
           | N_Property_Name
           | N_Modport_Port
           | N_Call =>
            Sem_Member_Select (Name);
            return;
         when N_Class =>
            Error_Msg_Sem (+Name, "class name cannot be selected, use ::");
            --  TODO: maybe resolve name to extract the type.
            return;
         when N_Bit_Select =>
            Error_Msg_Sem (+Name, "prefix cannot be selected");
            return;
         when others =>
            Error_Kind ("sem_dotted_name", Pfx);
      end case;
   end Sem_Dotted_Name;

   function Sem_Bit_Select_Name (Name : Node) return Node
   is
      Ref : Node;
      Ref_Type : Node;
      Idx : Node;
      Idx_Type : Node;
      Res : Node;
      Expr : Node;
   begin
      Ref := Sem_Name (Get_Name (Name));
      Set_Name (Name, Ref);

      Ref_Type := Get_Expr_Type (Ref);

      Expr := Get_Expression (Name);
      if Expr /= Null_Node and then Get_Kind (Expr) = N_Infinity then
         if Ref_Type /= Null_Node and then Get_Kind (Ref_Type) /= N_Queue_Cst
         then
            Error_Msg_Sem (+Name, "only queues can be indexed with '$'");
         end if;
      else
         if Ref_Type /= Null_Node
           and then Get_Kind (Ref_Type) = N_Associative_Array_Cst
         then
            --  Use the index type of the map to analyze the expression.  This
            --  is important for string literals, because it can be either a
            --  numeric or a string type.
            Idx_Type := Get_Type_Index_Type (Ref_Type);
         else
            Idx_Type := Null_Node;
         end if;
         Idx := Sem_Expression (Expr, Idx_Type);
         Set_Expression (Name, Idx);
      end if;

      if Ref_Type = Null_Node then
         --  Error.
         return Name;
      end if;
      case Get_Kind (Ref_Type) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst
           | N_Dynamic_Array_Cst
           | N_Queue_Cst
           | N_Associative_Array_Cst =>
            Set_Expr_Type (Name, Get_Type_Element_Type (Ref_Type));
         when N_String_Type =>
            --  1800-2017 6.16 String data type
            --  Indexing.  Returns a byte, the ASCII code at the
            --  given index.
            Set_Expr_Type (Name, Signed_Byte_Type);
         when others =>
            Error_Msg_Sem (+Name, "name cannot be indexed");
            return Name;
      end case;

      --  Transform to indexed_name unless this is really a bit
      --  select.
      case Get_Kind (Ref_Type) is
         when Nkinds_Vector_Types =>
            if Get_Kind (Get_Type_Element_Type (Ref_Type))
              in Nkinds_Scalar_Types
            then
               --  Result is a bit.
               return Name;
            end if;
            --  Result is a nested vector.
            Res := Create_Node (N_Indexed_Name);
         when N_String_Type =>
            --  String index is special as the result is not a name: it
            --  cannot be used as a ref.
            Res := Create_Node (N_String_Index);
         when N_Associative_Array_Cst =>
            Res := Create_Node (N_Associative_Index);
         when others =>
            Res := Create_Node (N_Indexed_Name);
      end case;
      Location_Copy (Res, Name);
      Set_Expr_Type (Res, Get_Expr_Type (Name));
      Set_Name (Res, Ref);
      Set_Expression (Res, Idx);
      Free_Node (Name);
      return Res;
   end Sem_Bit_Select_Name;

   --  1800-2017 11.5.1 Vector bit-select and part-select addressing
   function Sem_Non_Indexed_Part_Select_Name (Name : Node) return Node
   is
      Ref : Node;
      Ref_Type : Node;
      Msb, Lsb : Node;
      Msb_Int, Lsb_Int : Int32;
      Ref_Msb, Ref_Lsb : Int32;
      Is_Const : Boolean;
      Res_Type : Node;
      Res : Node;
   begin
      Ref := Sem_Name (Get_Name (Name));
      Set_Name (Name, Ref);
      Ref_Type := Get_Expr_Type (Ref);

      --  Indexes must be const unless selecting a queue.
      Is_Const := Ref_Type /= Null_Node
        and then Get_Kind (Ref_Type) /= N_Queue_Cst;

      --  1800-2017 11.5.1 Vector bit-select and part-select addressing
      --  Both msb_expr and lsb_expr shall be constant integer expressions.
      --  Each of these expressions shall be evaluated in a self-determined
      --  context.
      Msb := Sem_Expression (Get_Msb (Name), Null_Node);
      Set_Msb (Name, Msb);

      Msb_Int := 0;
      if not Is_Integral_Type (Get_Expr_Type (Msb))
        or else (Is_Const and then not Get_Is_Constant (Msb))
      then
         Error_Msg_Sem
           (+Msb, "part selected expression must be a constant integer");
      else
         if Is_Const then
            Msb_Int := Sem_Constant_Integer_Expression (Msb);
         end if;
      end if;

      Lsb := Sem_Expression (Get_Lsb (Name), Null_Node);
      Set_Lsb (Name, Lsb);

      Lsb_Int := 0;
      if not Is_Integral_Type (Get_Expr_Type (Lsb))
        or else (Is_Const and then not Get_Is_Constant (Lsb))
      then
         Error_Msg_Sem
           (+Lsb, "part selected expression must be a constant integer");
      else
         if Is_Const then
            Lsb_Int := Sem_Constant_Integer_Expression (Lsb);
         end if;
      end if;

      if Ref_Type = Null_Node then
         --  Error.
         return Name;
      end if;

      case Get_Kind (Ref_Type) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            --  1364-2005 5.5.1 Rules for expression types
            --  Part-select results are unsigned, regardless of the
            --  operands even if the part-select specified the entire
            --  vector.
            Res_Type := Get_Packed_Array_Type
              (Msb_Int, Lsb_Int, Get_Type_Element_Type (Ref_Type), False);
            Res := Create_Node (N_Part_Select_Cst);
         when N_Array_Cst =>
            Res_Type := Get_Array_Type
              (Msb_Int, Lsb_Int, Get_Type_Element_Type (Ref_Type));
            Res := Create_Node (N_Slice_Name_Cst);
         when N_Queue_Cst =>
            Res_Type := Get_Queue_Type (Get_Type_Element_Type (Ref_Type), -1);
            Res := Create_Node (N_Slice_Name_Cst);
         when others =>
            Error_Msg_Sem (+Name, "only arrays can be indexed");
            return Name;
      end case;

      --  Check if direction matches

      --  1800-2017 11.5.1 Vector bit-select and part-select addressing
      --  The first expression shall address a more significant bit than the
      --  second expression.
      case Get_Kind (Ref_Type) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst =>
            Ref_Msb := Get_Msb_Cst (Ref_Type);
            Ref_Lsb := Get_Lsb_Cst (Ref_Type);
            if Ref_Msb > Ref_Lsb then
               --  HI:LO
               if Msb_Int < Lsb_Int then
                  Error_Msg_Sem
                    (+Name, "incorrect direction of the selection");
               elsif Msb_Int > Ref_Msb or Lsb_Int < Ref_Lsb then
                  Warning_Msg_Sem (+Name, "selection out of the bounds");
               end if;
            else
               --  LO:HI
               if Msb_Int > Lsb_Int then
                  Error_Msg_Sem
                    (+Name, "incorrect direction of the selection");
               elsif Lsb_Int > Ref_Lsb or Msb_Int < Ref_Msb then
                  Warning_Msg_Sem (+Name, "selection out of the bounds");
               end if;
            end if;
         when others =>
            null;
      end case;

      --  Replace the node.
      Location_Copy (Res, Name);
      Set_Msb_Cst (Res, Msb_Int);
      Set_Lsb_Cst (Res, Lsb_Int);
      Set_Name (Res, Ref);
      Set_Expr_Type (Res, Res_Type);
      Free_Node (Name);

      return Res;
   end Sem_Non_Indexed_Part_Select_Name;

   --  1800-2017 11.5.1 Vector bit-select and part-select addressing
   function Sem_Indexed_Part_Select_Name (Name : Node) return Node
   is
      Ref : Node;
      Ref_Type : Node;
      Base, Width : Node;
      Width_Int : Int32;
      Res_Type : Node;
      Res : Node;
   begin
      Ref := Sem_Name (Get_Name (Name));
      Set_Name (Name, Ref);

      --  1800-2017 11.5.1 Vector bit-select and part-select addressing
      --  The msb_base_expr and lsb_base_expr shall be integer expressions,
      --  and the width_expr shall be a positive constant integer expression.
      --  Each of these expressions shall be evaluated in a self-determined
      --  context.
      Base := Sem_Expression (Get_Base_Expr (Name), Null_Node);
      Set_Base_Expr (Name, Base);

      if not Is_Integral_Type (Get_Expr_Type (Base)) then
         Error_Msg_Sem (+Base, "base expression must be an integer");
      end if;

      Width := Sem_Sub_Expression (Get_Width_Expr (Name), Null_Node);
      Set_Width_Expr (Name, Width);

      if not Is_Integral_Type (Get_Expr_Type (Width))
        or else not Get_Is_Constant (Width)
      then
         Error_Msg_Sem
           (+Width, "width expression must be a positive constant integer");
         Width_Int := 1;
      else
         Width_Int := Sem_Constant_Integer_Expression (Width);
         if Width_Int <= 0 then
            Error_Msg_Sem (+Width, "width expression must be positive");
            Width_Int := 1;
         end if;
      end if;

      Ref_Type := Get_Expr_Type (Ref);
      if Ref_Type = Null_Node then
         --  Error.
         return Name;
      end if;

      case Get_Kind (Ref_Type) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            --  1364-2005 5.5.1 Rules for expression types
            --  Part-select results are unsigned, regardless of the
            --  operands even if the part-select specified the entire
            --  vector.

            --  FIXME: bounds ?
            Res_Type := Get_Packed_Array_Type
              (Width_Int - 1, 0, Get_Type_Element_Type (Ref_Type), False);
            Res := Create_Node (N_Part_Select_Cst);
         when others =>
            Error_Msg_Sem (+Name, "only packed arrays can be selected");
            return Name;
      end case;

      case Nkinds_Indexed_Part_Select (Get_Kind (Name)) is
         when N_Plus_Part_Select =>
            Res := Create_Node (N_Plus_Part_Select_Cst);
         when N_Minus_Part_Select =>
            Res := Create_Node (N_Minus_Part_Select_Cst);
      end case;

      --  Replace the node.
      Location_Copy (Res, Name);
      Set_Base_Expr (Res, Base);
      Set_Width_Cst (Res, Width_Int);
      Set_Name (Res, Ref);
      Set_Expr_Type (Res, Res_Type);
      Free_Node (Name);

      return Res;
   end Sem_Indexed_Part_Select_Name;

   procedure Sem_Scoped_Name (Name : Node)
   is
      Res : Node;
      Pfx : Node;
   begin
      Res := Get_Declaration (Name);
      if Res /= Null_Node then
         --  Already resolved.  Prefix must be a package.
         Pfx := Get_Name (Name);
         Pfx := Get_Declaration (Pfx);
      else
         Pfx := Get_Name (Name);
         Pfx := Sem_Name (Pfx);
         Set_Name (Name, Pfx);
         Pfx := Get_Declaration (Pfx);
         if Pfx /= Null_Node then
            Res := Find_Name_In_Scope (Pfx, Name);
         else
            --  Not found.
            Res := Null_Node;
         end if;
         if Res = Null_Node then
            --  TODO: create an error decl ?
            Set_Expr_Type (Name, Error_Type);
            return;
         end if;
         Set_Declaration (Name, Res);
      end if;

      case Get_Kind (Res) is
         when N_Var =>
            if Get_Kind (Pfx) in Nkinds_Class
              and then not Get_Static_Flag (Res)
            then
               Error_Msg_Sem
                 (+Name, "cannot use :: for non-static property  %i", +Res);
            end if;
            Set_Expr_Type (Name, Get_Type_Data_Type (Res));
            Check_Visibility (Res, Name);
         when N_Function
           | N_Extern_Function =>
            if Get_Kind (Pfx) in Nkinds_Class
              and then not Get_Static_Flag (Res)
            then
               Error_Msg_Sem
                 (+Name, "cannot use :: for non-static method  %i", +Res);
            end if;
            Set_Expr_Type (Name, Get_Type_Data_Type (Res));
            Check_Visibility (Res, Name);
         when N_Enum_Name =>
            Set_Expr_Type (Name, Get_Expr_Type (Res));
         when N_Parameter =>
            Set_Expr_Type (Name, Get_Param_Type (Res));
         when N_Task
           | N_Extern_Task =>
            if Get_Kind (Pfx) in Nkinds_Class
              and then not Get_Static_Flag (Res)
            then
               Error_Msg_Sem
                 (+Name, "cannot use :: for a non-static method %i", +Res);
            end if;
            Check_Visibility (Res, Name);
         when N_Typedef =>
            null;
         when others =>
            Error_Kind ("sem_scoped_name", Res);
      end case;
   end Sem_Scoped_Name;

   procedure Sem_Name_Declaration (Name : Node)
   is
      Decl : constant Node := Get_Declaration (Name);
   begin
      if Decl = Null_Node then
         return;
      end if;

      case Get_Kind (Decl) is
         when N_Localparam
           | N_Parameter =>
            Set_Expr_Type (Name, Get_Param_Type (Decl));
            Set_Is_Constant (Name, True);
         when N_Enum_Name =>
            Set_Expr_Type (Name, Get_Expr_Type (Decl));
            Set_Is_Constant (Name, True);
         when N_Var =>
            Set_Expr_Type (Name, Get_Type_Data_Type (Decl));
            Check_Visibility (Decl, Name);
         when Nkinds_Nets
           | Nkinds_Tf_Port
           | Nkinds_Net_Port
           | N_Interface_Port
           | N_Modport_Port
           | N_Modport_Input
           | N_Modport_Output =>
            Set_Expr_Type (Name, Get_Type_Data_Type (Decl));
         when N_Return_Var
           | N_Iterator_Argument
           | N_Foreach_Variable =>
            Set_Expr_Type (Name, Get_Expr_Type (Decl));
         when N_Modport =>
            Set_Expr_Type (Name, Decl);
         when N_Function
           | N_Extern_Function =>
            --  1800-2017 13.4.1 Return values and void functions
            --  [...] or by assigning a value to the internal
            --  variable with the same name as the function.
            --  FIXME: check this is the current function.
            declare
               Ret : constant Node := Get_Return_Variable (Decl);
            begin
               Set_Declaration (Name, Ret);
               Set_Expr_Type (Name, Get_Expr_Type (Ret));
               Check_Visibility (Decl, Name);
            end;
         when N_Task =>
            Check_Visibility (Decl, Name);
         when N_Genvar =>
            if Get_Expr_Type (Decl) = Null_Node then
               Error_Msg_Sem
                 (+Name, "genvar cannot be referenced outside loop "
                    & "generate scheme");
            else
               Set_Expr_Type (Name, Get_Expr_Type (Decl));
            end if;
         when N_Type_Parameter =>
            Set_Expr_Type (Name, Get_Parameter_Type (Decl));
         when N_Class
           | N_Generic_Class
           | N_Instantiated_Class
           | N_Module_Instance
           | N_Program_Instance
           | N_Primitive_Instance
           | N_Interface_Instance
           | N_Constraint
           | N_Package =>
            null;
         when N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward =>
            declare
               Fwd_Decl : constant Node := Get_Forward_Type (Decl);
            begin
               if Fwd_Decl /= Null_Node then
                  Set_Declaration (Name, Fwd_Decl);
                  Set_Expr_Type (Name, Fwd_Decl);
               end if;
            end;
         when N_Typedef =>
            Set_Expr_Type (Name, Get_Type_Data_Type (Decl));
         when N_Predefined_Typedef =>
            Set_Expr_Type (Name, Get_Expr_Type (Decl));

         when N_Discipline =>
            Set_Expr_Type (Name, Decl);

         when others =>
            Error_Kind ("sem_name_declaration", Decl);
      end case;
   end Sem_Name_Declaration;

   --  Analyze NAME.  Set declaration and type (for name that designate an
   --  object).  Always return a valid name, but in case of error, the
   --  declaration field is not set.
   function Sem_Name (Name : Node) return Node
   is
      Res : Node;
   begin
      case Get_Kind (Name) is
         when N_Name
           | N_This_Name
           | N_Property_Name =>
            Sem_Name_Declaration (Name);
            return Name;

         when N_Scoped_Name =>
            Sem_Scoped_Name (Name);
            Sem_Name_Declaration (Name);
            return Name;

         when N_This =>
            Res := Get_Declaration (Name);
            if Res = Null_Node then
               Error_Msg_Sem
                 (+Name, "'this' cannot be used outside of a method");
            else
               Set_Expr_Type (Name, Get_Expr_Type (Res));
            end if;
            return Name;

         when N_Super =>
            Res := Get_Declaration (Name);
            if Res = Null_Node then
               --  FIXME: also allowed anywhere in a derived class.
               Error_Msg_Sem
                 (+Name, "'super' cannot be used outside of a method");
               return Name;
            end if;
            Res := Get_Expr_Type (Res);
            Res := Get_Base_Class_Type (Res);
            if Res = Null_Node then
               --  1800-2017 8.15 Super
               --  The SUPER keyword is used from within a derived class [...]
               Error_Msg_Sem
                 (+Name, "'super' cannot be used in a non-derived class");
               return Name;
            end if;
            Set_Expr_Type (Name, Get_Expr_Type (Res));
            return Name;

         when N_Class_Instance =>
            declare
               Klass : Node;
               Decl : Node;
            begin
               Klass := Sem_Name (Get_Class_Name (Name));
               Set_Class_Name (Name, Klass);
               Decl := Get_Declaration (Klass);
               if Decl = Null_Node then
                  return Null_Node;
               end if;
               case Get_Kind (Decl) is
                  when N_Typedef_Class =>
                     Set_Declaration (Klass, Get_Forward_Type (Decl));
                     Sem_Class_Instance (Name);
                  when N_Generic_Class =>
                     Sem_Class_Instance (Name);
                  when N_Instantiated_Class =>
                     Set_Declaration (Name, Decl);
                     Set_Expr_Type (Name, Decl);
                  when others =>
                     Error_Kind ("sem_name(class_instance)", Decl);
               end case;
               return Name;
            end;

         when N_Dotted_Name =>
            Sem_Dotted_Name (Name);
            return Name;

         when N_Hierarchical =>
            Sem_Hierarchical_Name (Name);
            Sem_Name_Declaration (Name);
            return Name;

         when N_Interface_Item =>
            declare
               Pfx : Node;
               Decl : Node;
            begin
               Pfx := Get_Name (Name);
               Pfx := Sem_Name (Pfx);
               if Pfx = Null_Node then
                  return Name;
               end if;
               Set_Name (Name, Pfx);
               Pfx := Get_Expr_Type (Pfx);
               pragma Assert (Pfx /= Null_Node);
               pragma Assert (Get_Kind (Pfx) = N_Interface_Declaration);
               Decl := Find_Name_In_Decls (Get_Items_Chain (Pfx), Name);
               Set_Declaration (Name, Decl);
               if Decl = Null_Node then
                  Error_Msg_Sem (+Name, "%i is not an item of %n", +Pfx);
               else
                  Sem_Name_Declaration (Name);
               end if;
               return Name;
            end;

         when N_Modport_Item =>
            declare
               Pfx : Node;
               Decl : Node;
            begin
               Pfx := Get_Name (Name);
               Pfx := Sem_Name (Pfx);
               if Pfx = Null_Node then
                  return Name;
               end if;
               Set_Name (Name, Pfx);
               Pfx := Get_Expr_Type (Pfx);
               pragma Assert (Pfx /= Null_Node);
               pragma Assert (Get_Kind (Pfx) = N_Modport);
               Decl := Find_Name_In_Decls (Get_Modport_Ports_Chain (Pfx),
                                           Name);
               Set_Declaration (Name, Decl);
               if Decl = Null_Node then
                  Error_Msg_Sem (+Name, "%i is not a port of %n", +Pfx);
               else
                  Sem_Name_Declaration (Name);
               end if;
               return Name;
            end;

         when N_Bit_Select =>
            return Sem_Bit_Select_Name (Name);

         when N_Part_Select =>
            return Sem_Non_Indexed_Part_Select_Name (Name);
         when N_Plus_Part_Select
           | N_Minus_Part_Select =>
            return Sem_Indexed_Part_Select_Name (Name);

         when N_Call =>
            Sem_Subroutine_Call_Name (Name);
            Res := Sem_Subroutine_Call_Suffix (Name);
            return Res;

         when others =>
            Error_Kind ("sem_name", Name);
      end case;
   end Sem_Name;

   function Sem_Tf_Name (Name : Node) return Node is
   begin
      case Get_Kind (Name) is
         when N_Name =>
            return Name;
         when N_Hierarchical =>
            Sem_Hierarchical_Name (Name);
            return Name;
         when N_Dotted_Name =>
            Sem_Dotted_Name (Name);
            return Name;
         when N_Scoped_Name =>
            --  A scoped name may not be yet resolved if the scope is a
            --  parameter type.
            Sem_Scoped_Name (Name);
            return Name;
         when N_Method_Name
           | N_This_Name =>
            return Name;
         when others =>
            Error_Kind ("sem_tf_name", Name);
      end case;
   end Sem_Tf_Name;

   function Sem_Lvalue (Lval : Node;
                        Allow_Net : Boolean := False;
                        Allow_Var : Boolean := False)
                       return Node
   is
      Res : Node;
      Base : Node;
      Vtype : Node;
   begin
      case Get_Kind (Lval) is
         when N_Name
           | N_Hierarchical
           | N_Bit_Select
           | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Property_Name
           | N_This_Name
           | N_Dotted_Name
           | N_Scoped_Name
           | N_Interface_Item
           | N_Modport_Item =>
            Res := Sem_Name (Lval);
            --  and continue below.
         when N_Concatenation =>
            declare
               El : Node;
               Expr : Node;
               Etype : Node;
               Width : Width_Type;
            begin
               El := Get_Expressions (Lval);
               Width := 0;
               while El /= Null_Node loop
                  Expr := Get_Expression (El);
                  Expr := Sem_Lvalue (Expr,
                                      Allow_Net => Allow_Net,
                                      Allow_Var => Allow_Var);
                  if Expr /= Null_Node then
                     Set_Expression (El, Expr);
                     Etype := Get_Expr_Type (Expr);
                     if Is_Integral_Type (Etype) then
                        Width := Width + Get_Type_Width (Etype);
                     else
                        --  FIXME: Ref ?
                        Error_Msg_Sem
                          (+Expr, "only integral types can be concatenated");
                     end if;
                  end if;
                  El := Get_Chain (El);
               end loop;
               --  FIXME: bit vs logic ?
               Etype := Get_Packed_Array_Type
                 (Int32 (Width) - 1, 0, Unsigned_Logic_Type, False);
               Set_Expr_Type (Lval, Etype);
            end;
            return Lval;
         when others =>
            Error_Kind ("sem_lvalue(1)", Lval);
      end case;

      Base := Get_Base_Lvalue (Res);
      loop
         if Base = Null_Node then
            return Res;
         end if;

         case Get_Kind (Base) is
            when Nkinds_Net_Port =>
               Base := Get_Redeclaration (Base);
               pragma Assert (Base /= Null_Node);
            when N_Interface_Item
               | N_Modport_Item =>
               return Res;
            when N_Var
               | N_Return_Var
               | N_Foreach_Variable
               | N_This_Var
               | N_This
               | Nkinds_Tf_Port
               | N_Genvar =>
               --  FIXME: genvar is not an lvalue except for loop_generate!
               if not Allow_Var then
                  Error_Msg_Sem (+Lval, "lvalue must be a net");
               end if;
               return Res;
            when Nkinds_Nets =>
               if not Allow_Net then
                  Error_Msg_Sem (+Lval, "lvalue must be a variable");
               else
                  Vtype := Get_Type_Data_Type (Base);
                  case Get_Kind (Vtype) is
                     when N_Array_Cst =>
                        Error_Msg_Sem
                          (+Lval, "assignment to memories are not allowed");
                     when N_Log_Packed_Array_Cst =>
                        null;
                     when N_Logic_Type
                       | N_Bit_Type =>
                        null;
                     when others =>
                        Error_Kind ("sem_lvalue(wire)", Vtype);
                  end case;
               end if;
               return Res;
            when others =>
               Error_Kind ("sem_lvalue(2)", Base);
         end case;
      end loop;
   end Sem_Lvalue;

   function Sem_Branch_Lvalue (Lval : Node) return Node
   is
      Func : Node;
   begin
      if Get_Kind (Lval) /= N_Call then
         Error_Msg_Sem (+Lval, "lvalue must be a nature access call");
         return Lval;
      end if;

      Func := Get_Subroutine (Lval);
      if Get_Kind (Func) /= N_Name then
         Error_Msg_Sem (+Lval, "lvalue must be a nature access call");
         return Lval;
      end if;

      Func := Get_Declaration (Func);
      if Get_Kind (Func) /= N_Nature_Access then
         Error_Msg_Sem (+Lval, "lvalue must be a nature access call");
         return Lval;
      end if;

      Set_Expr_Type (Lval, Get_Parent (Func));

      --  TODO: build nature access function
      return Lval;
   end Sem_Branch_Lvalue;

end Verilog.Sem_Names;
