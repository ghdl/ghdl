--  Verilog semantic analyzer (declarations)
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
with Types; use Types;
with Errorout; use Errorout;

with Verilog.Standard; use Verilog.Standard;
with Verilog.Nodes_Meta;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Sem_Types; use Verilog.Sem_Types;
with Verilog.Sem_Scopes; use Verilog.Sem_Scopes;
with Verilog.Sem_Expr; use Verilog.Sem_Expr;
with Verilog.Sem_Names; use Verilog.Sem_Names;
with Verilog.Resolve_Names;
with Verilog.Sem_Eval; use Verilog.Sem_Eval;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Allocates;

package body Verilog.Sem_Decls is
   --  Analyze the type of DECL.
   procedure Sem_Decl_Data_Type (Decl : Node)
   is
      Typ : constant Node := Get_Data_Type (Decl);
   begin
      if Get_Type_Owner (Decl) then
         Sem_Data_Type (Typ);
      else
         --  Predefined type or shared type.
         pragma Assert (Get_Expr_Type (Typ) /= Null_Node);
         null;
      end if;
   end Sem_Decl_Data_Type;

   --  For list of identifiers: they share the same type.
   procedure Sem_Decl_List_Data_Type (Head : Node)
   is
      Parent : Node;
      Old : Node;
      Ntype : Node;
      El : Node;
      El_Type : Node;
      Depth : Natural;
      Next_El : Node;
   begin
      if not Get_Has_Identifier_List (Head) then
         --  The first element of the list: analyze the whole type.
         Sem_Decl_Data_Type (Head);
         return;
      end if;

      --  The declarations may have a different type due to arrays, eg:
      --    bit [3:0] a, b [0:7];

      --  Find the shared part.
      Parent := Head;
      Old := Get_Data_Type (Parent);
      Depth := 0;
      loop
         case Get_Kind (Old) is
            when N_Typedef
              | N_Predefined_Typedef
              | N_Logic_Type
              | N_Bit_Type =>
               --  Builtin type.
               pragma Assert (not Get_Type_Owner (Parent));
               exit;
            when N_Name
              | N_Scoped_Name
              | N_Packed_Array =>
               pragma Assert (Get_Type_Owner (Parent));
               exit;
            when N_String_Type =>
               exit;
            when N_Array
              | N_Dynamic_Array
              | N_Queue =>
               Parent := Old;
               Old := Get_Element_Data_Type (Old);
            when others =>
               --  FIXME: be sure all types are handled.
               Error_Kind ("sem_decl_list_data_type(1)", Old);
         end case;
         Depth := Depth + 1;
      end loop;

      --  Analyze the type.

      if Get_Type_Owner (Head) then
         Sem_Data_Type (Get_Data_Type (Head));
      end if;

      --  Find the new type that is shared with later declarations.

      Parent := Head;
      Ntype := Get_Data_Type (Parent);
      for I in 1 .. Depth loop
         Ntype := Get_Element_Data_Type (Ntype);
      end loop;

      El := Head;
      while Get_Has_Identifier_List (El) loop
         --  This is somewhat a mis-use of the flag: avoid to call this
         --  procedure for each element of the list.
         Set_Has_Identifier_List (El, False);

         Next_El := Get_Chain (El);

         --  TODO: remove that skip, just set Has_Identifier_List.
         if Get_Kind (El) in Nkinds_Net_Port
           and then Get_Redeclaration (El) = Next_El
           and then Get_Implicit_Flag (Next_El)
         then
            --  Copy type to the redeclaration.
            pragma Assert (not Get_Type_Owner (Next_El));
            Set_Data_Type (Next_El, Old);
            --  And skip it.
            Next_El := Get_Chain (Next_El);
         end if;
         El := Next_El;

         Parent := El;
         El_Type := Get_Data_Type (Parent);
         --  Note: El_Type can be a stalled node.
         loop
            if not Get_Type_Owner (Parent) then
               pragma Assert (El_Type = Old);
               if Parent = El then
                  Set_Data_Type (Parent, Ntype);
               else
                  Set_Element_Data_Type (Parent, Ntype);
               end if;
               exit;
            else
               case Get_Kind (El_Type) is
                  when N_Array
                    | N_Dynamic_Array
                    | N_Queue =>
                     pragma Assert (Get_Type_Owner (Parent));
                     Parent := El_Type;
                     El_Type := Get_Element_Data_Type (Parent);
                  when others =>
                     Error_Kind ("sem_decl_list_data_type(2)", El_Type);
               end case;
            end if;
         end loop;
      end loop;
   end Sem_Decl_List_Data_Type;

   --  3.10  Parameters.
   procedure Sem_Parameter (Param : Node)
   is
      Param_Type : Node;
      Expr : Node;
      Val : Node;
   begin
      if Get_Param_Type (Param) /= Null_Node then
         --  Already analyzed.  For localparam of generate blocks.
         pragma Assert
           (Get_Kind (Get_Parent (Param)) = N_Indexed_Generate_Block);
         Val := Get_Expression (Param);
         Verilog.Allocates.Allocate_Parameter (Param, Val);
         return;
      end if;

      --  IEEE1364-2005 4.10.1 Module parameters
      --  - A parameter with a range specification, but with no type
      --    specification, shall be (of ??) the range of the parameter
      --    declaration and shall be unsigned.  The sign and range shall not
      --    be affected by value overrides.
      --  - A parameter with a type specification, but with no range
      --    specification, shall be of the type specified.  A signed parameter
      --    shall default to the range of the final value assigned to the
      --    parameter, after any value overrides have been applied. [TODO].
      --  - A parameter with a signed type specification and with a range
      --    specification shall be signed and shall be (of ??) the range of its
      --    declaration.  The sign and range shall not be affected by value
      --    overrides.
      Param_Type := Get_Data_Type (Param);
      if Param_Type /= Null_Node then
         if Get_Type_Owner (Param) then
            Sem_Data_Type (Param_Type);
         end if;
         Param_Type := Get_Expr_Type (Param_Type);
      end if;

      --  Parameter expression (possibly from an override).
      Expr := Get_Expression (Param);
      if Get_Kind (Param) = N_Parameter then
         Val := Get_Override_Stmt (Param);
         if Val /= Null_Node then
            --  Override.
            if Get_Kind (Val) = N_Parameter_Value_Expr then
               Val := Get_Expression (Val);
            else
               --  TODO: defparam.
               raise Internal_Error;
            end if;
            Expr := Null_Node;
         else
            Val := Expr;
         end if;
      else
         Val := Expr;
      end if;

      if Val = Null_Node then
         Error_Msg_Sem (+Param, "no value for parameter %i", +Param);
         pragma Assert (Expr = Null_Node);
         Val := Build_Error_Expr (Param);
         Set_Expression (Param, Val);
         Expr := Val;
      end if;

      Val := Sem_Constant_Expression (Val, Param_Type);
      if Param_Type = Null_Node then
         --  IEEE1364-2005 4.10.1 Module parameters
         --  - A parameter declaration wih no type or range specification shall
         --    default to the type and range of the final value assigned to the
         --    parameter, after any value overrides have been applied.
         Param_Type := Get_Expr_Type (Val);
      end if;

      Set_Param_Type (Param, Param_Type);

      if Expr /= Null_Node then
         Val := Sem_Propagate_Length (Val, Param_Type);
         Set_Expression (Param, Val);
      end if;

      Verilog.Allocates.Allocate_Parameter (Param, Val);

      if Get_Kind (Param) = N_Parameter then
         Set_Parameter_Expression (Param, Val);
      end if;
   end Sem_Parameter;

   procedure Sem_Typedef_Type (Def : Node) is
   begin
      --  First analyze the type.  Self-reference are not possible.
      if Get_Resolved_Flag (Def) then
         return;
      end if;

      if Get_Mark_Flag (Def) then
         Error_Msg_Sem (+Def, "recursive type not allowed");
         return;
      end if;

      Set_Mark_Flag (Def, True);

      Sem_Decl_Data_Type (Def);

      Set_Mark_Flag (Def, False);
      Set_Resolved_Flag (Def, True);
   end Sem_Typedef_Type;

   --  For a forward typedef DEF, check that the corresponding completion DECL
   --  matches the type.
   procedure Check_Forward_Typedef_Data_Type (Def : Node; Decl : Node)
   is
      Decl1 : Node;
   begin
      case Nkinds_Forward_Typedef (Get_Kind (Def)) is
         when N_Typedef_Forward =>
            --  Can be any type.
            case Get_Kind (Decl) is
               when N_Typedef
                 | Nkinds_Forward_Typedef
                 | N_Class
                 | N_Generic_Class =>
                  null;
               when others =>
                  Error_Msg_Sem
                    (+Def, "completion of forward typedef must be a type");
            end case;
         when N_Typedef_Class =>
            if Get_Kind (Decl) = N_Typedef then
               --  Can follow a typedef with a class instantiation.
               Decl1 := Get_Type_Data_Type (Decl);
            else
               Decl1 := Decl;
            end if;

            if Decl1 /= Null_Node then
               case Get_Kind (Decl1) is
                  when Nkinds_Class
                    | N_Generic_Class
                    | N_Typedef_Forward
                    | N_Typedef_Class =>
                     null;
                  when others =>
                     Error_Msg_Sem
                       (+Def,
                        "completion of forward typedef class must be a class");
               end case;
            end if;
         when N_Typedef_Struct =>
            case Get_Kind (Def) is
               when N_Typedef_Struct =>
                  null;
               when N_Typedef_Forward =>
                  raise Internal_Error;
               when N_Typedef =>
                  case Get_Kind (Get_Data_Type (Def)) is
                     when N_Struct_Type
                       | N_Packed_Struct_Type =>
                        null;
                     when others =>
                        Error_Msg_Sem
                          (+Def, "completion of forward typedef struct "
                             & "must be a struct");
                  end case;
               when others =>
                  Error_Msg_Sem
                    (+Def, "completion of forward typedef struct "
                       & "must be a struct");
            end case;
      end case;
   end Check_Forward_Typedef_Data_Type;

   procedure Sem_Forward_Typedef (Def : Node)
   is
      Decl : Node;
   begin
      if not Get_Forward_Typedef_Flag (Def) then
         if Get_Forward_Type (Def) = Null_Node then
            Error_Msg_Sem (+Def, "no final type definition for %i", +Def);
            Set_Forward_Type (Def, Error_Typedef);
         end if;
      else
         --  1800-2017 6.18 User-defined types
         --  It shall be legal to have a forward type declaration in the same
         --  scope, either before or after the final type definition.

         --  1800-2017 6.18 User-defined types
         --  It shall be legal to have multiple forward type declarations for
         --  the same type identifier in the same scope.

         Decl := Get_Forward_Type (Def);

         --  If this is a second (or Nth) forward typedef, refer to the first
         --  one.
         if Get_Kind (Decl) = N_Typedef_Forward then
            Decl := Get_Forward_Type (Decl);
         end if;

         Check_Forward_Typedef_Data_Type (Def, Decl);
      end if;
   end Sem_Forward_Typedef;

   procedure Sem_Interface_Name (Name : Node)
   is
      Id : constant Name_Id := Get_Identifier (Name);
      Decl : Node;
   begin
      Decl := Sem_Scopes.Get_Definition (Id);
      if Decl = Null_Node then
         Error_Msg_Sem (+Name, "interface %i not declared", +Id);
      elsif Get_Kind (Decl) /= N_Interface_Declaration then
         Error_Msg_Sem (+Name, "interface expected, got %n", +Decl);
      else
         Set_Declaration (Name, Decl);
         Set_Expr_Type (Name, Decl);
      end if;
   end Sem_Interface_Name;

   procedure Sem_Interface_Type (Typ : Node) is
   begin
      case Get_Kind (Typ) is
         when N_Name =>
            Sem_Interface_Name (Typ);
         when N_Dotted_Name =>
            declare
               Pfx : constant Node := Get_Name (Typ);
               pragma Assert (Get_Kind (Pfx) = N_Name);
               Decl_Inter : Node;
               Decl_Modport : Node;
            begin
               Sem_Interface_Name (Pfx);
               Decl_Inter := Get_Declaration (Pfx);
               if Decl_Inter = Null_Node then
                  return;
               end if;
               Decl_Modport := Find_Name_In_Decls
                 (Get_Items_Chain (Decl_Inter), Typ);
               if Decl_Modport = Null_Node then
                  Error_Msg_Sem (+Typ, "modport %i not found in %n",
                                 (+Typ, +Decl_Inter));
                  return;
               end if;
               Set_Declaration (Typ, Decl_Modport);
               Set_Expr_Type (Typ, Decl_Modport);
            end;
         when N_Array =>
            declare
               Res : Node;
               El_Typ : Node;
            begin
               Res := Sem_Unpacked_Dimension (Typ);
               if Get_Type_Owner (Res) then
                  El_Typ := Get_Element_Data_Type (Res);
                  Sem_Interface_Type (El_Typ);
               end if;
               Set_Expr_Type (Typ, Res);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Interface_Type;

   --  For list of identifiers: they share the same type.
   procedure Sem_Decl_List_Interface_Type (Head : Node)
   is
      Typ : Node;
   begin
      if not Get_Has_Identifier_List (Head) then
         --  The first element of the list: analyze the whole type.
         pragma Assert (Get_Type_Owner (Head));
         Typ := Get_Data_Type (Head);
         Sem_Interface_Type (Typ);
      else
         --  TODO
         raise Internal_Error;
      end if;
   end Sem_Decl_List_Interface_Type;

   procedure Sem_Tf_Return_Type (Rtn : Node) is
   begin
      case Get_Kind (Rtn) is
         when N_Function
           | N_OOB_Function
           | N_Extern_Function
           | N_Import_DPI_Function =>
            if Get_Identifier (Rtn) = Name_New then
               --  Use a fake type to avoid a particular case.
               --  TODO: maybe set the real type (but through a typedef) ?
               Set_Data_Type (Rtn, Void_Typedef);
            else
               Sem_Decl_Data_Type (Rtn);
            end if;
         when others =>
            null;
      end case;
   end Sem_Tf_Return_Type;

   procedure Sem_Tf_Ports (Tf_Decl : Node)
   is
      Port : Node;
      Port_Type : Node;
      Lifetime : Lifetime_Type;
      Expr : Node;
   begin
      if Get_Ansi_Port_Flag (Tf_Decl) then
         Port_Type := Unsigned_Logic_Type;
         Port := Get_Tf_Ports_Chain (Tf_Decl);
         while Port /= Null_Node loop
            --  1800-2017 13.3 Tasks
            --  Each formal argument has a data type that can be explicitly
            --  declared or inherited from the previous argument.  If the data
            --  type is not explicitly declared, then the default data type is
            --  logic if it is the first argument of if the argument direction
            --  is explicitly specified.  Otherwise, the data type is inherited
            --  from the previous argument.
            --
            --  1800-2017 13.4 Functions
            --  Each formal argument has a data type that can be explicitly
            --  declared or inherited from the previous argument.  If the data
            --  type is not explicitly declared, then the default data type is
            --  logic if it is the first argument or if the argument direction
            --  is explicitly specified.  Otherwise the data type is inherited
            --  from the previous argument.
            --
            --  GHDL: the only difference in the text is the comma after
            --  'Otherwise' in the last sentence...
            if Get_Data_Type (Port) = Null_Node then
               pragma Assert (Get_Type_Owner (Port) = False);
               if Get_Has_Direction (Port) then
                  Port_Type := Unsigned_Logic_Typedef;
               end if;
               Set_Data_Type (Port, Port_Type);
            else
               Sem_Decl_Data_Type (Port);
               Port_Type := Get_Data_Type (Port);
            end if;

            Lifetime := Get_Lifetime (Tf_Decl);
            Set_Lifetime (Port, Lifetime);
            Set_Is_Automatic (Port, Lifetime = Life_Automatic);

            Expr := Get_Default_Value (Port);
            if Expr /= Null_Node then
               Expr := Sem_Expression (Expr, Get_Expr_Type (Port_Type));
               Set_Default_Value (Port, Expr);
            end if;

            Port := Get_Chain (Port);
         end loop;
      else
         Port := Get_Tf_Item_Declaration_Chain (Tf_Decl);
         while Port /= Null_Node loop
            if Get_Kind (Port) in Nkinds_Tf_Port then
               pragma Assert (not Get_Has_Identifier_List (Port));  --  TODO
               Sem_Decl_Data_Type (Port);
            end if;
            Port := Get_Chain (Port);
         end loop;
      end if;
   end Sem_Tf_Ports;

   --  Analyze subroutine RTN as a declaration: only the ports and the
   --  return type.  The body is not analyzed.
   --  Mark RTN as elaborated.
   procedure Sem_Subroutine_Declaration (Rtn : Node) is
   begin
      pragma Assert (Get_Fully_Analyzed_Flag (Rtn) = False);

      if Get_Mark_Flag (Rtn) then
         Error_Msg_Sem (+Rtn, "call to %i before elaboration", +Rtn);
         return;
      end if;

      --  Set lifetime.
      case Get_Kind (Rtn) is
         when N_Function
           | N_Task =>
            if Get_Kind (Get_Parent (Rtn)) in Nkinds_Any_Class then
               if Get_Has_Lifetime (Rtn)
                 and then Get_Lifetime (Rtn) = Life_Static
               then
                  --  1800-2017 8.6 Object methods
                  --  The lifetime of methods declared as part of a class type
                  --  shall be automatic.  It shall be illegal to declare a
                  --  class method with a static lifetime.
                  Error_Msg_Sem
                    (+Rtn, "class method must have automatic lifetime");
               end if;
            else
               if not Get_Has_Lifetime (Rtn) then
                  --  Not automatic by default.
                  --  FIXME.
                  Set_Lifetime (Rtn, Life_Static);
               end if;
            end if;
         when N_Import_DPI_Function
           | N_Extern_Task
           | N_Extern_Function =>
            Set_Lifetime (Rtn, Life_Automatic);
         when others =>
            Error_Kind ("sem_subroutine_declaration", Rtn);
      end case;

      Set_Mark_Flag (Rtn, True);

      Sem_Tf_Return_Type (Rtn);
      Sem_Tf_Ports (Rtn);

      Set_Mark_Flag (Rtn, False);
      Set_Fully_Analyzed_Flag (Rtn, True);
   end Sem_Subroutine_Declaration;

   procedure Sem_Out_Of_Block_Declaration (Item : Node) is
   begin
      --  1800-2017 8.24 Out-of-block declarations
      --  The class scope resolution operator is required in some situations
      --  in order to name the return type of a method with an out-of-block
      --  declaration.  When the return type of the out-of-block declaration
      --  is defined within the class, the class scope resolution operator
      --  shall be used to indicate the internal return type.
      Sem_Tf_Return_Type (Item);

      --  Ports have visibility to the class, so they are analyzed within the
      --  class.
   end Sem_Out_Of_Block_Declaration;

   --  Analyze the type part of a class.
   procedure Sem_Class_Type (Klass : Node)
   is
      Base_Class : constant Node := Get_Base_Class_Type (Klass);
      Base_Type : Node;
      Item : Node;
   begin
      pragma Assert (not Get_Type_Analyzed_Flag (Klass));
      --  FIXME: detect recursion.
      Set_Type_Analyzed_Flag (Klass, True);

      --  Parameters must have been type-analyzed.
      declare
         Param : Node;
         Param_Type : Node;
      begin
         Param := Get_Parameter_Port_Chain (Klass);
         while Param /= Null_Node loop
            case Get_Kind (Param) is
               when N_Type_Parameter =>
                  Param_Type := Get_Parameter_Type (Param);
                  if Get_Kind (Param_Type) in Nkinds_Class
                    and then not Get_Type_Analyzed_Flag (Param_Type)
                  then
                     Sem_Class_Type (Param_Type);
                  end if;
               when N_Parameter =>
                  null;
               when others =>
                  Error_Kind ("sem_class_type(param)", Param);
            end case;
            Param := Get_Chain (Param);
         end loop;
      end;

      if Base_Class = Null_Node then
         Set_Inheritance_Depth (Klass, 0);

         --  Need to resolve names of OOB methods.
         if Get_Has_Extern_Flag (Klass) then
            Resolve_Names.Resolve_Names_Class_Complete (Klass);
         end if;
      else
         --  Analyze base class.
         Sem_Data_Type (Base_Class);
         Base_Type := Get_Expr_Type (Base_Class);
         if Get_Kind (Base_Type) not in Nkinds_Class then
            Error_Msg_Sem (+Klass, "class base type must be a class");
         end if;

         --  Base type must have been type-analyzed.
         if not Get_Type_Analyzed_Flag (Base_Type) then
            Sem_Class_Type (Base_Type);
         end if;

         Set_Inheritance_Depth
           (Klass, Get_Inheritance_Depth (Get_Expr_Type (Base_Class)) + 1);
         Resolve_Names.Resolve_Names_Class_Complete (Klass);
      end if;

      Item := Get_Class_Item_Chain (Klass);
      --  Resolve_Names_Chain (Item);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Var =>
               Sem_Decl_List_Data_Type (Item);
            when N_Typedef =>
               --  Part of data_declaration
               Sem_Typedef_Type (Item);
            when N_Task
              | N_Extern_Task =>
               Sem_Subroutine_Declaration (Item);
            when N_Function
              | N_Extern_Function =>
               if Get_Identifier (Item) = Name_New then
                  Set_Class_Constructor (Klass, Item);
               end if;
               Sem_Subroutine_Declaration (Item);
            when N_Constraint =>
               --  TODO.
               null;
            when others =>
               Error_Kind ("sem_class_type", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Class_Type;

   procedure Sem_Discipline (Decl : Node)
   is
      Item : Node;
   begin
      Item := Get_Discipline_Items (Decl);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Discipline_Domain =>
               null;
            when N_Discipline_Potential
              | N_Discipline_Flow =>
               null;
            when others =>
               Error_Kind ("sem_discipline", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Discipline;

   procedure Sem_Nature (Decl : Node)
   is
      Item : Node;
   begin
      Item := Get_Nature_Items (Decl);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Nature_Attribute =>
               null;
            when N_Nature_Access =>
               null;
            when others =>
               Error_Kind ("sem_nature", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Nature;

   --  Generic procedure to analyze inner declarations of DECL.
   procedure Sem_Decl_Type_Inner (Decl : Node)
   is
      use Verilog.Nodes_Meta;
      Kind : constant Nkind := Get_Kind (Decl);
      Fields : constant Fields_Array := Get_Fields (Kind);
      F : Fields_Enum;
   begin
      for I in Fields'Range loop
         F := Fields (I);

         case Get_Field_Type (F) is
            when Nodes_Meta.Type_Node =>
               case Get_Field_Actual_Attribute (Decl, F) is
                  when Attr_None =>
                     Sem_Decl_Type (Get_Node (Decl, F));
                  when Attr_Ref | Attr_Forward_Ref =>
                     null;
                  when Attr_Chain =>
                     Sem_Decl_Type_Chain (Get_Node (Decl, F));
                  when Attr_Chain_Next =>
                     null;
               end case;
            when others =>
               null;
         end case;
      end loop;
   end Sem_Decl_Type_Inner;

   --  Analyze declarations in DECL; in particular the types.
   procedure Sem_Decl_Type (Decl : Node)
   is
      Kind : Nkind;
   begin
      if Decl = Null_Node then
         return;
      end if;

      Kind := Get_Kind (Decl);

      case Kind is
         when N_Var
           | Nkinds_Nets =>
            Sem_Decl_List_Data_Type (Decl);
         when Nkinds_Tf_Port =>
            --  Already analyzed by Sem_Subroutine_Declaration.
            null;
         when N_This_Var =>
            null;
         when N_Foreach_Variable =>
            --  TODO: set the type from the name ?
            null;
         when Nkinds_Net_Port =>
            Sem_Decl_List_Data_Type (Decl);
            --  Set type on implicit redeclared net.
            declare
               Obj : Node;
            begin
               Obj := Get_Redeclaration (Decl);

               if Obj = Null_Node then
                  --  There is no redeclaration, certainly because the port
                  --  was never referenced.
                  Resolve_Names.Create_Implicit_Net_For_Port_Declaration
                    (Decl, Obj);
               end if;
               if Get_Redeclaration_Flag (Obj)
                 and then Get_Data_Type (Obj) = Null_Node
               then
                  --  FIXME: should merge ?
                  Set_Data_Type (Obj, Get_Data_Type (Decl));
               end if;
            end;
         when N_Interface_Port
           | N_Modport_Port =>
            Sem_Decl_List_Interface_Type (Decl);
         when N_Parameter
           | N_Localparam =>
            --  Analyze the type of the parameters and the value, as it can
            --  be used in types.
            Sem_Parameter (Decl);
         when N_Typedef =>
            Sem_Typedef_Type (Decl);
         when N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward =>
            Sem_Forward_Typedef (Decl);
         when N_Class =>
            if not Get_Type_Analyzed_Flag (Decl) then
               Sem_Class_Type (Decl);
            end if;
         when N_Discipline =>
            Sem_Discipline (Decl);
         when N_Nature =>
            Sem_Nature (Decl);
         when N_Port =>
            null;
         when N_Parameter_Value_Expr =>
            null;
         when N_Genvar
            | N_Generate_Region
            | N_If_Generate
            | N_Loop_Generate =>
            null;
         when N_Blocking_Assign
            | N_Noblk_Assign
            | Nkinds_Gate
            | N_Assign
            | N_Assign_Operator
            | N_Unpack_Assign
            | N_Pack_Assign
            | N_Pack_Unpack_Assign
            | N_Call
            | N_Disable_Fork
            | N_Wait_Fork
            | N_Trigger
            | N_Implicit_Event
            | N_Membership
            | N_Concatenation
            | N_Part_Select
            | N_Bit_Select
            | N_Name
            | N_This_Name
            | N_Dotted_Name
            | N_Property_Name
            | N_Hierarchical
            | N_Interface_Item
            | N_Modport_Item
            | N_Scoped_Name
            | N_Class_Instance
            | N_Parenthesis_Expr
            | N_Or
            | N_Short_Circuit_Op
            | N_Binary_Op
            | N_Unary_Op
            | N_Cond_Op
            | N_Number
            | N_Time_Literal
            | N_String_Literal
            | Nkinds_Inc_Dec
            | Nkinds_Edge
            | N_System_Call
            | N_Subroutine_Call_Stmt
            | N_Return_Stmt
            | N_Break_Stmt
            | N_Continue_Stmt
            | N_Port_Connection
            | N_Package_Import
            | N_Assert_Property
            | N_Assume_Property
            | N_Contribution =>
            null;
         when N_Module
           | N_Program_Declaration
           | N_Generic_Class =>
            --  Only instances are analyzed.
            null;
         when N_Primitive =>
            null;

         when N_Module_Instance
           | N_Program_Instance =>
            --  Analyze declarations in the instance.  Cannot use the normal
            --  code as the instance is a module, and a module is not analyzed
            --  by the regular code.
            declare
               M : constant Node := Get_Instance (Decl);
            begin
               if Get_Kind (M) = N_Foreign_Module then
                  return;
               end if;

               Sem_Decl_Type_Chain (Get_Parameter_Port_Chain (M));
               Sem_Decl_Type_Chain (Get_Ports_Chain (M));
               Sem_Decl_Type_Chain (Get_Items_Chain (M));
            end;
         when N_Interface_Instance =>
            null;
         when N_Interface_Declaration =>
            if Get_Parameter_Port_Chain (Decl) /= Null_Node then
               --  Must be instantiated.
               return;
            end if;
            Sem_Decl_Type_Chain (Get_Parameter_Port_Chain (Decl));
            Sem_Decl_Type_Chain (Get_Ports_Chain (Decl));
            Sem_Decl_Type_Chain (Get_Items_Chain (Decl));
         when N_Modport =>
            Sem_Decl_Type_Chain (Get_Modport_Ports_Chain (Decl));
         when N_Modport_Input
            | N_Modport_Output =>
            declare
               Expr : Node;
               Name : Node;
            begin
               Expr := Get_Expression (Decl);
               if Expr /= Null_Node
                 and then Get_Kind (Expr) = N_Name
               then
                  Name := Get_Declaration (Expr);
                  if Name /= Null_Node then
                     Set_Expr_Type (Expr, Get_Data_Type (Name));
                     Set_Data_Type (Decl, Get_Data_Type (Name));
                  else
                     --  An expression
                     raise Internal_Error;
                  end if;
               else
                  --  Either an expression or an error.
                  raise Internal_Error;
               end if;
            end;
         when N_Task
            | N_Function =>
            if not Get_Fully_Analyzed_Flag (Decl) then
               Sem_Subroutine_Declaration (Decl);
            end if;
            --  Types within the body will be analyzed when the body is.
         when N_OOB_Task
            | N_OOB_Function =>
            Sem_Out_Of_Block_Declaration (Decl);
         when N_Export_DPI_Function =>
            --  TODO: check the name is a function
            --  TODO: check duplicate export ?
            --  TODO: check profile ?
            null;
         when N_Attribute =>
            declare
               Expr : Node;
            begin
               Expr := Get_Expression (Decl);
               if Expr /= Null_Node then
                  Expr := Sem_Expression (Expr, Null_Node);
                  if Expr /= Null_Node
                    and then not Get_Is_Constant (Expr)
                  then
                     Error_Msg_Sem_Non_Constant
                       (Expr, "attribute value must be constant");
                  end if;
                  Set_Expression (Decl, Expr);
               end if;
            end;
         when N_Package
            | N_Extern_Function
            | N_Import_DPI_Function
            | Nkinds_Process
            | N_Seq_Block
            | N_Par_Block
            | N_If
            | Nkinds_Case
            | N_Case_Item
            | N_Default_Case_Item
            | N_For
            | N_Foreach
            | N_While
            | N_Do_While
            | N_Repeat
            | N_Forever
            | N_Repeat_Control
            | N_Event_Control
            | N_Delay_Control
            | N_Wait
            | N_Simple_Immediate_Assert
            | N_Label_Stmt
            | N_Analog =>
            Sem_Decl_Type_Inner (Decl);

         when N_Specify =>
            null;

         when others =>
            Error_Kind ("sem_decl_type", Decl);
      end case;
   end Sem_Decl_Type;

   procedure Sem_Decl_Type_Chain (Chain : Node)
   is
      Decl : Node;
   begin
      Decl := Chain;
      while Decl /= Null_Node loop
         Sem_Decl_Type (Decl);
         Decl := Get_Chain (Decl);
      end loop;
   end Sem_Decl_Type_Chain;
end Verilog.Sem_Decls;
