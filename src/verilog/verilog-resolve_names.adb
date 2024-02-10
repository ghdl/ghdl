--  Verilog name resolution (first pass)
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
with Errorout; use Errorout;
with Std_Names;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Sem_Scopes; use Verilog.Sem_Scopes;
with Verilog.Sem_Names; use Verilog.Sem_Names;
with Verilog.Sem_Utils;

package body Verilog.Resolve_Names is
   procedure Resolve_Names (Item : Node);
   procedure Resolve_Names_Chain (Items : Node);
   procedure Resolve_Names_Name (Name : Node; Soft : Boolean := False);
   procedure Resolve_Names_Type (Atype : Node; Owner : Boolean);
   procedure Add_Names_Chain (Chain : Node);

   procedure Add_Item_Decl (Item : Node);

   --  Name resolution mode.
   type Resolve_Mode_Type is
     (
      Mode_Normal,
      Mode_Class,
      Mode_Complete
     );

   Resolve_Mode : Resolve_Mode_Type;

   --  ITEM declares a type (typedef, class).  Handle existing forward typedef.
   procedure Resolve_Forward_Typedef (Item : Node)
   is
      Decl : Node;
   begin
      Decl := Peek_Scope_Decl (Get_Identifier (Item));
      if Decl /= Null_Node
        and then Get_Kind (Decl) in Nkinds_Forward_Typedef
        --  Check same scope for inheritance
        and then Get_Parent (Decl) = Get_Parent (Item)
      then
         --  Already declared as a typedef.  Set link and use the full
         --  definition.
         pragma Assert (Get_Forward_Type (Decl) = Null_Node);
         Set_Forward_Type (Decl, Item);
         Set_Forward_Typedef_Flag (Item, True);
         Replace_Decl (Item);
      else
         Add_Decl (Item);
      end if;
   end Resolve_Forward_Typedef;

   procedure Add_Data_Object_Decl (Decl : Node)
   is
      Id : constant Name_Id := Get_Identifier (Decl);
      Prev : constant Node := Get_Decl (Id);
      Old : Node;
   begin
      if Prev /= Null_Node
        and then Get_Kind (Prev) in Nkinds_Net_Port
        and then Get_Parent (Prev) = Get_Parent (Decl)
      then
         Old := Get_Redeclaration (Prev);
         if Old = Null_Node then
            --  Redeclaration of a port as a net/var.

            Set_Redeclaration (Prev, Decl);
            Set_Redeclaration_Flag (Decl, True);
         elsif Old = Decl then
            --  ANSI-C port.
            null;
         else
            --  1800-2017 23.2.2.1 Non-ANSI style port declarations
            --  If a port declaration includes a net or variable type, then
            --  the port is considered completely declared, and it is an error
            --  for the port to be declared again in a viable or net data type
            --  declaration. [...]
            Error_Msg_Sem (+Decl, "port %i was already redeclared", +Id);
            Error_Msg_Sem
              (+Old, " (location of the previous redeclaration)");
         end if;
      else
         Add_Decl (Decl);
      end if;
   end Add_Data_Object_Decl;

   --  1800-2017 6.10 Implicit declarations
   --  1364-2005 4.5 Implicit declarations
   --  In the absence of an explicit declaration, an implicit net of default
   --  net type shall be assumed in the following circumstances: [...]
   procedure Create_Default_Net (Name : Node; Ntype : Node; Res : out Node) is
   begin
      Res := Create_Node (N_Wire);
      Location_Copy (Res, Name);
      Set_Identifier (Res, Get_Identifier (Name));
      Set_Data_Type (Res, Ntype);
      Set_Implicit_Flag (Res, True);
   end Create_Default_Net;

   --  1800-2017 6.10 Implicit declarations
   --  1364-2005 4.5 Implicit declarations
   --  - If an identifier appears on the left-hand side of a continuous
   --    assignment statement, and that identifier has not been declared
   --    previously in the scope where the continuous assignment statement
   --    appears or in any scope whose declarations can be directly referenced
   --    from the scope where the continuous assignment statement appears
   --    (see 12.7), then an implicit scalar net of default net type shall be
   --    assumed.
   procedure Resolve_Names_Assign (Asgn : Node; Constr : in out Items_Constr)
   is
      Lhs : constant Node := Get_Lvalue (Asgn);
      Net : Node;
   begin
      if Get_Kind (Lhs) = N_Name
        and then Get_Decl (Get_Identifier (Lhs)) = Null_Node
      then
         Create_Default_Net (Lhs, Unsigned_Logic_Typedef, Net);
         Set_Declaration (Lhs, Net);
         Set_Expr_Type (Lhs, Get_Type_Data_Type (Net));
         Append_Node (Constr, Net);
         Add_Decl (Net);
      else
         Resolve_Names (Lhs);
      end if;

      Resolve_Names (Get_Assign_Delay (Asgn));
      Resolve_Names (Get_Expression (Asgn));
   end Resolve_Names_Assign;

   --  1800-2017 6.10 Implicit declarations
   --  1364-2005 4.5 Implicit declarations
   --  - If an identifier is used in the terminal list of a primitive instance
   --    or a module instance, and that identifier has not been declared
   --    previously in the scope where the instantiation appears or in any
   --    scope whose declarations can be directly referenced from the scope
   --    where the instantiation appears (see 12.7), then an implicit scalar
   --    net of default net type shall be assumed.
   procedure Resolve_Names_Instance (Chain : Node;
                                     Constr : in out Items_Constr)
   is
      Conn : Node;
      N : Node;
      Net : Node;
   begin
      Conn := Chain;
      while Conn /= Null_Node loop
         case Nkinds_Terminal_Or_Connection (Get_Kind (Conn)) is
            when N_Wildcard_Connection =>
               null;
            when N_Port_Connection
              | Nkinds_Terminal =>
               N := Get_Expression (Conn);
               if N /= Null_Node
                 and then Get_Kind (N) = N_Name
                 and then Get_Decl (Get_Identifier (N)) = Null_Node
               then
                  --  Create a default net.
                  Create_Default_Net (N, Unsigned_Logic_Typedef, Net);
                  Append_Node (Constr, Net);
                  Add_Decl (Net);
                  Set_Declaration (N, Net);
                  Set_Expr_Type (N, Get_Type_Data_Type (Net));
               else
                  Resolve_Names (N);
               end if;
         end case;
         Conn := Get_Chain (Conn);
      end loop;
   end Resolve_Names_Instance;

   --  1800-2017 6.10 Implicit declarations
   --  - If an identifier is used in a port expression declaration, then an
   --    implicit net of default net type shall be assumed, with the vector
   --    width of the port expression declaration.
   procedure Create_Implicit_Net_For_Port_Declaration
     (Port : Node; Net : out Node) is
   begin
      Create_Default_Net (Port, Get_Data_Type (Port), Net);

      --  Insert just after.
      Set_Chain (Net, Get_Chain (Port));
      Set_Chain (Port, Net);

      Set_Parent (Net, Get_Parent (Port));
      Set_Redeclaration (Port, Net);
      Set_Redeclaration_Flag (Net, True);
      Set_Implicit_Flag (Net, True);
   end Create_Implicit_Net_For_Port_Declaration;

   procedure Resolve_Names_Chain_Implicit (Chain : in out Node; Parent : Node)
   is
      Constr : Items_Constr;
      Sub_Constr : Items_Constr;

      Item : Node;
      Next_Item : Node;
   begin
      Init_Constr (Constr, Parent);

      Item := Chain;
      while Item /= Null_Node loop
         Next_Item := Get_Chain (Item);
         Set_Chain (Item, Null_Node);

         Init_Constr (Sub_Constr, Parent);
         case Get_Kind (Item) is
            when N_Assign =>
               Resolve_Names_Assign (Item, Sub_Constr);
            when N_Module_Instance =>
               Resolve_Names_Chain (Get_Parameter_Values (Item));
               --  FIXME: the instance could be an interface instance, which
               --  doesn't create implicit nets...
               Resolve_Names_Instance (Get_Connections (Item), Sub_Constr);
               Add_Decl (Item);
            when Nkinds_Gate =>
               Resolve_Names_Instance (Get_Gate_Terminals (Item), Sub_Constr);
               Add_Decl (Item);
            when others =>
               Resolve_Names (Item);
         end case;

         --  Advance Last as implicit nets for ports may have been inserted.
         Update_Constr (Constr);

         --  Insert nodes from CONSTR
         Append_Constr (Constr, Sub_Constr);

         --  Append ITEM
         pragma Assert (Get_Parent (Item) = Parent);
         Set_Parent (Item, Null_Node);
         Append_Node (Constr, Item);

         Item := Next_Item;
      end loop;
      Chain := Get_Constr_Chain (Constr);
   end Resolve_Names_Chain_Implicit;

   procedure Maybe_Mutate_Name (Name : Node; Decl : Node)
   is
      Parent : constant Node := Get_Parent (Decl);
      This_Decl : Node;
   begin
      if Get_Kind (Parent) in Nkinds_Class
        and then not Get_Static_Flag (Decl)
      then
         Mutate_Name (Name, N_This_Name);
         This_Decl := Get_Decl (Std_Names.Name_This);
         Set_This_Declaration (Name, This_Decl);
      end if;
   end Maybe_Mutate_Name;

   procedure Resolve_Names_Identifier (Name : Node; Soft : Boolean)
   is
      Id : constant Name_Id := Get_Identifier (Name);
      Decl : Node;
      Decl2 : Node;
   begin
      pragma Assert (Resolve_Mode = Mode_Complete
                       or else Get_Declaration (Name) = Null_Node);
      Decl := Get_Decl (Id);

      if Decl = Null_Node then
         if Soft then
            --  For the prefix of a hierarchical name.
            return;
         end if;
         case Resolve_Mode is
            when Mode_Normal =>
               null;
            when Mode_Class =>
               --  Can be a name for an inherited property or method.
               return;
            when Mode_Complete =>
               --  Was already resolved.
               if Get_Declaration (Name) /= Null_Node then
                  return;
               end if;
         end case;
         Error_Msg_Sem (+Name, "no declaration for %i", +Id);
         return;
      end if;

      if Get_Kind (Decl) in Nkinds_Net_Port then
         --  If the identifier refers to a port, freeze the port by
         --  creating the net type if not already present.
         Decl2 := Get_Redeclaration (Decl);
         if Decl2 = Null_Node then
            Create_Implicit_Net_For_Port_Declaration (Decl, Decl2);
         end if;
         Set_Declaration (Name, Decl2);
      else
         Set_Declaration (Name, Decl);

         --  For a property add an implicit 'this.' prefix.
         --  This is done at the call for a method too.
         if Get_Kind (Decl) = N_Var then
            Maybe_Mutate_Name (Name, Decl);
         end if;
      end if;
   end Resolve_Names_Identifier;

   function Get_Class_Declaration (Name : Node) return Node
   is
      Res : Node;
   begin
      Res := Name;
      loop
         case Get_Kind (Res) is
            when N_Name =>
               Res := Get_Declaration (Res);
               if Res = Null_Node then
                  return Null_Node;
               end if;
            when N_Typedef_Class =>
               Res := Get_Forward_Type (Res);
               if Res = Null_Node then
                  return Null_Node;
               end if;
            when N_Type_Parameter =>
               Res := Get_Parameter_Type (Res);
               if Res = Null_Node then
                  return Null_Node;
               end if;
            when N_Generic_Class
              | N_Class
              | N_Instantiated_Class =>
               --  Instantiated_Class is created by sem.
               return Res;
            when N_Class_Instance =>
               Res := Get_Class_Name (Res);
            when N_Typedef =>
               return Get_Type_Data_Type (Res);
            when others =>
               Error_Kind ("get_class_declaration", Res);
         end case;
      end loop;
   end Get_Class_Declaration;

   procedure Resolve_Names_Scoped_Name (Name : Node)
   is
      Scope_Name : Node;
      Scope_Decl : Node;
   begin
      Scope_Name := Get_Name (Name);

      --  1800-2017 8.23 Class scope resolution operator ::
      --  The left operand of the scope resolution operator :: shall be a
      --  class type name, package name, covergroup type name, coverpoint
      --  name, cross name, typedef name, or type parameter name.
      case Get_Kind (Scope_Name) is
         when N_Scoped_Name =>
            Resolve_Names_Scoped_Name (Scope_Name);
            Scope_Decl := Get_Declaration (Scope_Name);
         when N_Class_Instance =>
            Resolve_Names_Name (Scope_Name);
            Scope_Decl := Get_Class_Declaration (Scope_Name);
         when N_Name =>
            --  Can be a package name or a class name.  What if it is both,
            --  given that package names are in their own namespace ?
            --  FIXME.
            if Resolve_Mode = Mode_Complete
              and then Get_Declaration (Scope_Name) /= Null_Node
            then
               return;
            end if;
            declare
               Id : constant Name_Id := Get_Identifier (Scope_Name);
               Pkg : constant Node := Get_Package (Id);
               Decl : constant Node := Get_Decl (Id);
            begin
               if Pkg = Null_Node and Decl = Null_Node then
                  if Resolve_Mode /= Mode_Class then
                     Error_Msg_Sem (+Scope_Name, "no declaration for %i", +Id);
                  end if;
                  return;
               end if;
               if Pkg /= Null_Node and Decl /= Null_Node then
                  Error_Msg_Sem (+Scope_Name, "ambiguous name %i", +Id);
                  return;
               end if;
               if Pkg /= Null_Node then
                  Scope_Decl := Pkg;
               else
                  Scope_Decl := Decl;
               end if;
               Set_Declaration (Scope_Name, Scope_Decl);
            end;
         when others =>
            Error_Kind ("resolve_names_scoped_name(1)", Scope_Name);
      end case;

      if Scope_Decl = Null_Node then
         return;
      end if;

      --  1800-2017 8.23 Class scope resolution operator ::
      --  The left operand of the scope resolution operator ::
      --  shall be a class type name, package name, covergroup
      --  type name, coverpoint name, cross name, typedef name or
      --  a type parameter name.
      if Get_Kind (Scope_Decl) = N_Package then
         declare
            Decl : Node;
         begin
            Decl := Find_Name_In_Scope (Scope_Decl, Name);
            if Decl /= Null_Node then
               Set_Declaration (Name, Decl);
            else
               Error_Msg_Sem
                 (+Name, "%i is not declared in %i", (+Name, +Scope_Decl));
            end if;
         end;
      end if;
   end Resolve_Names_Scoped_Name;

   --  Return True iff PFX is an appropriate prefix for a hierarchical name;
   --  otherwise it is a normal dotted name.
   function Is_Hierarchical_Prefix (Pfx : Node) return Nkind
   is
      Decl : Node;
   begin
      case Get_Kind (Pfx) is
         when N_Name
           | N_Hierarchical =>
            null;
         when N_Dotted_Name
           | N_Scoped_Name
           | N_Class
           | N_This_Name
           | N_Iterator_Argument
           | N_Property_Name
           | N_Call
           | N_This
           | N_Super =>
            return N_Dotted_Name;
         when N_Bit_Select =>
            --  FIXME: what about generate ?
            return N_Dotted_Name;
         when others =>
            Error_Kind ("is_hierarchical_prefix", Pfx);
      end case;

      --  1800-2017 23.6 Hierarchical names
      --  The hierarchy of names can be viewed as a tree structure, where each
      --  module instance, generate block instance, task, function, or named
      --  begin-end or fork-join block defines a new hierarchical level, or
      --  scope, in a particular branch of the tree.

      Decl := Get_Declaration (Pfx);
      if Decl = Null_Node then
         --  No declaration, so probably a hierarchical name.
         case Resolve_Mode is
            when Mode_Normal
              | Mode_Complete =>
               return N_Hierarchical;
            when Mode_Class =>
               --  Don't know yet.
               return N_Dotted_Name;
         end case;
      end if;
      case Get_Kind (Decl) is
         when N_Module_Instance =>
            return N_Hierarchical;
         when N_Interface_Port =>
            return N_Interface_Item;
         when N_Modport_Port =>
            return N_Modport_Item;
         when N_Function
           | N_Extern_Function =>
            --  Dotted name on a return value.
            return N_Dotted_Name;
         when Nkinds_Tf_Port
           | Nkinds_Net_Port
           | N_Var
           | N_Foreach_Variable
           | N_Iterator_Argument
           | Nkinds_Nets
           | N_Interface_Declaration
           | N_Constraint =>
            --  Normal dotted name.
            return N_Dotted_Name;
         when N_Class =>
            --  Certainly an error.
            return N_Dotted_Name;
         when others =>
            Error_Kind ("is_hierarchical_prefix(2)", Decl);
      end case;
   end Is_Hierarchical_Prefix;

   procedure Resolve_Names_Name (Name : Node; Soft : Boolean := False) is
   begin
      case Get_Kind (Name) is
         when N_Name =>
            Resolve_Names_Identifier (Name, Soft);
         when N_Bit_Select =>
            Resolve_Names_Name (Get_Name (Name));
            Resolve_Names (Get_Expression (Name));
         when N_Part_Select =>
            Resolve_Names_Name (Get_Name (Name));
            Resolve_Names (Get_Msb (Name));
            Resolve_Names (Get_Lsb (Name));
         when N_Plus_Part_Select
           | N_Minus_Part_Select =>
            Resolve_Names_Name (Get_Name (Name));
            Resolve_Names (Get_Base_Expr (Name));
            Resolve_Names (Get_Width_Expr (Name));
         when N_Dotted_Name =>
            declare
               Pfx : constant Node := Get_Name (Name);
               Kind : Nkind;
            begin
               Resolve_Names_Name (Pfx, True);

               Kind := Is_Hierarchical_Prefix (Pfx);
               if Kind /= N_Dotted_Name then
                  Mutate_Dotted_Name (Name, Kind);
               end if;
            end;
         when N_This
           | N_Super =>
            declare
               Res : Node;
            begin
               Res := Get_Decl (Std_Names.Name_This);
               Set_Declaration (Name, Res);
            end;
         when N_Class_Instance =>
            Resolve_Names_Name (Get_Class_Name (Name));
            Resolve_Names_Chain (Get_Parameter_Values (Name));
         when N_Call =>
            Resolve_Names_Name (Get_Subroutine (Name));
            Resolve_Names_Chain (Get_Arguments (Name));

         when N_Scoped_Name =>
            Resolve_Names_Scoped_Name (Name);

         when N_This_Name =>
            pragma Assert (Resolve_Mode = Mode_Complete);
            null;
         when N_Hierarchical =>
            --  This is resolved by Sem_Name.
            null;
            --  TODO: parameters.
--         when N_Scoped_Name =>
--            Resolve_Names_Scoped_Name (Name);
--         when N_Call
--           | N_Randomize_Call =>
--            Resolve_Names_Call (Name);
         when others =>
            Error_Kind ("resolve_names_name", Name);
      end case;
   end Resolve_Names_Name;

   procedure Resolve_Names_Array_Range (Atype : Node) is
   begin
      Resolve_Names (Get_Msb (Atype));
      Resolve_Names (Get_Lsb (Atype));
   end Resolve_Names_Array_Range;

   procedure Resolve_Names_Type (Atype : Node; Owner : Boolean) is
   begin
      if Atype = Null_Node then
         --  No type
         return;
      end if;
      if not Owner then
         --  Not the owner, so already handled (and maybe already resolved).
         return;
      end if;

      case Get_Kind (Atype) is
         when N_Packed_Array
           | N_Array =>
            Resolve_Names_Array_Range (Atype);
            Resolve_Names_Type
              (Get_Element_Data_Type (Atype), Get_Type_Owner (Atype));
         when N_String_Type
           | N_Chandle_Type
           | N_Void_Type =>
            null;
         when N_Enum_Type =>
            Resolve_Names_Type
              (Get_Enum_Base_Data_Type (Atype), Get_Type_Owner (Atype));
            declare
               El : Node;
            begin
               El := Get_Enum_Names (Atype);
               while El /= Null_Node loop
                  pragma Assert (Get_Kind (El) = N_Enum_Name);
                  Resolve_Names (Get_Expression (El));
                  Add_Decl (El);
                  El := Get_Chain (El);
               end loop;
            end;
         when N_Associative_Array =>
            declare
               Index : constant Node := Get_Index_Data_Type (Atype);
            begin
               if Index /= Null_Node then
                  Resolve_Names_Type (Index, Get_Type_Owner_2 (Atype));
               end if;
               Resolve_Names_Type
                 (Get_Element_Data_Type (Atype), Get_Type_Owner (Atype));
            end;
         when N_Dynamic_Array =>
            Resolve_Names_Type
              (Get_Element_Data_Type (Atype), Get_Type_Owner (Atype));
         when N_Queue =>
            Resolve_Names (Get_Maximum_Size_Expr (Atype));
            Resolve_Names_Type
              (Get_Element_Data_Type (Atype), Get_Type_Owner (Atype));
         when N_Struct_Type
           | N_Packed_Struct_Type =>
            --  FIXME: this doesn't appear in the manual, but check that all
            --  member identifiers are unique.
            declare
               Member : Node;
               Expr : Node;
            begin
               Open_Name_Space;

               Member := Get_Members (Atype);
               while Member /= Null_Node loop
                  Add_Decl (Member);

                  Resolve_Names_Type
                    (Get_Data_Type (Member), Get_Type_Owner (Member));
                  Expr := Get_Expression (Member);
                  if Expr /= Null_Node then
                     Resolve_Names (Expr);
                  end if;
                  Member := Get_Chain (Member);
               end loop;

               Close_Name_Space;
            end;
         when N_Name =>
            --  FIXME: use Reolve_Names_Identifier, but also search for
            --   interfaces ??
            declare
               Id : constant Name_Id := Get_Identifier (Atype);
               Decl : Node;
            begin
               --  First try normal namespace (for a typedef).
               Decl := Get_Decl (Id);
               if Decl = Null_Node then
                  --  Second try definition namespace (for an interface).
                  Decl := Get_Definition (Id);
               end if;
               if Decl = Null_Node then
                  case Resolve_Mode is
                     when Mode_Normal =>
                        null;
                     when Mode_Class =>
                        return;
                     when Mode_Complete =>
                        --  Already resolved.
                        if Get_Declaration (Atype) /= Null_Node then
                           return;
                        end if;
                  end case;
                  Error_Msg_Sem (+Atype, "no declaration for %i", +Id);
               else
                  Set_Declaration (Atype, Decl);
               end if;
            end;
         when N_Scoped_Name =>
            Resolve_Names_Name (Atype);
         when N_Dotted_Name =>
            declare
               Pfx_Name : constant Node := Get_Name (Atype);
               Pfx_Decl : Node;
               Port_Decl : Node;
            begin
               Resolve_Names_Name (Pfx_Name);
               Pfx_Decl := Get_Declaration (Pfx_Name);
               if Pfx_Decl /= Null_Node then
                  if Get_Kind (Pfx_Decl) /= N_Interface_Declaration then
                     Error_Msg_Sem
                       (+Pfx_Name, "%i must designed an interface (found %n)",
                        (+Pfx_Name, +Pfx_Decl));
                  else
                     Port_Decl := Find_Name_In_Decls
                       (Get_Items_Chain (Pfx_Decl), Atype);
                     if Port_Decl = Null_Node then
                        Error_Msg_Sem (+Atype, "modport %i not found in %n",
                                       (+Atype, +Pfx_Decl));
                     else
                        --  TODO: check is a modport ?
                        Set_Declaration (Atype, Port_Decl);
                     end if;
                  end if;
               end if;
            end;
         when N_Class_Instance =>
            Resolve_Names_Name (Get_Class_Name (Atype));
            Resolve_Names_Chain (Get_Parameter_Values (Atype));
         when N_Typedef =>
            null;
         when N_Virtual_Interface =>
            null;
         when others =>
            Error_Kind ("resolve_names_type", Atype);
      end case;
   end Resolve_Names_Type;

   procedure Resolve_Names_Data_Type (Decl : Node) is
   begin
      Resolve_Names_Type (Get_Data_Type (Decl), Get_Type_Owner (Decl));
   end Resolve_Names_Data_Type;

   procedure Resolve_Names_Subroutine_Decl (Rtn : Node)
   is
      Port : Node;
   begin
      --  Function data type.
      case Get_Kind (Rtn) is
         when N_Function
           | N_Extern_Function
           | N_Import_DPI_Function =>
            Resolve_Names_Data_Type (Rtn);
         when N_Task
           | N_Extern_Task =>
            null;
         when others =>
            raise Internal_Error;
      end case;

      --  Ports
      --  Note: according to UVM, a port is visible to the next ones (so a
      --   default value may depend on a previous port).  So we need to add
      --   the declarations, so we need to open and close a name space.
      if Get_Ansi_Port_Flag (Rtn) then
         Open_Name_Space;
         Port := Get_Tf_Ports_Chain (Rtn);
         while Port /= Null_Node loop
            Resolve_Names_Data_Type (Port);
            Resolve_Names (Get_Default_Value (Port));
            Add_Decl (Port);
            Port := Get_Chain (Port);
         end loop;
         Close_Name_Space;
      end if;
   end Resolve_Names_Subroutine_Decl;

   procedure Resolve_Names_Subroutine_Body (Rtn : Node)
   is
      Kind : constant Nkinds_Any_Tf := Get_Kind (Rtn);
      This_Var : Node;
   begin
      --  1800-2017 23.9 Scope rules
      --  The following elements define a new scope in SystemVerilog:
      --  - Functions
      --  [...]
      Open_Name_Space;

      if Get_Ansi_Port_Flag (Rtn) then
         Add_Names_Chain (Get_Tf_Ports_Chain (Rtn));
      end if;

      --  Add visibility over the function for the return variable.
      case Kind is
         when N_Function
           | N_Extern_Function =>
            Add_Decl (Rtn);
         when N_Task
           | N_Extern_Task =>
            null;
         when N_OOB_Task
           | N_OOB_Function =>
            raise Internal_Error;
      end case;

      if Sem_Utils.Is_Method (Rtn)
        and then not Get_Static_Flag (Rtn)
      then
         if Resolve_Mode /= Mode_Complete then
            --  Create the implicit variable for THIS.
            This_Var := Create_Node (N_This_Var);
            Location_Copy (This_Var, Rtn);
            Set_Identifier (This_Var, Std_Names.Name_This);
            Set_Parent (This_Var, Rtn);
            pragma Assert (Get_This_Variable (Rtn) = Null_Node);
            Set_This_Variable (Rtn, This_Var);
         else
            This_Var := Get_This_Variable (Rtn);
            pragma Assert (This_Var /= Null_Node);
         end if;

         Add_Decl (This_Var);
      end if;

      Resolve_Names_Chain (Get_Tf_Item_Declaration_Chain (Rtn));
      Resolve_Names_Chain (Get_Statements_Chain (Rtn));

      Close_Name_Space;
   end Resolve_Names_Subroutine_Body;

   --  Add inherited names, set visibility of parent classes.
   procedure Add_Names_Inherited (Base : Node)
   is
      Decl : Node;
      Base_Base : Node;
   begin
      if Base = Null_Node then
         return;
      end if;

      --  Extract class definition.
      Decl := Get_Expr_Type (Base);

      if Decl = Null_Node then
         --  Error in name resolution.
         return;
      end if;

      if Get_Kind (Decl) not in Nkinds_Any_Class then
         --  Incorrect name (or type parameter).
         return;
      end if;

      --  First base class
      --  The Base_Root_Class has no parent.
      if Decl /= Base_Root_Class then
         Base_Base := Get_Base_Class_Type (Decl);
         if Base_Base = Null_Node then
            Add_Class_Decls (Base_Root_Class);
         else
            Add_Names_Inherited (Base_Base);
         end if;
      end if;

      Add_Class_Decls (Decl);
   end Add_Names_Inherited;

   --  Revert visibility of parent classes.
   --  TODO: remove, it does nothing.
   procedure Clear_Names_Inherited (Base : Node)
   is
      Decl : Node;
      Base_Base : Node;
   begin
      if Base = Null_Node then
         return;
      end if;

      --  Extract class definition.
      Decl := Get_Expr_Type (Base);

      if Decl = Null_Node then
         --  Error in name resolution.
         return;
      end if;

      if Get_Kind (Decl) not in Nkinds_Any_Class then
         --  Incorrect name (or type parameter).
         return;
      end if;

      if Decl /= Base_Root_Class then
         Base_Base := Get_Base_Class_Type (Decl);
         Clear_Names_Inherited (Base_Base);
      end if;
   end Clear_Names_Inherited;

   procedure Resolve_Names_Compilation_Unit (Cu : Node)
   is
      Items : constant Node := Get_Descriptions (Cu);
   begin
      --  Identifiers must be resolved.
      Resolve_Mode := Mode_Normal;

      --  1800-2017 3.13 Name spaces
      --  c) The compilation-unit scope name space exists outside the module,
      --     interface, package, checker, program, and primitive constructs.
      --     It unifies the definitions of the functions, tasks, checkers,
      --     parameters, named events, net declarations, variable declarations,
      --     and user-defined types within the compilation-unit scope.
      Open_Name_Space;

      --  Makes $unit visible.
      Add_Decl (Cu);

      --  1800-2017 26.7 The std built-in package
      --  The built-in package is implicitly wildcard imported into the
      --  compilation-unit scope of every compilation unit.
      if Built_In_Std_Package /= Null_Node then
         Wildcard_Import (Built_In_Std_Package);
      end if;

      Resolve_Names_Chain (Items);
      Close_Name_Space;
   end Resolve_Names_Compilation_Unit;

   procedure Resolve_Names_Package_Import (Import : Node)
   is
      Name : constant Node := Get_Item_Name (Import);
   begin
      case Get_Kind (Name) is
         when N_Scoped_Name =>
            declare
               Decl : Node;
            begin
               Resolve_Names_Scoped_Name (Name);
               --  TODO: check the prefix is a package.
               Decl := Get_Declaration (Name);
               if Decl /= Null_Node then
                  Add_Decl (Decl);
               end if;
            end;
         when N_Wildcard_Name =>
            declare
               Pfx : constant Node := Get_Name (Name);
               Id : constant Name_Id := Get_Identifier (Pfx);
               Pkg : constant Node := Get_Package (Id);
            begin
               if Pkg = Null_Node then
                  Error_Msg_Sem (+Pfx, "no package declaration for %i", +Id);
                  return;
               end if;
               Set_Declaration (Pfx, Pkg);
               pragma Assert (Get_Kind (Pkg) = N_Package);
               Wildcard_Import (Pkg);
            end;
         when others =>
            Error_Kind ("resolve_names_package_import", Name);
      end case;
   end Resolve_Names_Package_Import;

   procedure Resolve_Names_Port (Port : Node)
   is
      Port_Type : constant Node := Get_Data_Type (Port);
      Base_Type : Node;
      Parent_Type : Node;
      Id : Name_Id;
      Decl_Inter : Node;
      Decl_Type : Node;
      Decl_Port : Node;
      Pfx : Node;
   begin
      --  If the type is a name, it could be an interface name (which is
      --  not resolved now).
      --  If the name is a dotted name, it must be a modport.
      Parent_Type := Port;
      Base_Type := Port_Type;
      while Get_Kind (Base_Type) = N_Array loop
         pragma Assert (Get_Type_Owner (Parent_Type));
         Resolve_Names_Array_Range (Base_Type);
         Parent_Type := Base_Type;
         Base_Type := Get_Element_Data_Type (Base_Type);
      end loop;

      if Get_Kind (Base_Type) = N_Name then
         declare
            Id : constant Name_Id := Get_Identifier (Base_Type);
            Decl_Type : Node;
         begin
            Decl_Type := Sem_Scopes.Get_Decl (Id);

            if Decl_Type /= Null_Node then
               --  A type name (typedef).
               Set_Declaration (Base_Type, Decl_Type);
            else
               --  Possibly an interface port (that cannot be resolved).
               Mutate_Port (Port, N_Interface_Port);
               Set_Data_Type (Port, Port_Type);
            end if;
         end;
      elsif Get_Kind (Base_Type) = N_Dotted_Name then
         --  Can only be a modport.
         Mutate_Port (Port, N_Modport_Port);
         Set_Data_Type (Port, Port_Type);
      else
         --  No interface declaration, or masked by a declaration.
         Resolve_Names_Type (Base_Type, Get_Type_Owner (Parent_Type));
      end if;

      if False and then Get_Kind (Port_Type) = N_Name
      then
         --  Could be a normal net/variable port or a interface port.
         --  FIXME: what to do in case of ambiguity ?
         --  TODO: instantiated interface.
         Id := Get_Identifier (Port_Type);
         Decl_Inter := Sem_Scopes.Get_Definition (Id);
         Decl_Type := Sem_Scopes.Get_Decl (Id);

         if Decl_Inter /= Null_Node and then Decl_Type = Null_Node then
            --  Not a net port, but an interface port.
            --  TODO: check direction is implicit.
            Mutate_Port (Port, N_Interface_Port);
            Set_Interface_Name (Port, Port_Type);
            Set_Declaration (Port_Type, Decl_Inter);
         else
            --  No interface declaration, or masked by a declaration.
            Resolve_Names_Type (Port_Type, True);
         end if;
      elsif False and then Get_Kind (Port_Type) = N_Dotted_Name
      then
         --  Certainly a modport.
         Pfx := Get_Name (Port_Type);
         Id := Get_Identifier (Pfx);
         Decl_Inter := Sem_Scopes.Get_Definition (Id);
         if Decl_Inter = Null_Node then
            Error_Msg_Sem (+Pfx, "interface %i not declared", +Id);
         elsif Get_Kind (Decl_Inter) /= N_Interface_Declaration then
            Error_Msg_Sem (+Pfx, "%i must designed an interface (found %n)",
                           (+Id, +Decl_Inter));
         else
            Decl_Port := Find_Name_In_Decls
              (Get_Items_Chain (Decl_Inter), Port_Type);
            if Decl_Port = Null_Node then
               Error_Msg_Sem (+Port_Type, "modport %i not found in %n",
                              (+Id, +Decl_Inter));
            else
               Mutate_Port (Port, N_Modport_Port);
               Set_Interface_Name (Port, Port_Type);
               Set_Declaration (Port_Type, Decl_Port);
            end if;
         end if;
      end if;

      if Get_Kind (Port) = N_Input then
         Resolve_Names (Get_Default_Value (Port));
      end if;
   end Resolve_Names_Port;

   procedure Reparent_Chain (Items : Node; Parent : Node)
   is
      Item : Node;
   begin
      Item := Items;
      while Item /= Null_Node loop
         Set_Parent (Item, Parent);
         Item := Get_Chain (Item);
      end loop;
   end Reparent_Chain;

   procedure Resolve_Names_Loop_Generate (Item : Node)
   is
      Init : constant Node := Get_For_Initialization (Item);
      Lparam : Node;
      Gen_Var : Node;
      Blk : Node;
      Items : Node;
   begin
      Blk := Null_Node;
      if Init /= Null_Node then
         if Get_Kind (Init) = N_Genvar then
            Open_Name_Space;
            Add_Decl (Init);
            Resolve_Names (Get_Expression (Init));
            Gen_Var := Init;
         else
            pragma Assert (Get_Kind (Init) = N_Blocking_Assign);
            Resolve_Names (Init);
            Gen_Var := Get_Lvalue (Init);
            Gen_Var := Get_Declaration (Gen_Var);
            if Get_Kind (Gen_Var) /= N_Genvar then
               Gen_Var := Null_Node;
            end if;
         end if;

         --  Add a localparam that copies the genvar.
         if Gen_Var /= Null_Node then
            Lparam := Create_Node (N_Localparam);
            Location_Copy (Lparam, Gen_Var);
            Set_Identifier (Lparam, Get_Identifier (Gen_Var));

            Blk := Get_Generate_Block (Item);
            if Get_Kind (Blk) /= N_Generate_Block then
               Blk := Create_Node (N_Generate_Block);
               Location_Copy (Blk, Item);
               Set_Parent (Blk, Get_Parent (Item));
               Items := Get_Generate_Block (Item);
               Reparent_Chain (Items, Blk);
               Set_Generate_Item_Chain (Blk, Items);
               Set_Generate_Block (Item, Blk);
            end if;
            Set_Parent (Lparam, Blk);
            Set_Chain (Lparam, Get_Generate_Item_Chain (Blk));
            Set_Generate_Item_Chain (Blk, Lparam);
         end if;
      end if;
      Resolve_Names (Get_Condition (Item));
      Resolve_Names (Get_Step_Assign (Item));
      Resolve_Names (Blk);

      if Init /= Null_Node and then Get_Kind (Init) = N_Genvar then
         Close_Name_Space;
      end if;
   end Resolve_Names_Loop_Generate;

   procedure Resolve_Names_If_Generate (Item : Node)
   is
      Blk : Node;
      Gblk : Node;
   begin
      Resolve_Names (Get_Condition (Item));
      Blk := Get_True_Block (Item);
      if Blk /= Null_Node and then Get_Kind (Blk) /= N_Generate_Block then
         Gblk := Create_Node (N_Generate_Block);
         Location_Copy (Gblk, Blk);
         Set_Parent (Gblk, Item);
         Set_Parent (Blk, Gblk);
         Set_Generate_Item_Chain (Gblk, Blk);
         Blk := Gblk;
         Set_True_Block (Item, Blk);
      end if;
      Resolve_Names (Blk);

      Blk := Get_False_Block (Item);
      if Blk /= Null_Node and then Get_Kind (Blk) /= N_Generate_Block then
         Gblk := Create_Node (N_Generate_Block);
         Location_Copy (Gblk, Blk);
         Set_Parent (Gblk, Item);
         Set_Parent (Blk, Gblk);
         Set_Generate_Item_Chain (Gblk, Blk);
         Blk := Gblk;
         Set_False_Block (Item, Blk);
      end if;
      Resolve_Names (Blk);
   end Resolve_Names_If_Generate;

   --  Resolve names in KLASS: first parameters and base class.
   --  If the base is not generic, also resolve names in items.
   procedure Resolve_Names_Class (Klass : Node)
   is
      Base : constant Node := Get_Base_Class_Type (Klass);
      Params : constant Node := Get_Parameter_Port_Chain (Klass);
      Items : constant Node := Get_Class_Item_Chain (Klass);
      Prev_Resolve_Mode : constant Resolve_Mode_Type := Resolve_Mode;
      Item : Node;
   begin
      pragma Assert (not Get_Mark_Flag (Klass));

      Resolve_Forward_Typedef (Klass);
      Set_Class_Visibility (Klass, Visibility_Public);

      --  Resolve parameters and base class.
      Open_Name_Space;
      Resolve_Names_Chain (Params);
      Resolve_Names_Type (Base, Get_Type_Owner (Klass));

      --  Make subroutines visible.
      Add_Tf_Decls (Items);

      --  If there is a base class, resolve in 'mode_class': without
      --  visibility on the parent class.  A second pass will made with this
      --  visibility.
      if Base /= Null_Node then
         Resolve_Mode := Mode_Class;
      end if;

      --  First pass: everything but bodies.
      Item := Items;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Function
              | N_Task
              | N_Extern_Function
              | N_Extern_Task =>
               Resolve_Names_Subroutine_Decl (Item);
            when others =>
               Resolve_Names (Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      --  Second pass: subroutine bodies
      --  So that any property is visible to the subroutines.
      Item := Items;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Function
              | N_Task =>
               Resolve_Names_Subroutine_Body (Item);
            when N_Extern_Function
              | N_Extern_Task =>
               null;
            when others =>
               null;
         end case;
         Item := Get_Chain (Item);
      end loop;

      Resolve_Mode := Prev_Resolve_Mode;

      Close_Name_Space;
   end Resolve_Names_Class;

   --  Resolve names in class KLASS.
   --  Note: base class must have been resolved and analyzed.
   procedure Resolve_Names_Class_Complete (Klass : Node)
   is
      Base : constant Node := Get_Base_Class_Type (Klass);
      Params : constant Node := Get_Parameter_Port_Chain (Klass);
      Items : constant Node := Get_Class_Item_Chain (Klass);
      Item : Node;
      Oob_Decl : Node;
   begin
      pragma Assert (Resolve_Mode = Mode_Normal);
      Resolve_Mode := Mode_Complete;

      --  Base class is known.
      pragma Assert (Base /= Null_Node or else Get_Has_Extern_Flag (Klass));

      --  First pass:
      --  resolve names in declarations, resolve subroutines.
      --  May need an immediate second pass in case of forward typedef.
      --  May need a call to Resolve_Names_Class_Items_Second in case of OOB
      --  subroutines to check that all extern methods have been declared.
      --  TODO: what about nested classes ?  It doesn't make sense to have
      --    visibility on parent properties.

      --  1800-2017 23.9 Scope rules
      --  The following elements define a new scope in SystemVerilog:
      --  - Classes
      --  [...]
      Open_Name_Space;

      Add_Names_Inherited (Base);
      Add_Names_Chain (Params);
      Add_Tf_Decls (Items);

      if Base /= Null_Node then
         Resolve_Names_Chain (Items);
      else
         --  Note: Add_Names_Chain doesn't add subroutines.
         Add_Names_Chain (Items);
      end if;

      if Get_Has_Extern_Flag (Klass) then
         Item := Items;
         while Item /= Null_Node loop
            if Nkind_In (Get_Kind (Item), N_Extern_Function, N_Extern_Task)
            then
               --  First the ports of the OOB declaration.
               Oob_Decl := Get_Out_Of_Block_Declaration (Item);
               if Get_Ansi_Port_Flag (Oob_Decl) then
                  Open_Name_Space;
                  Resolve_Names_Chain (Get_Tf_Ports_Chain (Oob_Decl));
                  Close_Name_Space;
               end if;

               Resolve_Names_Subroutine_Body (Item);
            end if;
            Item := Get_Chain (Item);
         end loop;
      end if;

      Clear_Names_Inherited (Base);

      Close_Name_Space;

      Resolve_Mode := Mode_Normal;
   end Resolve_Names_Class_Complete;

   procedure Add_Item_Decl (Item : Node) is
   begin
      case Get_Kind (Item) is
         when N_Parameter
            | N_Localparam
            | N_Var
            | Nkinds_Tf_Port =>
            Add_Decl (Item);
            if Get_Type_Owner (Item) then
               Add_Item_Type_Decl (Get_Data_Type (Item));
            end if;
         when N_Type_Parameter =>
            Add_Decl (Item);
         when N_Constraint =>
            Add_Decl (Item);
         when N_Import_DPI_Function
           | N_Export_DPI_Function
           | N_Extern_Function
           | N_Extern_Task
           | N_Function
           | N_Task
           | N_OOB_Function
           | N_OOB_Task =>
            --  Already added by Add_Tf_Decl.
            null;
         when Nkinds_Forward_Typedef =>
            if not Get_Forward_Typedef_Flag (Item) then
               declare
                  Fwd_Type : constant Node := Get_Forward_Type (Item);
               begin
                  if Fwd_Type /= Null_Node then
                     Add_Decl (Fwd_Type);
                  end if;
               end;
            end if;
         when N_Typedef =>
            if not Get_Forward_Typedef_Flag (Item) then
               Add_Decl (Item);
               if Get_Type_Owner (Item) then
                  Add_Item_Type_Decl (Get_Data_Type (Item));
               end if;
            end if;
         when N_Class
           | N_Generic_Class =>
            if not Get_Forward_Typedef_Flag (Item) then
               Add_Decl (Item);
            end if;

         when N_Assign =>
            null;
         when N_Module_Instance =>
            Add_Decl (Item);
         when N_Always
           | N_Initial =>
            --  FIXME: unless there is a block ?
            null;

         when N_Loop_Generate
           | N_If_Generate =>
            null;
         when N_Array_Generate_Block =>
            Add_Decl (Item);
         when N_Indexed_Generate_Block =>
            null;
         when N_Generate_Block =>
            if Get_Identifier (Item) /= Null_Identifier then
               Add_Decl (Item);
            end if;

         when N_Discipline
           | N_Nature =>
            Add_Decl (Item);

         when N_Module
           | N_Foreign_Module =>
            --  Not the same namespace.
            --  TODO: except for nested modules ?
            null;

         when N_Package
           | N_Interface_Declaration =>
            --  Not the same namespace.
            null;

         when others =>
            Error_Kind ("add_item_decl", Item);
      end case;
   end Add_Item_Decl;

   procedure Add_Names_Chain (Chain : Node)
   is
      Item : Node;
   begin
      Item := Chain;
      while Item /= Null_Node loop
         Add_Item_Decl (Item);
         Item := Get_Chain (Item);
      end loop;
   end Add_Names_Chain;

   --  Add default connections.
   procedure Sem_Wildcard_Connections_Expand
     (Inst : Node; Wildcard : Node; Last_Conn : Node)
   is
      Module : constant Node := Get_Instance (Inst);
      Mod_Port : Node;
      First, Last : Node;
      Name : Node;
      Conn : Node;
      Conn_Name : Node;
   begin
      Init_Chain (First, Last);

      Mod_Port := Get_Ports_Chain (Module);
      while Mod_Port /= Null_Node loop
         if not Get_Connected_Flag (Mod_Port) then
            --  1800-2017 23.3.2.4 Connecting module instances using
            --    wildard named port connections (.*)
            --  2) Using .* does not create a sufficient reference for
            --     a wildcard import of a name from a package.
            Name := Get_Decl_No_Import (Get_Identifier (Mod_Port));
            if Name = Null_Node then
               --  1800-2017 23.3.2.4 Connecting module instances using
               --    wildard named port connections (.*)
               --  1) [...] When using .*, however, the default value
               --     shall be used if the name does not exist in the
               --     instantiating scope.
               if Get_Kind (Mod_Port) /= N_Input
                 or else Get_Default_Value (Mod_Port) = Null_Node
               then
                  Error_Msg_Sem
                    (+Wildcard, "no name nor default value for port %i",
                     +Mod_Port);
               else
                  Conn := Create_Node (N_Default_Connection);
                  Location_Copy (Conn, Wildcard);
                  Set_Port (Conn, Mod_Port);
                  Append_Chain (First, Last, Conn);
                  Set_Connected_Flag (Mod_Port, True);
               end if;
            else
               case Get_Kind (Mod_Port) is
                  when N_Interface_Port =>
                     --  FIXME: check NAME is an interface.
                     if False and then
                       Get_Declaration (Get_Data_Type (Mod_Port))
                       /= Get_Declaration (Get_Interface_Name (Name))
                     then
                        Error_Msg_Sem
                          (+Wildcard, "cannot create implicit connection "
                             & "for port %i: interface mismatch",
                           +Mod_Port);
                        goto Skip;
                     end if;
                  when Nkinds_Net_Port =>
                     --  FIXME: what about interface port ?
                     --  FIXME: check NAME is an object.
                     --  if False and then
                     --    not Are_Equivalent_Types (Get_Type (Mod_Port),
                     --                              Get_Type (Name))
                     --  then
                     --     Error_Msg_Sem
                     --       (+Wildcard, "cannot create implicit connection"
                     --          & " for port %i: types mismatch",
                     --        +Mod_Port);
                     --     goto Skip;
                     --  end if;
                     null;
                  when others =>
                     raise Internal_Error;
               end case;

               --  Create the implicit connection.
               Conn_Name := Create_Node (N_Name);
               Location_Copy (Conn_Name, Wildcard);
               Set_Identifier (Conn_Name, Get_Identifier (Mod_Port));
               Set_Declaration (Conn_Name, Name);

               Conn := Create_Node (N_Implicit_Connection);
               Location_Copy (Conn, Wildcard);
               --  Set_Identifier (Conn, Get_Identifier (Mod_Port));
               Set_Port (Conn, Mod_Port);
               Set_Expression (Conn, Conn_Name);
               Append_Chain (First, Last, Conn);
               Set_Connected_Flag (Mod_Port, True);

               <<Skip>> null;
            end if;
         end if;
         Mod_Port := Get_Chain (Mod_Port);
      end loop;

      if First /= Null_Node then
         --  Append created ports.
         Set_Chain (Last_Conn, First);
      end if;
   end Sem_Wildcard_Connections_Expand;

   procedure Sem_Wildcard_Connections_Inner_Chain (Chain : Node)
   is
      Item : Node;
   begin
      Add_Tf_Decls (Chain);
      Item := Chain;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Nkinds_Process
               | N_Assign
               | N_Assert_Property
               | N_Assume_Property
               | N_Analog =>
               null;
            when N_Package_Import =>
               null;
            when N_Genvar =>
               null;
            when N_Module_Instance =>
               Add_Decl (Item);

               --  Expand wildcard.
               declare
                  Conn : Node;
                  Last_Conn : Node;
                  Wildcard : Node;
               begin
                  Wildcard := Null_Node;
                  Last_Conn := Null_Node;
                  Conn := Get_Connections (Item);
                  while Conn /= Null_Node loop
                     case Nkinds_Connection (Get_Kind (Conn)) is
                        when N_Port_Connection
                          | N_Implicit_Connection
                          | N_Default_Connection =>
                           null;
                        when N_Wildcard_Connection =>
                           Wildcard := Conn;
                     end case;
                     Last_Conn := Conn;
                     Conn := Get_Chain (Conn);
                  end loop;
                  if Wildcard /= Null_Node then
                     Sem_Wildcard_Connections_Expand
                       (Item, Wildcard, Last_Conn);
                  end if;
               end;
            when N_Interface_Instance
              | Nkinds_Gate =>
               Add_Decl (Item);
            when Nkinds_Forward_Typedef
              | N_Typedef =>
               if not Get_Forward_Typedef_Flag (Item) then
                  Add_Decl (Item);
               end if;
            when Nkinds_Tf =>
               null;
            when Nkinds_Net_Port =>
               --  Redeclared.
               null;
            when N_Var
              | Nkinds_Nets
              | N_Class
              | N_Parameter
              | N_Localparam =>
               Add_Decl (Item);
            when N_Specify =>
               null;
            when N_Generate_Region
              | N_Generate_Block =>
               Sem_Wildcard_Connections_Inner_Chain
                 (Get_Generate_Item_Chain (Item));
            when N_Loop_Generate
               | N_If_Generate =>
               --  Already expanded.
               null;
            when others =>
               Error_Kind ("sem_wildcard_connections_inner_chain", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Wildcard_Connections_Inner_Chain;

   --  Make wildcard connections within INST explicit.
   procedure Sem_Wildcard_Connections (Inst : Node)
   is
      Module : constant Node := Get_Declaration (Get_Module (Inst));
      Module_Parent : constant Node := Get_Parent (Module);
      Item : Node;
   begin
      pragma Assert (Get_Kind (Module_Parent) = N_Compilation_Unit);

      --  Build the same environment as Module.
      --  (Same code as in Sem_Names).
      Open_Name_Space;
      Add_Decl (Module_Parent);

      --  Declarations in the compilation unit before the module.
      Item := Get_Descriptions (Module_Parent);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Module =>
               exit when Item = Module;
            when others =>
               Add_Item_Decl (Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      --  The module (if any).
      Item := Get_Instance (Inst);
      Open_Name_Space;
      Add_Decl_Chain (Get_Parameter_Port_Chain (Item));
      Sem_Wildcard_Connections_Inner_Chain (Get_Items_Chain (Item));
      Close_Name_Space;

      Close_Name_Space;

      --  Recurse.
      Resolve_Wildcard_Connections_Chain (Get_Items_Chain (Item));
   end Sem_Wildcard_Connections;

   procedure Resolve_Wildcard_Connections_Chain (Chain : Node)
   is
      Item : Node;
   begin
      Item := Chain;
      while Item /= Null_Node loop
         if Get_Kind (Item) = N_Module_Instance then
            Sem_Wildcard_Connections (Item);
         end if;
         Item := Get_Chain (Item);
      end loop;
   end Resolve_Wildcard_Connections_Chain;

   procedure Reparent_Items (Chain : Node; Parent : Node)
   is
      Item : Node;
   begin
      Item := Chain;
      while Item /= Null_Node loop
         Set_Parent (Item, Parent);
         Item := Get_Chain (Item);
      end loop;
   end Reparent_Items;

   procedure Resolve_Names_OOB_Subroutine (Decl : Node)
   is
      Klass_Name : constant Node := Get_OOB_Prefix (Decl);
      Prev_Mode : constant Resolve_Mode_Type := Resolve_Mode;
      Klass : Node;
      Method : Node;
      Prev : Node;
      Items : Node;
   begin
      if Get_Kind (Decl) = N_OOB_Function then
         Resolve_Names_Data_Type (Decl);
      end if;

      Resolve_Names_Identifier (Klass_Name, False);
      Klass := Get_Declaration (Klass_Name);

      if Klass = Null_Node then
         return;
      end if;

      case Get_Kind (Klass) is
         when N_Class
           | N_Generic_Class =>
            null;
         when others =>
            Error_Msg_Sem (+Klass_Name,
                           "%i does not designate a class", +Klass_Name);
            return;
      end case;

      --  1800-2017 8.24 Out-of-block declarations
      --  An out-of-block declaration shall be declared in the same
      --  scope as the class declaration [...]
      if Get_Parent (Decl) /= Get_Parent (Klass) then
         Error_Msg_Sem
           (+Decl, "out-of-block declaration not in the same scope "
              & "as the class");
         return;
      end if;

      Method := Find_Name_In_Scope (Klass, Decl);
      if Method = Null_Node then
         Error_Msg_Sem
           (+Decl, "no method %i declared in class %i", (+Klass_Name, +Klass));
         return;
      end if;

      case Get_Kind (Method) is
         when N_Extern_Function
           | N_Extern_Task =>
            null;
         when others =>
            Error_Msg_Sem
              (+Decl, "%i does not designate an extern method", +Klass_Name);
            return;
      end case;

      --  1800-2017 8.24 Out-of-block declarations
      --  It shall be an error if more than one out-of-block
      --  declaration is provided for a particular EXTERN method.
      Prev := Get_Out_Of_Block_Declaration (Method);
      if Prev /= Null_Node then
         Error_Msg_Sem (+Decl, "duplicate out-of-block declaration");
         return;
      end if;

      Set_Out_Of_Block_Declaration (Method, Decl);

      --  Move declarations and statements to the extern method.
      Items := Get_Tf_Item_Declaration_Chain (Decl);
      Set_Tf_Item_Declaration_Chain (Method, Items);
      Set_Tf_Item_Declaration_Chain (Decl, Null_Node);
      Reparent_Items (Items, Method);

      Items := Get_Statements_Chain (Decl);
      Set_Statements_Chain (Method, Items);
      Set_Statements_Chain (Decl, Null_Node);
      Reparent_Items (Items, Method);

      --  Try to resolve names using 'class' mode (ie don't emit an
      --  error if an identifier is not defined).
      Resolve_Mode := Mode_Class;

      --  First the ports of the OOB declaration.
      if Get_Ansi_Port_Flag (Decl) then
         Open_Name_Space;
         Resolve_Names_Chain (Get_Tf_Ports_Chain (Decl));
         Close_Name_Space;
      end if;

      --  Then the body.
      Resolve_Names_Subroutine_Body (Method);

      Resolve_Mode := Prev_Mode;
   end Resolve_Names_OOB_Subroutine;

   --  1800-2017 7.12 array manipulation methods
   procedure Resolve_Names_Array_Method_Call (Item : Node)
   is
      Arg : constant Node := Get_Iterator_Argument (Item);
      With_Expr : constant Node := Get_With_Expression (Item);
   begin
      Resolve_Names_Name (Get_Subroutine (Item));
      Resolve_Names (Get_Expression (Item));
      if With_Expr /= Null_Node then
         Open_Name_Space;
         Add_Decl (Arg);
         Resolve_Names (With_Expr);
         Close_Name_Space;
      end if;
   end Resolve_Names_Array_Method_Call;

   --  Resolve names in ITEM.  Set SECOND to true if a second pass is required.
   --  Second pass needed:
   --  * for forward typedef: check the type has been defined.
   --  * for out-of-block methods: analyze the method in the context of the
   --    class.
   --  * for class: th base class is a forward typedef, (need to check that
   --    all extern declarations are defined)
   procedure Resolve_Names (Item : Node) is
   begin
      if Item = Null_Node then
         return;
      end if;

      case Get_Kind (Item) is
         when N_Compilation_Unit =>
            Resolve_Names_Compilation_Unit (Item);
         when N_Package_Import =>
            Resolve_Names_Package_Import (Item);

         when N_Package =>
            declare
               Items : constant Node := Get_Package_Item_Chain (Item);
            begin
               Sem_Scopes.Add_Package (Item);

               Open_Name_Space;
               Add_Tf_Decls (Items);
               Resolve_Names_Chain (Items);
               Close_Name_Space;
            end;
         when N_Module
            | N_Interface_Declaration
            | N_Program_Declaration =>
            declare
               Items : Node;
               Imp : Node;
            begin
               --  TODO: nested declarations
               Sem_Scopes.Add_Definition (Item);

               Open_Name_Space;

               Imp := Get_Package_Import_Chain (Item);
               while Imp /= Null_Node loop
                  Resolve_Names_Package_Import (Imp);
                  Imp := Get_Chain (Imp);
               end loop;

               Resolve_Names_Chain (Get_Parameter_Port_Chain (Item));
               if Get_Ansi_Port_Flag (Item) then
                  Resolve_Names_Chain (Get_Ports_Chain (Item));
               end if;
               Items := Get_Items_Chain (Item);
               Add_Tf_Decls (Items);
               Resolve_Names_Chain_Implicit (Items, Item);
               Set_Items_Chain (Item, Items);
               Close_Name_Space;
            end;
         when N_Primitive =>
            Sem_Scopes.Add_Definition (Item);

            Open_Name_Space;
            if Get_Ansi_Port_Flag (Item) then
               Resolve_Names_Chain (Get_Ports_Chain (Item));
            else
               Resolve_Names_Chain (Get_Udp_Port_Declaration_Chain (Item));
            end if;
            Close_Name_Space;

         --  Module items

         when N_Class
           | N_Generic_Class =>
            Resolve_Names_Class (Item);
         when N_Typedef =>
            Resolve_Forward_Typedef (Item);
            Resolve_Names_Data_Type (Item);
         when Nkinds_Forward_Typedef =>
            declare
               Decl : Node;
            begin
               pragma Assert (Get_Forward_Type (Item) = Null_Node);
               --  Maybe the type was already declared.
               Decl := Peek_Scope_Decl (Get_Identifier (Item));
               if Decl = Null_Node then
                  Add_Decl (Item);
               else
                  --  The current name can be a forward type.
                  Set_Forward_Type (Item, Decl);
                  Set_Forward_Typedef_Flag (Item, True);
               end if;
            end;
         when N_Parameter
           | N_Localparam =>
            Resolve_Names_Data_Type (Item);
            Resolve_Names (Get_Expression (Item));
            Add_Decl (Item);
         when N_Type_Parameter =>
            Resolve_Names_Type
              (Get_Default_Type (Item), Get_Type_Owner (Item));
            Add_Decl (Item);
         when N_Parameter_Value_Expr =>
            Resolve_Names (Get_Expression (Item));
         when N_Parameter_Value_Type =>
            Resolve_Names_Data_Type (Item);
         when Nkinds_Net_Port =>
            Resolve_Names_Port (Item);
            Add_Decl (Item);
         when Nkinds_Process =>
            Resolve_Names (Get_Statement (Item));
         when N_Generate_Block =>
            --  1800-2017 3.13 Name spaces
            --  f) the block name space is introduced by named or unnamed
            --     blocks [...]
            declare
               Chain : Node;
            begin
               Open_Name_Space;
               if Get_Identifier (Item) /= Null_Identifier then
                  Add_Decl (Item);
               end if;
               Chain := Get_Generate_Item_Chain (Item);
               Add_Tf_Decls (Chain);
               Resolve_Names_Chain_Implicit (Chain, Item);
               Set_Generate_Item_Chain (Item, Chain);
               Close_Name_Space;
            end;
         when N_Var =>
            Resolve_Names_Data_Type (Item);
            --  Note: it is possible to refer the variable in the expression.
            --  eg (in uvm):  T e = e.first();
            Add_Data_Object_Decl (Item);
            Resolve_Names (Get_Expression (Item));
         when N_Function
           | N_Task =>
            Resolve_Names_Subroutine_Decl (Item);
            Resolve_Names_Subroutine_Body (Item);
         when N_OOB_Function
           | N_OOB_Task =>
            Resolve_Names_OOB_Subroutine (Item);
         when N_Extern_Function
           | N_Extern_Task =>
            Resolve_Names_Subroutine_Decl (Item);
         when N_Import_DPI_Function =>
            Resolve_Names_Subroutine_Decl (Item);
         when Nkinds_Tf_Port =>
            Resolve_Names_Data_Type (Item);
            Resolve_Names (Get_Default_Value (Item));
            Add_Decl (Item);
         when N_Export_DPI_Function
           | N_Export_DPI_Task =>
            null;
         when N_Wire_Direct =>
            Resolve_Names_Data_Type (Item);
            Add_Data_Object_Decl (Item);
            Resolve_Names (Get_Expression (Item));
         when Nkinds_Delay_Nets =>
            Resolve_Names (Get_Net_Delay (Item));
            Resolve_Names_Data_Type (Item);
            Add_Data_Object_Decl (Item);
            Resolve_Names (Get_Expression (Item));
         when N_Port_Connection =>
            Resolve_Names (Get_Expression (Item));
         when N_Genvar =>
            Add_Decl (Item);
         when N_Generate_Region =>
            Resolve_Names_Chain (Get_Generate_Item_Chain (Item));
         when N_Loop_Generate =>
            Resolve_Names_Loop_Generate (Item);
         when N_If_Generate =>
            Resolve_Names_If_Generate (Item);
         when N_Clocking =>
            Resolve_Names (Get_Event (Item));
            Open_Name_Space;
            Resolve_Names_Chain (Get_Clocking_Item_Chain (Item));
            Close_Name_Space;
            Add_Decl (Item);
         when N_Clock_Var =>
            Resolve_Names (Get_Input_Skew (Item));
            Resolve_Names (Get_Output_Skew (Item));
            Resolve_Names (Get_Expression (Item));
            Add_Decl (Item);
         when N_Modport =>
            Open_Name_Space;
            Resolve_Names_Chain (Get_Modport_Ports_Chain (Item));
            Close_Name_Space;
            Add_Decl (Item);
         when N_Modport_Input
           | N_Modport_Output
           | N_Modport_Inout
           | N_Modport_Ref =>
            declare
               Expr : Node;
               Name : Node;
            begin
               Expr := Get_Expression (Item);
               if Expr /= Null_Node then
                  Resolve_Names (Get_Expression (Item));
               else
                  Expr := Get_Decl (Get_Identifier (Item));
                  if Expr = Null_Node then
                     Error_Msg_Sem (+Item, "no declaration for %i", +Item);
                  elsif Get_Parent (Expr) /= Get_Parent (Get_Parent (Item))
                  then
                     Error_Msg_Sem
                       (+Item, "port %i not declared in interface", +Item);
                  else
                     --  Create a reference.
                     Name := Create_Node (N_Name);
                     Location_Copy (Name, Item);
                     Set_Identifier (Name, Get_Identifier (Item));
                     Set_Declaration (Name, Expr);

                     Set_Expression (Item, Name);
                  end if;
               end if;
            end;
            Add_Decl (Item);
         when N_Modport_Clocking =>
            Add_Decl (Item);
         when N_Virtual_Interface =>
            null;

         when Nkinds_Gate =>
            Resolve_Names (Get_Gate_Delay (Item));
            Resolve_Names_Chain (Get_Gate_Terminals (Item));
            Add_Decl (Item);
         when Nkinds_Terminal =>
            Resolve_Names (Get_Expression (Item));

         when N_Defparam =>
            Resolve_Names (Get_Lvalue (Item));
            Resolve_Names (Get_Expression (Item));

         when N_Default_Clocking =>
            Resolve_Names (Get_Event (Item));
            Resolve_Names_Chain (Get_Clocking_Item_Chain (Item));
            Add_Decl (Item);

         --  Statements

         when N_Seq_Block
           | N_Par_Block =>
            --  1800-2017 3.13 Name spaces
            --  f) the block name space is introduced by named or unnamed
            --     blocks [...]
            declare
               Decls_Chain : constant Node :=
                 Get_Block_Item_Declaration_Chain (Item);
            begin
               Open_Name_Space;
               if Get_Identifier (Item) /= Null_Identifier then
                  Add_Decl (Item);
               end if;
               Add_Tf_Decls (Decls_Chain);
               Resolve_Names_Chain (Decls_Chain);
               Resolve_Names_Chain (Get_Statements_Chain (Item));
               Close_Name_Space;
            end;
         when N_Blocking_Assign
            | N_Noblk_Assign
            | N_Unpack_Assign
            | N_Pack_Assign
            | N_Pack_Unpack_Assign =>
            Resolve_Names (Get_Lvalue (Item));
            Resolve_Names (Get_Control (Item));
            Resolve_Names (Get_Expression (Item));
         when N_Assign_Operator =>
            Resolve_Names (Get_Lvalue (Item));
            Resolve_Names (Get_Expression (Item));
         when N_Proc_Assign
            | N_Force_Assign =>
            Resolve_Names (Get_Lvalue (Item));
            Resolve_Names (Get_Expression (Item));
         when N_Release
            | N_Proc_Deassign =>
            Resolve_Names (Get_Lvalue (Item));
         when N_Delay_Control
            | N_Event_Control
            | N_Cycle_Delay =>
            Resolve_Names (Get_Expression (Item));
            Resolve_Names (Get_Statement (Item));
         when N_Repeat_Control =>
            Resolve_Names (Get_Expression (Item));
            Resolve_Names (Get_Control (Item));
            Resolve_Names (Get_Statement (Item));
         when N_If =>
            Resolve_Names (Get_Condition (Item));
            Resolve_Names (Get_True_Stmt (Item));
            Resolve_Names (Get_False_Stmt (Item));
         when N_For =>
            declare
               Init : constant Node := Get_For_Initialization (Item);
               Has_Scope : constant Boolean :=
                 Init /= Null_Node and then Get_Kind (Init) = N_Var;
            begin
               if Has_Scope then
                  Open_Name_Space;
               end if;
               Resolve_Names (Init);
               Resolve_Names (Get_Condition (Item));
               Resolve_Names (Get_Step_Assign (Item));
               Resolve_Names (Get_Statement (Item));
               if Has_Scope then
                  Close_Name_Space;
               end if;
            end;
         when N_While
           | N_Do_While =>
            Resolve_Names (Get_Condition (Item));
            Resolve_Names (Get_Statement (Item));
         when N_Wait =>
            Resolve_Names (Get_Condition (Item));
            Resolve_Names (Get_Statement (Item));
         when N_Wait_Fork =>
            null;
         when N_Foreach =>
            --  1800-2017 12.7.3 The foreach-loop
            --  As in a for-loop (12.7.1), a foreach-loop creates an implicit
            --  begin-end block around the loop statement, containing
            --  declarations of the loop variables with automatic lifetime.
            Open_Name_Space;
            Resolve_Names_Chain (Get_Foreach_Variables (Item));
            Resolve_Names (Get_Foreach_Array (Item));
            Resolve_Names (Get_Statement (Item));
            Close_Name_Space;
         when N_Foreach_Variable =>
            Add_Decl (Item);
         when Nkinds_Case =>
            Resolve_Names (Get_Expression (Item));
            Resolve_Names_Chain (Get_Case_Items (Item));
         when N_Case_Item =>
            Resolve_Names (Get_Expression (Item));
            Resolve_Names (Get_Statement (Item));
         when N_Default_Case_Item =>
            Resolve_Names (Get_Statement (Item));
         when N_Forever =>
            Resolve_Names (Get_Statement (Item));
         when N_Repeat =>
            Resolve_Names (Get_Expression (Item));
            Resolve_Names (Get_Statement (Item));
         when N_Return_Stmt =>
            Resolve_Names (Get_Expression (Item));
         when N_Subroutine_Call_Stmt =>
            Resolve_Names (Get_Call (Item));
         when N_System_Call =>
            declare
               Arg : Node;
               Expr : Node;
            begin
               --  FIXME: cannot use Resolve_Names_Chain because no error are
               --  emitted on undefined name (could be a hierarchy).
               Arg := Get_Arguments (Item);
               while Arg /= Null_Node loop
                  Expr := Get_Expression (Arg);
                  if Expr /= Null_Node then
                     Resolve_Names (Expr);
                  end if;
                  Arg := Get_Chain (Arg);
               end loop;
            end;
         when N_Argument =>
            Resolve_Names (Get_Expression (Item));
         when N_Break_Stmt
           | N_Continue_Stmt =>
            null;
         when N_Disable_Fork =>
            null;
         when N_Trigger =>
            Resolve_Names (Get_Event (Item));
         when N_Label_Stmt =>
            Resolve_Names (Get_Statements_Chain (Item));
         when N_Simple_Immediate_Assert =>
            Resolve_Names (Get_Condition (Item));
            Resolve_Names (Get_Pass_Stmt (Item));
            Resolve_Names (Get_Else_Stmt (Item));

         --  Expressions

         when N_Number
           | N_Real_Number
           | N_Unbased_Literal
           | N_Bignum
           | N_String_Literal
           | N_Null
           | N_Time_Literal
           | N_Infinity
           | N_Implicit_Event =>
            null;
         when N_This =>
            declare
               Decl : constant Node := Get_Decl (Std_Names.Name_This);
            begin
               Set_Declaration (Item, Decl);
            end;
         when N_Aggregate_Literal =>
            Resolve_Names_Chain (Get_Elements (Item));
         when N_Aggregate_Element =>
            Resolve_Names (Get_Expression (Item));
         when N_Stream_Expression =>
            Resolve_Names (Get_Expression (Item));
         when N_New_Call =>
            Resolve_Names_Chain (Get_Arguments (Item));
         when N_Dynamic_Array_New =>
            Resolve_Names (Get_Size_Expression (Item));
            Resolve_Names (Get_Init_Expression (Item));
         when N_Binary_Op
           | N_Short_Circuit_Op =>
            Resolve_Names (Get_Left (Item));
            Resolve_Names (Get_Right (Item));
         when N_Unary_Op =>
            Resolve_Names (Get_Expression (Item));
         when N_Cond_Op =>
            Resolve_Names (Get_Condition (Item));
            Resolve_Names (Get_Cond_True (Item));
            Resolve_Names (Get_Cond_False (Item));
         when Nkinds_Inc_Dec =>
            Resolve_Names_Name (Get_Lvalue (Item));
         when N_Parenthesis_Expr =>
            Resolve_Names (Get_Expression (Item));
         when N_Concatenation =>
            Resolve_Names (Get_Replication (Item));
            Resolve_Names_Chain (Get_Expressions (Item));
         when N_Element =>
            Resolve_Names (Get_Expression (Item));
         when N_Name
           | N_Bit_Select
           | N_Dotted_Name
           | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Scoped_Name =>
            Resolve_Names_Name (Item);
         when N_Right_Streaming_Expr
           | N_Left_Streaming_Expr =>
            Resolve_Names (Get_Expression (Item));
            Resolve_Names_Chain (Get_Expressions (Item));
         when N_Left_Streaming_Type
           | N_Right_Streaming_Type =>
            --  TODO: size type.
            Resolve_Names_Chain (Get_Expressions (Item));
         when N_Type_Cast =>
            Resolve_Names_Type (Get_Cast_Data_Type (Item),
                                Get_Type_Owner (Item));
            Resolve_Names (Get_Expression (Item));
         when N_Size_Cast =>
            Resolve_Names (Get_Size_Expression (Item));
            Resolve_Names (Get_Expression (Item));
         when N_Bits_Type =>
            Resolve_Names_Type (Item, Get_Type_Owner (Item)); -- ??
         when N_Bits_Expr =>
            Resolve_Names (Get_Expression (Item));
         when N_Membership =>
            Resolve_Names (Get_Expression (Item));
            Resolve_Names_Chain (Get_Expressions (Item));
         when N_Value_Range =>
            Resolve_Names (Get_Msb (Item));
            Resolve_Names (Get_Lsb (Item));
         when Nkinds_Edge =>
            Resolve_Names (Get_Expression (Item));
         when N_Or =>
            Resolve_Names (Get_Left (Item));
            Resolve_Names (Get_Right (Item));
         when N_Call =>
            declare
               Rtn : Node;
               Decl : Node;
            begin
               Rtn := Get_Subroutine (Item);
               Resolve_Names_Name (Rtn);
               if Get_Kind (Rtn) = N_Name then
                  Decl := Get_Declaration (Rtn);
                  if Decl /= Null_Node then
                     Maybe_Mutate_Name (Rtn, Decl);
                  end if;
               end if;
               Resolve_Names_Chain (Get_Arguments (Item));
            end;
         when N_Array_Method_Call =>
            Resolve_Names_Array_Method_Call (Item);
         when N_Randomize_Call =>
            Resolve_Names_Name (Get_Subroutine (Item));
            Resolve_Names_Chain (Get_Arguments (Item));
            Resolve_Names (Get_With_Expression (Item));
            Resolve_Names_Chain (Get_Constraint_Block_Chain (Item));

         when N_Constraint =>
            Resolve_Names_Chain (Get_Constraint_Block_Chain (Item));
            Add_Decl (Item);
         when N_Constraint_Expression =>
            Resolve_Names (Get_Expression (Item));
         when N_Constraint_If =>
            Resolve_Names (Get_Condition (Item));
            Resolve_Names (Get_Cond_True (Item));
            Resolve_Names (Get_Cond_False (Item));
         when N_Constraint_Foreach =>
            Open_Name_Space;
            Resolve_Names_Chain (Get_Foreach_Variables (Item));
            Resolve_Names (Get_Foreach_Array (Item));
            Resolve_Names (Get_Constraint_Set (Item));
            Close_Name_Space;
         when N_Assert_Property
           | N_Assume_Property =>
            Resolve_Names (Get_Clocking_Event (Item));
            Resolve_Names (Get_Pass_Stmt (Item));
            Resolve_Names (Get_Else_Stmt (Item));
         when N_This_Name
           | N_Hierarchical =>
            pragma Assert (Resolve_Mode = Mode_Complete);
            null;

         when N_Discipline =>
            Add_Decl (Item);
         when N_Nature =>
            Add_Decl (Item);
            Resolve_Names_Chain (Get_Nature_Items (Item));
         when N_Nature_Attribute =>
            null;
         when N_Nature_Access =>
            Add_Decl (Item);

         when N_Analog =>
            Resolve_Names (Get_Statement (Item));
         when N_Contribution =>
            Resolve_Names (Get_Lvalue (Item));
            Resolve_Names (Get_Expression (Item));

         when N_Specify =>
            null;

         when others =>
            Error_Kind ("resolve_names", Item);
      end case;
   end Resolve_Names;

   procedure Resolve_Names_Chain (Items : Node)
   is
      Item : Node;
   begin
      --  Resolve name for parameters, typedef.
      Item := Items;
      while Item /= Null_Node loop
         Resolve_Names (Item);
         Item := Get_Chain (Item);
      end loop;
   end Resolve_Names_Chain;
end Verilog.Resolve_Names;
