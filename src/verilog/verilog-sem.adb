--  Verilog semantic analyzer
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

with Verilog.Types; use Verilog.Types;
with Verilog.Flags; use Verilog.Flags;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Sem_Utils; use Verilog.Sem_Utils;
with Verilog.Sem_Decls; use Verilog.Sem_Decls;
with Verilog.Sem_Types; use Verilog.Sem_Types;
with Verilog.Sem_Scopes; use Verilog.Sem_Scopes;
with Verilog.Sem_Upwards; use Verilog.Sem_Upwards;
with Verilog.Sem_Expr; use Verilog.Sem_Expr;
with Verilog.Sem_Stmts; use Verilog.Sem_Stmts;
with Verilog.Sem_Names; use Verilog.Sem_Names;
with Verilog.Resolve_Names;
with Verilog.Sem_Eval; use Verilog.Sem_Eval;
with Verilog.Sem_Instances;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Nutils; use Verilog.Nutils;

package body Verilog.Sem is
   procedure Sem_Instantiated_Module (Module : Node);
   procedure Sem_Subroutines_Chain (Items : Node);
   procedure Sem_Items_Chain (Items : Node);
   procedure Sem_Overrides_Chain (Chain : Node);

   procedure Sem_If_Generate (N : Node; Base : Node);

   --  Analyze RTN body.
   procedure Sem_Subroutine_Body (Rtn : Node)
   is
      Decls_Chain : constant Node := Get_Tf_Item_Declaration_Chain (Rtn);
      Retvar : Node;
      This_Var : Node;
   begin
      pragma Assert (Get_Fully_Analyzed_Flag (Rtn));

      --  Automatic variables are always allocated on the stack frame.
      Set_Is_Automatic (Rtn, True);

      if Nkind_In (Get_Kind (Rtn), N_Function, N_Extern_Function)
        and then Get_Data_Type (Rtn) /= Void_Typedef
        and then Get_Identifier (Rtn) /= Name_New  -- Not for constructor ?
      then
         --  1800-2017 13.4.1 Return values and void functions
         --  The function definition shall implicitly declare a variable,
         --  internal to the functions, with the same name as the function.
         --  FIXME: 'visibility' ? even for void functions ? is_automatic ?
         Retvar := Create_Node (N_Return_Var);
         Location_Copy (Retvar, Rtn);
         Set_Identifier (Retvar, Get_Identifier (Rtn));
         Set_Parent (Retvar, Rtn);
         Set_Return_Variable (Rtn, Retvar);
         Set_Expr_Type (Retvar, Get_Type_Data_Type (Rtn));
         Set_Is_Automatic (Retvar, Get_Lifetime (Rtn) = Life_Automatic);

         --  The return variable is not made visible, it is referenced by using
         --  the function name.
      end if;

      --  Note: 'this' implicit parameter is added by resolve_names.
      This_Var := Get_This_Variable (Rtn);
      if This_Var /= Null_Node then
         pragma Assert (Sem_Utils.Is_Method (Rtn));

         declare
            Class_Type : constant Node := Get_Parent (Rtn);
         begin
            pragma Assert (Get_Kind (Class_Type) in Nkinds_Class);
            Set_Expr_Type (This_Var, Class_Type);
         end;
         Set_Is_Automatic (This_Var, True);
      end if;

      Sem_Decl_Type_Chain (Decls_Chain);
      Sem_Decl_Type_Chain (Get_Statements_Chain (Rtn));

      Sem_Block_Items_Declaration (Decls_Chain);
      Sem_Subroutine_Statements (Rtn);
   end Sem_Subroutine_Body;

   --  1800-2017 18.5 Constraint blocks
   procedure Sem_Constraint_Chain (Chain : Node)
   is
      Item : Node;
      Expr : Node;
   begin
      Item := Chain;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Constraint_Expression =>
               --  1800-2017 18.5 Constraint blocks
               --  A constraint_expression is any SystemVerilog expression, or
               --  one of the constraint-specific operators, dist and ->
               Expr := Get_Expression (Item);
               Expr := Sem_Expression (Expr, Null_Node);
               Set_Expression (Item, Expr);
            when N_Constraint_If =>
               Sem_Cond_Expression (Item);
               Sem_Constraint_Chain (Get_Cond_True (Item));
               Sem_Constraint_Chain (Get_Cond_False (Item));
            when N_Constraint_Foreach =>
               Sem_Foreach_Variables (Item);
               Sem_Constraint_Chain (Get_Constraint_Set (Item));
            when others =>
               Error_Kind ("sem_constraint", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Constraint_Chain;

   --  1800-2017 18.5 Constraint blocks
   procedure Sem_Constraint (Decl : Node)
   is
      Chain : Node;
   begin
      Chain := Get_Constraint_Block_Chain (Decl);
      Sem_Constraint_Chain (Chain);
   end Sem_Constraint;

   procedure Class_Visibility_Enter (Klass : Node)
   is
      Parent : Node;
   begin
      pragma Assert (Get_Class_Visibility (Klass) = Visibility_Public);
      Set_Class_Visibility (Klass, Visibility_Local);

      Parent := Klass;
      loop
         Parent := Iterate_Base_Class_Type (Parent);
         exit when Parent = Null_Node;
         pragma Assert (Get_Class_Visibility (Parent) = Visibility_Public);
         Set_Class_Visibility (Parent, Visibility_Protected);
      end loop;
   end Class_Visibility_Enter;

   procedure Class_Visibility_Leave (Klass : Node)
   is
      Parent : Node;
   begin
      pragma Assert (Get_Class_Visibility (Klass) = Visibility_Local);
      Set_Class_Visibility (Klass, Visibility_Public);

      Parent := Klass;
      loop
         Parent := Iterate_Base_Class_Type (Parent);
         exit when Parent = Null_Node;
         pragma Assert (Get_Class_Visibility (Parent) = Visibility_Protected);
         Set_Class_Visibility (Parent, Visibility_Public);
      end loop;
   end Class_Visibility_Leave;

   procedure Sem_Class_Type_Methods (Klass : Node)
   is
      Params : constant Node := Get_Parameter_Port_Chain (Klass);
      Base : constant Node := Get_Base_Class_Type (Klass);
      Items : constant Node := Get_Class_Item_Chain (Klass);
      Param : Node;
      Item : Node;
      Has_Typedef : Boolean;
   begin
      --  Base class must have been fully analyzed while interned.
      if Base /= Null_Node then
         declare
            Base_Class : constant Node := Get_Expr_Type (Base);
         begin
            if not Get_Fully_Analyzed_Flag (Base_Class) then
               Sem_Class_Type_Methods (Base_Class);
            end if;
         end;
      end if;

      --  Likewise for parameters.
      Param := Params;
      while Param /= Null_Node loop
         if Get_Kind (Param) = N_Type_Parameter then
            declare
               Param_Type : constant Node := Get_Parameter_Type (Param);
            begin
               if Get_Kind (Param_Type) in Nkinds_Class
                 and then not Get_Fully_Analyzed_Flag (Param_Type)
               then
                  Sem_Class_Type_Methods (Param_Type);
               end if;
            end;
         end if;
         Param := Get_Chain (Param);
      end loop;

      pragma Assert (not Get_Fully_Analyzed_Flag (Klass));
      if Get_Mark_Flag (Klass) then
         --  Recursion
         raise Internal_Error;
      end if;
      Set_Mark_Flag (Klass, True);

      --  Give full visbility over private items.
      Class_Visibility_Enter (Klass);

      --  1800-2017 8.24 Out-of-block declaration
      --  An out-of-block method declaration shall be able to access all
      --  declarations of the class in which the corresponding prototype is
      --  declared.
      Has_Typedef := False;
      Item := Items;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Typedef =>
               declare
                  Typedef_Type : constant Node := Get_Type_Data_Type (Item);
               begin
                  if Get_Kind (Typedef_Type) in Nkinds_Class
                    and then not Get_Fully_Analyzed_Flag (Typedef_Type)
                  then
                     Has_Typedef := True;
                  end if;
               end;
            when N_Var =>
               declare
                  Var_Type : constant Node := Get_Type_Data_Type (Item);
               begin
                  if Get_Kind (Var_Type) in Nkinds_Class
                    and then not Get_Fully_Analyzed_Flag (Var_Type)
                  then
                     Has_Typedef := True;
                  end if;
               end;
               Sem_Var (Item);
            when N_Task
              | N_Function =>
               Sem_Subroutine_Body (Item);
            when N_Extern_Function
              | N_Extern_Task =>
               declare
                  Decl : constant Node :=
                    Get_Out_Of_Block_Declaration (Item);
               begin
                  if Decl = Null_Node then
                     Error_Msg_Sem
                       (+Item, "missing out-of-block declaration "
                          & "for extern method %i", +Item);
                  else
                     --  The ports of the OOB declaration are not instantiated,
                     --  therefore they can be shared by many instances.
                     if not Get_Fully_Analyzed_Flag (Decl) then
                        Sem_Tf_Ports (Decl);

                        --  Prevent Sem_Subroutine_Body to re-analyze ports.
                        Set_Fully_Analyzed_Flag (Decl, True);

                        --  TODO: check signature.
                     end if;

                     Sem_Subroutine_Body (Item);
                  end if;
               end;
            when N_Constraint =>
               Sem_Constraint (Item);
            when others =>
               Error_Kind ("sem_class_type_methods", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      --  Only public items are visible from outside the class.
      Class_Visibility_Leave (Klass);

      Set_Mark_Flag (Klass, False);
      Set_Fully_Analyzed_Flag (Klass, True);

      if Has_Typedef then
         Item := Items;
         while Item /= Null_Node loop
            case Get_Kind (Item) is
               when N_Typedef
                 | N_Var =>
                  declare
                     Decl_Type : constant Node := Get_Type_Data_Type (Item);
                  begin
                     if Get_Kind (Decl_Type) in Nkinds_Class
                       and then not Get_Fully_Analyzed_Flag (Decl_Type)
                     then
                        Sem_Class_Type_Methods (Decl_Type);
                     end if;
                  end;
               when others =>
                  null;
            end case;
            Item := Get_Chain (Item);
         end loop;
      end if;
   end Sem_Class_Type_Methods;

   --  1800-2017 23.2.2.1 Non-ANSI style port declarations.
   --  These declarations appear in items list.
   --  Also called for port declarations in ANSI header.
   procedure Sem_Port_Declaration (Port : Node)
   is
      Decl : constant Node := Get_Redeclaration (Port);
      Port_Type : Node;
      Decl_Type : Node;
   begin
      Port_Type := Get_Data_Type (Port);
      pragma Assert (Port_Type /= Null_Node);

      --  Set type on implicit redeclared net.
      if Decl /= Null_Node
        and then Get_Kind (Decl) in Nkinds_Nets
      then
         if Get_Implicit_Flag (Decl) then
            Set_Data_Type (Decl, Port_Type);
         else
            --  Redeclaration of a port as a net/var.

            --  1800-2017 23.2.2.1 Non-ANSI style port declarations
            --  If a port declaration does not include a net or variable type,
            --  then the port can be again declared in a net or variable
            --  declaration. If the net or variable is declared as a vector,
            --  the range specification between the two declarations of a
            --  port shall be identical.
            if Get_Expr_Type (Port_Type) /= Get_Type_Data_Type (Decl) then
               Error_Msg_Sem
                 (+Decl, "type between port and redeclaration doesn't match");
            end if;
         end if;
      end if;

      if False then
         --  Implicit nets must have been created.
         pragma Assert (Decl /= Null_Node);

         if Get_Complete_Flag (Port) then
            pragma Assert (Get_Data_Type (Decl) = Null_Node);
            Set_Data_Type (Decl, Port_Type);
         elsif Get_Kind (Decl) in Nkinds_Nets
           and then Get_Implicit_Flag (Decl)
         then
            pragma Assert (Get_Data_Type (Decl) /= Null_Node);
            Set_Data_Type (Decl, Port_Type);
         else
            pragma Assert (Get_Data_Type (Decl) /= Null_Node);
            Sem_Decl_Data_Type (Decl);
            Decl_Type := Get_Data_Type (Decl);

            if Decl_Type /= Port_Type then
               --  Check compatibility.
               raise Program_Error;
            end if;
         end if;
      end if;
   end Sem_Port_Declaration;

   --  3.2  Nets and registers.
   procedure Sem_Var (Var : Node)
   is
      Parent : constant Node := Get_Parent (Var);
      Expr : Node;
      Life : Lifetime_Type;
      Auto : Boolean;
   begin
      Life := Get_Lifetime (Var);
      case Get_Kind (Parent) is
         when N_Module
           | N_Package
           | N_Generate_Block
           | N_Interface_Declaration
           | N_Compilation_Unit =>
            --  1800-2017 6.21 Scope and lifetime
            --  Variable declared outside a module, program, interface,
            --  checker, task, or function are local to the compilation unit
            --  and have a static lifetime (exist for the whole simulation).
            --  Variables declared inside a module, interface, program, or
            --  checker, but outside a task, process, or function, are local
            --  in scope and have a static lifetime.
            if Get_Has_Lifetime (Var)
              and then Get_Lifetime (Var) = Life_Automatic
            then
               Error_Msg_Sem
                 (+Var, "variables cannot be automatic in that context");
            end if;
            Life := Life_Static;
            Auto := False;
         when Nkinds_Any_Tf
           | N_Seq_Block
           | N_Par_Block =>
            --  1800-2017 6.21 Scope and lifetime
            --  Variables declared inside a static task, function, or block
            --  are local in scope a default to a static lifetime.  Specific
            --  variables within a static task, function, or block can be
            --  explicitly declared as automatic.  Such variables have the
            --  lifetime of the call or block and are initialized on each
            --  entry to the call or block.
            --
            --  Tasks and functions may be declared as automatic.  Variables
            --  in an automatic task, function, or block are local in scope,
            --  default to the lifetime of the call or block, and are
            --  initialized on each entry to the call or block.  An automatic
            --  block is one in which declarations are automatic by default.
            --  Specific variables within an automatic task, function, or
            --  block can be explicitly declared as static.  Such variables
            --  have a static lifetime.
            if Get_Has_Lifetime (Var) then
               Auto := Get_Lifetime (Var) = Life_Automatic;
            else
               Auto := Get_Is_Automatic (Parent);
               Life := Get_Lifetime (Parent);
            end if;
         when N_For =>
            Auto := Get_Lifetime (Parent) = Life_Automatic;
            Life := Life_Automatic;
         when N_Class
           | N_Instantiated_Class =>
            if Get_Has_Lifetime (Var) then
               Auto := Life = Life_Automatic;
            elsif Get_Static_Flag (Var) then
               Auto := False;
               Life := Life_Static;
            else
               --  Automatic by default.
               Auto := True;
               Life := Life_Automatic;
            end if;
         when others =>
            Error_Kind ("sem_var", Parent);
      end case;
      Set_Is_Automatic (Var, Auto);
      Set_Lifetime (Var, Life);

      Expr := Get_Expression (Var);
      if Expr /= Null_Node then
         Expr := Sem_Expression (Expr, Get_Type_Data_Type (Var));
         Set_Expression (Var, Expr);
      end if;
   end Sem_Var;

   --  3.2  Nets
   procedure Sem_Net_Declaration (Net : Node)
   is
      Expr : Node;
   begin
      Expr := Get_Expression (Net);
      if Expr /= Null_Node then
         Expr := Sem_Expression (Expr, Get_Type_Data_Type (Net));
         Set_Expression (Net, Expr);
      end if;
   end Sem_Net_Declaration;

   procedure Sem_Continuous_Assign (Stmt : Node)
   is
      Lval : Node;
      Lval_Type : Node;
      Expr : Node;
   begin
      --  SystemVerilog 3.0 LRM 9.5 Continuous assignments
      --  SystemVerilog removes this restriction, and permits continuous
      --  assignments to drive nets, 'logic' variables, and any other type of
      --  variables, except 'reg' variables.
      --  FIXME: reject 'reg' variables
      Lval := Get_Lvalue (Stmt);
      Lval := Sem_Lvalue (Lval,
                          Allow_Net => True,
                          Allow_Var => Std >= Verilog_Sv_3_0);
      Set_Lvalue (Stmt, Lval);

      Lval_Type := Get_Expr_Type (Lval);
      if Lval_Type = Null_Node then
         return;
      end if;

      --  IEEE 1800-2012 10.7
      --  The size of the left-hand size of an assignment forms the context for
      --  the right-hand expression.
      Expr := Sem_Expression (Get_Expression (Stmt), Lval_Type);
      Set_Expression (Stmt, Expr);

      if Get_Assign_Delay (Stmt) /= Null_Node then
         --  TODO
         -- raise Program_Error;
         null;
      end if;
   end Sem_Continuous_Assign;

   --  Analyze Initial or Always construct.
   procedure Sem_Construct (Stmt : Node) is
   begin
      Sem_Statement (Get_Statement (Stmt));
   end Sem_Construct;

   procedure Sem_Port_Connection_Collapse (Conn : Node; Expr : Node)
   is
      Port : constant Node := Get_Port (Conn);
      Loconn : Node;
      Decl : Node;
      Expr1 : Node;
   begin
      if Expr = Null_Node then
         --  Port if not connected.
         Set_Collapse_Flag (Conn, False);
         return;
      end if;

      --  The port must be a simple port_expression (only a name).
      if Get_Kind (Port) = N_Port then
         Loconn := Get_Expression (Port);
         pragma Assert (Get_Kind (Loconn) = N_Name);
         Loconn := Get_Declaration (Loconn);
      else
         Loconn := Port;
      end if;
      pragma Assert (Get_Kind (Loconn) in Nkinds_Net_Port);
      Loconn := Get_Redeclaration (Loconn);
      pragma Assert (Loconn /= Null_Node);

      Expr1 := Insert_Assignment_Compatible
        (Get_Type_Data_Type (Loconn), Expr, Conn);
      Set_Expression (Conn, Expr1);

      case Get_Kind (Expr1) is
         when N_Name =>
            Decl := Get_Declaration (Expr1);
            if Get_Kind (Decl) in Nkinds_Net_Port then
               Decl := Get_Redeclaration (Decl);
            end if;
            case Get_Kind (Decl) is
               when N_Var
                 | Nkinds_Nets =>
                  Set_Collapse_Flag (Conn, True);
               when others =>
                  Set_Collapse_Flag (Conn, False);
            end case;
         when others =>
            Set_Collapse_Flag (Conn, False);
      end case;
   end Sem_Port_Connection_Collapse;

   procedure Sem_Foreign_Port_Connection (Port : Node; Conn : Node)
   is
      pragma Unreferenced (Port);
      Expr : Node;
   begin
      Expr := Get_Expression (Conn);
      if Expr /= Null_Node then
         Expr := Sem_Expression (Expr, Null_Node);
         Set_Expression (Conn, Expr);
      end if;
   end Sem_Foreign_Port_Connection;

   procedure Sem_Port_Connection (Port : Node; Conn : Node)
   is
      Expr : Node;
   begin
      if Get_Kind (Get_Parent (Port)) = N_Foreign_Module then
         Sem_Foreign_Port_Connection (Port, Conn);
         return;
      end if;

      case Get_Kind (Port) is
         when Nkinds_Net_Port
           | N_Port =>
            Expr := Get_Expression (Conn);
            if Expr /= Null_Node then
               Expr := Sem_Expression (Expr, Null_Node);
               Set_Expression (Conn, Expr);
            end if;
            --  FIXME: check type ?  Done in sem_port_connection_collapse.
            Sem_Port_Connection_Collapse (Conn, Expr);
         when N_Interface_Port =>
            Expr := Get_Expression (Conn);
            if Get_Kind (Expr) = N_Name then
               Expr := Get_Declaration (Expr);
            else
               Expr := Null_Node;
            end if;
            if Expr /= Null_Node
              and then Get_Kind (Expr) /= N_Interface_Instance
            then
               Error_Msg_Sem
                 (+Conn,
                  "only an interface can be connected to an interface");
            else
               --  FIXME: check matching interface
               Set_Collapse_Flag (Conn, True);
            end if;
         when N_Modport_Port =>
            Expr := Get_Expression (Conn);
            Expr := Sem_Name (Expr);
            if Expr = Null_Node then
               return;
            end if;
            Set_Expression (Conn, Expr);
            if Get_Kind (Get_Expr_Type (Expr)) /= N_Modport then
               Error_Msg_Sem
                 (+Conn, "only a modport can be connected to a modport");
            else
               --  FIXME: check matching modport
               Set_Collapse_Flag (Conn, True);
            end if;
         when others =>
            Error_Kind ("sem_port_connection", Port);
      end case;
   end Sem_Port_Connection;

   function Is_Ordered_Connection (Conn : Node) return Boolean is
   begin
      if Conn = Null_Node then
         --  If there is no connection, it's ordered connection.
         return True;
      end if;
      case Nkinds_Connection (Get_Kind (Conn)) is
         when N_Wildcard_Connection
           | N_Implicit_Connection =>
            return False;
         when N_Port_Connection =>
            return Get_Identifier (Conn) = Null_Identifier;
         when N_Default_Connection =>
            --  Not from user.
            raise Internal_Error;
      end case;
   end Is_Ordered_Connection;

   --  Analyze connections.  Both the instance and the port expressions have
   --  been analyzed.
   --  INST is the instantiation, DEF is the definition of the module,
   --   programm or interface that is instantiated.
   procedure Sem_Port_Connections_Identifier (Inst : Node; Def : Node)
   is
      Inst_Conn : Node;
      Last_Conn : Node;
      Conn : Node;
      Mod_Port : Node;
   begin
      Inst_Conn := Get_Connections (Inst);
      Mod_Port := Get_Ports_Chain (Def);
      Last_Conn := Null_Node;

      if Is_Ordered_Connection (Inst_Conn) then
         --  Ordered port connection
         while Inst_Conn /= Null_Node loop
            if Mod_Port = Null_Node then
               Error_Msg_Sem (+Inst_Conn, "too many connections");
               --  Update Last_Conn.
               loop
                  Last_Conn := Inst_Conn;
                  Inst_Conn := Get_Chain (Inst_Conn);
                  exit when Inst_Conn = Null_Node;
               end loop;
               exit;
            end if;

            Set_Connected_Flag (Mod_Port, True);

            Set_Port (Inst_Conn, Mod_Port);

            Sem_Port_Connection (Mod_Port, Inst_Conn);

            Last_Conn := Inst_Conn;
            Inst_Conn := Get_Chain (Inst_Conn);
            Mod_Port := Get_Chain (Mod_Port);
         end loop;
      else
         --  Connection by name.

         --  Number ports, put them in a namespace.
         Open_Name_Space;
         declare
            Port : Node;
            Oid : Obj_Id;
            Assocs : Node_Arr_Acc;
            Assoc : Node;
            First_Conn : Node;
         begin
            Oid := No_Obj_Id;
            Port := Mod_Port;
            while Port /= Null_Node loop
               pragma Assert (Get_Obj_Id (Port) = No_Obj_Id);
               case Get_Kind (Port) is
                  when N_Port =>
                     if Get_Identifier (Port) /= No_Name_Id then
                        Add_Decl (Port);
                     end if;
                  when Nkinds_Net_Port
                     | N_Interface_Port
                     | N_Modport_Port =>
                     Add_Decl (Port);
                  when others =>
                     raise Internal_Error;
               end case;
               Oid := Oid + 1;
               Set_Obj_Id (Port, Oid);
               Port := Get_Chain (Port);
            end loop;

            --  For each connection, find the corresponding named port.

            Assocs := new Node_Array'(1 .. Int32 (Oid) => Null_Node);

            while Inst_Conn /= Null_Node loop
               case Nkinds_Connection (Get_Kind (Inst_Conn)) is
                  when N_Port_Connection =>
                     declare
                        Id : constant Name_Id := Get_Identifier (Inst_Conn);
                        pragma Assert (Id /= Null_Identifier);
                     begin
                        --  Search corresponding port.
                        Port := Get_Decl_No_Import (Id);
                        if Port = Null_Node
                          or else Get_Parent (Port) /= Def
                        then
                           Error_Msg_Sem
                             (+Inst_Conn, "no port %i in module", +Inst_Conn);
                        elsif Get_Connected_Flag (Port) then
                           Error_Msg_Sem
                             (+Inst_Conn,
                              "port %i already connected", +Inst_Conn);
                        else
                           Assocs (Int32 (Get_Obj_Id (Port))) := Inst_Conn;
                           Set_Connected_Flag (Port, True);
                           Set_Port (Inst_Conn, Port);
                           Sem_Port_Connection (Port, Inst_Conn);
                        end if;
                     end;
                  when N_Wildcard_Connection =>
                     null;
                  when N_Implicit_Connection =>
                     Port := Get_Port (Inst_Conn);
                     pragma Assert (Port /= Null_Node);
                     pragma Assert (Get_Parent (Port) = Def);
                     pragma Assert (Get_Connected_Flag (Port));
                     Assocs (Int32 (Get_Obj_Id (Port))) := Inst_Conn;
                     Sem_Port_Connection (Port, Inst_Conn);
                  when N_Default_Connection =>
                     --  Not from user.
                  raise Internal_Error;
               end case;
               Last_Conn := Inst_Conn;
               Inst_Conn := Get_Chain (Inst_Conn);
            end loop;
            Close_Name_Space;

            --  Reorder conns, clear obj_id in ports.
            Port := Mod_Port;
            Init_Chain (First_Conn, Last_Conn);
            for I in Assocs'Range loop
               --  TODO: keep original order ?
               --  TODO: keep duplicate connections, wildcard connection ?
               Assoc := Assocs (I);
               if Assoc /= Null_Node then
                  --  Be sure the chain is not set.
                  Set_Chain (Assoc, Null_Node);
               else
                  --  Create an empty connection for the unassociated port
                  Assoc := Create_Node (N_Port_Connection);
                  Location_Copy (Assoc, Inst);
                  Set_Port (Assoc, Port);
               end if;
               Append_Chain (First_Conn, Last_Conn, Assoc);
               Set_Obj_Id (Port, No_Obj_Id);
               Port := Get_Chain (Port);
            end loop;
            Set_Connections (Inst, First_Conn);

            Free (Assocs);
         end;
      end if;

      declare
         First, Last : Node;
      begin
         Init_Chain (First, Last);

         Mod_Port := Get_Ports_Chain (Def);
         while Mod_Port /= Null_Node loop
            if not Get_Connected_Flag (Mod_Port)
              and then Get_Kind (Mod_Port) = N_Input
            then
               if Get_Default_Value (Mod_Port) /= Null_Node then
                  --  Append a default connection.
                  Conn := Create_Node (N_Default_Connection);
                  Location_Copy (Conn, Inst);
                  Set_Port (Conn, Mod_Port);

                  Append_Chain (First, Last, Conn);
               else
                  if not Flag_Synthesis then
                     Warning_Msg_Sem
                       (+Inst, "input port %i of module instance %i of %i is "
                          & "not connected",
                        (+Mod_Port, +Inst, +Def));
                  end if;
               end if;
            end if;
            Mod_Port := Get_Chain (Mod_Port);
         end loop;

         if First /= Null_Node then
            --  Append created ports.
            if Last_Conn = Null_Node then
               Set_Connections (Inst, First);
            else
               Set_Chain (Last_Conn, First);
            end if;
         end if;
      end;

      --  Note: connected flag is not cleared.
   end Sem_Port_Connections_Identifier;

   procedure Sem_Gate (Gate : Node)
   is
      Term : Node;
      Expr : Node;
   begin
      pragma Assert (Get_Gate_Delay (Gate) = Null_Node);
      pragma Assert (Get_Range (Gate) = Null_Node);

      Term := Get_Gate_Terminals (Gate);
      while Term /= Null_Node loop
         Expr := Get_Expression (Term);

         case Get_Kind (Term) is
            when N_Input_Terminal
              | N_Control_Terminal =>
               Expr := Sem_Expression (Expr, Null_Node);
            when N_Inout_Terminal
              | N_Output_Terminal =>
               Expr := Sem_Lvalue (Expr, Allow_Net => True);
            when others =>
               raise Internal_Error;
         end case;

         --  Check type ?
         Set_Expression (Term, Expr);

         Term := Get_Chain (Term);
      end loop;
   end Sem_Gate;

   --  1800-2017 27.4 Loop generate constructs
   procedure Sem_Loop_Generate (N : Node)
   is
      Init : constant Node := Get_For_Initialization (N);
      Blk : constant Node := Get_Generate_Block (N);
      Gen_Var : Node;
      Expr : Node;
      Asgn : Node;
      Val : Int32;
      Nblk : Node;
      Lblk : Node;
      Lblk_Id : Name_Id;
      Param : Node;
      First, Last : Node;
      Stmts : Node;
      Orig_Parent : Node;
      Num : Node;
   begin
      --  Initialization.
      case Get_Kind (Init) is
         when N_Blocking_Assign =>
            Gen_Var := Get_Lvalue (Init);
            pragma Assert (Get_Kind (Gen_Var) = N_Name);
            Gen_Var := Get_Declaration (Gen_Var);
            if Gen_Var = Null_Node then
               --  1800-2017 27.4 Loop generate constructs
               --  The loop index variable shall be declared in a genvar
               --  declaration prior to its use in a loop generate scheme.
               return;
            end if;
            if Get_Kind (Gen_Var) /= N_Genvar then
               --  1800-2017 27.4 Loop generate constructs
               --  Both the initialization and iteration assignments in the
               --  loop generate scheme shall assign to the same genvar.
               Error_Msg_Sem (+N, "%i doesn't refer to a genvar", +Gen_Var);
               return;
            end if;
         when N_Genvar =>
            Gen_Var := Init;
         when others =>
            raise Internal_Error;
      end case;

      --  1800-2017 27.4 Loop generate constructs
      --  The genvar is used as an integer during elaboration to evaluate the
      --  generate loop [...].
      --  GHDL: so assume it is an integer.
      Expr := Sem_Expression (Get_Expression (Init), Null_Node);
      Set_Expression (Init, Expr);
      Set_Expr_Type (Gen_Var, Get_Expr_Type (Expr));

      Sem_Cond_Expression (N);

      --  1800-2017 27.4 Loop generate constructs
      --  Both the initialization and iteration assignments in the loop
      --  generate scheme shall assign to the same genvar.
      Asgn := Get_Step_Assign (N);
      Sem_Statement (Asgn);
      case Get_Kind (Asgn) is
         when N_Blocking_Assign
           | Nkinds_Inc_Dec =>
            declare
               Dest : constant Node := Get_Lvalue (Asgn);
            begin
               if Get_Kind (Dest) /= N_Name then
                  Error_Msg_Sem (+Dest, "target in iteration must be genvar");
                  return;
               end if;
               if Get_Declaration (Dest) /= Gen_Var then
                  Error_Msg_Sem
                    (+Dest,
                     "iteration assignment must assign genvar %i", +Gen_Var);
                  return;
               end if;
            end;
         when others =>
            Error_Msg_Sem (+Asgn, "incorrect iteration in loop generate");
            return;
      end case;

      --  Reference to genvar is not allowed anymore after this point.
      --  Set_Type (Gen_Var, Null_Node);

      --  1800-2017 27.4 Loop generate constructs
      --  The array is considered to be declared even if the loop generate
      --  scheme resulted in no instances of the generate block.
      --
      --  Create a scope, using the name of the block (if any).
      Nblk := Create_Node (N_Array_Generate_Block);
      Set_Location (Nblk, Get_Location (N));
      Set_Parent (Nblk, Get_Parent (N));
      --  Insert.
      Set_Chain (Nblk, Get_Chain (N));
      Set_Chain (N, Nblk);

      if Get_Kind (Blk) = N_Generate_Block
        and then Get_Identifier (Blk) /= Null_Identifier
      then
         Lblk_Id := Get_Identifier (Blk);
      else
         --  TODO: create genblk identifier.
         Lblk_Id := Null_Identifier;
      end if;
      Set_Identifier (Nblk, Lblk_Id);

      --  Generate.
      pragma Assert (Get_Kind (Blk) = N_Generate_Block);
      Stmts := Get_Generate_Item_Chain (Blk);
      Orig_Parent := Blk;
      Init_Chain (First, Last);
      Val := Sem_Constant_Integer_Expression (Get_Expression (Init));
      loop
         Set_Generate_Index (Gen_Var, Val);
         exit when Sem_Constant_Integer_Expression (Get_Condition (N)) = 0;

         --  FIXME: check unique.

         --  Copy.
         Lblk := Create_Node (N_Indexed_Generate_Block);
         Set_Location (Lblk, Get_Location (Blk));
         Set_Identifier (Lblk, Lblk_Id);
         Set_Generate_Index (Lblk, Val);
         Set_Parent (Lblk, Nblk);

         Append_Chain (First, Last, Lblk);

         --  Instantiate statements (the first one being the localparam).
         Param := Verilog.Sem_Instances.Instantiate_Generate_Block
           (Stmts, Orig_Parent, Lblk);
         pragma Assert (Get_Kind (Param) = N_Localparam);
         Set_Generate_Item_Chain (Lblk, Param);

         --  Add localparam.
         Set_Param_Type (Param, Get_Expr_Type (Expr));

         Num := Create_Node (N_Number);
         Set_Location (Num, Get_Location (Gen_Var));
         Set_Expr_Type (Num, Get_Expr_Type (Expr));
         Set_Number_Lo_Val (Num, To_Uns32 (Val));
         Set_Number_Base (Num, Base_Decimal);

         Set_Expression (Param, Num);

         --  Analyze declarations.
         Verilog.Sem_Instances.Instantiate_Design (Param);
         Sem_Overrides_Chain (Param);
         Sem_Decl_Type_Chain (Param);

         --  Iterate.
         case Get_Kind (Asgn) is
            when N_Blocking_Assign =>
               Val := Sem_Constant_Integer_Expression (Get_Expression (Asgn));
            when N_Post_Increment =>
               Val := Val + 1;
            when others =>
               Error_Kind ("sem_loop_generate(step)", Asgn);
         end case;
      end loop;
      Set_Generate_Item_Chain (Nblk, First);
   end Sem_Loop_Generate;

   --  1800-2017 27.5 Conditional generate constructs
   function Sem_Conditional_Generate (N : Node; Base : Node) return Node
   is
      Blk : Node;
   begin
      --  No generate block.
      if N = Null_Node then
         return Null_Node;
      end if;

      case Get_Kind (N) is
         when N_If_Generate =>
            Sem_If_Generate (N, Base);
            --  Keep the statement.
            return N;
         when N_Case_Generate =>
            --  TODO
            raise Internal_Error;
         when N_Generate_Block =>
            --  Insert
            Blk := N;
         when others =>
            --  Create generate block.
            Blk := Create_Node (N_Generate_Block);
            Set_Location (Blk, Get_Location (N));
            Set_Generate_Item_Chain (Blk, N);
      end case;

      --  Insert the generate_block after the if_generate statement.
      Set_Parent (Blk, Get_Parent (Base));
      Set_Chain (Blk, Get_Chain (Base));
      Set_Chain (Base, Blk);

      Verilog.Sem_Instances.Instantiate_Design (Get_Generate_Item_Chain (Blk));

      --  Analyze declarations.
      Sem_Decl_Type_Chain (Get_Generate_Item_Chain (Blk));

      --  N has been moved.
      return Null_Node;
   end Sem_Conditional_Generate;

   --  1800-2017 27.5 Conditional generate constructs
   procedure Sem_If_Generate (N : Node; Base : Node)
   is
      Blk : Node;
   begin
      Sem_Cond_Expression (N);

      if Sem_Constant_Integer_Expression (Get_Condition (N)) /= 0 then
         Blk := Get_True_Block (N);
         Blk := Sem_Conditional_Generate (Blk, Base);
         Set_True_Block (N, Blk);
      else
         Blk := Get_False_Block (N);
         Blk := Sem_Conditional_Generate (Blk, Base);
         Set_False_Block (N, Blk);
      end if;
   end Sem_If_Generate;

   procedure Sem_Item (Item : Node) is
   begin
      case Get_Kind (Item) is
         when N_Module_Instance =>
            null;
         when N_Interface_Instance =>
            null;
         when N_Program_Instance =>
            null;
         when N_Parameter
           | N_Localparam =>
            null;
         when Nkinds_Net_Port =>
            Add_Decl (Item);
            Sem_Port_Declaration (Item);
         when N_Var =>
            Sem_Var (Item);
         when Nkinds_Nets =>
            Sem_Net_Declaration (Item);
         when N_Assign =>
            Sem_Continuous_Assign (Item);
         when N_Function
           | N_Task
           | N_OOB_Function
           | N_OOB_Task =>
            null;
         when N_Import_DPI_Function =>
            null;
         when N_Always
           | N_Always_Ff
           | N_Always_Comb
           | N_Always_Latch
           | N_Initial =>
            Sem_Construct (Item);
         when Nkinds_Gate =>
            Sem_Gate (Item);

         when N_Genvar =>
            null;
         when N_Generate_Region =>
            Sem_Items_Chain (Get_Generate_Item_Chain (Item));
         when N_Generate_Block =>
            Sem_Items_Chain (Get_Generate_Item_Chain (Item));
         when N_Array_Generate_Block =>
            Sem_Items_Chain (Get_Generate_Item_Chain (Item));
         when N_Indexed_Generate_Block =>
            Sem_Items_Chain (Get_Generate_Item_Chain (Item));
         when N_Loop_Generate =>
            Sem_Loop_Generate (Item);
         when N_If_Generate =>
            Sem_If_Generate (Item, Item);

         when N_Typedef =>
            null;
         when N_Typedef_Class
           | N_Typedef_Struct
           | N_Typedef_Forward =>
            null;
         when N_Class
           | N_Generic_Class =>
            null;
         when N_Export_DPI_Function =>
            --  TODO: check not imported, check declared as a function...
            null;

         when N_Assert_Property =>
            --  TODO
            null;

         when N_Package_Import =>
            null;

         when N_Analog =>
            Sem_Statement (Get_Statement (Item));

         when N_Specify =>
            --  TODO
            null;

         when others =>
            Error_Kind ("sem_item", Item);
      end case;
   end Sem_Item;

   --  Analyze ITEMS
   procedure Sem_Items_Chain (Items : Node)
   is
      Item : Node;
   begin
      Item := Items;
      while Item /= Null_Node loop
         Sem_Item (Item);

         Item := Get_Chain (Item);
      end loop;
   end Sem_Items_Chain;

   --  Second phase: analyze subroutines body.

   procedure Sem_Data_Type_Subroutines (Decl : Node)
   is
      Typ : constant Node := Get_Type_Data_Type (Decl);
   begin
      if Get_Kind (Typ) in Nkinds_Class
        and then not Get_Fully_Analyzed_Flag (Typ)
      then
         Sem_Class_Type_Methods (Typ);
      end if;
   end Sem_Data_Type_Subroutines;

   procedure Sem_Block_Items_Declaration (Items : Node)
   is
      Item : Node;
   begin
      Item := Items;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Var =>
               --  Sem_Data_Type_Subroutines (Item);
               Sem_Var (Item);
            when Nkinds_Tf_Port =>
               Add_Decl (Item);
               --  Already handled.
               null;
            when N_Typedef =>
               null;
            when others =>
               Error_Kind ("sem_block_items_declaration", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Block_Items_Declaration;

   procedure Sem_Subroutines (Item : Node) is
   begin
      case Get_Kind (Item) is
         when N_Module_Instance
           | N_Interface_Instance
           | N_Program_Instance =>
            null;
         when N_Parameter
           | N_Localparam
           | N_Input
           | N_Output
           | N_Inout =>
            null;
         when N_Specify =>
            null;
         when N_Var
           | Nkinds_Nets =>
            Sem_Data_Type_Subroutines (Item);
         when N_Function
           | N_Task =>
            Sem_Subroutine_Body (Item);
         when N_OOB_Function
           | N_OOB_Task =>
            null;
         when N_Import_DPI_Function =>
            null;
         when N_Always
           | N_Always_Ff
           | N_Always_Comb
           | N_Always_Latch
           | N_Initial
           | N_Assign =>
            null;
         when Nkinds_Gate =>
            null;
         when N_Genvar =>
            --  Skip.
            null;
         when N_Generate_Region
            | N_Array_Generate_Block
            | N_Indexed_Generate_Block
            | N_Generate_Block =>
            Sem_Subroutines_Chain (Get_Generate_Item_Chain (Item));
         when N_If_Generate
            | N_Loop_Generate =>
            null;
         when N_Analog =>
            --  No subroutines.
            null;
         when N_Typedef =>
            Sem_Data_Type_Subroutines (Item);
         when N_Class =>
            if not Get_Fully_Analyzed_Flag (Item) then
               Sem_Class_Type_Methods (Item);
            end if;
         when N_Generic_Class =>
            null;
         when Nkinds_Forward_Typedef =>
            null;
         when N_Export_DPI_Function =>
            null;
         when N_Extern_Function
           | N_Extern_Task =>
            null;

         when N_Assert_Property =>
            --  TODO
            null;

         when N_Package_Import =>
            null;
         when others =>
            Error_Kind ("sem_subroutines", Item);
      end case;
   end Sem_Subroutines;

   procedure Sem_Subroutines_Chain (Items : Node)
   is
      Item : Node;
   begin
      Item := Items;
      while Item /= Null_Node loop
         Sem_Subroutines (Item);
         Item := Get_Chain (Item);
      end loop;
   end Sem_Subroutines_Chain;

   --  IEEE 1364-2005 12.3.3 Port declarations
   --  Each port_identifier in a port_expression in the list of ports for
   --  the module declaration shall also be declared in the body of the
   --  module as one of the following port declarations: input, output, or
   --  inout (bidirectional).
   procedure Check_Module_Port_Declarations (Module : Node)
   is
      Port : Node;
      Expr : Node;
      Decl : Node;
   begin
      Port := Get_Ports_Chain (Module);
      while Port /= Null_Node loop
         pragma Assert (Get_Kind (Port) = N_Port);
         Expr := Get_Expression (Port);
         case Get_Kind (Expr) is
            when N_Name =>
               Decl := Get_Decl (Get_Identifier (Expr));
               if Decl /= Null_Node
                 and then Get_Kind (Decl) not in Nkinds_Net_Port
               then
                  Error_Msg_Sem
                    (+Port, "port %i is not declared in module body", +Expr);
               else
                  --  TODO: maybe already set ?
                  Set_Declaration (Expr, Decl);
               end if;
            when others =>
               Error_Kind ("sem_module_port_declarations", Expr);
         end case;
         Port := Get_Chain (Port);
      end loop;

      --  TODO: check that all port declaration appears in the list of port.
      --  This isn't an error (according to IEEE 1364), but worth a warning.
   end Check_Module_Port_Declarations;

   --  Third stage: recurse within instantiated modules.
   procedure Sem_Module_Chain (Items : Node)
   is
      Item : Node;
   begin
      Item := Items;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Module_Instance
              | N_Program_Instance =>
               declare
                  Sub_Module : constant Node := Get_Instance (Item);
               begin
                  Set_Parent (Sub_Module, Get_Parent (Item));

                  Sem_Upwards.Enter_Scope (Item);

                  --  Analyze the module.
                  Sem_Instantiated_Module (Sub_Module);

                  --  Analyze the connections.
                  Sem_Port_Connections_Identifier (Item, Get_Instance (Item));

                  Sem_Upwards.Leave_Scope;
               end;
            when N_Interface_Instance =>
               Sem_Upwards.Enter_Scope (Item);
               Sem_Port_Connections_Identifier (Item, Get_Instance_Ref (Item));
               Sem_Upwards.Leave_Scope;
            when N_Generate_Region
              | N_Array_Generate_Block
              | N_Indexed_Generate_Block
              | N_Generate_Block =>
               Sem_Module_Chain (Get_Generate_Item_Chain (Item));
            when N_Var
              | Nkinds_Process
              | Nkinds_Nets
              | Nkinds_Net_Port
              | Nkinds_Gate
              | Nkinds_Forward_Typedef
              | Nkinds_Tf
              | N_Typedef
              | N_Assign
              | N_Parameter
              | N_Localparam
              | N_Genvar
              | N_Loop_Generate
              | N_If_Generate
              | N_Assert_Property
              | N_Class
              | N_Analog
              | N_Specify =>
               null;
            when N_Package_Import =>
               null;
            when others =>
               Error_Kind ("sem_module_chain", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Module_Chain;

   --  The real work.
   --  Analyze a module (that is instantiated) and the whole hierarchy below
   --  it.
   procedure Sem_Instantiated_Module (Module : Node)
   is
      Items : Node;
      Item : Node;
   begin
      Items := Get_Items_Chain (Module);

      --  1800-2017 23.9 Scope rules
      --  1364-2005 12.7 Scope rules
      --  The following elements define a new scope in Verilog:
      --  - Modules
      --  [...]
      Open_Name_Space;

      --  Ports.
      --  Default values.
      if Get_Ansi_Port_Flag (Module) then
         declare
            Item : Node;
            Def : Node;
         begin
            Item := Get_Ports_Chain (Module);
            while Item /= Null_Node loop
               if Get_Kind (Item) = N_Input then
                  Def := Get_Default_Value (Item);
                  if Def /= Null_Node then
                     Def := Sem_Expression (Def, Get_Type_Data_Type (Item));
                     Set_Default_Value (Item, Def);
                  end if;
               end if;
               Item := Get_Chain (Item);
            end loop;
         end;
      end if;

      if False and Get_Ansi_Port_Flag (Module) then
         Item := Get_Ports_Chain (Module);
         while Item /= Null_Node loop
            case Nkinds_Port (Get_Kind (Item)) is
               when Nkinds_Net_Port =>
                  Sem_Port_Declaration (Item);
               when N_Interface_Port =>
                  null;
               when N_Modport_Port =>
                  null;
            end case;
            Item := Get_Chain (Item);
         end loop;
      else
         --  FIXME: check ports name are unique.
         null;
      end if;

      --  Make task and functions visibles.
      --  Add_Tf_Decls (Items);

      --  Module items.
      Sem_Items_Chain (Items);

      if not Get_Ansi_Port_Flag (Module) then
         --  Check each port expression is also declared in the module as
         --  a port.
         Check_Module_Port_Declarations (Module);
      end if;

      Close_Name_Space;

      --  Second pass: analyze subroutine bodies.

      Sem_Subroutines_Chain (Items);

      --  Third pass: recurse within instances.
      Sem_Module_Chain (Items);
   end Sem_Instantiated_Module;

   --  Deal with ports (ANSI or non-ANSI) just after parsing.
   --  For ANSI ports: put net/var declarations in the items list.
   --  For non-ANSI ports: set port identifier (if same as port expression)
   procedure Sem_Ports_List (Module : Node)
   is
      Port : Node;
      Decl : Node;
      Expr : Node;
      First, Last : Node;
   begin
      if Get_Ansi_Port_Flag (Module) then
         --  Put declared net/var to the chain of items.
         Init_Chain (First, Last);

         Port := Get_Ports_Chain (Module);
         while Port /= Null_Node loop
            --  Put net in the list of module items.
            if Get_Kind (Port) in Nkinds_Net_Port then
               Decl := Get_Redeclaration (Port);
               if Decl /= Null_Node then
                  Append_Chain (First, Last, Decl);
               end if;
            end if;

            Port := Get_Chain (Port);
         end loop;

         --  Prepend port nets/vars.
         if Last /= Null_Node then
            Set_Chain (Last, Get_Items_Chain (Module));
            Set_Items_Chain (Module, First);
         end if;
      else
         --  1800-2017 23.2.2.2 ANSI style list of port declarations
         --  When a port item is just a simple port identifier, that
         --  identifier is used as both a reference to an interface item
         --  and a port identifier.
         --
         --  GHDL: the reference in 1800-2017 is not very clear.
         --
         --  1364-2005 12.3.6 Connecting module instance ports by name
         --  The port name shall be the name specified in the module
         --  declaration. The port name cannot be a bit-select, a
         --  part-select, or a concatenation of ports.  If the module
         --  declaration was implicit, the port_expression shall be a
         --  simple identifier or escaped identifier, which shall be
         --  used as the port name.
         Port := Get_Ports_Chain (Module);
         while Port /= Null_Node loop
            if Get_Identifier (Port) = No_Name_Id then
               Expr := Get_Expression (Port);
               if Expr /= Null_Node and then Get_Kind (Expr) = N_Name then
                  Set_Identifier (Port, Get_Identifier (Expr));
               end if;
            end if;

            Port := Get_Chain (Port);
         end loop;
      end if;
      null;
   end Sem_Ports_List;

   procedure Sem_Package (Pkg : Node)
   is
      Items : constant Node := Get_Package_Item_Chain (Pkg);
   begin
      Sem_Items_Chain (Items);

      --  Second pass: analyze subroutine bodies.

      Sem_Subroutines_Chain (Items);
   end Sem_Package;

   --  Return the first parameter item from MODULE, or null_node if none.
   function Get_First_Parameter (Module : Node) return Node
   is
      Item : Node;
   begin
      Item := Get_Items_Chain (Module);
      while Item /= Null_Node loop
         if Get_Kind (Item) = N_Parameter then
            return Item;
         end if;
         Item := Get_Chain (Item);
      end loop;
      return Null_Node;
   end Get_First_Parameter;

   --  From a parameter value assignment in a module instance, overrides
   --  parameter values of the instance.
   procedure Sem_Instance_Parameter_Value_Assignment (Inst : Node)
   is
      Module : constant Node := Get_Instance (Inst);
      Param_Decl : Node;
      Param_Item : Node;
      Param_Val : Node;
   begin
      Param_Val := Get_Parameter_Values (Inst);
      if Param_Val = Null_Node then
         --  No parameter values.
         return;
      end if;

      --  Search for the first parameter.
      Param_Decl := Get_Parameter_Port_Chain (Module);
      if Param_Decl = Null_Node then
         Param_Item := Get_First_Parameter (Module);
         Param_Decl := Param_Item;
      else
         Param_Item := Null_Node;
      end if;

      if Get_Identifier (Param_Val) = Null_Identifier then
         --  1800-2017 23.10.2.1 Parameter value assignment by ordered list
         loop
            if Param_Decl = Null_Node then
               Error_Msg_Sem
                 (+Param_Val, "too many parameters value assignment");
               exit;
            end if;

            --  TODO: handle possible defparam.
            pragma Assert (Get_Override_Stmt (Param_Decl) = Null_Node);

            Set_Override_Stmt (Param_Decl, Param_Val);
            Set_Parameter (Param_Val, Param_Decl);

            Param_Val := Get_Chain (Param_Val);
            exit when Param_Val = Null_Node;

            --  Next parameter.
            if Param_Item = Null_Node then
               Param_Decl := Get_Chain (Param_Decl);
               if Param_Decl = Null_Node then
                  Param_Item := Get_First_Parameter (Module);
                  Param_Decl := Param_Item;
               end if;
            else
               loop
                  Param_Item := Get_Chain (Param_Item);
                  exit when Param_Item = Null_Node;
                  exit when Get_Kind (Param_Item) = N_Parameter;
               end loop;
               Param_Decl := Param_Item;
            end if;
         end loop;
      else
         --  1800-2017 23.10.2.2 Parameter value assignment by name
         --  TODO.
         while Param_Val /= Null_Node loop
            --  First the parameter ID.
            Param_Decl := Find_Name_In_Decls
              (Get_Parameter_Port_Chain (Module), Param_Val);
            if Param_Decl = Null_Node then
               Param_Decl := Find_Name_In_Decls
                 (Get_Items_Chain (Module), Param_Val);
            end if;

            if Param_Decl = Null_Node then
               Error_Msg_Sem
                 (+Param_Val, "no parameter named %i in module %i",
                  (+Param_Val, +Module));
            elsif Get_Kind (Param_Decl) /= N_Parameter then
               Error_Msg_Sem
                 (+Param_Val, "name %i doesn't designate a parameter",
                  +Param_Val);
            else
               if Get_Override_Stmt (Param_Decl) /= Null_Node then
                  --  TODO: handle defparam.
                  Error_Msg_Sem
                    (+Param_Val, "parameter %i already assigned", +Param_Val);
               else
                  Set_Override_Stmt (Param_Decl, Param_Val);
                  Set_Parameter (Param_Val, Param_Decl);
               end if;
            end if;
            Param_Val := Get_Chain (Param_Val);
         end loop;
      end if;
   end Sem_Instance_Parameter_Value_Assignment;

   --  Analyze parameter override (either defparam or parameter_port_list).
   procedure Sem_Overrides_Chain (Chain : Node)
   is
      Item : Node;
   begin
      Item := Chain;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Module_Instance =>
               Sem_Instance_Parameter_Value_Assignment (Item);
               Sem_Overrides_Chain (Get_Items_Chain (Get_Instance (Item)));
            when N_Defparam =>
               --  TODO.
               raise Internal_Error;
            when others =>
               null;
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Sem_Overrides_Chain;

   --  Finish analysis of class instances.
   --  A class can be instantiated almost anywhere (e.g in any expression using
   --  a scope operator or a cast operator).
   --  To avoid full walk or recursion, they are simply fully analyzed at the
   --  end.
   procedure Sem_All_Class_Instance
   is
      It : Instance_Class_Iterator;
      Cls : Node;
   begin
      Init_Instance_Class_Iterator (It);
      loop
         Cls := Get_Instance_Class_Iterator (It);
         exit when Cls = Null_Node;

         if not Get_Fully_Analyzed_Flag (Cls) then
            Sem_Class_Type_Methods (Cls);
         end if;

         Next_Instance_Class_Iterator (It);
      end loop;
   end Sem_All_Class_Instance;

   --  First analyze pass, just after parse and before elaboration.
   procedure Sem_Compilation_Unit (Source : Node)
   is
      Item : Node;
   begin
      --  1800-2017 3.12.1 Compilation units
      --  $unit is the name of the scope that encompasses a compilation unit.
      Set_Identifier (Source, Std_Names.Name_D_Unit);

      --  Resolve names as much as possible, but not hierarchical names.
      --  Also creates default nets for ports.
      Resolve_Names.Resolve_Names_Compilation_Unit (Source);

      Item := Get_Descriptions (Source);

      --  First pass: analyze type of declarations.
      Sem_Decl_Type_Chain (Item);

      --  Second pass: analyze declarations at unit level.
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Package_Import =>
               null;
            when N_Parameter
              | N_Localparam =>
               null;
            when N_Typedef =>
               Sem_Data_Type_Subroutines (Item);
            when Nkinds_Forward_Typedef =>
               null;
            when N_Package =>
               Sem_Package (Item);
            when N_Module
              | N_Interface_Declaration
              | N_Program_Declaration =>
               --  Just add redeclarations of the ports in the module.
               Sem_Ports_List (Item);
            when N_Class =>
               if not Get_Fully_Analyzed_Flag (Item) then
                  Sem_Class_Type_Methods (Item);
               end if;
            when N_Var =>
               Sem_Data_Type_Subroutines (Item);
               Sem_Var (Item);
            when N_Function
              | N_Task =>
               Sem_Subroutine_Body (Item);
            when N_OOB_Function
              | N_OOB_Task =>
               null;
            when N_Discipline
              | N_Nature =>
               null;
            when N_Primitive =>
               --  TODO
               null;
            when N_Generic_Class =>
               null;
            when others =>
               Error_Kind ("sem_compilation_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      Sem_All_Class_Instance;
   end Sem_Compilation_Unit;

   procedure Sem_Design (Root : Node)
   is
      Items : constant Node := Get_Items_Chain (Root);
   begin
      --  Start resolution of hierarchical names: build the root scope.
      Sem_Upwards.Init (Root);

      --  Apply parameter assignments and overrides.
      Sem_Overrides_Chain (Items);

      --  Analyze type of declarations.
      --  This is done before analyzing expressions, do that any declaration
      --  can be referenced by hierarchical names in expressions.
      Sem_Decl_Type_Chain (Items);

      --  Make wildcard connections explicit.
      --  This cannot be done earlier as the modules are resolved only
      --  during elaboration (modules are defined in any order, config may
      --  change the association).
      Resolve_Names.Resolve_Wildcard_Connections_Chain (Items);

      --  Finish analysis: expressions and statements.
      --  FIXME: explain how generates are handled.
      Sem_Module_Chain (Items);
   end Sem_Design;
end Verilog.Sem;
