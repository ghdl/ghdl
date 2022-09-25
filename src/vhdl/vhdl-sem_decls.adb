--  Semantic analysis.
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
with Errorout; use Errorout;
with Types; use Types;
with Std_Names;
with Vhdl.Tokens;
with Flags; use Flags;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Sem; use Vhdl.Sem;
with Vhdl.Sem_Utils; use Vhdl.Sem_Utils;
with Vhdl.Sem_Expr; use Vhdl.Sem_Expr;
with Vhdl.Sem_Scopes; use Vhdl.Sem_Scopes;
with Vhdl.Sem_Names; use Vhdl.Sem_Names;
with Vhdl.Sem_Specs; use Vhdl.Sem_Specs;
with Vhdl.Sem_Types; use Vhdl.Sem_Types;
with Vhdl.Sem_Psl;
with Vhdl.Sem_Inst;
with Vhdl.Xrefs; use Vhdl.Xrefs;

package body Vhdl.Sem_Decls is
   --  Region that can declare signals.  Used to add implicit declarations.
   Current_Signals_Region : Implicit_Declaration_Type :=
     (Null_Iir, Null_Iir, Null_Iir, False, Null_Iir);

   procedure Push_Signals_Declarative_Part
     (Cell: out Implicit_Declaration_Type; Decls_Parent : Iir) is
   begin
      Cell := Current_Signals_Region;
      Current_Signals_Region :=
        (Decls_Parent, Null_Iir, Null_Iir, False, Null_Iir);
   end Push_Signals_Declarative_Part;

   procedure Pop_Signals_Declarative_Part
     (Cell: in Implicit_Declaration_Type) is
   begin
      Current_Signals_Region := Cell;
   end Pop_Signals_Declarative_Part;

   --  Insert the implicit signal declaration after LAST_DECL.
   procedure Insert_Implicit_Declaration (Last_Decl : Iir) is
   begin
      if Last_Decl = Null_Iir then
         Set_Declaration_Chain (Current_Signals_Region.Decls_Parent,
                                Current_Signals_Region.Implicit_Decl);
      else
         Set_Chain (Last_Decl, Current_Signals_Region.Implicit_Decl);
      end if;
   end Insert_Implicit_Declaration;

   --  Add Attr as an implicit declaration in the current region.
   procedure Add_Implicit_Declaration (Attr : Iir)
   is
      Decl : Iir;
   begin
      --  There must be a declarative part for implicit signals.
      pragma Assert (Current_Signals_Region.Decls_Parent /= Null_Iir);

      --  Attr_Chain must be empty.
      pragma Assert (Get_Attr_Chain (Attr) = Null_Iir);

      if Current_Signals_Region.Implicit_Decl = Null_Iir then
         --  Create the signal_attribute_declaration to hold all the implicit
         --  signals.
         Decl := Create_Iir (Iir_Kind_Attribute_Implicit_Declaration);
         Location_Copy (Decl, Attr);
         Set_Parent (Decl, Current_Signals_Region.Decls_Parent);

         --  Save the implicit declaration.
         Current_Signals_Region.Implicit_Decl := Decl;

         --  Append SIG (this is the first one).
         Set_Attribute_Implicit_Chain (Decl, Attr);

         if Current_Signals_Region.Decls_Analyzed then
            --  Declarative region was completely analyzed.  Just append DECL
            --  at the end of declarations.
            Insert_Implicit_Declaration (Current_Signals_Region.Last_Decl);
            Current_Signals_Region.Last_Decl :=
              Current_Signals_Region.Implicit_Decl;
         end if;
      else
         --  Append SIG.
         Set_Attr_Chain (Current_Signals_Region.Last_Attribute, Attr);
      end if;
      Current_Signals_Region.Last_Attribute := Attr;

      Set_Attribute_Implicit_Declaration
        (Attr, Current_Signals_Region.Implicit_Decl);
   end Add_Implicit_Declaration;

   --  Insert pending implicit declarations after the last analyzed LAST_DECL,
   --  and update it.  Then the caller has to insert the declaration which
   --  created the implicit declarations.
   procedure Insert_Pending_Implicit_Declarations
     (Parent : Iir; Last_Decl : in out Iir) is
   begin
      if Current_Signals_Region.Decls_Parent = Parent
        and then Current_Signals_Region.Implicit_Decl /= Null_Iir
      then
         --  There are pending implicit declarations.  Can happen only
         --  during analysis of declarations, therefore when declarations are
         --  not fully analyzed.
         pragma Assert (not Current_Signals_Region.Decls_Analyzed);

         --  Add pending implicit declarations before the current one.
         Insert_Implicit_Declaration (Last_Decl);
         Last_Decl := Current_Signals_Region.Implicit_Decl;

         --  Detach the implicit declaration.
         Current_Signals_Region.Implicit_Decl := Null_Iir;
         Current_Signals_Region.Last_Attribute := Null_Iir;
      end if;
   end Insert_Pending_Implicit_Declarations;

   --  Mark the end of declaration analysis.  New implicit declarations will
   --  simply be appended to the last declaration.
   procedure End_Of_Declarations_For_Implicit_Declarations
     (Parent : Iir; Last_Decl : Iir) is
   begin
      if Current_Signals_Region.Decls_Parent = Parent then
         pragma Assert (not Current_Signals_Region.Decls_Analyzed);

         --  All declarations have been analyzed, new implicit declarations
         --  will be appended.
         Current_Signals_Region.Decls_Analyzed := True;
         Current_Signals_Region.Last_Decl := Last_Decl;
      end if;
   end End_Of_Declarations_For_Implicit_Declarations;

   procedure Mark_Subprogram_Used (Subprg : Iir)
   is
      N : Iir;
   begin
      N := Subprg;
      loop
         exit when Get_Use_Flag (N);
         Set_Use_Flag (N, True);
         N := Sem_Inst.Get_Origin (N);
         --  The origin may also be an instance.
         exit when N = Null_Iir;
      end loop;
   end Mark_Subprogram_Used;

   --  Emit an error if the type of DECL is a file type, access type,
   --  protected type or if a subelement of DECL is an access type.
   procedure Check_Signal_Type (Decl : Iir)
   is
      Decl_Type : constant Iir := Get_Type (Decl);
   begin
      if Get_Signal_Type_Flag (Decl_Type) then
         return;
      end if;

      if Is_Error (Decl_Type) then
         return;
      end if;

      Error_Msg_Sem (+Decl, "type of %n cannot be %n", (+Decl, +Decl_Type));
      case Get_Kind (Decl_Type) is
         when Iir_Kind_File_Type_Definition =>
            null;
         when Iir_Kind_Protected_Type_Declaration =>
            null;
         when Iir_Kind_Interface_Type_Definition =>
            null;
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            null;
         when Iir_Kinds_Array_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            Error_Msg_Sem (+Decl, "(%n has an access subelement)", +Decl_Type);
         when others =>
            Error_Kind ("check_signal_type", Decl_Type);
      end case;
   end Check_Signal_Type;

   procedure Check_Nature_Type (Decl : Iir)
   is
      Decl_Type : constant Iir := Get_Type (Decl);
   begin
      if not Is_Nature_Type (Decl_Type) then
         Error_Msg_Sem (+Decl, "type of %n must only have float", +Decl);
      end if;
   end Check_Nature_Type;

   procedure Sem_Interface_Object_Declaration
     (Inter, Last : Iir; Interface_Kind : Interface_Kind_Type)
   is
      A_Type: Iir;
      Default_Value: Iir;
   begin
      --  Avoid the reanalysed duplicated types.
      --  This is not an optimization, since the unanalysed type must have
      --  been freed.
      A_Type := Get_Subtype_Indication (Inter);
      if A_Type = Null_Iir then
         if Last = Null_Iir or else not Get_Has_Identifier_List (Last) then
            --  Subtype indication was not parsed.
            A_Type := Create_Error_Type (Null_Iir);
            Set_Subtype_Indication (Inter, A_Type);
         else
            pragma Assert (Get_Is_Ref (Inter));
            A_Type := Get_Type (Last);
            Default_Value := Get_Default_Value (Last);
            Set_Subtype_Indication (Inter, Get_Subtype_Indication (Last));
         end if;
      else
         A_Type := Sem_Subtype_Indication (A_Type);
         Set_Subtype_Indication (Inter, A_Type);
         A_Type := Get_Type_Of_Subtype_Indication (A_Type);
         Set_Type (Inter, A_Type);

         Default_Value := Get_Default_Value (Inter);
         if Default_Value /= Null_Iir and then not Is_Error (A_Type) then
            Deferred_Constant_Allowed := True;
            Default_Value := Sem_Expression_Wildcard
              (Default_Value, A_Type, Is_Object_Fully_Constrained (Inter));
            Default_Value :=
              Eval_Expr_Check_If_Static (Default_Value, A_Type);
            Deferred_Constant_Allowed := False;
            Check_Read (Default_Value);
         end if;
      end if;

      Set_Name_Staticness (Inter, Locally);
      Xref_Decl (Inter);

      if not Is_Error (A_Type) then
         Set_Type (Inter, A_Type);

         if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
            if Get_Guarded_Signal_Flag (Inter) then
               case Get_Signal_Kind (Inter) is
                  when Iir_Bus_Kind =>
                     --  LRM93 4.3.1.2
                     --  It is also an error if a guarded signal of a scalar
                     --  type is neither a resolved signal nor a subelement of
                     --  a resolved signal.
                     if not Get_Resolved_Flag (A_Type) then
                        Error_Msg_Sem
                          (+Inter, "%n of guarded %n is not resolved",
                           (+A_Type, +Inter));
                     end if;

                     --  LRM 2.1.1.2  Signal parameter
                     --  It is an error if the declaration of a formal signal
                     --  parameter includes the reserved word BUS.
                     if Flags.Vhdl_Std >= Vhdl_93
                       and then Interface_Kind in Parameter_Interface_List
                       and then not Flags.Flag_Relaxed_Rules
                     then
                        Error_Msg_Sem
                          (+Inter, "signal parameter can't be of kind bus");
                     end if;
                  when Iir_Register_Kind =>
                     --  LRM93 4.3.2 Interface declarations
                     --  Grammar for interface_signal_declaration.
                     Error_Msg_Sem
                       (+Inter, "interface signal can't be of kind register");
               end case;
            end if;
            Set_Type_Has_Signal (A_Type);
         end if;

         case Get_Kind (Inter) is
            when Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_Signal_Declaration =>
               --  LRM 4.3.2  Interface declarations
               --  For an interface constant declaration or an interface
               --  signal declaration, the subtype indication must define
               --  a subtype that is neither a file type, an access type,
               --  nor a protected type.  Moreover, the subtype indication
               --  must not denote a composite type with a subelement that
               --  is a file type, an access type, or a protected type.
               Check_Signal_Type (Inter);
            when Iir_Kind_Interface_Variable_Declaration =>
               case Get_Kind (Get_Base_Type (A_Type)) is
                  when Iir_Kind_File_Type_Definition =>
                     if Flags.Vhdl_Std >= Vhdl_93
                       and then not Flags.Flag_Relaxed_Rules
                     then
                        Error_Msg_Sem
                          (+Inter,
                           "variable formal can't be a file (vhdl 93)");
                     end if;
                  when Iir_Kind_Protected_Type_Declaration =>
                     --  LRM 2.1.1.1  Constant and variable parameters
                     --  It is an error if the mode of the parameter is
                     --  other that INOUT.
                     if Get_Mode (Inter) /= Iir_Inout_Mode then
                        Error_Msg_Sem
                          (+Inter,
                           "parameter of protected type must be inout");
                     end if;
                  when others =>
                     null;
               end case;
            when Iir_Kind_Interface_File_Declaration =>
               if Get_Kind (Get_Base_Type (A_Type))
                 /= Iir_Kind_File_Type_Definition
               then
                  Error_Msg_Sem
                    (+Inter, "file formal type must be a file type");
               end if;
            when Iir_Kind_Interface_Quantity_Declaration =>
               Check_Nature_Type (Inter);
            when others =>
               --  Inter is not an interface.
               raise Internal_Error;
         end case;

         if Default_Value /= Null_Iir then
            Set_Default_Value (Inter, Default_Value);

            --  LRM 4.3.2  Interface declarations.
            --  It is an error if a default expression appears in an
            --  interface declaration and any of the following conditions
            --  hold:
            --   -  The mode is linkage
            --   -  The interface object is a formal signal parameter
            --   -  The interface object is a formal variable parameter of
            --      mode other than in
            --   -  The subtype indication of the interface declaration
            --      denotes a protected type.
            case Get_Kind (Inter) is
               when Iir_Kind_Interface_Constant_Declaration =>
                  null;
               when Iir_Kind_Interface_Signal_Declaration =>
                  if Get_Mode (Inter) = Iir_Linkage_Mode then
                     Error_Msg_Sem
                       (+Inter,
                        "default expression not allowed for linkage port");
                  elsif Interface_Kind in Parameter_Interface_List then
                     Error_Msg_Sem
                       (+Inter,
                        "default expression not allowed for signal parameter");
                  end if;
               when Iir_Kind_Interface_Variable_Declaration =>
                  if Get_Mode (Inter) /= Iir_In_Mode then
                     Error_Msg_Sem
                       (+Inter, "default expression not allowed for"
                          & " out or inout variable parameter");
                  elsif Get_Kind (A_Type) = Iir_Kind_Protected_Type_Declaration
                  then
                     Error_Msg_Sem
                       (+Inter, "default expression not allowed for"
                          & " variable parameter of protected type");
                  end if;
               when Iir_Kind_Interface_File_Declaration =>
                  raise Internal_Error;
               when others =>
                  null;
            end case;
         end if;
      else
         Set_Type (Inter, Error_Type);
      end if;

      Sem_Scopes.Add_Name (Inter);

      --  By default, interface are not static.
      --  This may be changed just below.
      Set_Expr_Staticness (Inter, None);

      case Interface_Kind is
         when Generic_Interface_List =>
            --  LRM93 1.1.1
            --  The generic list in the formal generic clause defines
            --  generic constants whose values may be determined by the
            --  environment.
            if Get_Kind (Inter) /= Iir_Kind_Interface_Constant_Declaration then
               Error_Msg_Sem (+Inter, "generic %n must be a constant", +Inter);
            else
               --   LRM93 7.4.2 (Globally static primaries)
               --   3. a generic constant.
               Set_Expr_Staticness (Inter, Globally);
            end if;
         when Port_Interface_List =>
            case Get_Kind (Inter) is
               when Iir_Kind_Interface_Signal_Declaration
                 | Iir_Kind_Interface_Terminal_Declaration
                 | Iir_Kind_Interface_Quantity_Declaration =>
                  null;
               when others =>
                  if AMS_Vhdl then
                     Error_Msg_Sem
                       (+Inter,
                        "port %n must be a signal, a terminal or a quantity",
                        +Inter);
                  else
                     Error_Msg_Sem
                       (+Inter, "port %n must be a signal", +Inter);
                  end if;
            end case;
         when Parameter_Interface_List =>
            if Get_Kind (Inter) = Iir_Kind_Interface_Variable_Declaration
              and then Interface_Kind = Function_Parameter_Interface_List
              and then (
                Vhdl_Std < Vhdl_19
                or else Get_Pure_Flag (Get_Parent (Inter))
              )
            then
               Error_Msg_Sem (+Inter, "variable interface parameter are not "
                                & "allowed for a function (use a constant)");
            end if;

            --  By default, we suppose a subprogram read the activity of
            --  a signal.
            --  This will be adjusted when the body is analyzed.
            if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration
              and then Get_Mode (Inter) in Iir_In_Modes
            then
               Set_Has_Active_Flag (Inter, True);
            end if;

            case Get_Mode (Inter) is
               when Iir_Unknown_Mode =>
                  raise Internal_Error;
               when Iir_In_Mode =>
                  null;
               when Iir_Inout_Mode
                 | Iir_Out_Mode =>
                  if Interface_Kind = Function_Parameter_Interface_List
                    and then
                    Get_Kind (Inter) /= Iir_Kind_Interface_File_Declaration
                  then
                     if Vhdl_Std < Vhdl_19 then
                        Error_Msg_Sem
                          (+Inter,
                           "mode of a function parameter cannot " &
                           "be inout or out");
                     elsif Get_Pure_Flag (Get_Parent (Inter)) then
                        Error_Msg_Sem
                          (+Inter,
                           "mode of a pure function's parameter cannot " &
                           "be inout or out");
                     end if;
                  end if;
               when Iir_Buffer_Mode
                 | Iir_Linkage_Mode =>
                  Error_Msg_Sem
                    (+Inter, "buffer or linkage mode is not allowed "
                       & "for a subprogram parameter");
            end case;
      end case;
   end Sem_Interface_Object_Declaration;

   procedure Sem_Interface_Terminal_Declaration (Inter, Last : Iir)
   is
      Nature: Iir;
   begin
      --  Avoid the reanalysed duplicated natures.
      Nature := Get_Subnature_Indication (Inter);
      if Nature = Null_Iir then
         if Last = Null_Iir or else not Get_Has_Identifier_List (Last) then
            --  Subnature indication was not parsed.
            Nature := Create_Error (Null_Iir);
            Set_Subtype_Indication (Inter, Nature);
         else
            Nature := Get_Nature (Last);
         end if;
      else
         Nature := Sem_Subnature_Indication (Nature);
         Set_Subnature_Indication (Inter, Nature);
         Nature := Get_Nature_Of_Subnature_Indication (Nature);
      end if;

      Set_Name_Staticness (Inter, Locally);
      Xref_Decl (Inter);

      Set_Nature (Inter, Nature);

      Sem_Scopes.Add_Name (Inter);
   end Sem_Interface_Terminal_Declaration;

   procedure Sem_Interface_Package_Declaration (Inter : Iir)
   is
      Pkg : Iir;
   begin
      --  LRM08 6.5.5 Interface package declarations
      --  the uninstantiated_package_name shall denote an uninstantiated
      --  package declared in a package declaration.
      Pkg := Sem_Uninstantiated_Package_Name (Inter);
      if Pkg = Null_Iir or else Is_Error (Pkg) then
         return;
      end if;

      if Get_Is_Within_Flag (Pkg) then
         --  Looks obvious, but there is apparently no such rule in the LRM.
         --  Catch error like:
         --    package gen is
         --      generic(package g2 is new gen generic map(<>));
         --    end;
         Error_Msg_Sem (+Inter, "generic package formal cannot be itself");
         return;
      end if;

      if Get_Generic_Map_Aspect_Chain (Inter) /= Null_Iir then
         Sem_Generic_Association_Chain (Get_Package_Header (Pkg), Inter);
         --  Not yet fully supported - need to check the instance.
      end if;

      Sem_Inst.Instantiate_Package_Declaration (Inter, Pkg);

      Sem_Scopes.Add_Name (Inter);
      Set_Is_Within_Flag (Inter, True);
      Xref_Decl (Inter);
   end Sem_Interface_Package_Declaration;

   function Create_Implicit_Interface_Function (Name : Name_Id;
                                                Decl : Iir;
                                                Interface_Chain : Iir;
                                                Return_Type : Iir)
                                               return Iir
   is
      Operation : Iir_Function_Declaration;
   begin
      Operation := Create_Iir (Iir_Kind_Interface_Function_Declaration);
      Location_Copy (Operation, Decl);
      Set_Parent (Operation, Get_Parent (Decl));
      Set_Interface_Declaration_Chain (Operation, Interface_Chain);
      Set_Return_Type (Operation, Return_Type);
      Set_Identifier (Operation, Name);
      Set_Visible_Flag (Operation, True);
      Set_Pure_Flag (Operation, True);
      Compute_Subprogram_Hash (Operation);
      return Operation;
   end Create_Implicit_Interface_Function;

   procedure Sem_Interface_Type_Declaration (Inter : Iir)
   is
      Def : Iir;
      Finters : Iir;
      Op_Eq, Op_Neq : Iir;
   begin
      --  Create type definition.  The definition is owned by the declaration.
      Def := Create_Iir (Iir_Kind_Interface_Type_Definition);
      Set_Location (Def, Get_Location (Inter));
      Set_Type_Declarator (Def, Inter);
      Set_Type (Inter, Def);
      Set_Interface_Type_Definition (Inter, Def);
      Set_Type_Staticness (Def, None);
      Set_Resolved_Flag (Def, False);
      Set_Signal_Type_Flag (Def, True);
      Set_Has_Signal_Flag (Def, False);

      --  Create operations for the interface type.
      Finters := Create_Anonymous_Interface (Def);
      Set_Chain (Finters, Create_Anonymous_Interface (Def));

      Op_Eq := Create_Implicit_Interface_Function
        (Std_Names.Name_Op_Equality,
         Inter, Finters, Std_Package.Boolean_Type_Definition);

      Op_Neq := Create_Implicit_Interface_Function
        (Std_Names.Name_Op_Inequality,
         Inter, Finters, Std_Package.Boolean_Type_Definition);

      Set_Interface_Type_Subprograms (Inter, Op_Eq);
      Set_Chain (Op_Eq, Op_Neq);

      Sem_Scopes.Add_Name (Inter);
      Sem_Scopes.Add_Name (Op_Eq);
      Sem_Scopes.Add_Name (Op_Neq);
      Xref_Decl (Inter);
   end Sem_Interface_Type_Declaration;

   procedure Sem_Interface_Subprogram_Declaration (Inter : Iir) is
   begin
      Sem_Subprogram_Specification (Inter);
      Sem_Scopes.Add_Name (Inter);
      Xref_Decl (Inter);
   end Sem_Interface_Subprogram_Declaration;

   procedure Sem_Interface_Chain (Interface_Chain: Iir;
                                  Interface_Kind : Interface_Kind_Type)
   is
      --  Control visibility of interface object.  See below for its use.
      Immediately_Visible : constant Boolean :=
        Interface_Kind = Generic_Interface_List
        and then Flags.Vhdl_Std >= Vhdl_08;

      Inter : Iir;

      --  LAST is the last interface declaration that has a type.  This is
      --  used to set type and default value for the following declarations
      --  that appeared in a list of identifiers.
      Last : Iir;
   begin
      Last := Null_Iir;

      Inter := Interface_Chain;
      while Inter /= Null_Iir loop
         case Iir_Kinds_Interface_Declaration (Get_Kind (Inter)) is
            when Iir_Kinds_Interface_Object_Declaration =>
               Sem_Interface_Object_Declaration (Inter, Last, Interface_Kind);
               Last := Inter;
            when Iir_Kind_Interface_Terminal_Declaration =>
               Sem_Interface_Terminal_Declaration (Inter, Last);
               Last := Inter;
            when Iir_Kind_Interface_Package_Declaration =>
               Sem_Interface_Package_Declaration (Inter);
            when Iir_Kind_Interface_Type_Declaration =>
               Sem_Interface_Type_Declaration (Inter);
            when Iir_Kinds_Interface_Subprogram_Declaration =>
               Sem_Interface_Subprogram_Declaration (Inter);
         end case;

         --  LRM08 6.5.6 Interface lists
         --  A name that denotes an interface object declared in a port
         --  interface list of a prameter interface list shall not appear in
         --  any interface declaration within the interface list containing the
         --  denoted interface object expect to declare this object.
         --  A name that denotes an interface declaration in a generic
         --  interface list may appear in an interface declaration within the
         --  interface list containing the denoted interface declaration.
         if Immediately_Visible then
            Name_Visible (Inter);
         end if;

         Inter := Get_Chain (Inter);
      end loop;

      --  LRM 10.3  Visibility
      --  A declaration is visible only within a certain part of its scope;
      --  this starts at the end of the declaration [...]

      --  LRM 4.3.2.1  Interface List
      --  A name that denotes an interface object must not appear in any
      --  interface declaration within the interface list containing the
      --  denotes interface except to declare this object.

      --  GHDL: this is achieved by making the interface object visible after
      --   having analyzed the interface list.
      if not Immediately_Visible then
         Inter := Interface_Chain;
         while Inter /= Null_Iir loop
            Name_Visible (Inter);
            Inter := Get_Chain (Inter);
         end loop;
      end if;
   end Sem_Interface_Chain;

   --  Analyze a type or an anonymous type declaration.
   procedure Sem_Type_Declaration (Decl: Iir; Is_Global : Boolean)
   is
      Def: Iir;
      Inter : Name_Interpretation_Type;
      Old_Decl : Iir;
      St_Decl : Iir_Subtype_Declaration;
      Bt_Def : Iir;
   begin
      --  Check if DECL complete a previous incomplete type declaration.
      Inter := Get_Interpretation (Get_Identifier (Decl));
      if Valid_Interpretation (Inter)
        and then Is_In_Current_Declarative_Region (Inter)
      then
         Old_Decl := Get_Declaration (Inter);
         if Get_Kind (Old_Decl) /= Iir_Kind_Type_Declaration
           or else (Get_Kind (Get_Type_Definition (Old_Decl)) /=
                      Iir_Kind_Incomplete_Type_Definition)
         then
            Old_Decl := Null_Iir;
         else
            Set_Incomplete_Type_Declaration (Decl, Old_Decl);
         end if;
      else
         Old_Decl := Null_Iir;
      end if;

      if Old_Decl = Null_Iir then
         if Get_Kind (Decl) = Iir_Kind_Type_Declaration then
            --  This is necessary at least for enumeration type definition.
            --  Type declaration for anonymous types don't have name, only
            --  their subtype have names.  Those are added later.
            Sem_Scopes.Add_Name (Decl);
         end if;
      else
         --  This is a way to prevent:
         --    type a;
         --    type a is access a;
         --  which is non-sense.
         Set_Visible_Flag (Old_Decl, False);
      end if;

      -- Check the definition of the type.
      Def := Get_Type_Definition (Decl);
      if Def = Null_Iir then
         --  Incomplete type declaration
         Def := Create_Iir (Iir_Kind_Incomplete_Type_Definition);
         Location_Copy (Def, Decl);
         Set_Type_Definition (Decl, Def);
         Set_Signal_Type_Flag (Def, True);
         Set_Type_Declarator (Def, Decl);
         Set_Visible_Flag (Decl, True);
         Xref_Decl (Decl);

         return;

      end if;

      --  A complete type declaration.
      if Old_Decl = Null_Iir then
         Xref_Decl (Decl);
      else
         Xref_Body (Decl, Old_Decl);
      end if;

      Def := Sem_Type_Definition (Def, Decl);
      if Def = Null_Iir then
         return;
      end if;

      case Get_Kind (Def) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            --  Some type declaration are in fact subtype declarations.
            St_Decl := Create_Iir (Iir_Kind_Subtype_Declaration);
            Location_Copy (St_Decl, Decl);
            Set_Identifier (St_Decl, Get_Identifier (Decl));
            Set_Parent (St_Decl, Get_Parent (Decl));
            Set_Type (St_Decl, Def);
            Set_Subtype_Indication (St_Decl, Def);
            Set_Type_Declarator (Def, St_Decl);
            Set_Chain (St_Decl, Get_Chain (Decl));
            Set_Chain (Decl, St_Decl);

            --  The type declaration declares the base type.
            Bt_Def := Get_Base_Type (Def);
            Set_Type_Definition (Decl, Bt_Def);
            Set_Type_Declarator (Bt_Def, Decl);
            Set_Subtype_Definition (Decl, Def);

            if Old_Decl = Null_Iir then
               Sem_Scopes.Add_Name (St_Decl);
            end if;

            Sem_Scopes.Name_Visible (St_Decl);

            --  The implicit subprogram will be added in the
            -- scope just after.
            Create_Implicit_Operations (Decl, False);

         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_File_Type_Definition =>
            St_Decl := Null_Iir;
            Set_Type_Declarator (Def, Decl);

            Sem_Scopes.Name_Visible (Decl);

            --  The implicit subprogram will be added in the
            -- scope just after.
            Create_Implicit_Operations (Decl, False);

         when Iir_Kind_Protected_Type_Declaration =>
            Set_Type_Declarator (Def, Decl);
            St_Decl := Null_Iir;
            --  No implicit subprograms.

         when others =>
            Error_Kind ("sem_type_declaration", Def);
      end case;

      if Old_Decl /= Null_Iir then
         --  Complete the type definition.
         declare
            Old_Def : constant Iir := Get_Type_Definition (Old_Decl);
            Ref : Iir;
         begin
            Set_Signal_Type_Flag (Old_Def, Get_Signal_Type_Flag (Def));
            Ref := Get_Incomplete_Type_Ref_Chain (Old_Def);
            while Is_Valid (Ref) loop
               pragma Assert
                 (Get_Kind (Ref) = Iir_Kind_Access_Type_Definition);
               Check_Access_Type_Restrictions (Ref, Def);
               Ref := Get_Incomplete_Type_Ref_Chain (Ref);
            end loop;
            Set_Complete_Type_Definition (Old_Def, Def);

            --  The identifier now designates the complete type declaration.
            if St_Decl = Null_Iir then
               Replace_Name (Get_Identifier (Decl), Old_Decl, Decl);
            else
               Replace_Name (Get_Identifier (Decl), Old_Decl, St_Decl);
            end if;
         end;
      end if;

      if Is_Global then
         Set_Type_Has_Signal (Def);
      end if;
   end Sem_Type_Declaration;

   procedure Sem_Subtype_Declaration (Decl: Iir; Is_Global : Boolean)
   is
      Def: Iir;
      Ind : Iir;
   begin
      --  Real hack to skip subtype declarations of anonymous type decls.
      if Get_Visible_Flag (Decl) then
         return;
      end if;

      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      --  Analyze the definition of the type.
      Ind := Get_Subtype_Indication (Decl);
      Ind := Sem_Subtype_Indication (Ind);
      Set_Subtype_Indication (Decl, Ind);
      Def := Get_Type_Of_Subtype_Indication (Ind);
      if Def = Null_Iir or else Is_Error (Def) then
         return;
      end if;

      if not Is_Anonymous_Type_Definition (Def)
        and then Get_Kind (Def) /= Iir_Kind_Protected_Type_Declaration
      then
         --  There is no added constraints and therefore the subtype
         --  declaration is in fact an alias of the type.  Create a copy so
         --  that it has its own type declarator.
         --  (Except for protected types).
         Def := Copy_Subtype_Indication (Def);
         Location_Copy (Def, Decl);
         Set_Subtype_Type_Mark (Def, Ind);
         Set_Subtype_Indication (Decl, Def);
      end if;

      Set_Type (Decl, Def);
      Set_Type_Declarator (Def, Decl);
      Name_Visible (Decl);
      if Is_Global then
         Set_Type_Has_Signal (Def);
      end if;
   end Sem_Subtype_Declaration;

   --  If DECL is a constant declaration, and there is already a incomplete
   --  constant declaration in the current scope with the same name, then
   --  return it. Otherwise, return NULL.
   function Get_Deferred_Constant (Decl : Iir) return Iir
   is
      Deferred_Const : Iir;
      Interp : Name_Interpretation_Type;
   begin
      if Get_Kind (Decl) /= Iir_Kind_Constant_Declaration then
         return Null_Iir;
      end if;
      Interp := Get_Interpretation (Get_Identifier (Decl));
      if not Valid_Interpretation (Interp) then
         return Null_Iir;
      end if;

      if not Is_In_Current_Declarative_Region (Interp)
        or else Is_Potentially_Visible (Interp)
      then
         --  Deferred and full declarations must be declared in the same
         --  declarative region.
         return Null_Iir;
      end if;

      Deferred_Const := Get_Declaration (Interp);
      if Get_Kind (Deferred_Const) /= Iir_Kind_Constant_Declaration then
         return Null_Iir;
      end if;
      if not Get_Deferred_Declaration_Flag (Deferred_Const) then
         --  Just a 'normal' duplicate declaration
         return Null_Iir;
      end if;
      --  LRM93 4.3.1.1
      --  The corresponding full constant declaration, which defines the value
      --  of the constant, must appear in the body of the package.
      if Get_Kind (Get_Library_Unit (Get_Current_Design_Unit))
        /= Iir_Kind_Package_Body
      then
         Error_Msg_Sem
           (+Decl, "full constant declaration must appear in package body");
      end if;
      return Deferred_Const;
   end Get_Deferred_Constant;

   procedure Sem_Object_Type_From_Value (Decl : Iir; Value : Iir)
   is
      Atype : constant Iir := Get_Type (Decl);
      Value_Type : constant Iir := Get_Type (Value);
   begin
      if not Is_Fully_Constrained_Type (Atype)
        and then not Is_Error (Value_Type)
      then
         if Get_Type_Staticness (Value_Type) >= Globally then
            Set_Type (Decl, Value_Type);
         end if;
      end if;
   end Sem_Object_Type_From_Value;

   --  LAST_DECL is set only if DECL is part of a list of declarations (they
   --  share the same type and the same default value).
   procedure Sem_Object_Declaration (Decl: Iir; Last_Decl : Iir)
   is
      Deferred_Const : constant Iir := Get_Deferred_Constant (Decl);
      Atype: Iir;
      Default_Value : Iir;
      Staticness : Iir_Staticness;
   begin
      --  LRM08 12.2 Scope of declarations
      --  Then scope of a declaration [...] extends from the beginning of the
      --  declaration [...]
      if Deferred_Const = Null_Iir then
         Sem_Scopes.Add_Name (Decl);
         Xref_Decl (Decl);
      else
         Xref_Ref (Decl, Deferred_Const);
      end if;

      --  Analyze type and default value:
      Atype := Get_Subtype_Indication (Decl);
      if Atype /= Null_Iir then
         Atype := Sem_Subtype_Indication (Atype);
         Set_Subtype_Indication (Decl, Atype);
         Atype := Get_Type_Of_Subtype_Indication (Atype);
         if Atype = Null_Iir then
            Atype := Create_Error_Type (Get_Type (Decl));
         end if;
         Set_Type (Decl, Atype);

         Default_Value := Get_Default_Value (Decl);
         if Default_Value /= Null_Iir then
            Default_Value := Sem_Expression_Wildcard
              (Default_Value, Atype, Is_Object_Fully_Constrained (Decl));
            if Default_Value = Null_Iir then
               Default_Value :=
                 Create_Error_Expr (Get_Default_Value (Decl), Atype);
            end if;
            Check_Read (Default_Value);
            Default_Value := Eval_Expr_Check_If_Static (Default_Value, Atype);
         end if;
      else
         pragma Assert (Get_Kind (Last_Decl) = Get_Kind (Decl));
         pragma Assert (Get_Has_Identifier_List (Last_Decl)
                          or Flag_Force_Analysis);
         Set_Is_Ref (Decl, True);
         Default_Value := Get_Default_Value (Last_Decl);
         Atype := Get_Subtype_Indication (Last_Decl);
         Set_Subtype_Indication (Decl, Atype);
         Atype := Get_Type (Last_Decl);
         Set_Type (Decl, Atype);
      end if;

      Set_Default_Value (Decl, Default_Value);
      Set_Name_Staticness (Decl, Locally);
      Set_Visible_Flag (Decl, True);

      --  LRM93 2.6
      --  The subtype indication given in the full declaration of the deferred
      --  constant must conform to that given in the deferred constant
      --  declaration.
      if Deferred_Const /= Null_Iir
        and then not Are_Trees_Equal (Get_Type (Decl),
                                      Get_Type (Deferred_Const))
      then
         Error_Msg_Sem
           (+Decl,
            "subtype indication doesn't conform with the deferred constant");
      end if;

      --  LRM93 4.3.1.3
      --  It is an error if a variable declaration declares a variable that is
      --  of a file type.
      --
      --  LRM93 4.3.1.1
      --  It is an error if a constant declaration declares a constant that is
      --  of a file type, or an access type, or a composite type which has
      --  subelement that is a file type of an access type.
      --
      --  LRM93 4.3.1.2
      --  It is an error if a signal declaration declares a signal that is of
      --  a file type [or an access type].
      case Get_Kind (Atype) is
         when Iir_Kind_File_Type_Definition =>
            Error_Msg_Sem (+Decl, "%n cannot be of type file", +Decl);
         when Iir_Kind_Error =>
            null;
         when others =>
            if Get_Kind (Decl) /= Iir_Kind_Variable_Declaration then
               Check_Signal_Type (Decl);
            end if;
      end case;

      if Is_Valid (Default_Value)
        and then not Eval_Is_In_Bound (Default_Value, Atype)
        and then Get_Kind (Default_Value) /= Iir_Kind_Overflow_Literal
      then
         Warning_Msg_Sem
           (Warnid_Runtime_Error, +Decl,
            "default value constraints don't match object type ones");
         Default_Value := Build_Overflow (Default_Value, Atype);
         Set_Default_Value (Decl, Default_Value);
      end if;

      case Get_Kind (Decl) is
         when Iir_Kind_Constant_Declaration =>
            --  LRM93 4.3.1.1
            --  If the assignment symbol ":=" followed by an expression is not
            --  present in a constant declaration, then the declaration
            --  declares a deferred constant.
            --  Such a constant declaration may only appear in a package
            --  declaration.
            if Deferred_Const /= Null_Iir then
               Set_Deferred_Declaration (Decl, Deferred_Const);
               Set_Deferred_Declaration (Deferred_Const, Decl);
            end if;
            if Default_Value = Null_Iir then
               if Deferred_Const /= Null_Iir then
                  Error_Msg_Sem
                    (+Decl,
                     "full constant declaration must have a default value");
               else
                  Set_Deferred_Declaration_Flag (Decl, True);
               end if;
               if Get_Kind (Get_Parent (Decl)) /= Iir_Kind_Package_Declaration
               then
                  Error_Msg_Sem
                    (+Decl, "a constant must have a default value");
               end if;
               Set_Expr_Staticness (Decl, Globally);
            else
               --  LRM93 7.4.1: a locally static primary is defined:
               --  A constant (other than deferred constant) explicitly
               --  declared by a constant declaration and initialized
               --  with a locally static expression.
               --  Note: the staticness of the full declaration may be locally.
               if False and Deferred_Const /= Null_Iir then
                  --  This is a deferred constant.
                  Staticness := Globally;
               else
                  Staticness := Min (Get_Expr_Staticness (Default_Value),
                                     Get_Type_Staticness (Atype));
                  --  What about expr staticness of c in:
                  --    constant c : bit_vector (a to b) := "01";
                  --  where a and b are not locally static ?
                  --Staticness := Get_Expr_Staticness (Default_Value);

                  --  LRM 7.4.2 (Globally static primaries)
                  --  5. a constant
                  if Staticness < Globally then
                     Staticness := Globally;
                  end if;
               end if;
               Set_Expr_Staticness (Decl, Staticness);
            end if;

         when Iir_Kind_Signal_Declaration =>
            --  LRM93 4.3.1.2
            --  It is also an error if a guarded signal of a scalar type is
            --  neither a resolved signal nor a subelement of a resolved
            --  signal.
            if Get_Guarded_Signal_Flag (Decl)
              and then not Get_Resolved_Flag (Atype)
            then
               Error_Msg_Sem (+Decl, "guarded %n must be resolved", +Decl);
            end if;
            Set_Expr_Staticness (Decl, None);
            Set_Has_Disconnect_Flag (Decl, False);
            Set_Type_Has_Signal (Atype);

         when Iir_Kind_Variable_Declaration =>
            --  GHDL: restriction for shared variables are checked during
            --  parse.
            if Flags.Vhdl_Std >= Vhdl_00 then
               declare
                  Parent : constant Iir := Get_Parent (Decl);
                  Base_Type : constant Iir := Get_Base_Type (Atype);
                  Is_Protected : constant Boolean :=
                    Get_Kind (Base_Type) = Iir_Kind_Protected_Type_Declaration;
               begin
                  --  LRM00 4.3.1.3
                  --  The base type of the subtype indication of a
                  --  shared variable declaration must be a protected type.
                  if Get_Shared_Flag (Decl) and not Is_Protected then
                     Error_Msg_Sem_Relaxed
                       (Decl, Warnid_Shared,
                        "type of a shared variable must be a protected type");
                  end if;

                  --  LRM00 4.3.1.3  Variable declarations
                  --  If a given variable appears (directly or indirectly)
                  --  within a protected type body, then the base type
                  --  denoted by the subtype indication of the variable
                  --  declarations must not be a protected type defined by
                  --  the protected type body.
                  --  FIXME: indirectly ?
                  if Is_Protected
                    and then Get_Kind (Parent) = Iir_Kind_Protected_Type_Body
                    and then Base_Type
                    = Get_Protected_Type_Declaration (Parent)
                  then
                     Error_Msg_Sem (+Decl, "variable type must not be of the "
                                      & "protected type body");
                  end if;
               end;
            end if;
            Set_Expr_Staticness (Decl, None);
         when Iir_Kind_Free_Quantity_Declaration =>
            Check_Nature_Type (Decl);
            Set_Expr_Staticness (Decl, None);
         when others =>
            Error_Kind ("sem_object_declaration", Decl);
      end case;

      case Get_Kind (Decl) is
         when Iir_Kind_Constant_Declaration =>
            --  LRM93 3.2.1.1
            --  For a constant declared by an object declaration, the index
            --  ranges are defined by the initial value, if the subtype of the
            --  constant is unconstrained; otherwise they are defined by this
            --  subtype.
            if Default_Value /= Null_Iir then
               Sem_Object_Type_From_Value (Decl, Default_Value);
            end if;

         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Free_Quantity_Declaration =>
            --  LRM93 3.2.1.1 / LRM08 5.3.2.2
            --  For a variable or signal declared by an object declaration, the
            --  subtype indication of the corresponding object declaration
            --  must define a constrained array subtype.
            declare
               Ind : constant Iir := Get_Subtype_Indication (Decl);
            begin
               if not (Is_Valid (Ind)
                         and then Kind_In (Ind, Iir_Kind_Subtype_Attribute,
                                           Iir_Kind_Element_Attribute))
                 and then not Is_Fully_Constrained_Type (Atype)
               then
                  Report_Start_Group;
                  Error_Msg_Sem
                    (+Decl,
                     "declaration of %n with unconstrained %n is not allowed",
                     (+Decl, +Atype));
                  if Default_Value /= Null_Iir then
                     Error_Msg_Sem (+Decl, "(even with a default value)");
                  end if;
                  Report_End_Group;
               end if;
            end;

         when others =>
            Error_Kind ("sem_object_declaration(2)", Decl);
      end case;
   end Sem_Object_Declaration;

   procedure Sem_File_Declaration (Decl: Iir_File_Declaration; Last_Decl : Iir)
   is
      Atype: Iir;
      Logical_Name: Iir;
      Open_Kind : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Set_Expr_Staticness (Decl, None);
      Xref_Decl (Decl);

      -- Try to find a type.
      Atype := Get_Subtype_Indication (Decl);
      if Atype /= Null_Iir then
         Atype := Sem_Subtype_Indication (Atype);
         Set_Subtype_Indication (Decl, Atype);
         Atype := Get_Type_Of_Subtype_Indication (Atype);
         if Atype = Null_Iir then
            Atype := Create_Error_Type (Get_Type (Decl));
         end if;
      else
         Set_Is_Ref (Decl, True);
         Atype := Get_Subtype_Indication (Last_Decl);
         Set_Subtype_Indication (Decl, Atype);
         Atype := Get_Type (Last_Decl);
      end if;
      Set_Type (Decl, Atype);

      --  LRM93 4.3.1.4
      --  The subtype indication of a file declaration must define a file
      --  subtype.
      if Get_Kind (Atype) /= Iir_Kind_File_Type_Definition then
         Error_Msg_Sem (+Decl, "file subtype expected for a file declaration");
         return;
      end if;

      Logical_Name := Get_File_Logical_Name (Decl);
      --  LRM93 4.3.1.4
      --  The file logical name must be an expression of predefined type
      --  STRING.
      if Logical_Name /= Null_Iir then
         Logical_Name := Sem_Expression (Logical_Name, String_Type_Definition);
         if Logical_Name /= Null_Iir then
            Check_Read (Logical_Name);
            Set_File_Logical_Name (Decl, Logical_Name);
         end if;
      end if;

      Open_Kind := Get_File_Open_Kind (Decl);
      if Open_Kind /= Null_Iir then
         Open_Kind :=
           Sem_Expression (Open_Kind, File_Open_Kind_Type_Definition);
         if Open_Kind /= Null_Iir then
            Check_Read (Open_Kind);
            Set_File_Open_Kind (Decl, Open_Kind);
         end if;
      else
         --  LRM93 4.3.1.4
         --  If a file open kind expression is not included in the file open
         --  information of a given file declaration, then the default value
         --  of READ_MODE is used during elaboration of the file declaration.
         --
         --  LRM87 4.3.1.4
         --  The default mode is IN, if no mode is specified.
         if Get_Mode (Decl) = Iir_Unknown_Mode then
            if Flags.Vhdl_Std = Vhdl_87 then
               Set_Mode (Decl, Iir_In_Mode);
            else
               null;
               --  Set_File_Open_Kind (Decl, File_Open_Kind_Read_Mode);
            end if;
         end if;
      end if;
      Name_Visible (Decl);

      --  LRM 93 2.2
      --  If a pure function is the parent of a given procedure, then
      --  that procedure must not contain a reference to an explicitly
      --  declared file object [...]
      --
      --  A pure function must not contain a reference to an explicitly
      --  declared file.

      --  Note: this check is also performed when a file is referenced.
      --    But a file can be declared without being explicitly referenced.
      declare
         Parent : constant Iir := Get_Parent (Decl);
         Spec : Iir;
      begin
         case Get_Kind (Parent) is
            when Iir_Kind_Function_Body =>
               Spec := Get_Subprogram_Specification (Parent);
               if Get_Pure_Flag (Spec) then
                  Error_Msg_Sem_Relaxed
                    (Decl, Warnid_Pure,
                     "cannot declare a file in a pure function");
               end if;
            when Iir_Kind_Procedure_Body =>
               Spec := Get_Subprogram_Specification (Parent);
               Set_Purity_State (Spec, Impure);
               Set_Impure_Depth (Parent, Iir_Depth_Impure);
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Error_Kind ("sem_file_declaration", Parent);
            when others =>
               null;
         end case;
      end;
   end Sem_File_Declaration;

   procedure Sem_Source_Quantity_Declaration (Decl : Iir; Last_Decl : Iir)
   is
      Atype: Iir;
      Expr : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Set_Expr_Staticness (Decl, None);
      Xref_Decl (Decl);

      -- Try to find a type.
      Atype := Get_Subtype_Indication (Decl);
      if Atype /= Null_Iir then
         Atype := Sem_Subtype_Indication (Atype);
         Set_Subtype_Indication (Decl, Atype);
         Atype := Get_Type_Of_Subtype_Indication (Atype);
         if Atype = Null_Iir then
            Atype := Create_Error_Type (Get_Type (Decl));
         elsif not Is_Nature_Type (Atype) then
            Error_Msg_Sem
              (+Decl, "type of %n must only have float types", +Decl);
         end if;
      else
         Atype := Get_Type (Last_Decl);
      end if;
      Set_Type (Decl, Atype);

      --  AMS-LRM17 6.4.2.7 Quantity declarations
      --  The type of the magnitude simple expression, phase simple expression,
      --  and power simple expression in a source aspect shall be that of the
      --  source quantity.
      case Iir_Kinds_Source_Quantity_Declaration (Get_Kind (Decl)) is
         when Iir_Kind_Spectrum_Quantity_Declaration =>
            Expr := Get_Magnitude_Expression (Decl);
            if Expr /= Null_Iir then
               Expr := Sem_Expression (Expr, Atype);
               Set_Magnitude_Expression (Decl, Expr);
            end if;
            Expr := Get_Phase_Expression (Decl);
            if Expr /= Null_Iir then
               Expr := Sem_Expression (Expr, Atype);
               Set_Phase_Expression (Decl, Expr);
            end if;
         when Iir_Kind_Noise_Quantity_Declaration =>
            Expr := Get_Power_Expression (Decl);
            if Expr /= Null_Iir then
               Expr := Sem_Expression (Expr, Atype);
               Set_Power_Expression (Decl, Expr);
            end if;
      end case;

      --  TODO: It is an error if the name of a source quantity appears in an
      --  expression in a source aspect.

      Name_Visible (Decl);
   end Sem_Source_Quantity_Declaration;

   procedure Sem_Attribute_Declaration (Decl: Iir_Attribute_Declaration)
   is
      A_Type : Iir;
      Ident : Name_Id;
   begin
      --  LRM93 4.4
      --  The identifier is said to be the designator of the attribute.
      Ident := Get_Identifier (Decl);
      if Ident in Std_Names.Name_Id_Attributes
        or else (Flags.Vhdl_Std = Vhdl_87
                 and then Ident in Std_Names.Name_Id_Vhdl87_Attributes)
        or else (Flags.Vhdl_Std > Vhdl_87
                 and then Ident in Std_Names.Name_Id_Vhdl93_Attributes)
      then
         Error_Msg_Sem (+Decl, "predefined attribute %i overriden", +Decl);
      end if;
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      A_Type := Get_Type_Mark (Decl);
      if A_Type /= Null_Iir then
         A_Type := Sem_Type_Mark (A_Type);
         Set_Type_Mark (Decl, A_Type);
         A_Type := Get_Type (A_Type);
      else
         A_Type := Create_Error_Type (Decl);
      end if;

      Set_Type (Decl, A_Type);

      --  LRM93 4.4  Attribute declarations.
      --  It is an error if the type mark denotes an access type, a file type,
      --  a protected type, or a composite type with a subelement that is
      --  an access type, a file type, or a protected type.
      --  The subtype need not be constrained.
      Check_Signal_Type (Decl);
      Name_Visible (Decl);
   end Sem_Attribute_Declaration;

   procedure Sem_Component_Declaration (Component: Iir_Component_Declaration)
   is
   begin
      Sem_Scopes.Add_Name (Component);
      Xref_Decl (Component);

      --  LRM 10.1 Declarative region
      --  6. A component declaration.
      Open_Declarative_Region;

      Sem_Interface_Chain
        (Get_Generic_Chain (Component), Generic_Interface_List);
      Sem_Interface_Chain
        (Get_Port_Chain (Component), Port_Interface_List);

      Close_Declarative_Region;

      Name_Visible (Component);
   end Sem_Component_Declaration;

   procedure Sem_Object_Alias_Declaration (Alias: Iir_Object_Alias_Declaration)
   is
      N_Name: constant Iir := Get_Name (Alias);
      N_Type: Iir;
      Name_Type : Iir;
   begin
      --  LRM93 4.3.3.1 Object Aliases.
      --  1. A signature may not appear in a declaration of an object alias.
      -- FIXME: todo.
      --
      --  2. The name must be a static name that denotes an object.
      if Get_Name_Staticness (N_Name) < Globally then
         Error_Msg_Sem (+Alias, "aliased name must be a static name");
      end if;

      --  LRM93 4.3.3.1
      --  The base type of the name specified in an alias declaration must be
      --  the same as the base type of the type mark in the subtype indication
      --  (if the subtype indication is present);
      Name_Type := Get_Type (N_Name);
      N_Type := Get_Subtype_Indication (Alias);
      if N_Type = Null_Iir then
         Set_Type (Alias, Name_Type);
         N_Type := Name_Type;
      else
         --  FIXME: must be analyzed before calling Name_Visibility.
         N_Type := Sem_Subtype_Indication (N_Type);
         Set_Subtype_Indication (Alias, N_Type);
         N_Type := Get_Type_Of_Subtype_Indication (N_Type);
         if N_Type /= Null_Iir then
            Set_Type (Alias, N_Type);
            if Get_Base_Type (N_Type) /= Get_Base_Type (Name_Type) then
               Error_Msg_Sem
                 (+Alias, "base type of aliased name and name mismatch");
            end if;
         end if;

         --  LRM08 6.6.2 Object aliases
         --  The following rules apply yo object aliases:
         --  b) If the name is an external name, a subtype indication shall not
         --     appear in the alias declaration.
         if Get_Kind (N_Name) in Iir_Kinds_External_Name then
            Error_Msg_Sem
              (+Alias,
               "subtype indication not allowed in alias of external name");
         end if;
      end if;

      --  LRM93 4.3.3.1
      --  This type must not be a multi-dimensional array type.
      if Get_Kind (N_Type) in Iir_Kinds_Array_Type_Definition then
         if not Is_One_Dimensional_Array_Type (N_Type) then
            Error_Msg_Sem
              (+Alias,
               "aliased name must not be a multi-dimensional array type");
         end if;
         if Get_Type_Staticness (N_Type) = Locally
           and then Get_Type_Staticness (Name_Type) = Locally
           and then Eval_Discrete_Type_Length
           (Get_Nth_Element (Get_Index_Subtype_List (N_Type), 0))
           /= Eval_Discrete_Type_Length
           (Get_Nth_Element (Get_Index_Subtype_List (Name_Type), 0))
         then
            Error_Msg_Sem
              (+Alias, "number of elements not matching in type and name");
         end if;
      end if;

      Set_Name_Staticness (Alias, Get_Name_Staticness (N_Name));
      Set_Expr_Staticness (Alias, Get_Expr_Staticness (N_Name));
      if Is_Signal_Object (N_Name) then
         Set_Type_Has_Signal (N_Type);
      end if;
   end Sem_Object_Alias_Declaration;

   function Signature_Match (N_Entity : Iir; Sig : Iir_Signature)
                            return Boolean
   is
      List : constant Iir_Flist := Get_Type_Marks_List (Sig);
      Inter : Iir;
      El : Iir;
   begin
      case Get_Kind (N_Entity) is
         when Iir_Kind_Enumeration_Literal =>
            --  LRM93 2.3.2  Signatures
            --  * Similarly, a signature is said to match the parameter and
            --    result type profile of a given enumeration literal if
            --    the signature matches the parameter and result type profile
            --    of the subprogram equivalent to the enumeration literal,
            --    defined in Section 3.1.1
            if Get_Return_Type_Mark (Sig) = Null_Iir then
               return False;
            end if;
            return List = Null_Iir_Flist
              and then (Get_Type (N_Entity)
                          = Get_Type (Get_Return_Type_Mark (Sig)));
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            --  LRM93 2.3.2  Signatures
            --  * if the reserved word RETURN is present, the subprogram is
            --    a function and the base type of the type mark following
            --    the reserved word in the signature is the same as the base
            --    type of the return type of the function, [...]
            if Get_Return_Type_Mark (Sig) = Null_Iir then
               return False;
            end if;
            if Get_Type (Get_Return_Type_Mark (Sig)) /=
              Get_Base_Type (Get_Return_Type (N_Entity))
            then
               return False;
            end if;
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            --  LRM93 2.3.2  Signatures
            --  * [...] or the reserved word RETURN is absent and the
            --    subprogram is a procedure.
            if Get_Return_Type_Mark (Sig) /= Null_Iir then
               return False;
            end if;
         when others =>
            --  LRM93 2.3.2  Signatures
            --  A signature distinguishes between overloaded subprograms and
            --  overloaded enumeration literals based on their parameter
            --  and result type profiles.
            return False;
      end case;

      --  LRM93 2.3.2  Signature
      --  * the number of type marks prior the reserved word RETURN, if any,
      --    matches the number of formal parameters of the subprogram;
      --  * at each parameter position, the base type denoted by the type
      --    mark of the signature is the same as the base type of the
      --    corresponding formal parameter of the subprogram; [and finally, ]
      Inter := Get_Interface_Declaration_Chain (N_Entity);
      if List = Null_Iir_Flist then
         return Inter = Null_Iir;
      end if;
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if Inter = Null_Iir then
            --  More type marks in the signature than in the interface.
            return False;
         end if;
         if Get_Base_Type (Get_Type (Inter)) /= Get_Type (El) then
            return False;
         end if;
         Inter := Get_Chain (Inter);
      end loop;
      --  Match only if the number of type marks is the same.
      return Inter = Null_Iir;
   end Signature_Match;

   --  Extract from NAME the named entity whose profile matches with SIG.
   function Sem_Signature (Name : Iir; Sig : Iir_Signature) return Iir
   is
      List : constant Iir_Flist := Get_Type_Marks_List (Sig);
      Res : Iir;
      El : Iir;
      Error : Boolean;
      Ov_List : Iir_List;
      Ov_It : List_Iterator;
   begin
      --  Sem signature.
      if List /= Null_Iir_Flist then
         for I in Flist_First .. Flist_Last (List) loop
            El := Get_Nth_Element (List, I);
            El := Sem_Type_Mark (El);
            Set_Nth_Element (List, I, El);

            --  Reuse the Type field of the name for the base type.  This is
            --  a deviation from the use of Type in a name, but restricted to
            --  analysis of signatures.
            Set_Type (El, Get_Base_Type (Get_Type (El)));
         end loop;
      end if;
      El := Get_Return_Type_Mark (Sig);
      if El /= Null_Iir then
         El := Sem_Type_Mark (El);
         Set_Return_Type_Mark (Sig, El);
         --  Likewise.
         Set_Type (El, Get_Base_Type (Get_Type (El)));
      end if;

      --  FIXME: what to do in case of error ?
      Res := Null_Iir;
      Error := False;
      if Is_Overload_List (Name) then
         Ov_List := Get_Overload_List (Name);
         Ov_It := List_Iterate (Ov_List);
         while Is_Valid (Ov_It) loop
            El := Get_Element (Ov_It);
            if Signature_Match (El, Sig) then
               if Res = Null_Iir then
                  Res := El;
               else
                  Error := True;
                  Report_Start_Group;
                  Error_Msg_Sem
                    (+Sig,
                     "cannot resolve signature, many matching subprograms:");
                  Error_Msg_Sem (+Res, "found: %n", +Res);
               end if;
               if Error then
                  Error_Msg_Sem (+El, "found: %n", +El);
               end if;
            end if;
            Next (Ov_It);
         end loop;
         if Error then
            Report_End_Group;
         end if;

         --  Free the overload list (with a workaround as only variables can
         --  be free).
         declare
            Name_Ov : Iir;
         begin
            Name_Ov := Name;
            Free_Overload_List (Name_Ov);
         end;
      else
         if Signature_Match (Name, Sig) then
            Res := Name;
         end if;
      end if;

      if Error then
         return Null_Iir;
      end if;
      if Res = Null_Iir then
         Error_Msg_Sem
           (+Sig, "cannot resolve signature, no matching subprogram");
      end if;

      return Res;
   end Sem_Signature;

   --  Create implicit aliases for an alias ALIAS of a type or of a subtype.
   procedure Add_Aliases_For_Type_Alias (Alias : Iir)
   is
      N_Entity : constant Iir := Get_Named_Entity (Get_Name (Alias));
      Def : constant Iir := Get_Base_Type (Get_Type (N_Entity));
      Type_Decl : constant Iir := Get_Type_Declarator (Def);
      Last : Iir;
      El : Iir;
      Enum_List : Iir_Flist;

      --  Append an implicit alias
      procedure Add_Implicit_Alias (Decl : Iir)
      is
         N_Alias : constant Iir_Non_Object_Alias_Declaration :=
           Create_Iir (Iir_Kind_Non_Object_Alias_Declaration);
         N_Name : constant Iir := Create_Iir (Iir_Kind_Simple_Name);
      begin
         --  Create the name (can be in fact a character literal or a symbol
         --  operator).
         Location_Copy (N_Name, Alias);
         Set_Identifier (N_Name, Get_Identifier (Decl));
         Set_Named_Entity (N_Name, Decl);

         Location_Copy (N_Alias, Alias);
         Set_Identifier (N_Alias, Get_Identifier (Decl));
         Set_Name (N_Alias, N_Name);
         Set_Parent (N_Alias, Get_Parent (Alias));
         Set_Implicit_Alias_Flag (N_Alias, True);

         Sem_Scopes.Add_Name (N_Alias);
         Set_Visible_Flag (N_Alias, True);

         --  Append in the declaration chain.
         Set_Chain (N_Alias, Get_Chain (Last));
         Set_Chain (Last, N_Alias);
         Last := N_Alias;
      end Add_Implicit_Alias;
   begin
      Last := Alias;

      if Get_Kind (Def) = Iir_Kind_Enumeration_Type_Definition then
         --  LRM93 4.3.3.2  Non-Object Aliases
         --  3.  If the name denotes an enumeration type, then one
         --      implicit alias declaration for each of the
         --      literals of the type immediatly follows the alias
         --      declaration for the enumeration type; [...]
         --
         --  LRM08 6.6.3 Nonobject aliases
         --  c)  If the name denotes an enumeration type or a subtype of an
         --      enumeration type, then one implicit alias declaration for each
         --      of the literals of the base type immediately follows the
         --      alias declaration for the enumeration type; [...]
         Enum_List := Get_Enumeration_Literal_List (Def);
         for I in Flist_First .. Flist_Last (Enum_List) loop
            El := Get_Nth_Element (Enum_List, I);
            --  LRM93 4.3.3.2  Non-Object Aliases
            --      [...] each such implicit declaration has, as its alias
            --      designator, the simple name or character literal of the
            --      literal, and has, as its name, a name constructed by taking
            --      the name of the alias for the enumeration type and
            --      substituting the simple name or character literal being
            --      aliased for the simple name of the type.  Each implicit
            --      alias has a signature that matches the parameter and result
            --      type profile of the literal being aliased.
            --
            --  LRM08 6.6.3 Nonobject aliases
            --      [...] each such implicit declaration has, as its alias
            --      designator, the simple name or character literal of the
            --      literal and has, as its name, a name constructed by taking
            --      the name of the alias for the enumeration type or subtype
            --      and substituing the simple name or character literal being
            --      aliased for the simple name of the type or subtype.  Each
            --      implicit alias has a signature that matches the parameter
            --      and result type profile of the literal being aliased.
            Add_Implicit_Alias (El);
         end loop;
      end if;

      --  LRM93 4.3.3.2  Non-Object Aliases
      --  4.  Alternatively, if the name denotes a physical type
      --      [...]
      --  GHDL: this is not possible, since a physical type is
      --  anonymous (LRM93 is buggy on this point).
      --
      --  LRM08 6.6.3 Nonobject aliases
      --  d)  Alternatively, if the name denotes a subtype of a physical type,
      --      [...]
      if Get_Kind (Def) = Iir_Kind_Physical_Type_Definition then
         --  LRM08 6.3.3 Nonobject aliases
         --      [...] then one implicit alias declaration for each of the
         --      units of the base type immediately follows the alias
         --      declaration for the physical type; each such implicit
         --      declaration has, as its alias designator, the simple name of
         --      the unit and has, as its name, a name constructed by taking
         --      the name of the alias for the subtype of the physical type
         --      and substituting the simple name of the unit being aliased for
         --      the simple name of the subtype.
         El := Get_Unit_Chain (Def);
         while El /= Null_Iir loop
            Add_Implicit_Alias (El);
            El := Get_Chain (El);
         end loop;
      end if;

      --  LRM93 4.3.3.2  Non-Object Aliases
      --  5.  Finally, if the name denotes a type, then implicit
      --      alias declarations for each predefined operator
      --      for the type immediatly follow the explicit alias
      --      declaration for the type, and if present, any
      --      implicit alias declarations for literals or units
      --      of the type.
      --      Each implicit alias has a signature that matches the
      --      parameter and result type profule of the implicit
      --      operator being aliased.
      --
      --  LRM08 6.6.3 Nonobject aliases
      --  e)  Finally, if the name denotes a type of a subtype, then implicit
      --      alias declarations for each predefined operation for the type
      --      immediately follow the explicit alias declaration for the type or
      --      subtype and, if present, any implicit alias declarations for
      --      literals or units of the type.  Each implicit alias has a
      --      signature that matches the parameter and result type profile of
      --      the implicit operation being aliased.
      El := Get_Chain (Type_Decl);
      while El /= Null_Iir loop
         if Is_Implicit_Subprogram (El)
           and then Is_Operation_For_Type (El, Def)
         then
            Add_Implicit_Alias (El);
            El := Get_Chain (El);
         else
            exit;
         end if;
      end loop;
   end Add_Aliases_For_Type_Alias;

   procedure Sem_Non_Object_Alias_Declaration
     (Alias : Iir_Non_Object_Alias_Declaration)
   is
      use Std_Names;
      N_Entity : constant Iir := Get_Named_Entity (Get_Name (Alias));
      Id : Name_Id;
   begin
      case Get_Kind (N_Entity) is
         when Iir_Kinds_Subprogram_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration =>
            --  LRM93 4.3.3.2  Non-Object Aliases
            --  2.  A signature is required if the name denotes a subprogram
            --      (including an operator) or enumeration literal.
            if Get_Alias_Signature (Alias) = Null_Iir then
               Error_Msg_Sem (+Alias, "signature required for subprogram");
            end if;
         when Iir_Kind_Enumeration_Literal =>
            if Get_Alias_Signature (Alias) = Null_Iir then
               Error_Msg_Sem
                 (+Alias, "signature required for enumeration literal");
            end if;
         when Iir_Kind_Type_Declaration =>
            Add_Aliases_For_Type_Alias (Alias);
         when Iir_Kind_Subtype_Declaration =>
            --  LRM08 6.6.3 Nonobject aliases
            --  ... or a subtype ...
            if Flags.Vhdl_Std >= Vhdl_08 then
               Add_Aliases_For_Type_Alias (Alias);
            end if;
         when Iir_Kind_Nature_Declaration =>
            null;
         when Iir_Kinds_Object_Declaration =>
            raise Internal_Error;
         when Iir_Kind_Attribute_Declaration
           | Iir_Kind_Component_Declaration =>
            null;
         when Iir_Kind_Terminal_Declaration =>
            --  TODO: should have Sem_Terminal_Alias_Declaration.
            null;
         when Iir_Kind_Library_Declaration =>
            --  Not explicitly allowed before vhdl-08.
            null;
         when Iir_Kind_Base_Attribute =>
            Error_Msg_Sem (+Alias, "base attribute not allowed in alias");
            return;
         when others =>
            Error_Kind ("sem_non_object_alias_declaration", N_Entity);
      end case;

      Id := Get_Identifier (Alias);

      case Id is
         when Name_Characters =>
            --  LRM 4.3.3  Alias declarations
            --  If the alias designator is a character literal, the
            --  name must denote an enumeration literal.
            if Get_Kind (N_Entity) /= Iir_Kind_Enumeration_Literal then
               Error_Msg_Sem
                 (+Alias,
                  "alias of a character must denote an enumeration literal");
               return;
            end if;
         when Name_Id_Operators
           | Name_Shift_Operators
           | Name_Word_Operators =>
            --  LRM 4.3.3  Alias declarations
            --  If the alias designator is an operator symbol, the
            --  name must denote a function, and that function then
            --  overloads the operator symbol.  In this latter case,
            --  the operator symbol and the function both must meet the
            --  requirements of 2.3.1.
            if Get_Kind (N_Entity) /= Iir_Kind_Function_Declaration then
               Error_Msg_Sem
                 (+Alias, "alias of an operator must denote a function");
               return;
            end if;
            Check_Operator_Requirements (Id, N_Entity);
         when others =>
            null;
      end case;
   end Sem_Non_Object_Alias_Declaration;

   function Sem_Alias_Declaration (Alias : Iir) return Iir
   is
      Name : Iir;
      Sig : Iir_Signature;
      N_Entity : Iir;
      Res : Iir;
   begin
      Xref_Decl (Alias);

      Name := Get_Name (Alias);
      case Get_Kind (Name) is
         when Iir_Kind_Signature =>
            Sig := Name;
            Name := Get_Signature_Prefix (Sig);
            Sem_Name (Name);
            Set_Signature_Prefix (Sig, Name);
         when Iir_Kind_String_Literal8 =>
            --  Try to have a good error message.
            if Get_Subtype_Indication (Alias) = Null_Iir then
               Error_Msg_Sem (+Name, "signature required for operature name");
            else
               Error_Msg_Sem (+Name, "object name required");
            end if;
            Name := Create_Error_Name (Name);
            Set_Name (Alias, Name);
            return Alias;
         when Iir_Kind_Error =>
            pragma Assert (Flags.Flag_Force_Analysis);
            return Alias;
         when others =>
            Sem_Name (Name);
            Sig := Null_Iir;
      end case;

      N_Entity := Get_Named_Entity (Name);
      if N_Entity = Error_Mark then
         return Alias;
      end if;

      if Is_Overload_List (N_Entity) then
         if Sig = Null_Iir then
            Error_Msg_Sem
              (+Alias, "signature required for alias of a subprogram");
            return Alias;
         end if;
      end if;

      if Sig /= Null_Iir then
         N_Entity := Sem_Signature (N_Entity, Sig);
      end if;
      if N_Entity = Null_Iir then
         return Alias;
      end if;

      Set_Named_Entity (Name, N_Entity);
      Name := Finish_Sem_Name (Name);
      Set_Name (Alias, Name);

      if Is_Object_Name (N_Entity) then
         --  Object alias declaration.

         Sem_Scopes.Add_Name (Alias);
         Name_Visible (Alias);

         if Sig /= Null_Iir then
            Error_Msg_Sem (+Sig, "signature not allowed for object alias");
         end if;
         Sem_Object_Alias_Declaration (Alias);
         return Alias;
      else
         --  Non object alias declaration.

         if Get_Subtype_Indication (Alias) /= Null_Iir then
            Error_Msg_Sem
              (+Alias,
               "subtype indication shall not appear in a nonobject alias");
         end if;

         Res := Create_Iir (Iir_Kind_Non_Object_Alias_Declaration);
         Location_Copy (Res, Alias);
         Set_Parent (Res, Get_Parent (Alias));
         Set_Chain (Res, Get_Chain (Alias));
         Set_Identifier (Res, Get_Identifier (Alias));
         Set_Name (Res, Get_Name (Alias));
         Set_Alias_Signature (Res, Sig);

         if Is_Valid (Sig) then
            --  The prefix is owned by the non_object_alias_declaration.
            Set_Signature_Prefix (Sig, Null_Iir);
         end if;

         Sem_Scopes.Add_Name (Res);
         Name_Visible (Res);

         Free_Iir (Alias);

         if Get_Kind (Name) in Iir_Kinds_Denoting_And_External_Name then
            Sem_Non_Object_Alias_Declaration (Res);
         else
            Error_Msg_Sem
              (+Name, "name of nonobject alias is not a name");

            --  Create a simple name to an error node.
            N_Entity := Create_Error (Name);
            Name := Create_Iir (Iir_Kind_Simple_Name);
            Location_Copy (Name, N_Entity);
            Set_Identifier (Name, Get_Identifier (Res));  --  Better idea ?
            Set_Named_Entity (Name, N_Entity);
            Set_Base_Name (Name, Name);
            Set_Name (Res, Name);
         end if;

         return Res;
      end if;
   end Sem_Alias_Declaration;

   procedure Sem_Group_Template_Declaration
     (Decl : Iir_Group_Template_Declaration) is
   begin
      Sem_Scopes.Add_Name (Decl);
      Sem_Scopes.Name_Visible (Decl);
      Xref_Decl (Decl);
   end Sem_Group_Template_Declaration;

   procedure Sem_Group_Declaration (Group : Iir_Group_Declaration)
   is
      use Vhdl.Tokens;

      Constituent_List : Iir_Flist;
      Template : Iir_Group_Template_Declaration;
      Template_Name : Iir;
      Class, Prev_Class : Token_Type;
      El : Iir;
      El_Name : Iir;
      El_Entity : Iir_Entity_Class;
   begin
      Sem_Scopes.Add_Name (Group);
      Xref_Decl (Group);

      Template_Name := Sem_Denoting_Name (Get_Group_Template_Name (Group));
      Set_Group_Template_Name (Group, Template_Name);
      Template := Get_Named_Entity (Template_Name);
      if Get_Kind (Template) /= Iir_Kind_Group_Template_Declaration then
         Error_Class_Match (Template_Name, "group template");
         return;
      end if;
      Constituent_List := Get_Group_Constituent_List (Group);
      El_Entity := Get_Entity_Class_Entry_Chain (Template);
      Prev_Class := Tok_Eof;
      for I in Flist_First .. Flist_Last (Constituent_List) loop
         El := Get_Nth_Element (Constituent_List, I);

         Sem_Name (El);

         if El_Entity = Null_Iir then
            Error_Msg_Sem
              (+Group, "too many elements in group constituent list");
            exit;
         end if;

         Class := Get_Entity_Class (El_Entity);
         if Class = Tok_Box then
            --  LRM93 4.6
            --  An entity class entry that includes a box (<>) allows zero
            --  or more group constituents to appear in this position in the
            --  corresponding group declaration.
            Class := Prev_Class;
         else
            Prev_Class := Class;
            El_Entity := Get_Chain (El_Entity);
         end if;

         El_Name := Get_Named_Entity (El);
         if Is_Error (El_Name) then
            null;
         elsif Is_Overload_List (El_Name) then
            Error_Overload (El_Name);
         else
            El := Finish_Sem_Name (El);
            Set_Nth_Element (Constituent_List, I, El);
            El_Name := Get_Named_Entity (El);

            --  Statements are textually afer the group declaration.  To avoid
            --  adding a flag on each node with a base_name, this field is
            --  cleared, as we don't care about base name.
            if Class = Tok_Label then
               Set_Is_Forward_Ref (El, True);
            end if;
            Set_Base_Name (El, Null_Iir);

            --  LRM93 4.7
            --  It is an error if the class of any group constituent in the
            --  group constituent list is not the same as the class specified
            --  by the corresponding entity class entry in the entity class
            --  entry list of the group template.
            if Get_Entity_Class_Kind (El_Name) /= Class then
               Error_Msg_Sem (+El, "constituent not of class %t", +Class);
            end if;
         end if;
      end loop;

      --  End of entity_class list reached or zero or more constituent allowed.
      if not (El_Entity = Null_Iir
              or else Get_Entity_Class (El_Entity) = Tok_Box)
      then
         Error_Msg_Sem
           (+Group, "not enough elements in group constituent list");
      end if;
      Set_Visible_Flag (Group, True);
   end Sem_Group_Declaration;

   procedure Sem_Nature_Declaration (Decl : Iir)
   is
      Def : Iir;
   begin
      Def := Get_Nature (Decl);
      if Def /= Null_Iir then
         Sem_Scopes.Add_Name (Decl);
         Xref_Decl (Decl);

         Def := Sem_Nature_Definition (Def, Decl);
         if Def /= Null_Iir then
            Set_Nature_Declarator (Def, Decl);
            Sem_Scopes.Name_Visible (Decl);
         end if;
      end if;
   end Sem_Nature_Declaration;

   procedure Sem_Subnature_Declaration (Decl: Iir)
   is
      Def: Iir;
      Ind : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      --  Analyze the definition of the type.
      Ind := Get_Subnature_Indication (Decl);
      Ind := Sem_Subnature_Indication (Ind);
      Set_Subnature_Indication (Decl, Ind);
      Def := Get_Nature_Of_Subnature_Indication (Ind);
      if Def = Null_Iir or else Is_Error (Def) then
         return;
      end if;

      if not Is_Anonymous_Nature_Definition (Def) then
         --  There is no added constraints and therefore the subtype
         --  declaration is in fact an alias of the type.  Create a copy so
         --  that it has its own type declarator.
         raise Internal_Error;
      end if;

      Set_Nature (Decl, Def);
      Set_Nature_Declarator (Def, Decl);
      Name_Visible (Decl);
   end Sem_Subnature_Declaration;

   procedure Sem_Terminal_Declaration (Decl : Iir; Last_Decl : Iir)
   is
      Def, Nature : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      Set_Name_Staticness (Decl, Locally);

      Def := Get_Subnature_Indication (Decl);
      if Def = Null_Iir then
         Nature := Get_Nature (Last_Decl);
      else
         Nature := Sem_Subnature_Indication (Def);
         if Nature /= Null_Iir then
            Set_Subnature_Indication (Decl, Nature);
            Nature := Get_Nature_Of_Subnature_Indication (Nature);
         end if;
      end if;

      if Nature /= Null_Iir then
         Set_Nature (Decl, Nature);
         Sem_Scopes.Name_Visible (Decl);
      end if;
   end Sem_Terminal_Declaration;

   procedure Sem_Branch_Quantity_Declaration (Decl : Iir; Prev_Decl : Iir)
   is
      Plus_Name : Iir;
      Minus_Name : Iir;
      Branch_Type : Iir;
      Value : Iir;
      Is_Second : Boolean;
      Nat : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      Plus_Name := Get_Plus_Terminal_Name (Decl);
      if Plus_Name = Null_Iir then
         --  List of identifier.
         Is_Second := True;
         Plus_Name := Get_Plus_Terminal (Prev_Decl);
         Minus_Name := Get_Minus_Terminal (Prev_Decl);
         if Get_Kind (Decl) = Get_Kind (Prev_Decl) then
            --  Keep the same default value for all across and all through.
            Value := Get_Default_Value (Prev_Decl);
         else
            --  But do not use the across default value for through quantity.
            Value := Get_Default_Value (Decl);
         end if;
      else
         Is_Second := False;
         Plus_Name := Sem_Terminal_Name (Plus_Name);
         Set_Plus_Terminal_Name (Decl, Plus_Name);
         Plus_Name := Strip_Denoting_Name (Plus_Name);
         Minus_Name := Get_Minus_Terminal_Name (Decl);
         if Minus_Name /= Null_Iir then
            Minus_Name := Sem_Terminal_Name (Minus_Name);
            Set_Minus_Terminal_Name (Decl, Minus_Name);
            Minus_Name := Strip_Denoting_Name (Minus_Name);
         else
            --  AMS-LRM17 6.4.2.7 Quantity declarations
            --  A terminal aspect that does not include an explicit minus
            --  terminal name is equivalent to a terminal aspect with the
            --  given plus terminal name and the name of the reference
            --  terminal of the simple nature of its nature as the minus
            --  terminal name.
            --
            --  GHDL: FIXME: isn't it self-referential with the definition of
            --  the terminal nature ?
            if Is_Error (Plus_Name) then
               Minus_Name := Error_Mark;
            else
               Minus_Name := Get_Reference
                 (Get_Nature_Simple_Nature (Get_Nature (Plus_Name)));
            end if;
         end if;
         Value := Get_Default_Value (Decl);
      end if;
      Set_Plus_Terminal (Decl, Plus_Name);
      Set_Minus_Terminal (Decl, Minus_Name);

      if not Is_Error (Plus_Name) and then not Is_Error (Minus_Name) then
         declare
            Plus_Nature : constant Iir := Get_Nature (Plus_Name);
            Minus_Nature : constant Iir := Get_Nature (Minus_Name);
            Plus_Composite : constant Boolean :=
              Is_Composite_Nature (Plus_Nature);
            Minus_Composite : constant Boolean :=
              Is_Composite_Nature (Minus_Nature);
         begin
            --  AMS-LRM17 6.4.2.7 Quantity declarations
            --  If the terminals denoted by the terminal names of a terminal
            --  aspect are both of composite natures, then they shall be of the
            --  same nature, [and for each element of the plus terminal there
            --  shall be a matching element of the minus terminal.]
            --  If one terminal is a terminal of a composite nature and the
            --  other of a scalar nature, then the scalar nature nature shall
            --  be the nature of the scalar subelements of the composite
            --  terminal.
            if (Plus_Composite and Minus_Composite)
              or else (not Plus_Composite and not Minus_Composite)
            then
               if (Get_Base_Nature (Plus_Nature)
                     /= Get_Base_Nature (Minus_Nature))
               then
                  Error_Msg_Sem
                    (+Decl, "terminals must be of the same nature");
               end if;
               Nat := Plus_Nature;
            elsif Plus_Composite then
               pragma Assert (not Minus_Composite);
               if (Get_Nature_Simple_Nature (Plus_Nature)
                     /= Get_Base_Nature (Minus_Nature))
               then
                  Error_Msg_Sem
                    (+Decl, "minus terminal must be of the nature of "
                       & "plus subelements");
               end if;
               Nat := Plus_Nature;
            else
               pragma Assert (Minus_Composite and not Plus_Composite);
               if (Get_Nature_Simple_Nature (Minus_Nature)
                     /= Get_Base_Nature (Plus_Nature))
               then
                  Error_Msg_Sem
                    (+Decl, "plus terminal must be of the nature of "
                       & "minus subelements");
               end if;
               Nat := Minus_Nature;
            end if;
         end;

         case Iir_Kinds_Branch_Quantity_Declaration (Get_Kind (Decl)) is
            when Iir_Kind_Across_Quantity_Declaration =>
               Branch_Type := Get_Across_Type (Nat);
            when Iir_Kind_Through_Quantity_Declaration =>
               Branch_Type := Get_Through_Type (Nat);
         end case;
         pragma Assert (Branch_Type /= Null_Iir);
      else
         Branch_Type := Error_Mark;
      end if;
      Set_Type (Decl, Branch_Type);

      Set_Name_Staticness (Decl, Locally);
      Set_Expr_Staticness (Decl, None);

      if not Is_Second and then Value /= Null_Iir then
         Value := Sem_Expression (Value, Branch_Type);
      end if;
      Set_Default_Value (Decl, Value);

      --  TODO: tolerance

      Sem_Scopes.Name_Visible (Decl);
   end Sem_Branch_Quantity_Declaration;

   --  Analyze declaration DECL.
   --  PREV_DECL is the previous one (used for declaration like
   --    signal a, b : mytype; ) to get type and default value from the
   --  previous declaration.
   --  IS_GLOBAL must be true when the declaration can be used by an external
   --   file (so for package and entities).
   --  ATTR_SPEC_CHAIN is the chain of attribute specifications, used to
   --   handle the 'others' case.
   procedure Sem_Declaration (Decl : in out Iir;
                              Prev_Decl : in out Iir;
                              Is_Global : Boolean;
                              Attr_Spec_Chain : in out Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration =>
            Sem_Type_Declaration (Decl, Is_Global);
         when Iir_Kind_Subtype_Declaration =>
            Sem_Subtype_Declaration (Decl, Is_Global);
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Free_Quantity_Declaration =>
            Sem_Object_Declaration (Decl, Prev_Decl);
         when Iir_Kind_File_Declaration =>
            Sem_File_Declaration (Decl, Prev_Decl);
         when Iir_Kinds_Source_Quantity_Declaration =>
            Sem_Source_Quantity_Declaration (Decl, Prev_Decl);
         when Iir_Kind_Attribute_Declaration =>
            Sem_Attribute_Declaration (Decl);
         when Iir_Kind_Attribute_Specification =>
            Sem_Attribute_Specification (Decl);
            if Get_Entity_Name_List (Decl) in Iir_Flists_All_Others then
               Set_Attribute_Specification_Chain (Decl, Attr_Spec_Chain);
               Attr_Spec_Chain := Decl;
            end if;
         when Iir_Kind_Component_Declaration =>
            Sem_Component_Declaration (Decl);

         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            if Is_Implicit_Subprogram (Decl) then
               Sem_Scopes.Add_Name (Decl);
               --  Implicit subprogram are already visible.
            else
               Sem_Subprogram_Declaration (Decl);
               if Is_Global
                 and then Get_Kind (Decl) = Iir_Kind_Function_Declaration
                 and then Is_A_Resolution_Function (Decl, Null_Iir)
               then
                  Set_Resolution_Function_Flag (Decl, True);
               end if;
            end if;
         when Iir_Kind_Function_Body
            | Iir_Kind_Procedure_Body =>
            Sem_Subprogram_Body (Decl);
         when Iir_Kind_Function_Instantiation_Declaration
            | Iir_Kind_Procedure_Instantiation_Declaration =>
            Sem_Subprogram_Instantiation_Declaration (Decl);

         when Iir_Kind_Non_Object_Alias_Declaration =>
            --  Added by Sem_Alias_Declaration.  Need to check that no
            --  existing attribute specification apply to them.
            null;
         when Iir_Kind_Object_Alias_Declaration =>
            Decl := Sem_Alias_Declaration (Decl);
            --  An alias may add new alias declarations. Do not skip
            --  them: check that no existing attribute specifications
            --  apply to them.
         when Iir_Kind_Use_Clause =>
            Sem_Use_Clause (Decl);
         when Iir_Kind_Configuration_Specification =>
            null;
         when Iir_Kind_Disconnection_Specification =>
            Sem_Disconnection_Specification (Decl);
         when Iir_Kind_Step_Limit_Specification =>
            Sem_Step_Limit_Specification (Decl);
         when Iir_Kind_Group_Template_Declaration =>
            Sem_Group_Template_Declaration (Decl);
         when Iir_Kind_Group_Declaration =>
            Sem_Group_Declaration (Decl);
         when Iir_Kinds_Signal_Attribute =>
            --  Added by sem, so nothing to do.
            null;
         when Iir_Kind_Protected_Type_Body =>
            Sem_Protected_Type_Body (Decl);

         when Iir_Kind_Package_Declaration =>
            Sem_Package_Declaration (Decl);
         when Iir_Kind_Package_Body =>
            Sem_Package_Body (Decl);
         when Iir_Kind_Package_Instantiation_Declaration =>
            Sem_Package_Instantiation_Declaration (Decl);

         when Iir_Kind_Nature_Declaration =>
            Sem_Nature_Declaration (Decl);
         when Iir_Kind_Subnature_Declaration =>
            Sem_Subnature_Declaration (Decl);
         when Iir_Kind_Terminal_Declaration =>
            Sem_Terminal_Declaration (Decl, Prev_Decl);
         when Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            Sem_Branch_Quantity_Declaration (Decl, Prev_Decl);

         when Iir_Kind_Psl_Declaration =>
            Sem_Psl.Sem_Psl_Declaration (Decl);
         when Iir_Kind_Psl_Default_Clock =>
            Sem_Psl.Sem_Psl_Default_Clock (Decl);

         when others =>
            Error_Kind ("sem_declaration_chain", Decl);
      end case;

      if Attr_Spec_Chain /= Null_Iir then
         Check_Post_Attribute_Specification (Attr_Spec_Chain, Decl);
      end if;

      --  Insert *before* DECL pending implicit signal declarations created
      --  for DECL after LAST_DECL.  This updates LAST_DECL.
      Insert_Pending_Implicit_Declarations (Get_Parent (Decl), Prev_Decl);
   end Sem_Declaration;

   procedure Sem_Declaration_Chain (Parent : Iir)
   is
      Decl : Iir;
      --  Chain of attribute specifications, to check that no declaration
      --  appears after an 'others' entity_name_list.
      Attr_Spec_Chain : Iir;

      --  New declaration chain (declarations like implicit signals may be
      --  added, some like aliases may mutate).
      Last_Decl : Iir;

      --  If IS_GLOBAL is set, then declarations may be seen outside of unit.
      --  This must be set for entities and packages (except when
      --   Flags.Flag_Whole_Analyze is set).
      --  This controls whether a type is used for a signal.
      --  When Flag_Whole_Analyze is false, we are conservative and assume
      --  that any global type is used for a signal (when allowed).
      Is_Global : Boolean;
   begin
      case Get_Kind (Parent) is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration =>
            Is_Global := not Flags.Flag_Whole_Analyze;
         when others =>
            Is_Global := False;
      end case;

      --  Due to implicit declarations, the list can grow during sem.
      Decl := Get_Declaration_Chain (Parent);
      Last_Decl := Null_Iir;
      Attr_Spec_Chain := Null_Iir;

      while Decl /= Null_Iir loop

         Sem_Declaration (Decl, Last_Decl, Is_Global, Attr_Spec_Chain);

         if Last_Decl = Null_Iir then
            --  Append now to handle expand names.
            Set_Declaration_Chain (Parent, Decl);
         else
            Set_Chain (Last_Decl, Decl);
         end if;
         Last_Decl := Decl;
         Decl := Get_Chain (Decl);
      end loop;

      --  Keep the point of insertion for implicit signal declarations.
      End_Of_Declarations_For_Implicit_Declarations (Parent, Last_Decl);
   end Sem_Declaration_Chain;

   procedure Check_Full_Declaration (Decls_Parent : Iir; Decl: Iir)
   is
      procedure Warn_Unused (E : Iir) is
      begin
         Warning_Msg_Sem (Warnid_Unused, +E, "%n is never referenced", +E);
      end Warn_Unused;

      El: Iir;

      --  If set, emit a warning if a declaration is not used.
      Check_Unused : Boolean;
   begin
      --  LRM 3.5 Protected types.
      --  Each protected type declaration appearing immediatly within a given
      --  declaration region must have exactly one corresponding protected type
      --  body appearing immediatly within the same declarative region and
      --  textually subsequent to the protected type declaration.

      --  LRM 3.3.1 Incomplete type declarations
      --  For each incomplete type declaration, there must be a corresponding
      --  full type declaration with the same identifier.  This full type
      --  declaration must occur later and immediatly within the same
      --  declarative part as the incomplete type declaration to which it
      --  correspinds.

      --  LRM 4.3.1.1 Constant declarations
      --  If the assignment symbol ":=" followed by an expression is not
      --  present in a constant declaration, then the declaration declares a
      --  deferred constant.  Such a constant declaration must appear in a
      --  package declaration.  The corresponding full constant declaration,
      --  which defines the value of the constant, must appear in the body of
      --  the package (see 2.6).

      --  LRM 2.2 Subprogram bodies
      --  If both a declaration and a body are given, [...].  Furthermore,
      --  both the declaration and the body must occur immediatly within the
      --  same declaration region.

      --  Set Check_Unused.
      Check_Unused := False;
      if Is_Warning_Enabled (Warnid_Unused) then
         case Get_Kind (Decl) is
            when Iir_Kind_Entity_Declaration =>
               --  May be used in architecture.
               null;
            when Iir_Kind_Architecture_Body
              | Iir_Kind_Block_Statement =>
               --  Might be used in a configuration.
               Check_Unused := True;
            when  Iir_Kind_Generate_Statement_Body =>
               --  Might be used in a configuration.
               Check_Unused := True;
            when Iir_Kind_Package_Body
              | Iir_Kind_Protected_Type_Body =>
               --  Check only for declarations of the body.
               if Decls_Parent = Decl then
                  Check_Unused := True;
               end if;
            when Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body
              | Iir_Kind_Process_Statement
              | Iir_Kind_Sensitized_Process_Statement =>
               Check_Unused := True;
            when others =>
               --  Note: Check_Full_Declaration is not called
               --   for package declarations or protected type declarations.
               Error_Kind ("check_full_declaration", Decl);
         end case;
      end if;

      El := Get_Declaration_Chain (Decls_Parent);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Constant_Declaration =>
               if Get_Deferred_Declaration_Flag (El) then
                  if Get_Deferred_Declaration (El) = Null_Iir then
                     Error_Msg_Sem
                       (+Decl,
                        "missing value for constant declared at %l", +El);
                  end if;
               end if;
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               if not Is_Implicit_Subprogram (El)
                 and then Get_Subprogram_Body (El) = Null_Iir
               then
                  Error_Msg_Sem
                    (+Decl, "missing body for %n declared at %l", (+El, +El));
               end if;
            when Iir_Kind_Type_Declaration =>
               declare
                  Def : constant Iir := Get_Type_Definition (El);
               begin
                  if Get_Kind (Def) = Iir_Kind_Incomplete_Type_Definition
                    and then Is_Null (Get_Complete_Type_Definition (Def))
                  then
                     Error_Msg_Sem
                       (+El, "missing full type declaration for %n", +El);
                  elsif Get_Kind (Def) = Iir_Kind_Protected_Type_Declaration
                    and then Get_Protected_Type_Body (Def) = Null_Iir
                  then
                     Error_Msg_Sem
                       (+El, "missing protected type body for %n", +El);
                  end if;
               end;
            when Iir_Kind_Package_Declaration =>
               if Is_Null (Get_Package_Origin (El))
                 and then Get_Need_Body (El)
                 and then Get_Package_Body (El) = Null_Iir
               then
                  Error_Msg_Sem (+El, "missing package body for %n", +El);
               end if;
            when others =>
               null;
         end case;

         if Check_Unused then
            --  All subprograms declared in the specification (package or
            --  protected type) have only their *body* in the body.
            --  Therefore, they don't appear as declaration in body.
            --  Only private subprograms appears as declarations.
            case Get_Kind (El) is
               when Iir_Kind_Function_Declaration
                 | Iir_Kind_Procedure_Declaration =>
                  if not Get_Use_Flag (El)
                    and then not Is_Implicit_Subprogram (El)
                    and then not Is_Second_Subprogram_Specification (El)
                  then
                     Warn_Unused (El);
                  end if;
               when Iir_Kind_Signal_Declaration
                 | Iir_Kind_Variable_Declaration
                 | Iir_Kind_Component_Declaration
                 | Iir_Kind_Subtype_Declaration =>
                  if not Get_Use_Flag (El) then
                     Warn_Unused (El);
                  end if;
               when Iir_Kind_Type_Declaration =>
                  if not Get_Use_Flag (El) then
                     Warn_Unused (El);
                  else
                     declare
                        Def : constant Iir := Get_Type_Definition (El);
                        Lits : Iir_Flist;
                        Lit : Iir;
                     begin
                        if Get_Kind (Def)
                          = Iir_Kind_Enumeration_Type_Definition
                        then
                           Lits := Get_Enumeration_Literal_List (Def);
                           for I in Flist_First .. Flist_Last (Lits) loop
                              Lit := Get_Nth_Element (Lits, I);
                              if not Get_Use_Flag (Lit) then
                                 Warn_Unused (Lit);
                              end if;
                           end loop;
                        end if;
                     end;
                  end if;
               when others =>
                  null;
            end case;
         end if;

         El := Get_Chain (El);
      end loop;
   end Check_Full_Declaration;

   procedure Sem_Iterator (Iterator : Iir_Iterator_Declaration;
                           Staticness : Iir_Staticness)
   is
      It_Range: constant Iir := Get_Discrete_Range (Iterator);
      It_Type : Iir;
      A_Range: Iir;
   begin
      Xref_Decl (Iterator);

      A_Range := Sem_Discrete_Range_Integer (It_Range);
      if A_Range = Null_Iir then
         Set_Type (Iterator, Create_Error_Type (It_Range));
         return;
      end if;

      Set_Discrete_Range (Iterator, Null_Iir);

      It_Type := Range_To_Subtype_Indication (A_Range);
      Set_Subtype_Indication (Iterator, It_Type);
      Set_Type (Iterator, Get_Type_Of_Subtype_Indication (It_Type));

      Set_Expr_Staticness (Iterator, Staticness);
   end Sem_Iterator;
end Vhdl.Sem_Decls;
