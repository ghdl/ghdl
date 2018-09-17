--  Semantic analysis.
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
with Errorout; use Errorout;
with Types; use Types;
with Std_Names;
with Tokens;
with Flags; use Flags;
with Std_Package; use Std_Package;
with Ieee.Std_Logic_1164;
with Iir_Chains;
with Evaluation; use Evaluation;
with Iirs_Utils; use Iirs_Utils;
with Sem; use Sem;
with Sem_Expr; use Sem_Expr;
with Sem_Scopes; use Sem_Scopes;
with Sem_Names; use Sem_Names;
with Sem_Specs; use Sem_Specs;
with Sem_Types; use Sem_Types;
with Sem_Psl;
with Sem_Inst;
with Xrefs; use Xrefs;
use Iir_Chains;

package body Sem_Decls is
   function Create_Anonymous_Interface
     (Atype : Iir) return Iir_Interface_Constant_Declaration;
   function Create_Implicit_Function (Name : Name_Id;
                                      Decl : Iir;
                                      Def : Iir_Predefined_Functions;
                                      Interface_Chain : Iir;
                                      Return_Type : Iir)
                                     return Iir;

   --  Region that can declare signals.  Used to add implicit declarations.
   Current_Signals_Region : Implicit_Signal_Declaration_Type :=
     (Null_Iir, Null_Iir, Null_Iir, False, Null_Iir);

   procedure Push_Signals_Declarative_Part
     (Cell: out Implicit_Signal_Declaration_Type; Decls_Parent : Iir) is
   begin
      Cell := Current_Signals_Region;
      Current_Signals_Region :=
        (Decls_Parent, Null_Iir, Null_Iir, False, Null_Iir);
   end Push_Signals_Declarative_Part;

   procedure Pop_Signals_Declarative_Part
     (Cell: in Implicit_Signal_Declaration_Type) is
   begin
      Current_Signals_Region := Cell;
   end Pop_Signals_Declarative_Part;

   --  Insert the implicit signal declaration after LAST_DECL.
   procedure Insert_Implicit_Signal (Last_Decl : Iir) is
   begin
      if Last_Decl = Null_Iir then
         Set_Declaration_Chain (Current_Signals_Region.Decls_Parent,
                                Current_Signals_Region.Implicit_Decl);
      else
         Set_Chain (Last_Decl, Current_Signals_Region.Implicit_Decl);
      end if;
   end Insert_Implicit_Signal;

   --  Add SIG as an implicit declaration in the current region.
   procedure Add_Declaration_For_Implicit_Signal (Sig : Iir)
   is
      Decl : Iir;
   begin
      --  We deal only with signal attribute.
      pragma Assert (Get_Kind (Sig) in Iir_Kinds_Signal_Attribute);

      --  There must be a declarative part for implicit signals.
      pragma Assert (Current_Signals_Region.Decls_Parent /= Null_Iir);

      --  Attr_Chain must be empty.
      pragma Assert (Get_Attr_Chain (Sig) = Null_Iir);

      if Current_Signals_Region.Implicit_Decl = Null_Iir then
         --  Create the signal_attribute_declaration to hold all the implicit
         --  signals.
         Decl := Create_Iir (Iir_Kind_Signal_Attribute_Declaration);
         Location_Copy (Decl, Sig);
         Set_Parent (Decl, Current_Signals_Region.Decls_Parent);

         --  Save the implicit declaration.
         Current_Signals_Region.Implicit_Decl := Decl;

         --  Append SIG (this is the first one).
         Set_Signal_Attribute_Chain (Decl, Sig);

         if Current_Signals_Region.Decls_Analyzed then
            --  Declarative region was completely analyzed.  Just append DECL
            --  at the end of declarations.
            Insert_Implicit_Signal (Current_Signals_Region.Last_Decl);
         end if;
      else
         --  Append SIG.
         Set_Attr_Chain (Current_Signals_Region.Last_Attribute_Signal, Sig);
      end if;
      Current_Signals_Region.Last_Attribute_Signal := Sig;

      Set_Signal_Attribute_Declaration
        (Sig, Current_Signals_Region.Implicit_Decl);
   end Add_Declaration_For_Implicit_Signal;

   --  Insert pending implicit declarations after the last analyzed LAST_DECL,
   --  and update it.  Then the caller has to insert the declaration which
   --  created the implicit declarations.
   procedure Insert_Pending_Implicit_Declarations
     (Parent : Iir; Last_Decl : in out Iir) is
   begin
      if Current_Signals_Region.Decls_Parent = Parent
        and then Current_Signals_Region.Implicit_Decl /= Null_Iir
      then
         pragma Assert (not Current_Signals_Region.Decls_Analyzed);

         --  Add pending implicit declarations before the current one.
         Insert_Implicit_Signal (Last_Decl);
         Last_Decl := Current_Signals_Region.Implicit_Decl;

         --  Detach the implicit declaration.
         Current_Signals_Region.Implicit_Decl := Null_Iir;
         Current_Signals_Region.Last_Attribute_Signal := Null_Iir;
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
         pragma Assert (Last /= Null_Iir);
         A_Type := Get_Type (Last);
         Default_Value := Get_Default_Value (Last);
      else
         A_Type := Sem_Subtype_Indication (A_Type);
         Set_Subtype_Indication (Inter, A_Type);
         A_Type := Get_Type_Of_Subtype_Indication (A_Type);

         Default_Value := Get_Default_Value (Inter);
         if Default_Value /= Null_Iir and then not Is_Error (A_Type) then
            Deferred_Constant_Allowed := True;
            Default_Value := Sem_Expression (Default_Value, A_Type);
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
                     if Flags.Vhdl_Std >= Vhdl_93 then
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
            if Get_Kind (Inter) /= Iir_Kind_Interface_Signal_Declaration then
               Error_Msg_Sem (+Inter, "port %n must be a signal", +Inter);
            end if;
         when Parameter_Interface_List =>
            if Get_Kind (Inter) = Iir_Kind_Interface_Variable_Declaration
              and then Interface_Kind = Function_Parameter_Interface_List
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
                     Error_Msg_Sem
                       (+Inter,
                        "mode of a function parameter cannot be inout or out");
                  end if;
               when Iir_Buffer_Mode
                 | Iir_Linkage_Mode =>
                  Error_Msg_Sem
                    (+Inter, "buffer or linkage mode is not allowed "
                       & "for a subprogram parameter");
            end case;
      end case;
   end Sem_Interface_Object_Declaration;

   procedure Sem_Interface_Package_Declaration (Inter : Iir)
   is
      Pkg : Iir;
   begin
      --  LRM08 6.5.5 Interface package declarations
      --  the uninstantiated_package_name shall denote an uninstantiated
      --  package declared in a package declaration.
      Pkg := Sem_Uninstantiated_Package_Name (Inter);
      if Pkg = Null_Iir then
         return;
      end if;

      Sem_Inst.Instantiate_Package_Declaration (Inter, Pkg);

      if Get_Generic_Map_Aspect_Chain (Inter) /= Null_Iir then
         --  TODO
         raise Internal_Error;
      end if;

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
      --  Create type definition.
      Def := Create_Iir (Iir_Kind_Interface_Type_Definition);
      Set_Location (Def, Get_Location (Inter));
      Set_Type_Declarator (Def, Inter);
      Set_Type (Inter, Def);
      Set_Base_Type (Def, Def);
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

   --  LRM93 7.2.2
   --  A discrete array is a one-dimensional array whose elements are of a
   --  discrete type.
   function Is_Discrete_Array (Def : Iir) return Boolean
   is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            null;
         when others =>
            raise Internal_Error;
            -- return False;
      end case;
      if not Is_One_Dimensional_Array_Type (Def) then
         return False;
      end if;
      if Get_Kind (Get_Element_Subtype (Def))
        not in Iir_Kinds_Discrete_Type_Definition
      then
         return False;
      end if;
      return True;
   end Is_Discrete_Array;

   procedure Create_Implicit_File_Primitives
     (Decl : Iir_Type_Declaration; Type_Definition : Iir_File_Type_Definition)
   is
      use Iir_Chains.Interface_Declaration_Chain_Handling;
      Type_Mark : constant Iir := Get_File_Type_Mark (Type_Definition);
      Type_Mark_Type : constant Iir := Get_Type (Type_Mark);
      Proc: Iir_Procedure_Declaration;
      Func: Iir_Function_Declaration;
      Inter: Iir;
      Loc : Location_Type;
      File_Interface_Kind : Iir_Kind;
      Last_Interface : Iir;
      Last : Iir;
   begin
      Last := Decl;
      Loc := Get_Location (Decl);

      if Flags.Vhdl_Std >= Vhdl_93c then
         for I in 1 .. 2 loop
            --  Create the implicit file_open (form 1) declaration.
            --  Create the implicit file_open (form 2) declaration.
            Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
            Set_Location (Proc, Loc);
            Set_Parent (Proc, Get_Parent (Decl));
            Set_Identifier (Proc, Std_Names.Name_File_Open);
            Set_Visible_Flag (Proc, True);
            Set_Wait_State (Proc, False);
            Build_Init (Last_Interface);
            case I is
               when 1 =>
                  Set_Implicit_Definition (Proc, Iir_Predefined_File_Open);
               when 2 =>
                  Set_Implicit_Definition (Proc,
                                           Iir_Predefined_File_Open_Status);
                  --  status : out file_open_status.
                  Inter :=
                    Create_Iir (Iir_Kind_Interface_Variable_Declaration);
                  Set_Location (Inter, Loc);
                  Set_Identifier (Inter, Std_Names.Name_Status);
                  Set_Type (Inter,
                            Std_Package.File_Open_Status_Type_Definition);
                  Set_Mode (Inter, Iir_Out_Mode);
                  Set_Visible_Flag (Inter, True);
                  Append (Last_Interface, Proc, Inter);
            end case;
            --  File F : FT
            Inter := Create_Iir (Iir_Kind_Interface_File_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_F);
            Set_Type (Inter, Type_Definition);
            Set_Mode (Inter, Iir_Inout_Mode);
            Set_Visible_Flag (Inter, True);
            Append (Last_Interface, Proc, Inter);
            --  External_Name : in STRING
            Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_External_Name);
            Set_Type (Inter, Std_Package.String_Type_Definition);
            Set_Mode (Inter, Iir_In_Mode);
            Set_Visible_Flag (Inter, True);
            Append (Last_Interface, Proc, Inter);
            --  Open_Kind : in File_Open_Kind := Read_Mode.
            Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_Open_Kind);
            Set_Type (Inter, Std_Package.File_Open_Kind_Type_Definition);
            Set_Mode (Inter, Iir_In_Mode);
            Set_Default_Value
              (Inter,
               Build_Simple_Name (Std_Package.File_Open_Kind_Read_Mode, Loc));
            Set_Visible_Flag (Inter, True);
            Append (Last_Interface, Proc, Inter);
            Compute_Subprogram_Hash (Proc);
            -- Add it to the list.
            Insert_Incr (Last, Proc);
         end loop;

         --  Create the implicit file_close declaration.
         Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
         Set_Identifier (Proc, Std_Names.Name_File_Close);
         Set_Location (Proc, Loc);
         Set_Parent (Proc, Get_Parent (Decl));
         Set_Implicit_Definition (Proc, Iir_Predefined_File_Close);
         Set_Visible_Flag (Proc, True);
         Set_Wait_State (Proc, False);
         Build_Init (Last_Interface);
         Inter := Create_Iir (Iir_Kind_Interface_File_Declaration);
         Set_Identifier (Inter, Std_Names.Name_F);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Type_Definition);
         Set_Mode (Inter, Iir_Inout_Mode);
         Set_Visible_Flag (Inter, True);
         Append (Last_Interface, Proc, Inter);
         Compute_Subprogram_Hash (Proc);
         -- Add it to the list.
         Insert_Incr (Last, Proc);
      end if;

      if Flags.Vhdl_Std = Vhdl_87 then
         File_Interface_Kind := Iir_Kind_Interface_Variable_Declaration;
      else
         File_Interface_Kind := Iir_Kind_Interface_File_Declaration;
      end if;

      -- Create the implicit procedure read declaration.
      Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
      Set_Identifier (Proc, Std_Names.Name_Read);
      Set_Location (Proc, Loc);
      Set_Parent (Proc, Get_Parent (Decl));
      Set_Visible_Flag (Proc, True);
      Set_Wait_State (Proc, False);
      Build_Init (Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Visible_Flag (Inter, True);
      Append (Last_Interface, Proc, Inter);
      Inter := Create_Iir (Iir_Kind_Interface_Variable_Declaration);
      Set_Identifier (Inter, Std_Names.Name_Value);
      Set_Location (Inter, Loc);
      Set_Subtype_Indication (Inter, Build_Simple_Name (Decl, Loc));
      Set_Type (Inter, Type_Mark_Type);
      Set_Mode (Inter, Iir_Out_Mode);
      Set_Visible_Flag (Inter, True);
      Append (Last_Interface, Proc, Inter);
      if Get_Kind (Type_Mark_Type) in Iir_Kinds_Array_Type_Definition
        and then Get_Constraint_State (Type_Mark_Type) /= Fully_Constrained
      then
         Inter := Create_Iir (Iir_Kind_Interface_Variable_Declaration);
         Set_Identifier (Inter, Std_Names.Name_Length);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Std_Package.Natural_Subtype_Definition);
         Set_Mode (Inter, Iir_Out_Mode);
         Set_Visible_Flag (Inter, True);
         Append (Last_Interface, Proc, Inter);
         Set_Implicit_Definition (Proc, Iir_Predefined_Read_Length);
      else
         Set_Implicit_Definition (Proc, Iir_Predefined_Read);
      end if;
      Compute_Subprogram_Hash (Proc);
      -- Add it to the list.
      Insert_Incr (Last, Proc);

      -- Create the implicit procedure write declaration.
      Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
      Set_Identifier (Proc, Std_Names.Name_Write);
      Set_Location (Proc, Loc);
      Set_Parent (Proc, Get_Parent (Decl));
      Set_Visible_Flag (Proc, True);
      Set_Wait_State (Proc, False);
      Build_Init (Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_Out_Mode);
      Set_Name_Staticness (Inter, Locally);
      Set_Expr_Staticness (Inter, None);
      Set_Visible_Flag (Inter, True);
      Append (Last_Interface, Proc, Inter);
      Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
      Set_Identifier (Inter, Std_Names.Name_Value);
      Set_Location (Inter, Loc);
      Set_Subtype_Indication (Inter, Build_Simple_Name (Decl, Loc));
      Set_Type (Inter, Type_Mark_Type);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Visible_Flag (Inter, True);
      Append (Last_Interface, Proc, Inter);
      Set_Implicit_Definition (Proc, Iir_Predefined_Write);
      Compute_Subprogram_Hash (Proc);
      -- Add it to the list.
      Insert_Incr (Last, Proc);

      --  Create the implicit procedure flush declaration
      if Flags.Vhdl_Std >= Vhdl_08 then
         Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
         Set_Identifier (Proc, Std_Names.Name_Flush);
         Set_Location (Proc, Loc);
         Set_Parent (Proc, Get_Parent (Decl));
         Set_Visible_Flag (Proc, True);
         Set_Wait_State (Proc, False);
         Build_Init (Last_Interface);
         Inter := Create_Iir (File_Interface_Kind);
         Set_Identifier (Inter, Std_Names.Name_F);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Type_Definition);
         Set_Name_Staticness (Inter, Locally);
         Set_Expr_Staticness (Inter, None);
         Set_Visible_Flag (Inter, True);
         Append (Last_Interface, Proc, Inter);
         Set_Implicit_Definition (Proc, Iir_Predefined_Flush);
         Compute_Subprogram_Hash (Proc);
         -- Add it to the list.
         Insert_Incr (Last, Proc);
      end if;
      -- Create the implicit function endfile declaration.
      Func := Create_Iir (Iir_Kind_Function_Declaration);
      Set_Identifier (Func, Std_Names.Name_Endfile);
      Set_Location (Func, Loc);
      Set_Parent (Func, Get_Parent (Decl));
      Set_Visible_Flag (Func, True);
      Build_Init (Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Visible_Flag (Inter, True);
      Append (Last_Interface, Func, Inter);
      Set_Return_Type (Func, Std_Package.Boolean_Type_Definition);
      Set_Implicit_Definition (Func, Iir_Predefined_Endfile);
      Compute_Subprogram_Hash (Func);
      -- Add it to the list.
      Insert_Incr (Last, Func);
   end Create_Implicit_File_Primitives;

   function Create_Anonymous_Interface (Atype : Iir)
     return Iir_Interface_Constant_Declaration
   is
      Inter : Iir_Interface_Constant_Declaration;
   begin
      Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
      Location_Copy (Inter, Atype);
      Set_Identifier (Inter, Null_Identifier);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Type (Inter, Atype);
      return Inter;
   end Create_Anonymous_Interface;

   --  Create an implicit/predefined function for DECL.
   function Create_Implicit_Function (Name : Name_Id;
                                      Decl : Iir;
                                      Def : Iir_Predefined_Functions;
                                      Interface_Chain : Iir;
                                      Return_Type : Iir)
                                     return Iir
   is
      Operation : Iir_Function_Declaration;
   begin
      Operation := Create_Iir (Iir_Kind_Function_Declaration);
      Location_Copy (Operation, Decl);
      Set_Parent (Operation, Get_Parent (Decl));
      Set_Interface_Declaration_Chain (Operation, Interface_Chain);
      Set_Return_Type (Operation, Return_Type);
      Set_Implicit_Definition (Operation, Def);
      Set_Identifier (Operation, Name);
      Set_Visible_Flag (Operation, True);
      Compute_Subprogram_Hash (Operation);
      return Operation;
   end Create_Implicit_Function;

   procedure Create_Implicit_Operations
     (Decl : Iir; Is_Std_Standard : Boolean := False)
   is
      use Std_Names;
      Binary_Chain : Iir;
      Unary_Chain : Iir;
      Type_Definition : Iir;
      Last : Iir;

      procedure Add_Operation (Name : Name_Id;
                               Def : Iir_Predefined_Functions;
                               Interface_Chain : Iir;
                               Return_Type : Iir)
      is
         Operation : Iir_Function_Declaration;
      begin
         Operation := Create_Implicit_Function
           (Name, Decl, Def, Interface_Chain, Return_Type);
         Insert_Incr (Last, Operation);
      end Add_Operation;

      procedure Add_Relational (Name : Name_Id; Def : Iir_Predefined_Functions)
      is
      begin
         Add_Operation
           (Name, Def, Binary_Chain, Std_Package.Boolean_Type_Definition);
      end Add_Relational;

      procedure Add_Binary (Name : Name_Id; Def : Iir_Predefined_Functions) is
      begin
         Add_Operation (Name, Def, Binary_Chain, Type_Definition);
      end Add_Binary;

      procedure Add_Unary (Name : Name_Id; Def : Iir_Predefined_Functions) is
      begin
         Add_Operation (Name, Def, Unary_Chain, Type_Definition);
      end Add_Unary;

      procedure Add_To_String (Def : Iir_Predefined_Functions) is
      begin
         Add_Operation (Name_To_String, Def,
                        Unary_Chain, String_Type_Definition);
      end Add_To_String;

      procedure Add_Min_Max (Name : Name_Id; Def : Iir_Predefined_Functions)
      is
         Left, Right : Iir;
      begin
         Left := Create_Anonymous_Interface (Type_Definition);
         Set_Identifier (Left, Name_L);
         Right := Create_Anonymous_Interface (Type_Definition);
         Set_Identifier (Right, Name_R);
         Set_Chain (Left, Right);
         Add_Operation (Name, Def, Left, Type_Definition);
      end Add_Min_Max;

      procedure Add_Vector_Min_Max
        (Name : Name_Id; Def : Iir_Predefined_Functions)
      is
         Left : Iir;
      begin
         Left := Create_Anonymous_Interface (Type_Definition);
         Set_Identifier (Left, Name_L);
         Add_Operation
           (Name, Def, Left, Get_Element_Subtype (Type_Definition));
      end Add_Vector_Min_Max;

      procedure Add_Shift_Operators
      is
         Inter_Chain : Iir_Interface_Constant_Declaration;
         Inter_Int : Iir;
      begin
         Inter_Chain := Create_Anonymous_Interface (Type_Definition);

         Inter_Int := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
         Location_Copy (Inter_Int, Decl);
         Set_Identifier (Inter_Int, Null_Identifier);
         Set_Mode (Inter_Int, Iir_In_Mode);
         Set_Type (Inter_Int, Std_Package.Integer_Subtype_Definition);

         Set_Chain (Inter_Chain, Inter_Int);

         Add_Operation
           (Name_Sll, Iir_Predefined_Array_Sll, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Srl, Iir_Predefined_Array_Srl, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Sla, Iir_Predefined_Array_Sla, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Sra, Iir_Predefined_Array_Sra, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Rol, Iir_Predefined_Array_Rol, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Ror, Iir_Predefined_Array_Ror, Inter_Chain, Type_Definition);
      end Add_Shift_Operators;
   begin
      Last := Decl;

      Type_Definition := Get_Base_Type (Get_Type_Definition (Decl));
      if Get_Kind (Type_Definition) /= Iir_Kind_File_Type_Definition then
         Unary_Chain := Create_Anonymous_Interface (Type_Definition);
         Binary_Chain := Create_Anonymous_Interface (Type_Definition);
         Set_Chain (Binary_Chain, Unary_Chain);
      end if;

      case Get_Kind (Type_Definition) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Enum_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Enum_Inequality);
            Add_Relational (Name_Op_Greater, Iir_Predefined_Enum_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Enum_Greater_Equal);
            Add_Relational (Name_Op_Less, Iir_Predefined_Enum_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Enum_Less_Equal);

            if Flags.Vhdl_Std >= Vhdl_08 then
               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Enum_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Enum_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Enum_To_String);
               end if;

               --  LRM08 9.2.3 Relational operators
               --  The matching relational operators are predefined for the
               --  [predefined type BIT and for the] type STD_ULOGIC defined
               --  in package STD_LOGIC_1164.
               if Type_Definition = Ieee.Std_Logic_1164.Std_Ulogic_Type then
                  Add_Binary (Name_Op_Match_Equality,
                              Iir_Predefined_Std_Ulogic_Match_Equality);
                  Add_Binary (Name_Op_Match_Inequality,
                              Iir_Predefined_Std_Ulogic_Match_Inequality);
                  Add_Binary (Name_Op_Match_Less,
                              Iir_Predefined_Std_Ulogic_Match_Less);
                  Add_Binary (Name_Op_Match_Less_Equal,
                              Iir_Predefined_Std_Ulogic_Match_Less_Equal);
                  Add_Binary (Name_Op_Match_Greater,
                              Iir_Predefined_Std_Ulogic_Match_Greater);
                  Add_Binary (Name_Op_Match_Greater_Equal,
                              Iir_Predefined_Std_Ulogic_Match_Greater_Equal);
               end if;
            end if;

         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            declare
               Element_Type : Iir;

               Element_Array_Inter_Chain : Iir;
               Array_Element_Inter_Chain : Iir;
               Element_Element_Inter_Chain : Iir;
            begin
               Add_Relational
                 (Name_Op_Equality, Iir_Predefined_Array_Equality);
               Add_Relational
                 (Name_Op_Inequality, Iir_Predefined_Array_Inequality);
               if Is_Discrete_Array (Type_Definition) then
                  Add_Relational
                    (Name_Op_Greater, Iir_Predefined_Array_Greater);
                  Add_Relational
                    (Name_Op_Greater_Equal,
                     Iir_Predefined_Array_Greater_Equal);
                  Add_Relational
                    (Name_Op_Less, Iir_Predefined_Array_Less);
                  Add_Relational
                    (Name_Op_Less_Equal, Iir_Predefined_Array_Less_Equal);

                  --  LRM08 5.3.2.4 Predefined operations on array types
                  --  Given a type declaration that declares a discrete array
                  --  type T, the following operatons are implicitly declared
                  --  immediately following the type declaration:
                  --   function MINIMUM (L, R : T) return T;
                  --   function MAXIMUM (L, R : T) return T;
                  if Vhdl_Std >= Vhdl_08 then
                     Add_Min_Max (Name_Maximum, Iir_Predefined_Array_Maximum);
                     Add_Min_Max (Name_Minimum, Iir_Predefined_Array_Minimum);
                  end if;
               end if;

               Element_Type := Get_Element_Subtype (Type_Definition);

               if Is_One_Dimensional_Array_Type (Type_Definition) then
                  --  LRM93 7.2.4 Adding operators
                  --  The concatenation operator & is predefined for any
                  --  one-dimensional array type.
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Array_Array_Concat,
                                 Binary_Chain,
                                 Type_Definition);

                  Element_Array_Inter_Chain :=
                    Create_Anonymous_Interface (Element_Type);
                  Set_Chain (Element_Array_Inter_Chain, Unary_Chain);
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Element_Array_Concat,
                                 Element_Array_Inter_Chain,
                                 Type_Definition);

                  Array_Element_Inter_Chain :=
                    Create_Anonymous_Interface (Type_Definition);
                  Set_Chain (Array_Element_Inter_Chain,
                             Create_Anonymous_Interface (Element_Type));
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Array_Element_Concat,
                                 Array_Element_Inter_Chain,
                                 Type_Definition);

                  Element_Element_Inter_Chain :=
                    Create_Anonymous_Interface (Element_Type);
                  Set_Chain (Element_Element_Inter_Chain,
                             Create_Anonymous_Interface (Element_Type));
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Element_Element_Concat,
                                 Element_Element_Inter_Chain,
                                 Type_Definition);

                  --  LRM08 5.3.2.4 Predefined operations on array types
                  --  In addition, given a type declaration that declares a
                  --  one-dimensional array type T whose elements are of a
                  --  sclar type E, the following operations are implicitly
                  --  declared immediately following the type declaration:
                  --   function MINIMUM (L : T) return E;
                  --   function MAXIMUM (L : T) return E;
                  if Vhdl_Std >= Vhdl_08
                    and then (Get_Kind (Element_Type) in
                                Iir_Kinds_Scalar_Type_And_Subtype_Definition)
                  then
                     Add_Vector_Min_Max
                       (Name_Maximum, Iir_Predefined_Vector_Maximum);
                     Add_Vector_Min_Max
                       (Name_Minimum, Iir_Predefined_Vector_Minimum);
                  end if;

                  if Element_Type = Std_Package.Boolean_Type_Definition
                    or else Element_Type = Std_Package.Bit_Type_Definition
                  then
                     --  LRM93 7.2.1 Logical operators
                     --  LRM08 9.2.2 Logical operators
                     --  The binary logical operators AND, OR, NAND, NOR, XOR,
                     --  and XNOR, and the unary logical operator NOT are
                     --  defined for predefined types BIT and BOOLEAN.  They
                     --  are also defined for any one-dimensional array type
                     --  whose element type is BIT or BOOLEAN.

                     Add_Unary (Name_Not, Iir_Predefined_TF_Array_Not);

                     Add_Binary (Name_And, Iir_Predefined_TF_Array_And);
                     Add_Binary (Name_Or, Iir_Predefined_TF_Array_Or);
                     Add_Binary (Name_Nand, Iir_Predefined_TF_Array_Nand);
                     Add_Binary (Name_Nor, Iir_Predefined_TF_Array_Nor);
                     Add_Binary (Name_Xor, Iir_Predefined_TF_Array_Xor);
                     if Flags.Vhdl_Std > Vhdl_87 then
                        Add_Binary (Name_Xnor, Iir_Predefined_TF_Array_Xnor);

                        --  LRM93 7.2.3 Shift operators
                        --  The shift operators SLL, SRL, SLA, SRA, ROL and
                        --  ROR are defined for any one-dimensional array type
                        --  whose element type is either of the predefined
                        --  types BIT or BOOLEAN.
                        Add_Shift_Operators;
                     end if;

                     --  LRM08 9.2.2 Logical operators
                     --  For the binary operators AND, OR, NAND, NOR, XOR and
                     --  XNOR, the operands shall both be [of the same base
                     --  type,] or one operand shall be of a scalar type and
                     --  the other operand shall be a one-dimensional array
                     --  whose element type is the scalar type.  The result
                     --  type is the same as the base type of the operands if
                     --  [both operands are scalars of the same base type or]
                     --  both operands are arrays, or the same as the base type
                     --  of the array operand if one operand is a scalar and
                     --  the other operand is an array.
                     if Flags.Vhdl_Std >= Vhdl_08 then
                        Add_Operation
                          (Name_And, Iir_Predefined_TF_Element_Array_And,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_And, Iir_Predefined_TF_Array_Element_And,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Or, Iir_Predefined_TF_Element_Array_Or,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Or, Iir_Predefined_TF_Array_Element_Or,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nand, Iir_Predefined_TF_Element_Array_Nand,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nand, Iir_Predefined_TF_Array_Element_Nand,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nor, Iir_Predefined_TF_Element_Array_Nor,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nor, Iir_Predefined_TF_Array_Element_Nor,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xor, Iir_Predefined_TF_Element_Array_Xor,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xor, Iir_Predefined_TF_Array_Element_Xor,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xnor, Iir_Predefined_TF_Element_Array_Xnor,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xnor, Iir_Predefined_TF_Array_Element_Xnor,
                           Array_Element_Inter_Chain, Type_Definition);
                     end if;

                     if Flags.Vhdl_Std >= Vhdl_08 then
                        --  LRM08 9.2.2 Logical operations
                        --  The unary logical operators AND, OR, NAND, NOR,
                        --  XOR, and XNOR are referred to as logical reduction
                        --  operators.  The logical reduction operators are
                        --  predefined for any one-dimensional array type whose
                        --  element type is BIT or BOOLEAN.  The result type
                        --  for the logical reduction operators is the same as
                        --  the element type of the operand.
                        Add_Operation
                          (Name_And, Iir_Predefined_TF_Reduction_And,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Or, Iir_Predefined_TF_Reduction_Or,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Nand, Iir_Predefined_TF_Reduction_Nand,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Nor, Iir_Predefined_TF_Reduction_Nor,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Xor, Iir_Predefined_TF_Reduction_Xor,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Xnor, Iir_Predefined_TF_Reduction_Xnor,
                           Unary_Chain, Element_Type);
                     end if;
                  end if;

                  --  LRM08 9.2.3 Relational operators
                  --  The matching equality and matching inequality operatotrs
                  --  are also defined for any one-dimensional array type
                  --  whose element type is BIT or STD_ULOGIC.
                  if Flags.Vhdl_Std >= Vhdl_08 then
                     if Element_Type = Std_Package.Bit_Type_Definition then
                        Add_Operation
                          (Name_Op_Match_Equality,
                           Iir_Predefined_Bit_Array_Match_Equality,
                           Binary_Chain, Element_Type);
                        Add_Operation
                          (Name_Op_Match_Inequality,
                           Iir_Predefined_Bit_Array_Match_Inequality,
                           Binary_Chain, Element_Type);
                     elsif Element_Type = Ieee.Std_Logic_1164.Std_Ulogic_Type
                     then
                        Add_Operation
                          (Name_Op_Match_Equality,
                           Iir_Predefined_Std_Ulogic_Array_Match_Equality,
                           Binary_Chain, Element_Type);
                        Add_Operation
                          (Name_Op_Match_Inequality,
                           Iir_Predefined_Std_Ulogic_Array_Match_Inequality,
                           Binary_Chain, Element_Type);
                     end if;
                  end if;

                  --  LRM08 5.3.2.4  Predefined operations on array type
                  --
                  --  Given a type declaration that declares a one-dimensional
                  --  array type T whose element type is a character type that
                  --  contains only character literals, the following operation
                  --  is implicitely declared immediately following the type
                  --  declaration
                  if Vhdl_Std >= Vhdl_08
                    and then String_Type_Definition /= Null_Iir
                    and then (Get_Kind (Element_Type)
                                = Iir_Kind_Enumeration_Type_Definition)
                    and then Get_Only_Characters_Flag (Element_Type)
                  then
                     Add_Operation (Name_To_String,
                                    Iir_Predefined_Array_Char_To_String,
                                    Unary_Chain,
                                    String_Type_Definition);
                  end if;
               end if;
            end;

         when Iir_Kind_Access_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Access_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Access_Inequality);
            declare
               Deallocate_Proc: Iir_Procedure_Declaration;
               Var_Interface: Iir_Interface_Variable_Declaration;
            begin
               Deallocate_Proc :=
                 Create_Iir (Iir_Kind_Procedure_Declaration);
               Location_Copy (Deallocate_Proc, Decl);
               Set_Identifier (Deallocate_Proc, Std_Names.Name_Deallocate);
               Set_Implicit_Definition
                 (Deallocate_Proc, Iir_Predefined_Deallocate);
               Set_Parent (Deallocate_Proc, Get_Parent (Decl));

               Var_Interface :=
                 Create_Iir (Iir_Kind_Interface_Variable_Declaration);
               Location_Copy (Var_Interface, Decl);
               Set_Identifier (Var_Interface, Std_Names.Name_P);
               Set_Parent (Var_Interface, Deallocate_Proc);
               Set_Type (Var_Interface, Type_Definition);
               Set_Mode (Var_Interface, Iir_Inout_Mode);
               --Set_Purity_State (Deallocate_Proc, Impure);
               Set_Wait_State (Deallocate_Proc, False);
               Set_Visible_Flag (Deallocate_Proc, True);

               Set_Interface_Declaration_Chain
                 (Deallocate_Proc, Var_Interface);
               Compute_Subprogram_Hash (Deallocate_Proc);
               Insert_Incr (Last, Deallocate_Proc);
            end;

         when Iir_Kind_Record_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Record_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Record_Inequality);

         when Iir_Kind_Integer_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Integer_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Integer_Inequality);
            Add_Relational (Name_Op_Greater, Iir_Predefined_Integer_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Integer_Greater_Equal);
            Add_Relational (Name_Op_Less, Iir_Predefined_Integer_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Integer_Less_Equal);

            Add_Binary (Name_Op_Plus, Iir_Predefined_Integer_Plus);
            Add_Binary (Name_Op_Minus, Iir_Predefined_Integer_Minus);

            Add_Unary (Name_Op_Minus, Iir_Predefined_Integer_Negation);
            Add_Unary (Name_Op_Plus, Iir_Predefined_Integer_Identity);

            Add_Binary (Name_Op_Mul, Iir_Predefined_Integer_Mul);
            Add_Binary (Name_Op_Div, Iir_Predefined_Integer_Div);
            Add_Binary (Name_Mod, Iir_Predefined_Integer_Mod);
            Add_Binary (Name_Rem, Iir_Predefined_Integer_Rem);

            Add_Unary (Name_Abs, Iir_Predefined_Integer_Absolute);

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain
                 (Inter_Chain,
                  Create_Anonymous_Interface (Integer_Type_Definition));
               Add_Operation (Name_Op_Exp, Iir_Predefined_Integer_Exp,
                              Inter_Chain, Type_Definition);
            end;

            if Vhdl_Std >= Vhdl_08 then
               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Integer_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Integer_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Integer_To_String);
               end if;
            end if;

         when Iir_Kind_Floating_Type_Definition =>
            Add_Relational
              (Name_Op_Equality, Iir_Predefined_Floating_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Floating_Inequality);
            Add_Relational
              (Name_Op_Greater, Iir_Predefined_Floating_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Floating_Greater_Equal);
            Add_Relational
              (Name_Op_Less, Iir_Predefined_Floating_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Floating_Less_Equal);

            Add_Binary (Name_Op_Plus, Iir_Predefined_Floating_Plus);
            Add_Binary (Name_Op_Minus, Iir_Predefined_Floating_Minus);

            Add_Unary (Name_Op_Minus, Iir_Predefined_Floating_Negation);
            Add_Unary (Name_Op_Plus, Iir_Predefined_Floating_Identity);

            Add_Binary (Name_Op_Mul, Iir_Predefined_Floating_Mul);
            Add_Binary (Name_Op_Div, Iir_Predefined_Floating_Div);

            Add_Unary (Name_Abs, Iir_Predefined_Floating_Absolute);

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain
                 (Inter_Chain,
                  Create_Anonymous_Interface (Integer_Type_Definition));
               Add_Operation (Name_Op_Exp, Iir_Predefined_Floating_Exp,
                              Inter_Chain, Type_Definition);
            end;

            if Vhdl_Std >= Vhdl_08 then
               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Floating_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Floating_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Floating_To_String);
               end if;
            end if;

         when Iir_Kind_Physical_Type_Definition =>
            Add_Relational
              (Name_Op_Equality, Iir_Predefined_Physical_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Physical_Inequality);
            Add_Relational
              (Name_Op_Greater, Iir_Predefined_Physical_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Physical_Greater_Equal);
            Add_Relational
              (Name_Op_Less, Iir_Predefined_Physical_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Physical_Less_Equal);

            Add_Binary (Name_Op_Plus, Iir_Predefined_Physical_Plus);
            Add_Binary (Name_Op_Minus, Iir_Predefined_Physical_Minus);

            Add_Unary (Name_Op_Minus, Iir_Predefined_Physical_Negation);
            Add_Unary (Name_Op_Plus, Iir_Predefined_Physical_Identity);

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain
                 (Inter_Chain,
                  Create_Anonymous_Interface (Integer_Type_Definition));
               Add_Operation (Name_Op_Mul, Iir_Predefined_Physical_Integer_Mul,
                              Inter_Chain, Type_Definition);
               Add_Operation (Name_Op_Div, Iir_Predefined_Physical_Integer_Div,
                              Inter_Chain, Type_Definition);
            end;

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain :=
                 Create_Anonymous_Interface (Integer_Type_Definition);
               Set_Chain (Inter_Chain, Unary_Chain);
               Add_Operation (Name_Op_Mul, Iir_Predefined_Integer_Physical_Mul,
                              Inter_Chain, Type_Definition);
            end;

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain (Inter_Chain,
                          Create_Anonymous_Interface (Real_Type_Definition));
               Add_Operation (Name_Op_Mul, Iir_Predefined_Physical_Real_Mul,
                              Inter_Chain, Type_Definition);
               Add_Operation (Name_Op_Div, Iir_Predefined_Physical_Real_Div,
                              Inter_Chain, Type_Definition);
            end;

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain :=
                 Create_Anonymous_Interface (Real_Type_Definition);
               Set_Chain (Inter_Chain, Unary_Chain);
               Add_Operation (Name_Op_Mul, Iir_Predefined_Real_Physical_Mul,
                              Inter_Chain, Type_Definition);
            end;
            Add_Operation (Name_Op_Div, Iir_Predefined_Physical_Physical_Div,
                           Binary_Chain,
                           Std_Package.Convertible_Integer_Type_Definition);

            Add_Unary (Name_Abs, Iir_Predefined_Physical_Absolute);

            if Vhdl_Std >= Vhdl_08 then
               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Physical_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Physical_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Physical_To_String);
               end if;
            end if;

         when Iir_Kind_File_Type_Definition =>
            Create_Implicit_File_Primitives (Decl, Type_Definition);

         when Iir_Kind_Protected_Type_Declaration =>
            null;

         when others =>
            Error_Kind ("create_predefined_operations", Type_Definition);
      end case;

      if not Is_Std_Standard then
         return;
      end if;
      if Decl = Std_Package.Boolean_Type_Declaration then
         Add_Binary (Name_And, Iir_Predefined_Boolean_And);
         Add_Binary (Name_Or, Iir_Predefined_Boolean_Or);
         Add_Binary (Name_Nand, Iir_Predefined_Boolean_Nand);
         Add_Binary (Name_Nor, Iir_Predefined_Boolean_Nor);
         Add_Binary (Name_Xor, Iir_Predefined_Boolean_Xor);
         if Flags.Vhdl_Std > Vhdl_87 then
            Add_Binary (Name_Xnor, Iir_Predefined_Boolean_Xnor);
         end if;
         Add_Unary (Name_Not, Iir_Predefined_Boolean_Not);
      elsif Decl = Std_Package.Bit_Type_Declaration then
         Add_Binary (Name_And, Iir_Predefined_Bit_And);
         Add_Binary (Name_Or, Iir_Predefined_Bit_Or);
         Add_Binary (Name_Nand, Iir_Predefined_Bit_Nand);
         Add_Binary (Name_Nor, Iir_Predefined_Bit_Nor);
         Add_Binary (Name_Xor, Iir_Predefined_Bit_Xor);
         if Flags.Vhdl_Std > Vhdl_87 then
            Add_Binary (Name_Xnor, Iir_Predefined_Bit_Xnor);
         end if;
         Add_Unary (Name_Not, Iir_Predefined_Bit_Not);
         if Flags.Vhdl_Std >= Vhdl_08 then
            Add_Binary (Name_Op_Match_Equality,
                        Iir_Predefined_Bit_Match_Equality);
            Add_Binary (Name_Op_Match_Inequality,
                        Iir_Predefined_Bit_Match_Inequality);
            Add_Binary (Name_Op_Match_Less,
                        Iir_Predefined_Bit_Match_Less);
            Add_Binary (Name_Op_Match_Less_Equal,
                        Iir_Predefined_Bit_Match_Less_Equal);
            Add_Binary (Name_Op_Match_Greater,
                        Iir_Predefined_Bit_Match_Greater);
            Add_Binary (Name_Op_Match_Greater_Equal,
                        Iir_Predefined_Bit_Match_Greater_Equal);

            --  LRM08 9.2.9 Condition operator
            --  The unary operator ?? is predefined for type BIT defined in
            --  package STANDARD.
            Add_Operation (Name_Op_Condition, Iir_Predefined_Bit_Condition,
                           Unary_Chain, Std_Package.Boolean_Type_Definition);

         end if;
      elsif Decl = Std_Package.Universal_Real_Type_Declaration then
         declare
            Inter_Chain : Iir;
         begin
            Inter_Chain := Create_Anonymous_Interface (Type_Definition);
            Set_Chain
              (Inter_Chain,
               Create_Anonymous_Interface (Universal_Integer_Type_Definition));
            Add_Operation (Name_Op_Mul, Iir_Predefined_Universal_R_I_Mul,
                           Inter_Chain, Type_Definition);
            Add_Operation (Name_Op_Div, Iir_Predefined_Universal_R_I_Div,
                           Inter_Chain, Type_Definition);
         end;

         declare
            Inter_Chain : Iir;
         begin
            Inter_Chain :=
              Create_Anonymous_Interface (Universal_Integer_Type_Definition);
            Set_Chain (Inter_Chain, Unary_Chain);
            Add_Operation (Name_Op_Mul, Iir_Predefined_Universal_I_R_Mul,
                           Inter_Chain, Type_Definition);
         end;
      end if;
   end Create_Implicit_Operations;

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
         Set_Base_Type (Def, Def);
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
               Set_Designated_Type (Ref, Def);
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

      if not Is_Anonymous_Type_Definition (Def) then
         --  There is no added constraints and therefore the subtype
         --  declaration is in fact an alias of the type.  Create a copy so
         --  that it has its own type declarator.
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

   procedure Sem_Object_Declaration (Decl: Iir; Parent : Iir; Last_Decl : Iir)
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

         Default_Value := Get_Default_Value (Decl);
         if Default_Value /= Null_Iir then
            Default_Value := Sem_Expression (Default_Value, Atype);
            if Default_Value = Null_Iir then
               Default_Value :=
                 Create_Error_Expr (Get_Default_Value (Decl), Atype);
            end if;
            Check_Read (Default_Value);
            Default_Value := Eval_Expr_Check_If_Static (Default_Value, Atype);
         end if;
      else
         Default_Value := Get_Default_Value (Last_Decl);
         if Is_Valid (Default_Value) then
            Set_Is_Ref (Decl, True);
         end if;
         Atype := Get_Type (Last_Decl);
      end if;

      Set_Type (Decl, Atype);
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
               if Get_Kind (Parent) /= Iir_Kind_Package_Declaration then
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
           | Iir_Kind_Signal_Declaration =>
            --  LRM93 3.2.1.1 / LRM08 5.3.2.2
            --  For a variable or signal declared by an object declaration, the
            --  subtype indication of the corressponding object declaration
            --  must define a constrained array subtype.
            if not Is_Fully_Constrained_Type (Atype) then
               Error_Msg_Sem
                 (+Decl,
                  "declaration of %n with unconstrained %n is not allowed",
                  (+Decl, +Atype));
               if Default_Value /= Null_Iir then
                  Error_Msg_Sem (+Decl, "(even with a default value)");
               end if;
            end if;

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
         Parent : Iir;
         Spec : Iir;
      begin
         Parent := Get_Parent (Decl);
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

      A_Type := Sem_Type_Mark (Get_Type_Mark (Decl));
      Set_Type_Mark (Decl, A_Type);
      A_Type := Get_Type (A_Type);
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
                  Error_Msg_Sem
                    (+Sig,
                     "cannot resolve signature, many matching subprograms:",
                     Cont => True);
                  Error_Msg_Sem (+Res, "found: %n", (1 => +Res), Cont => True);
               end if;
               if Error then
                  Error_Msg_Sem (+El, "found: %n", +El);
               end if;
            end if;
            Next (Ov_It);
         end loop;

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
         when Iir_Kinds_Object_Declaration =>
            raise Internal_Error;
         when Iir_Kind_Attribute_Declaration
           | Iir_Kind_Component_Declaration =>
            null;
         when Iir_Kind_Library_Declaration =>
            --  Not explicitly allowed before vhdl-08.
            null;
         when Iir_Kind_Terminal_Declaration =>
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
      if Get_Kind (Name) = Iir_Kind_Signature then
         Sig := Name;
         Name := Get_Signature_Prefix (Sig);
         Sem_Name (Name);
         Set_Signature_Prefix (Sig, Name);
      else
         Sem_Name (Name);
         Sig := Null_Iir;
      end if;

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
      use Tokens;

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

   function Sem_Scalar_Nature_Definition (Def : Iir; Decl : Iir) return Iir
   is
      function Sem_Scalar_Nature_Typemark (T : Iir; Name : String) return Iir
      is
         Res : Iir;
      begin
         Res := Sem_Type_Mark (T);
         Res := Get_Type (Res);
         if Is_Error (Res) then
            return Real_Type_Definition;
         end if;
         --  LRM93 3.5.1
         --  The type marks must denote floating point types
         case Get_Kind (Res) is
            when Iir_Kind_Floating_Subtype_Definition
              | Iir_Kind_Floating_Type_Definition =>
               return Res;
            when others =>
               Error_Msg_Sem (+T, Name & "type must be a floating point type");
               return Real_Type_Definition;
         end case;
      end Sem_Scalar_Nature_Typemark;

      Tm : Iir;
      Ref : Iir;
   begin
      Tm := Get_Across_Type (Def);
      Tm := Sem_Scalar_Nature_Typemark (Tm, "across");
      Set_Across_Type (Def, Tm);

      Tm := Get_Through_Type (Def);
      Tm := Sem_Scalar_Nature_Typemark (Tm, "through");
      Set_Through_Type (Def, Tm);

      --  Declare the reference
      Ref := Get_Reference (Def);
      Set_Nature (Ref, Def);
      Set_Chain (Ref, Get_Chain (Decl));
      Set_Chain (Decl, Ref);

      return Def;
   end Sem_Scalar_Nature_Definition;

   function Sem_Nature_Definition (Def : Iir; Decl : Iir) return Iir
   is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Scalar_Nature_Definition =>
            return Sem_Scalar_Nature_Definition (Def, Decl);
         when others =>
            Error_Kind ("sem_nature_definition", Def);
            return Null_Iir;
      end case;
   end Sem_Nature_Definition;

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

   procedure Sem_Terminal_Declaration (Decl : Iir; Last_Decl : Iir)
   is
      Def, Nature : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      Def := Get_Nature (Decl);

      if Def = Null_Iir then
         Nature := Get_Nature (Last_Decl);
      else
         Nature := Sem_Subnature_Indication (Def);
      end if;

      if Nature /= Null_Iir then
         Set_Nature (Decl, Nature);
         Sem_Scopes.Name_Visible (Decl);
      end if;
   end Sem_Terminal_Declaration;

   procedure Sem_Branch_Quantity_Declaration (Decl : Iir; Last_Decl : Iir)
   is
      Plus_Name : Iir;
      Minus_Name : Iir;
      Branch_Type : Iir;
      Value : Iir;
      Is_Second : Boolean;
   begin
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      Plus_Name := Get_Plus_Terminal (Decl);
      if Plus_Name = Null_Iir then
         --  List of identifier.
         Is_Second := True;
         Plus_Name := Get_Plus_Terminal (Last_Decl);
         Minus_Name := Get_Minus_Terminal (Last_Decl);
         Value := Get_Default_Value (Last_Decl);
      else
         Is_Second := False;
         Plus_Name := Sem_Terminal_Name (Plus_Name);
         Minus_Name := Get_Minus_Terminal (Decl);
         if Minus_Name /= Null_Iir then
            Minus_Name := Sem_Terminal_Name (Minus_Name);
         end if;
         Value := Get_Default_Value (Decl);
      end if;
      Set_Plus_Terminal (Decl, Plus_Name);
      Set_Minus_Terminal (Decl, Minus_Name);
      case Get_Kind (Decl) is
         when Iir_Kind_Across_Quantity_Declaration =>
            Branch_Type := Get_Across_Type (Get_Nature (Plus_Name));
         when Iir_Kind_Through_Quantity_Declaration =>
            Branch_Type := Get_Through_Type (Get_Nature (Plus_Name));
         when others =>
            raise Program_Error;
      end case;
      Set_Type (Decl, Branch_Type);

      if not Is_Second and then Value /= Null_Iir then
         Value := Sem_Expression (Value, Branch_Type);
      end if;
      Set_Default_Value (Decl, Value);

      --  TODO: tolerance

      Sem_Scopes.Name_Visible (Decl);
   end Sem_Branch_Quantity_Declaration;

   procedure Sem_Declaration_Chain (Parent : Iir)
   is
      Decl : Iir;
      Attr_Spec_Chain : Iir;

      --  New declaration chain (declarations like implicit signals may be
      --  added, some like aliases may mutate).
      Last_Decl : Iir;

      --  Used for list of identifiers in object declarations to get the type
      --  and default value for the following declarations.
      Last_Obj_Decl : Iir;

      --  If IS_GLOBAL is set, then declarations may be seen outside of unit.
      --  This must be set for entities and packages (except when
      --   Flags.Flag_Whole_Analyze is set).
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
      Last_Obj_Decl := Null_Iir;

      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Type_Declaration
              | Iir_Kind_Anonymous_Type_Declaration =>
               Sem_Type_Declaration (Decl, Is_Global);
            when Iir_Kind_Subtype_Declaration =>
               Sem_Subtype_Declaration (Decl, Is_Global);
            when Iir_Kind_Signal_Declaration =>
               Sem_Object_Declaration (Decl, Parent, Last_Obj_Decl);
               Last_Obj_Decl := Decl;
            when Iir_Kind_Constant_Declaration =>
               Sem_Object_Declaration (Decl, Parent, Last_Obj_Decl);
               Last_Obj_Decl := Decl;
            when Iir_Kind_Variable_Declaration =>
               Sem_Object_Declaration (Decl, Parent, Last_Obj_Decl);
               Last_Obj_Decl := Decl;
            when Iir_Kind_File_Declaration =>
               Sem_File_Declaration (Decl, Last_Obj_Decl);
               Last_Obj_Decl := Decl;
            when Iir_Kind_Attribute_Declaration =>
               Sem_Attribute_Declaration (Decl);
            when Iir_Kind_Attribute_Specification =>
               Sem_Attribute_Specification (Decl, Parent);
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
            when Iir_Kind_Terminal_Declaration =>
               Sem_Terminal_Declaration (Decl, Last_Obj_Decl);
               Last_Obj_Decl := Decl;
            when Iir_Kind_Across_Quantity_Declaration
              | Iir_Kind_Through_Quantity_Declaration =>
               Sem_Branch_Quantity_Declaration (Decl, Last_Obj_Decl);
               Last_Obj_Decl := Decl;

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
         Insert_Pending_Implicit_Declarations (Parent, Last_Decl);

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
               --  FIXME: create a second level of warning.
               null;
            when  Iir_Kind_Generate_Statement_Body =>
               --  Might be used in a configuration.
               null;
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
                     Warning_Msg_Sem (Warnid_Unused, +El,
                                      "%n is never referenced", +El);
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
end Sem_Decls;
