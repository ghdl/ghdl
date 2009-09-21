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
with Iir_Chains;
with Evaluation; use Evaluation;
with Name_Table;
with Iirs_Utils; use Iirs_Utils;
with Sem; use Sem;
with Sem_Expr; use Sem_Expr;
with Sem_Scopes; use Sem_Scopes;
with Sem_Names; use Sem_Names;
with Sem_Specs; use Sem_Specs;
with Sem_Types; use Sem_Types;
with Xrefs; use Xrefs;
use Iir_Chains;

package body Sem_Decls is
   --  Emit an error if the type of DECL is a file type, access type,
   --  protected type or if a subelement of DECL is an access type.
   procedure Check_Signal_Type (Decl : Iir)
   is
      Decl_Type : Iir;
   begin
      Decl_Type := Get_Type (Decl);
      if Get_Signal_Type_Flag (Decl_Type) = False then
         Error_Msg_Sem ("type of " & Disp_Node (Decl)
                        & " cannot be " & Disp_Node (Decl_Type), Decl);
         case Get_Kind (Decl_Type) is
            when Iir_Kind_File_Type_Definition =>
               null;
            when Iir_Kind_Protected_Type_Declaration =>
               null;
            when Iir_Kind_Access_Type_Definition
              | Iir_Kind_Access_Subtype_Definition =>
               null;
            when Iir_Kinds_Array_Type_Definition
              | Iir_Kind_Record_Type_Definition
              | Iir_Kind_Record_Subtype_Definition =>
               Error_Msg_Sem ("(" & Disp_Node (Decl_Type)
                              & " has an access subelement)", Decl);
            when others =>
               Error_Kind ("check_signal_type", Decl_Type);
         end case;
      end if;
   end Check_Signal_Type;

   procedure Sem_Interface_Chain (Interface_Chain: Iir;
                                  Interface_Kind : Interface_Kind_Type)
   is
      El, A_Type: Iir;
      Proxy : Iir_Proxy;
      Default_Value: Iir;
   begin
      El := Interface_Chain;
      while El /= Null_Iir loop
         --  Avoid the reanalysed duplicated types.
         --  This is not an optimization, since the unanalysed type must have
         --  been freed.
         A_Type := Get_Type (El);
         if Get_Kind (A_Type) = Iir_Kind_Proxy then
            Proxy := A_Type;
            A_Type := Get_Type (Get_Proxy (Proxy));
            Default_Value := Get_Default_Value (Get_Proxy (Proxy));
            Free_Iir (Proxy);
         else
            A_Type := Sem_Subtype_Indication (A_Type);
            Default_Value := Get_Default_Value (El);
            if Default_Value /= Null_Iir and then A_Type /= Null_Iir then
               Deferred_Constant_Allowed := True;
               Default_Value := Sem_Expression (Default_Value, A_Type);
               Default_Value :=
                 Eval_Expr_Check_If_Static (Default_Value, A_Type);
               Deferred_Constant_Allowed := False;
               Check_Read (Default_Value);
            end if;
         end if;

         Set_Base_Name (El, El);
         Set_Name_Staticness (El, Locally);
         Xref_Decl (El);

         if A_Type /= Null_Iir then
            Set_Type (El, A_Type);

            if Get_Kind (El) = Iir_Kind_Signal_Interface_Declaration then
               case Get_Signal_Kind (El) is
                  when Iir_No_Signal_Kind =>
                     null;
                  when Iir_Bus_Kind =>
                     --  FIXME: where this test came from ?
                     --  FIXME: from 4.3.1.2 ?
                     if False
                       and
                       (Get_Kind (A_Type) not in Iir_Kinds_Subtype_Definition
                        or else Get_Resolution_Function (A_Type) = Null_Iir)
                     then
                        Error_Msg_Sem
                          (Disp_Node (A_Type)
                           & " of guarded " & Disp_Node (El)
                           & " is not resolved", El);
                     end if;

                     --  LRM 2.1.1.2  Signal parameter
                     --  It is an error if the declaration of a formal signal
                     --  parameter includes the reserved word BUS.
                     if Flags.Vhdl_Std >= Vhdl_93
                       and then Interface_Kind in Parameter_Kind_Subtype
                     then
                        Error_Msg_Sem ("signal parameter can't be of kind bus",
                                       El);
                     end if;
                  when Iir_Register_Kind =>
                     Error_Msg_Sem
                       ("interface signal can't be of kind register", El);
               end case;
               Set_Type_Has_Signal (A_Type);
            end if;

            case Get_Kind (El) is
               when Iir_Kind_Constant_Interface_Declaration
                 | Iir_Kind_Signal_Interface_Declaration =>
                  --  LRM 4.3.2  Interface declarations
                  --  For an interface constant declaration or an interface
                  --  signal declaration, the subtype indication must define
                  --  a subtype that is neither a file type, an access type,
                  --  nor a protected type.  Moreover, the subtype indication
                  --  must not denote a composite type with a subelement that
                  --  is a file type, an access type, or a protected type.
                  Check_Signal_Type (El);
               when Iir_Kind_Variable_Interface_Declaration =>
                  case Get_Kind (Get_Base_Type (A_Type)) is
                     when Iir_Kind_File_Type_Definition =>
                        if Flags.Vhdl_Std >= Vhdl_93 then
                           Error_Msg_Sem ("variable formal type can't be a "
                                          & "file type (vhdl 93)", El);
                        end if;
                     when Iir_Kind_Protected_Type_Declaration =>
                        --  LRM 2.1.1.1  Constant and variable parameters
                        --  It is an error if the mode of the parameter is
                        --  other that INOUT.
                        if Get_Mode (El) /= Iir_Inout_Mode then
                           Error_Msg_Sem
                             ("parameter of protected type must be inout", El);
                        end if;
                     when others =>
                        null;
                  end case;
               when Iir_Kind_File_Interface_Declaration =>
                  if Get_Kind (Get_Base_Type (A_Type))
                    /= Iir_Kind_File_Type_Definition
                  then
                     Error_Msg_Sem
                       ("file formal type must be a file type", El);
                  end if;
               when others =>
                  --  El is not an interface.
                  raise Internal_Error;
            end case;

            if Default_Value /= Null_Iir then
               Set_Default_Value (El, Default_Value);

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
               case Get_Kind (El) is
                  when Iir_Kind_Constant_Interface_Declaration =>
                     null;
                  when Iir_Kind_Signal_Interface_Declaration =>
                     if Get_Mode (El) = Iir_Linkage_Mode then
                        Error_Msg_Sem
                          ("default expression not allowed for linkage port",
                           El);
                     elsif Interface_Kind in Parameter_Kind_Subtype then
                        Error_Msg_Sem ("default expression not allowed"
                                       & " for signal parameter", El);
                     end if;
                  when Iir_Kind_Variable_Interface_Declaration =>
                     if Get_Mode (El) /= Iir_In_Mode then
                        Error_Msg_Sem ("default expression not allowed for"
                                       & " out/inout variable parameter", El);
                     elsif Get_Kind (A_Type)
                       = Iir_Kind_Protected_Type_Declaration
                     then
                        Error_Msg_Sem
                          ("default expression not allowed for"
                           & " variable parameter of protected type", El);
                     end if;
                  when Iir_Kind_File_Interface_Declaration =>
                     raise Internal_Error;
                  when others =>
                     null;
               end case;
            end if;
         else
            Set_Type (El, Error_Type);
         end if;

         Sem_Scopes.Add_Name (El);

         --  By default, interface are not static.
         --  This may be changed just below.
         Set_Expr_Staticness (El, None);

         case Interface_Kind is
            when Interface_Generic =>
               --  LRM93 1.1.1
               --  The generic list in the formal generic clause defines
               --  generic constants whose values may be determined by the
               --  environment.
               if Get_Kind (El) /= Iir_Kind_Constant_Interface_Declaration then
                  Error_Msg_Sem
                    ("generic " & Disp_Node (El) & " must be a constant",
                     El);
               else
                  --   LRM93 7.4.2 (Globally static primaries)
                  --   3. a generic constant.
                  Set_Expr_Staticness (El, Globally);
               end if;
            when Interface_Port =>
               if Get_Kind (El) /= Iir_Kind_Signal_Interface_Declaration then
                  Error_Msg_Sem
                    ("port " & Disp_Node (El) & " must be a signal", El);
               end if;
            when Interface_Procedure
              | Interface_Function =>
               if Get_Kind (El) = Iir_Kind_Variable_Interface_Declaration
                 and then Interface_Kind = Interface_Function
               then
                  Error_Msg_Sem ("variable interface parameter are not "
                                 & "allowed for a function (use a constant)",
                                 El);
               end if;

               --  By default, we suppose a subprogram read the activity of
               --  a signal.
               --  This will be adjusted when the body is analyzed.
               if Get_Kind (El) = Iir_Kind_Signal_Interface_Declaration
                 and then Get_Mode (El) in Iir_In_Modes
               then
                  Set_Has_Active_Flag (El, True);
               end if;

               case Get_Mode (El) is
                  when Iir_Unknown_Mode =>
                     raise Internal_Error;
                  when Iir_In_Mode =>
                     null;
                  when Iir_Inout_Mode
                    | Iir_Out_Mode =>
                     if Interface_Kind = Interface_Function
                       and then
                       Get_Kind (El) /= Iir_Kind_File_Interface_Declaration
                     then
                        Error_Msg_Sem ("mode of a function parameter cannot "
                                       & "be inout or out", El);
                     end if;
                  when Iir_Buffer_Mode
                    | Iir_Linkage_Mode =>
                     Error_Msg_Sem ("buffer or linkage mode is not allowed "
                                    & "for a subprogram parameter", El);
               end case;
         end case;
         El := Get_Chain (El);
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
      El := Interface_Chain;
      while El /= Null_Iir loop
         Name_Visible (El);
         El := Get_Chain (El);
      end loop;
   end Sem_Interface_Chain;

   function Is_One_Dimensional (Array_Def : Iir) return Boolean
   is
   begin
      return Get_Nbr_Elements (Get_Index_Subtype_List (Array_Def)) = 1;
   end Is_One_Dimensional;

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
      if not Is_One_Dimensional (Def) then
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
      Type_Mark: Iir;
      Proc: Iir_Implicit_Procedure_Declaration;
      Func: Iir_Implicit_Function_Declaration;
      Inter: Iir;
      Loc : Location_Type;
      File_Interface_Kind : Iir_Kind;
      Last_Interface : Iir;
      Last : Iir;
   begin
      Last := Decl;
      Type_Mark := Get_Type_Mark (Type_Definition);
      Loc := Get_Location (Decl);

      if Flags.Vhdl_Std >= Vhdl_93c then
         for I in 1 .. 2 loop
            --  Create the implicit file_open (form 1) declaration.
            --  Create the implicit file_open (form 2) declaration.
            Proc := Create_Iir (Iir_Kind_Implicit_Procedure_Declaration);
            Set_Location (Proc, Loc);
            Set_Parent (Proc, Get_Parent (Decl));
            Set_Identifier (Proc, Std_Names.Name_File_Open);
            Set_Type_Reference (Proc, Decl);
            Build_Init (Last_Interface);
            case I is
               when 1 =>
                  Set_Implicit_Definition (Proc, Iir_Predefined_File_Open);
               when 2 =>
                  Set_Implicit_Definition (Proc,
                                           Iir_Predefined_File_Open_Status);
                  --  status : out file_open_status.
                  Inter :=
                    Create_Iir (Iir_Kind_Variable_Interface_Declaration);
                  Set_Location (Inter, Loc);
                  Set_Identifier (Inter, Std_Names.Name_Status);
                  Set_Type (Inter,
                            Std_Package.File_Open_Status_Type_Definition);
                  Set_Mode (Inter, Iir_Out_Mode);
                  Set_Base_Name (Inter, Inter);
                  Append (Last_Interface, Proc, Inter);
            end case;
            --  File F : FT
            Inter := Create_Iir (Iir_Kind_File_Interface_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_F);
            Set_Type (Inter, Type_Definition);
            Set_Mode (Inter, Iir_Inout_Mode);
            Set_Base_Name (Inter, Inter);
            Append (Last_Interface, Proc, Inter);
            --  External_Name : in STRING
            Inter := Create_Iir (Iir_Kind_Constant_Interface_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_External_Name);
            Set_Type (Inter, Std_Package.String_Type_Definition);
            Set_Mode (Inter, Iir_In_Mode);
            Set_Base_Name (Inter, Inter);
            Append (Last_Interface, Proc, Inter);
            --  Open_Kind : in File_Open_Kind := Read_Mode.
            Inter := Create_Iir (Iir_Kind_Constant_Interface_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_Open_Kind);
            Set_Type (Inter, Std_Package.File_Open_Kind_Type_Definition);
            Set_Mode (Inter, Iir_In_Mode);
            Set_Base_Name (Inter, Inter);
            Set_Default_Value (Inter,
                               Std_Package.File_Open_Kind_Read_Mode);
            Append (Last_Interface, Proc, Inter);
            Compute_Subprogram_Hash (Proc);
            -- Add it to the list.
            Insert_Incr (Last, Proc);
         end loop;

         --  Create the implicit file_close declaration.
         Proc := Create_Iir (Iir_Kind_Implicit_Procedure_Declaration);
         Set_Identifier (Proc, Std_Names.Name_File_Close);
         Set_Location (Proc, Loc);
         Set_Parent (Proc, Get_Parent (Decl));
         Set_Implicit_Definition (Proc, Iir_Predefined_File_Close);
         Set_Type_Reference (Proc, Decl);
         Build_Init (Last_Interface);
         Inter := Create_Iir (Iir_Kind_File_Interface_Declaration);
         Set_Identifier (Inter, Std_Names.Name_F);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Type_Definition);
         Set_Mode (Inter, Iir_Inout_Mode);
         Set_Base_Name (Inter, Inter);
         Append (Last_Interface, Proc, Inter);
         Compute_Subprogram_Hash (Proc);
         -- Add it to the list.
         Insert_Incr (Last, Proc);
      end if;

      if Flags.Vhdl_Std = Vhdl_87 then
         File_Interface_Kind := Iir_Kind_Variable_Interface_Declaration;
      else
         File_Interface_Kind := Iir_Kind_File_Interface_Declaration;
      end if;

      -- Create the implicit procedure read declaration.
      Proc := Create_Iir (Iir_Kind_Implicit_Procedure_Declaration);
      Set_Identifier (Proc, Std_Names.Name_Read);
      Set_Location (Proc, Loc);
      Set_Parent (Proc, Get_Parent (Decl));
      Set_Type_Reference (Proc, Decl);
      Build_Init (Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Base_Name (Inter, Inter);
      Append (Last_Interface, Proc, Inter);
      Inter := Create_Iir (Iir_Kind_Variable_Interface_Declaration);
      Set_Identifier (Inter, Std_Names.Name_Value);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Mark);
      Set_Mode (Inter, Iir_Out_Mode);
      Set_Base_Name (Inter, Inter);
      Append (Last_Interface, Proc, Inter);
      if Get_Kind (Type_Mark) in Iir_Kinds_Array_Type_Definition
        and then Get_Constraint_State (Type_Mark) /= Fully_Constrained
      then
         Inter := Create_Iir (Iir_Kind_Variable_Interface_Declaration);
         Set_Identifier (Inter, Std_Names.Name_Length);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Std_Package.Natural_Subtype_Definition);
         Set_Mode (Inter, Iir_Out_Mode);
         Set_Base_Name (Inter, Inter);
         Append (Last_Interface, Proc, Inter);
         Set_Implicit_Definition (Proc, Iir_Predefined_Read_Length);
      else
         Set_Implicit_Definition (Proc, Iir_Predefined_Read);
      end if;
      Compute_Subprogram_Hash (Proc);
      -- Add it to the list.
      Insert_Incr (Last, Proc);

      -- Create the implicit procedure write declaration.
      Proc := Create_Iir (Iir_Kind_Implicit_Procedure_Declaration);
      Set_Identifier (Proc, Std_Names.Name_Write);
      Set_Location (Proc, Loc);
      Set_Parent (Proc, Get_Parent (Decl));
      Set_Type_Reference (Proc, Decl);
      Build_Init (Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_Out_Mode);
      Set_Base_Name (Inter, Inter);
      Set_Name_Staticness (Inter, Locally);
      Set_Expr_Staticness (Inter, None);
      Append (Last_Interface, Proc, Inter);
      Inter := Create_Iir (Iir_Kind_Constant_Interface_Declaration);
      Set_Identifier (Inter, Std_Names.Name_Value);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Mark);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Base_Name (Inter, Inter);
      Append (Last_Interface, Proc, Inter);
      Set_Implicit_Definition (Proc, Iir_Predefined_Write);
      Compute_Subprogram_Hash (Proc);
      -- Add it to the list.
      Insert_Incr (Last, Proc);

      -- Create the implicit function endfile declaration.
      Func := Create_Iir (Iir_Kind_Implicit_Function_Declaration);
      Set_Identifier (Func, Std_Names.Name_Endfile);
      Set_Location (Func, Loc);
      Set_Parent (Proc, Get_Parent (Decl));
      Set_Type_Reference (Proc, Decl);
      Build_Init (Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Base_Name (Inter, Inter);
      Append (Last_Interface, Func, Inter);
      Set_Return_Type (Func, Std_Package.Boolean_Type_Definition);
      Set_Implicit_Definition (Func, Iir_Predefined_Endfile);
      Compute_Subprogram_Hash (Func);
      -- Add it to the list.
      Insert_Incr (Last, Func);
   end Create_Implicit_File_Primitives;

   function Create_Anonymous_Interface (Atype : Iir)
     return Iir_Constant_Interface_Declaration
   is
      Inter : Iir_Constant_Interface_Declaration;
   begin
      Inter := Create_Iir (Iir_Kind_Constant_Interface_Declaration);
      Location_Copy (Inter, Atype);
      Set_Identifier (Inter, Null_Identifier);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Type (Inter, Atype);
      Set_Base_Name (Inter, Inter);
      return Inter;
   end Create_Anonymous_Interface;

   procedure Create_Implicit_Operations
     (Decl : Iir; Is_Std_Standard : Boolean := False)
   is
      use Std_Names;
      Binary_Chain : Iir;
      Unary_Chain : Iir;
      Type_Definition : Iir;
      Last : Iir;

      procedure Add_Operation
        (Name : Name_Id;
         Def : Iir_Predefined_Functions;
         Interface_Chain : Iir;
         Return_Type : Iir)
      is
         Operation : Iir_Implicit_Function_Declaration;
      begin
         Operation := Create_Iir (Iir_Kind_Implicit_Function_Declaration);
         Location_Copy (Operation, Decl);
         Set_Parent (Operation, Get_Parent (Decl));
         Set_Interface_Declaration_Chain (Operation, Interface_Chain);
         Set_Type_Reference (Operation, Decl);
         Set_Return_Type (Operation, Return_Type);
         Set_Implicit_Definition (Operation, Def);
         Set_Identifier (Operation, Name);
         Compute_Subprogram_Hash (Operation);
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

      procedure Add_Shift_Operators
      is
         Inter_Chain : Iir_Constant_Interface_Declaration;
         Inter_Int : Iir;
      begin
         Inter_Chain := Create_Anonymous_Interface (Type_Definition);

         Inter_Int := Create_Iir (Iir_Kind_Constant_Interface_Declaration);
         Location_Copy (Inter_Int, Decl);
         Set_Identifier (Inter_Int, Null_Identifier);
         Set_Mode (Inter_Int, Iir_In_Mode);
         Set_Type (Inter_Int, Std_Package.Integer_Subtype_Definition);
         Set_Base_Name (Inter_Int, Inter_Int);

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

      Type_Definition := Get_Base_Type (Get_Type (Decl));
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

         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            declare
               Inter_Chain : Iir;
               Element_Type : Iir;
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
               end if;

               Element_Type := Get_Element_Subtype (Type_Definition);

               if Is_One_Dimensional (Type_Definition) then
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Array_Array_Concat,
                                 Binary_Chain,
                                 Type_Definition);

                  Inter_Chain := Create_Anonymous_Interface (Element_Type);
                  Set_Chain (Inter_Chain, Unary_Chain);
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Element_Array_Concat,
                              Inter_Chain,
                              Type_Definition);

                  Inter_Chain := Create_Anonymous_Interface (Type_Definition);
                  Set_Chain (Inter_Chain,
                             Create_Anonymous_Interface (Element_Type));
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Array_Element_Concat,
                                 Inter_Chain,
                                 Type_Definition);

                  Inter_Chain := Create_Anonymous_Interface (Element_Type);
                  Set_Chain (Inter_Chain,
                             Create_Anonymous_Interface (Element_Type));
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Element_Element_Concat,
                                 Inter_Chain,
                                 Type_Definition);

                  --  LRM08 5.3.2.4  Predefined operations on array type
                  --
                  --  Given a type declaration that declares a one-dimensional
                  --  array type T whose element type is a character type that
                  --  contains only character literals, the following operation
                  --  is implicitely declared immediately following the type
                  --  declaration
                  if Vhdl_Std >= Vhdl_08
                    and then String_Type_Definition /= Null_Iir
                    and then Get_Kind (Get_Base_Type (Element_Type))
                    = Iir_Kind_Enumeration_Type_Definition
                    and then Get_Only_Characters_Flag (Element_Type)
                  then
                     Add_Operation (Name_To_String,
                                    Iir_Predefined_Array_To_String,
                                    Unary_Chain,
                                    String_Type_Definition);
                  end if;
               end if;

               if Is_Discrete_Array (Type_Definition) then
                  if Element_Type = Std_Package.Boolean_Type_Definition then
                     Add_Unary (Name_Not, Iir_Predefined_Boolean_Array_Not);

                     Add_Binary (Name_And, Iir_Predefined_Boolean_Array_And);
                     Add_Binary (Name_Or, Iir_Predefined_Boolean_Array_Or);
                     Add_Binary (Name_Nand, Iir_Predefined_Boolean_Array_Nand);
                     Add_Binary (Name_Nor, Iir_Predefined_Boolean_Array_Nor);
                     Add_Binary (Name_Xor, Iir_Predefined_Boolean_Array_Xor);
                     if Flags.Vhdl_Std > Vhdl_87 then
                        Add_Binary
                          (Name_Xnor, Iir_Predefined_Boolean_Array_Xnor);

                        Add_Shift_Operators;
                     end if;
                  elsif Element_Type = Std_Package.Bit_Type_Definition then
                     Add_Unary (Name_Not, Iir_Predefined_Bit_Array_Not);

                     Add_Binary (Name_And, Iir_Predefined_Bit_Array_And);
                     Add_Binary (Name_Or, Iir_Predefined_Bit_Array_Or);
                     Add_Binary (Name_Nand, Iir_Predefined_Bit_Array_Nand);
                     Add_Binary (Name_Nor, Iir_Predefined_Bit_Array_Nor);
                     Add_Binary (Name_Xor, Iir_Predefined_Bit_Array_Xor);
                     if Flags.Vhdl_Std > Vhdl_87 then
                        Add_Binary (Name_Xnor, Iir_Predefined_Bit_Array_Xnor);

                        Add_Shift_Operators;
                     end if;
                  end if;
               end if;
            end;

         when Iir_Kind_Access_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Access_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Access_Inequality);
            declare
               Deallocate_Proc: Iir_Implicit_Procedure_Declaration;
               Var_Interface: Iir_Variable_Interface_Declaration;
            begin
               Deallocate_Proc :=
                 Create_Iir (Iir_Kind_Implicit_Procedure_Declaration);
               Set_Identifier (Deallocate_Proc, Std_Names.Name_Deallocate);
               Set_Implicit_Definition
                 (Deallocate_Proc, Iir_Predefined_Deallocate);
               Var_Interface :=
                 Create_Iir (Iir_Kind_Variable_Interface_Declaration);
               Set_Identifier (Var_Interface, Std_Names.Name_P);
               Set_Type (Var_Interface, Type_Definition);
               Set_Mode (Var_Interface, Iir_Inout_Mode);
               Set_Base_Name (Var_Interface, Var_Interface);
               --Set_Purity_State (Deallocate_Proc, Impure);
               Set_Wait_State (Deallocate_Proc, False);
               Set_Type_Reference (Deallocate_Proc, Decl);

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
      if Decl = Std_Package.Boolean_Type then
         Add_Binary (Name_And, Iir_Predefined_Boolean_And);
         Add_Binary (Name_Or, Iir_Predefined_Boolean_Or);
         Add_Binary (Name_Nand, Iir_Predefined_Boolean_Nand);
         Add_Binary (Name_Nor, Iir_Predefined_Boolean_Nor);
         Add_Binary (Name_Xor, Iir_Predefined_Boolean_Xor);
         if Flags.Vhdl_Std > Vhdl_87 then
            Add_Binary (Name_Xnor, Iir_Predefined_Boolean_Xnor);
         end if;
         Add_Unary (Name_Not, Iir_Predefined_Boolean_Not);
      elsif Decl = Std_Package.Bit_Type then
         Add_Binary (Name_And, Iir_Predefined_Bit_And);
         Add_Binary (Name_Or, Iir_Predefined_Bit_Or);
         Add_Binary (Name_Nand, Iir_Predefined_Bit_Nand);
         Add_Binary (Name_Nor, Iir_Predefined_Bit_Nor);
         Add_Binary (Name_Xor, Iir_Predefined_Bit_Xor);
         if Flags.Vhdl_Std > Vhdl_87 then
            Add_Binary (Name_Xnor, Iir_Predefined_Bit_Xnor);
         end if;
         Add_Unary (Name_Not, Iir_Predefined_Bit_Not);
      elsif Decl = Std_Package.Universal_Real_Type then
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
           or else Get_Kind (Get_Type (Old_Decl)) /=
           Iir_Kind_Incomplete_Type_Definition
         then
            Old_Decl := Null_Iir;
         end if;
      else
         Old_Decl := Null_Iir;
      end if;

      if Old_Decl = Null_Iir then
         if Get_Kind (Decl) = Iir_Kind_Type_Declaration then
            --  This is necessary at least for enumeration type definition.
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
      Def := Get_Type (Decl);
      if Def = Null_Iir then
         --  Incomplete type declaration
         Def := Create_Iir (Iir_Kind_Incomplete_Type_Definition);
         Location_Copy (Def, Decl);
         Set_Type (Decl, Def);
         Set_Base_Type (Def, Def);
         Set_Signal_Type_Flag (Def, True);
         Set_Type_Declarator (Def, Decl);
         Set_Visible_Flag (Decl, True);
         Set_Incomplete_Type_List (Def, Create_Iir_List);
         Xref_Decl (Decl);
      else
         if Old_Decl = Null_Iir then
            Xref_Decl (Decl);
         else
            Xref_Body (Decl, Old_Decl);
         end if;
         Def := Sem_Type_Definition (Def, Decl);
         if Def /= Null_Iir then
            case Get_Kind (Def) is
               when Iir_Kind_Integer_Subtype_Definition
                 | Iir_Kind_Floating_Subtype_Definition
                 | Iir_Kind_Physical_Subtype_Definition
                 | Iir_Kind_Array_Subtype_Definition =>
                  --  Some type declaration are in fact subtype declarations.
                  St_Decl := Create_Iir (Iir_Kind_Subtype_Declaration);
                  Location_Copy (St_Decl, Decl);
                  Set_Identifier (St_Decl, Get_Identifier (Decl));
                  Set_Type (St_Decl, Def);
                  Set_Type_Declarator (Def, St_Decl);
                  Set_Chain (St_Decl, Get_Chain (Decl));
                  Set_Chain (Decl, St_Decl);

                  --  The type declaration declares the base type.
                  Bt_Def := Get_Base_Type (Def);
                  Set_Type (Decl, Bt_Def);
                  Set_Type_Declarator (Bt_Def, Decl);
                  Set_Subtype_Definition (Decl, Def);

                  if Old_Decl = Null_Iir then
                     Sem_Scopes.Add_Name (St_Decl);
                  else
                     Replace_Name (Get_Identifier (Decl), Old_Decl, St_Decl);
                     Set_Type_Declarator (Get_Type (Old_Decl), St_Decl);
                  end if;

                  Sem_Scopes.Name_Visible (St_Decl);

                  Sem_Scopes.Add_Visible_Type (Decl);

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
                  Sem_Scopes.Add_Visible_Type (Decl);

                  --  The implicit subprogram will be added in the
                  -- scope just after.
                  Create_Implicit_Operations (Decl, False);

               when Iir_Kind_Protected_Type_Declaration =>
                  Set_Type_Declarator (Def, Decl);
                  Sem_Scopes.Add_Visible_Type (Decl);
                  St_Decl := Null_Iir;
                  --  No implicit subprograms.

               when others =>
                  Error_Kind ("sem_type_declaration", Def);
            end case;

            if Old_Decl /= Null_Iir then
               --  Complete the type definition.
               declare
                  List : Iir_List;
                  El : Iir;
                  Old_Def : Iir;
               begin
                  Old_Def := Get_Type (Old_Decl);
                  Set_Signal_Type_Flag (Old_Def, Get_Signal_Type_Flag (Def));
                  List := Get_Incomplete_Type_List (Old_Def);
                  for I in Natural loop
                     El := Get_Nth_Element (List, I);
                     exit when El = Null_Iir;
                     Set_Designated_Type (El, Def);
                  end loop;
                  --  Complete the incomplete_type_definition node
                  --  (set type_declarator and base_type).

                  Set_Base_Type (Old_Def, Get_Base_Type (Def));
                  if St_Decl = Null_Iir then
                     Set_Type_Declarator (Old_Def, Decl);
                     Replace_Name (Get_Identifier (Decl), Old_Decl, Decl);
                  end if;
               end;
            end if;

            if Is_Global then
               Set_Type_Has_Signal (Def);
            end if;
         end if;
      end if;
   end Sem_Type_Declaration;

   procedure Sem_Subtype_Declaration (Decl: Iir; Is_Global : Boolean)
   is
      Def: Iir;
   begin
      --  Real hack to skip subtype declarations of anonymous type decls.
      if Get_Visible_Flag (Decl) then
         return;
      end if;

      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      -- Check the definition of the type.
      Def := Sem_Subtype_Indication (Get_Type (Decl));
      if Def = Null_Iir then
         return;
      end if;

      if not Is_Anonymous_Type_Definition (Def) then
         -- There is no added constraints and therefore the subtype
         -- declaration is in fact an alias of the type.
         Def := Copy_Subtype_Indication (Def);
         Location_Copy (Def, Decl);
      end if;

      Set_Type (Decl, Def);
      Set_Type_Declarator (Def, Decl);
      Name_Visible (Decl);
      if Is_Global then
         Set_Type_Has_Signal (Def);
      end if;
   end Sem_Subtype_Declaration;

   --  If DECL is a constant declaration, and there is already a constant
   --  declaration in the current scope with the same name, then return it.
   --  Otherwise, return NULL.
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
      if not Is_In_Current_Declarative_Region (Interp) then
         return Null_Iir;
      end if;
      Deferred_Const := Get_Declaration (Interp);
      if Get_Kind (Deferred_Const) /= Iir_Kind_Constant_Declaration then
         return Null_Iir;
      end if;
      --  LRM93 4.3.1.1
      --  The corresponding full constant declaration, which defines the value
      --  of the constant, must appear in the body of the package.
      if Get_Kind (Get_Library_Unit (Get_Current_Design_Unit))
        /= Iir_Kind_Package_Body
      then
         Error_Msg_Sem
           ("full constant declaration must appear in package body", Decl);
      end if;
      return Deferred_Const;
   end Get_Deferred_Constant;

   procedure Sem_Object_Declaration (Decl: Iir; Parent : Iir)
   is
      Atype: Iir;
      Default_Value : Iir;
      Proxy : Iir;
      Deferred_Const : Iir;
      Staticness : Iir_Staticness;
   begin
      Deferred_Const := Get_Deferred_Constant (Decl);

      --  Semantize type and default value:
      Atype := Get_Type (Decl);
      if Get_Kind (Atype) /= Iir_Kind_Proxy then
         Atype := Sem_Subtype_Indication (Atype);
         if Atype = Null_Iir then
            Atype := Create_Error_Type (Get_Type (Decl));
         end if;
      end if;

      if Deferred_Const = Null_Iir then
         Sem_Scopes.Add_Name (Decl);
         Xref_Decl (Decl);
      else
         Xref_Ref (Decl, Deferred_Const);
      end if;

      if Get_Kind (Atype) = Iir_Kind_Proxy then
         Proxy := Get_Proxy (Atype);
         Default_Value := Get_Default_Value (Proxy);
         Atype := Get_Type (Proxy);
         if Atype = Null_Iir then
            return;
         end if;
         Proxy := Get_Type (Decl);
         Free_Iir (Proxy);
      else
         Default_Value := Get_Default_Value (Decl);
         if Default_Value /= Null_Iir then
            Default_Value := Sem_Expression (Default_Value, Atype);
            if Default_Value = Null_Iir then
               Default_Value :=
                 Create_Error_Expr (Get_Default_Value (Decl), Atype);
            end if;
            Check_Read (Default_Value);
         end if;
      end if;
      Set_Type (Decl, Atype);
      Default_Value := Eval_Expr_Check_If_Static (Default_Value, Atype);
      Set_Default_Value (Decl, Default_Value);
      Set_Base_Name (Decl, Decl);
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
           ("subtype indication doesn't conform with the deferred constant",
            Decl);
      end if;

      --  LRM 4.3.1.3
      --  It is an error if a variable declaration declares a variable that is
      --  of a file type.
      --
      --  LRM 4.3.1.1
      --  It is an error if a constant declaration declares a constant that is
      --  of a file type, or an access type, or a composite type which has
      --  subelement that is a file type of an access type.
      --
      --  LRM 4.3.1.2
      --  It is an error if a signal declaration declares a signal that is of
      --  a file type [or an access type].
      case Get_Kind (Atype) is
         when Iir_Kind_File_Type_Definition =>
            Error_Msg_Sem (Disp_Node (Decl) & " cannot be of type file", Decl);
         when others =>
            if Get_Kind (Decl) /= Iir_Kind_Variable_Declaration then
               Check_Signal_Type (Decl);
            end if;
      end case;

      if not Check_Implicit_Conversion (Atype, Default_Value) then
         Error_Msg_Sem
           ("default value length does not match object type length", Decl);
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
                    ("full constant declaration must have a default value",
                     Decl);
               else
                  Set_Deferred_Declaration_Flag (Decl, True);
               end if;
               if Get_Kind (Parent) /= Iir_Kind_Package_Declaration then
                  Error_Msg_Sem ("a constant must have a default value", Decl);
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
            --  It is also an error if a guarded signal of a
            --  scalar type is neither a resolved signal nor a
            --  subelement of a resolved signal.
            if Get_Signal_Kind (Decl) /= Iir_No_Signal_Kind
              and then not Get_Resolved_Flag (Atype)
            then
               Error_Msg_Sem
                 ("guarded " & Disp_Node (Decl) & " must be resolved", Decl);
            end if;
            Set_Expr_Staticness (Decl, None);
            Set_Has_Disconnect_Flag (Decl, False);
            Set_Type_Has_Signal (Atype);

         when Iir_Kind_Variable_Declaration =>
            --  LRM93 4.3.1.3  Variable declarations
            --  Variable declared immediatly within entity declarations,
            --  architectures bodies, packages, packages bodies, and blocks
            --  must be shared variable.
            --  Variables declared immediatly within subprograms and
            --  processes must not be shared variables.
            --  Variables may appear in proteted type bodies; such
            --  variables, which must not be shared variables, represent
            --  shared data.
            case Get_Kind (Parent) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Architecture_Declaration
                 | Iir_Kind_Package_Declaration
                 | Iir_Kind_Package_Body
                 | Iir_Kind_Block_Statement =>
                  if not Get_Shared_Flag (Decl) then
                     Error_Msg_Sem
                       ("non shared variable declaration not allowed here",
                        Decl);
                  end if;
               when Iir_Kinds_Process_Statement
                 | Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body =>
                  if Get_Shared_Flag (Decl) then
                     Error_Msg_Sem
                       ("shared variable declaration not allowed here", Decl);
                  end if;
               when Iir_Kind_Protected_Type_Body =>
                  if Get_Shared_Flag (Decl) then
                     Error_Msg_Sem
                       ("variable of protected type body must not be shared",
                        Decl);
                  end if;
               when Iir_Kind_Protected_Type_Declaration =>
                  --  This is not allowed, but caught
                  --  in sem_protected_type_declaration.
                  null;
               when others =>
                  Error_Kind ("parse_declarative_part(2)", Parent);
            end case;

            if Flags.Vhdl_Std >= Vhdl_00 then
               declare
                  Base_Type : Iir;
                  Is_Protected : Boolean;
               begin
                  Base_Type := Get_Base_Type (Atype);
                  Is_Protected :=
                    Get_Kind (Base_Type) = Iir_Kind_Protected_Type_Declaration;

                  --  LRM00 4.3.1.3
                  --  The base type of the subtype indication of a
                  --  shared variable declaration must be a protected type.
                  if Get_Shared_Flag (Decl) and not Is_Protected then
                     Error_Msg_Sem
                       ("type of a shared variable must be a protected type",
                        Decl);
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
                     Error_Msg_Sem
                       ("variable type must not be of the protected type body",
                        Decl);
                  end if;
               end;
            end if;
            Set_Expr_Staticness (Decl, None);
         when others =>
            Error_Kind ("sem_object_declaration", Decl);
      end case;

      case Get_Kind (Decl) is
         when Iir_Kind_Constant_Declaration =>
            --  LRM93 §3.2.1.1
            --  For a constant declared by an object declaration, the index
            --  ranges are defined by the initial value, if the subtype of the
            --  constant is unconstrained; otherwise they are defined by this
            --  subtype.
            --if Default_Value = Null_Iir
            --  and then not Sem_Is_Constrained (Atype)
            --then
            --   Error_Msg_Sem ("constant declaration of unconstrained "
            --                  & Disp_Node (Atype) & " is not allowed", Decl);
            --end if;
            null;
            --if Deferred_Const = Null_Iir then
            --   Name_Visible (Decl);
            --end if;

         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration =>
            --  LRM93 §3.2.1.1
            --  For a variable or signal declared by an object declaration, the
            --  subtype indication of the corressponding object declaration
            --  must define a constrained array subtype.
            if not Is_Fully_Constrained_Type (Atype) then
               Error_Msg_Sem
                 ("declaration of " & Disp_Node (Decl)
                  & " with unconstrained " & Disp_Node (Atype)
                  & " is not allowed", Decl);
               if Default_Value /= Null_Iir then
                  Error_Msg_Sem ("(even with a default value)", Decl);
               end if;
            end if;

         when others =>
            Error_Kind ("sem_object_declaration(2)", Decl);
      end case;
   end Sem_Object_Declaration;

   procedure Sem_File_Declaration (Decl: Iir_File_Declaration)
   is
      Atype: Iir;
      Logical_Name: Iir;
      Open_Kind : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Set_Expr_Staticness (Decl, None);
      Set_Base_Name (Decl, Decl);
      Xref_Decl (Decl);

      -- Try to find a type.
      Atype := Get_Type (Decl);
      if Get_Kind (Atype) = Iir_Kind_Proxy then
         Atype := Get_Type (Get_Proxy (Atype));
         Free_Iir (Get_Type (Decl));
      else
         Atype := Sem_Subtype_Indication (Get_Type (Decl));
         if Atype = Null_Iir then
            return;
         end if;
      end if;
      Set_Type (Decl, Atype);

      --  LRM93 4.3.1.4
      --  The subtype indication of a file declaration must define a file
      --  subtype.
      if Get_Kind (Atype) /= Iir_Kind_File_Type_Definition then
         Error_Msg_Sem ("file subtype expected for a file declaration", Decl);
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
               Set_File_Open_Kind (Decl, File_Open_Kind_Read_Mode);
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
      if Flags.Vhdl_Std > Vhdl_93c then
         declare
            Parent : Iir;
            Spec : Iir;
         begin
            Parent := Get_Parent (Decl);
            case Get_Kind (Parent) is
               when Iir_Kind_Function_Body =>
                  Spec := Get_Subprogram_Specification (Parent);
                  if Get_Pure_Flag (Spec) then
                     Error_Msg_Sem
                       ("cannot declare a file in a pure function", Decl);
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
      end if;
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
         Error_Msg_Sem ("predefined attribute """ & Name_Table.Image (Ident)
                        & """ overriden", Decl);
      end if;
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      A_Type := Sem_Subtype_Indication (Get_Type (Decl));
      if A_Type = Null_Iir then
         return;
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

      Sem_Interface_Chain (Get_Generic_Chain (Component), Interface_Generic);
      Sem_Interface_Chain (Get_Port_Chain (Component), Interface_Port);

      Close_Declarative_Region;

      Name_Visible (Component);
   end Sem_Component_Declaration;

   procedure Sem_Object_Alias_Declaration (Alias: Iir_Object_Alias_Declaration)
   is
      N_Type: Iir;
      N_Name: Iir;
      Name : Iir;
      Name_Type : Iir;
   begin
      Sem_Scopes.Add_Name (Alias);
      Xref_Decl (Alias);

      Name := Get_Name (Alias);
      Sem_Name (Name, False);
      N_Name := Get_Named_Entity (Name);
      if N_Name = Error_Mark then
         return;
      end if;
      --  FIXME: overload list ?

      Name_Visible (Alias);

      case Get_Kind (N_Name) is
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_Element =>
            Set_Base_Name (Alias, Alias); -- Get_Base_Name (N_Name));
            Xref_Name (Name);
            Set_Name (Alias, N_Name);
         when others =>
            Error_Msg_Sem ("can only alias named object", Alias);
            return;
      end case;

      --  LRM93 4.3.3.1 Object Aliases.
      --  1. A signature may not appear in a declaration of an object alias.
      -- FIXME: todo.
      --
      --  2. The name must be a static name that denotes an object.
      if Get_Name_Staticness (N_Name) < Globally then
         Error_Msg_Sem ("aliased name must be a static name", Alias);
      end if;

      --  LRM93 4.3.3.1
      --  The base type of the name specified in an alias declaration must be
      --  the same as the base type of the type mark in the subtype indication
      --  (if the subtype indication is present);
      Name_Type := Get_Type (N_Name);
      N_Type := Get_Type (Alias);
      if N_Type = Null_Iir then
         Set_Type (Alias, Name_Type);
         N_Type := Name_Type;
      else
         N_Type := Sem_Subtype_Indication (N_Type);
         if N_Type /= Null_Iir then
            Set_Type (Alias, N_Type);
            if Get_Base_Type (N_Type) /= Get_Base_Type (Name_Type) then
               Error_Msg_Sem ("base type of aliased name and name mismatch",
                              Alias);
            end if;
         end if;
      end if;

      --  LRM93 4.3.3.1
      --  This type must not be a multi-dimensional array type.
      if Get_Kind (N_Type) in Iir_Kinds_Array_Type_Definition then
         if not Is_Unidim_Array_Type (N_Type) then
            Error_Msg_Sem
              ("aliased name must not be a multi-dimensional array type",
               Alias);
         end if;
         if Get_Type_Staticness (N_Type) = Locally
           and then Get_Type_Staticness (Name_Type) = Locally
           and then Eval_Discrete_Type_Length
           (Get_Nth_Element (Get_Index_Subtype_List (N_Type), 0))
           /= Eval_Discrete_Type_Length
           (Get_Nth_Element (Get_Index_Subtype_List (Name_Type), 0))
         then
            Error_Msg_Sem ("number of elements not matching in type and name",
                           Alias);
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
      List : Iir_List;
      Inter : Iir;
      El : Iir;
   begin
      List := Get_Type_Marks_List (Sig);
      case Get_Kind (N_Entity) is
         when Iir_Kind_Enumeration_Literal =>
            --  LRM93 2.3.2  Signatures
            --  * Similarly, a signature is said to match the parameter and
            --    result type profile of a given enumeration literal if
            --    the signature matches the parameter and result type profile
            --    of the subprogram equivalent to the enumeration literal,
            --    defined in Section 3.1.1
            return List = Null_Iir_List
              and then Get_Type (N_Entity) = Get_Return_Type (Sig);
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration =>
            --  LRM93 2.3.2  Signatures
            --  * if the reserved word RETURN is present, the subprogram is
            --    a function and the base type of the type mark following
            --    the reserved word in the signature is the same as the base
            --    type of the return type of the function, [...]
            if Get_Return_Type (Sig) /=
              Get_Base_Type (Get_Return_Type (N_Entity))
            then
               return False;
            end if;
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration =>
            --  LRM93 2.3.2  Signatures
            --  * [...] or the reserved word RETURN is absent and the
            --    subprogram is a procedure.
            if Get_Return_Type (Sig) /= Null_Iir then
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
      if List = Null_Iir_List then
         return Inter = Null_Iir;
      end if;
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         if El = Null_Iir and Inter = Null_Iir then
            return True;
         end if;
         if El = Null_Iir or Inter = Null_Iir then
            return False;
         end if;
         if Get_Base_Type (Get_Type (Inter)) /= El then
            return False;
         end if;
         Inter := Get_Chain (Inter);
      end loop;
      --  Avoid a spurious warning.
      return False;
   end Signature_Match;

   --  Extract from NAME the named entity whose profile matches with SIG.
   function Sem_Signature (Name : Iir; Sig : Iir_Signature) return Iir
   is
      Res : Iir;
      El : Iir;
      List : Iir_List;
      Error : Boolean;
   begin
      --  Sem signature.
      List := Get_Type_Marks_List (Sig);
      if List /= Null_Iir_List then
         for I in Natural loop
            El := Get_Nth_Element (List, I);
            exit when El = Null_Iir;
            El := Find_Declaration (El, Decl_Type);
            if El /= Null_Iir then
               Replace_Nth_Element (List, I, Get_Base_Type (El));
            end if;
         end loop;
      end if;
      El := Get_Return_Type (Sig);
      if El /= Null_Iir then
         El := Find_Declaration (El, Decl_Type);
         if El /= Null_Iir then
            Set_Return_Type (Sig, Get_Base_Type (El));
         end if;
      end if;

      Res := Null_Iir;
      Error := False;
      if Is_Overload_List (Name) then
         for I in Natural loop
            El := Get_Nth_Element (Get_Overload_List (Name), I);
            exit when El = Null_Iir;
            if Signature_Match (El, Sig) then
               if Res = Null_Iir then
                  Res := El;
               else
                  Error := True;
                  Error_Msg_Sem
                    ("cannot resolve signature, many matching subprograms:",
                     Sig);
                  Error_Msg_Sem ("found: " & Disp_Node (Res), Res);
               end if;
               if Error then
                  Error_Msg_Sem ("found: " & Disp_Node (El), El);
               end if;
            end if;
         end loop;
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
           ("cannot resolve signature, no matching subprogram", Sig);
      end if;
      return Res;
   end Sem_Signature;

   procedure Sem_Non_Object_Alias_Declaration
     (Alias : Iir_Non_Object_Alias_Declaration)
   is
      use Std_Names;
      Name : Iir;
      Sig : Iir_Signature;
      N_Entity : Iir;
      Id : Name_Id;
   begin
      Name := Get_Name (Alias);
      Sem_Name (Name, False);
      N_Entity := Get_Named_Entity (Name);
      if N_Entity = Error_Mark then
         return;
      end if;
      Xref_Decl (Alias);

      Sig := Get_Signature (Alias);
      if Is_Overload_List (N_Entity) then
         if Sig = Null_Iir then
            Error_Msg_Sem
              ("signature required for alias of a subprogram", Alias);
            return;
         end if;
      end if;

      if Sig /= Null_Iir then
         N_Entity := Sem_Signature (N_Entity, Sig);
      else
         case Get_Kind (N_Entity) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Implicit_Function_Declaration
              | Iir_Kind_Procedure_Declaration
              | Iir_Kind_Implicit_Procedure_Declaration =>
               --  LRM93 4.3.3.2  Non-Object Aliases
               --  2.  A signature is required if the name denotes a subprogram
               --      (including an operator) or enumeration literal.
               Error_Msg_Sem ("signature required for subprogram", Alias);
               return;
            when Iir_Kind_Enumeration_Literal =>
               Error_Msg_Sem ("signature required for enumeration literal",
                              Alias);
               return;
            when Iir_Kind_Type_Declaration =>
               declare
                  Def : Iir;
                  Last : Iir;
                  El : Iir;
                  Enum_List : Iir_Enumeration_Literal_List;

                  procedure Add_Implicit_Alias (Decl : Iir)
                  is
                     N_Alias : Iir_Non_Object_Alias_Declaration;
                  begin
                     N_Alias :=
                       Create_Iir (Iir_Kind_Non_Object_Alias_Declaration);
                     Location_Copy (N_Alias, Alias);
                     Set_Identifier (N_Alias, Get_Identifier (Decl));
                     Set_Name (N_Alias, Decl);

                     Add_Name (El, Get_Identifier (El), False);
                     Set_Visible_Flag (N_Alias, True);

                     --  Append in the declaration chain.
                     Set_Chain (N_Alias, Get_Chain (Last));
                     Set_Chain (Last, N_Alias);
                     Last := N_Alias;
                  end Add_Implicit_Alias;
               begin
                  Def := Get_Type (N_Entity);
                  Last := Alias;
                  if Get_Kind (Def) = Iir_Kind_Enumeration_Type_Definition
                  then
                     --  LRM93 4.3.3.2  Non-Object Aliases
                     --  3.  If the name denotes an enumeration type, then one
                     --      implicit alias declaration for each of the
                     --      literals of the type immediatly follows the alias
                     --      declaration for the enumeration type; [...]
                     Enum_List := Get_Enumeration_Literal_List (Def);
                     for I in Natural loop
                        El := Get_Nth_Element (Enum_List, I);
                        exit when El = Null_Iir;
                        --  LRM93 4.3.3.2  Non-Object Aliases
                        --      [...] each such implicit declaration has, as
                        --      its alias designator, the simple name or
                        --      character literal of the literal, and has,
                        --      as its name, a name constructed
                        --      by taking the name of the alias for the
                        --      enumeration type and substituting the simple
                        --      name or character literal being aliased for
                        --      the simple name of the type.
                        --      Each implicit alias has a signature that
                        --      matches the parameter and result type profile
                        --      of the literal being aliased.
                        Add_Implicit_Alias (El);
                     end loop;
                  end if;

                  --  LRM93 4.3.3.2  Non-Object Aliases
                  --  4.  Alternatively, if the name denotes a physical type
                  --      [...]
                  -- GHDL: this is not possible, since a physical type is
                  -- anonymous (LRM93 is buggy on this point).
                  if Get_Kind (Def) = Iir_Kind_Physical_Type_Definition then
                     raise Internal_Error;
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
                  El := Get_Chain (N_Entity);
                  while El /= Null_Iir loop
                     case Get_Kind (El) is
                        when Iir_Kind_Implicit_Function_Declaration
                          | Iir_Kind_Implicit_Procedure_Declaration =>
                           exit when Get_Type_Reference (El) /= N_Entity;
                        when others =>
                           exit;
                     end case;
                     Add_Implicit_Alias (El);
                     El := Get_Chain (El);
                  end loop;
               end;
            when Iir_Kinds_Object_Declaration =>
               Error_Msg_Sem
                 ("non-object alias cannot denotes an object", Alias);
               --  Do not return and add the name to avoid an error storm.
            when Iir_Kind_Subtype_Declaration
              | Iir_Kind_Attribute_Declaration =>
               null;
            when others =>
               Error_Kind ("sem_non_object_alias_declaration", N_Entity);
         end case;
      end if;
      if N_Entity = Null_Iir then
         return;
      end if;
      Set_Named_Entity (Name, N_Entity);
      Xref_Name (Name);

      Set_Name (Alias, N_Entity);

      Id := Get_Identifier (Alias);

      case Id is
         when Name_Characters =>
            --  LRM 4.3.3  Alias declarations
            --  If the alias designator is a character literal, the
            --  name must denote an enumeration literal.
            if Get_Kind (N_Entity) /= Iir_Kind_Enumeration_Literal then
               Error_Msg_Sem
                 ("alias of a character must denote an enumeration literal",
                  Alias);
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
            if Get_Kind (N_Entity) not in Iir_Kinds_Function_Declaration then
               Error_Msg_Sem
                 ("alias of an operator must denote a function", Alias);
               return;
            end if;
            Check_Operator_Requirements (Id, N_Entity);
         when others =>
            null;
      end case;
      Add_Name (Alias);
      Set_Visible_Flag (Alias, True);
   end Sem_Non_Object_Alias_Declaration;

   procedure Sem_Group_Template_Declaration
     (Decl : Iir_Group_Template_Declaration)
   is
   begin
      Sem_Scopes.Add_Name (Decl);
      Sem_Scopes.Name_Visible (Decl);
      Xref_Decl (Decl);
   end Sem_Group_Template_Declaration;

   procedure Sem_Group_Declaration (Group : Iir_Group_Declaration)
   is
      use Tokens;

      Constituent_List : Iir_Group_Constituent_List;
      Template : Iir_Group_Template_Declaration;
      Class, Prev_Class : Token_Type;
      El : Iir;
      El_Name : Iir;
      El_Entity : Iir_Entity_Class;
   begin
      Sem_Scopes.Add_Name (Group);
      Xref_Decl (Group);
      Template := Find_Declaration (Get_Group_Template_Name (Group),
                                    Decl_Group_Template);
      if Template = Null_Iir then
         return;
      end if;
      Set_Group_Template_Name (Group, Template);
      Constituent_List := Get_Group_Constituent_List (Group);
      El_Entity := Get_Entity_Class_Entry_Chain (Template);
      Prev_Class := Tok_Eof;
      for I in Natural loop
         El := Get_Nth_Element (Constituent_List, I);
         exit when El = Null_Iir;

         if El_Entity = Null_Iir then
            Error_Msg_Sem
              ("too many elements in group constituent list", Group);
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

         Sem_Name (El, False);
         El_Name := Get_Named_Entity (El);
         if El_Name /= Error_Mark then
            --  LRM93 4.7
            --  It is an error if the class of any group constituent in the
            --  group constituent list is not the same as the class specified
            --  by the corresponding entity class entry in the entity class
            --  entry list of the group template.
            if Get_Entity_Class_Kind (El_Name) /= Class then
               Error_Msg_Sem
                 ("constituent not of class '" & Tokens.Image (Class) & ''',
                  El);
            end if;
            Xref_Name (El);
         end if;
      end loop;

      --  End of entity_class list reached or zero or more constituent allowed.
      if not (El_Entity = Null_Iir
              or else Get_Entity_Class (El_Entity) = Tok_Box)
      then
         Error_Msg_Sem
           ("not enough elements in group constituent list", Group);
      end if;
      Set_Visible_Flag (Group, True);
   end Sem_Group_Declaration;

   --  Semantize every declaration of DECLS_PARENT.
   --  STMTS is the concurrent statement list associated with DECLS_PARENT
   --  if any, or null_iir.  This is used for specification.
   procedure Sem_Declaration_Chain (Parent : Iir; Is_Global : Boolean)
   is
      Decl: Iir;
      Last_Decl : Iir;
      Attr_Spec_Chain : Iir;
      Kind : Iir_Kind;
   begin
      --  Due to implicit declarations, the list can grow during sem.
      Decl := Get_Declaration_Chain (Parent);
      Last_Decl := Null_Iir;
      Attr_Spec_Chain := Null_Iir;

      loop
         << Again >> exit when Decl = Null_Iir;
         Kind := Get_Kind (Decl);
         case Kind is
            when Iir_Kind_Type_Declaration
              | Iir_Kind_Anonymous_Type_Declaration =>
               Sem_Type_Declaration (Decl, Is_Global);
            when Iir_Kind_Subtype_Declaration =>
               Sem_Subtype_Declaration (Decl, Is_Global);
            when Iir_Kind_Signal_Declaration =>
               Sem_Object_Declaration (Decl, Parent);
            when Iir_Kind_Constant_Declaration =>
               Sem_Object_Declaration (Decl, Parent);
            when Iir_Kind_Variable_Declaration =>
               Sem_Object_Declaration (Decl, Parent);
            when Iir_Kind_Attribute_Declaration =>
               Sem_Attribute_Declaration (Decl);
            when Iir_Kind_Attribute_Specification =>
               Sem_Attribute_Specification (Decl, Parent);
               if Get_Entity_Name_List (Decl) in Iir_Lists_All_Others then
                  Set_Attribute_Specification_Chain (Decl, Attr_Spec_Chain);
                  Attr_Spec_Chain := Decl;
               end if;
            when Iir_Kind_Component_Declaration =>
               Sem_Component_Declaration (Decl);
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               declare
                  Res : Iir;
               begin
                  Res := Sem_Subprogram_Declaration (Decl);
                  if Res /= Decl then
                     --  Replace DECL with RES.
                     if Last_Decl = Null_Iir then
                        Set_Declaration_Chain (Parent, Res);
                     else
                        Set_Chain (Last_Decl, Res);
                     end if;
                     Decl := Res;
                     --  Since RES is a body, no need to check for post
                     --  attribute specification.
                     goto Again;
                  end if;
                  if Is_Global
                    and then Kind = Iir_Kind_Function_Declaration
                    and then Is_A_Resolution_Function (Res, Null_Iir)
                  then
                     Set_Resolution_Function_Flag (Res, True);
                  end if;
               end;
            when Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body =>
               Sem_Subprogram_Body (Decl);
            when Iir_Kind_Implicit_Function_Declaration
              | Iir_Kind_Implicit_Procedure_Declaration =>
               Sem_Scopes.Add_Name (Decl);
               Name_Visible (Decl);
            when Iir_Kind_Object_Alias_Declaration =>
               Sem_Object_Alias_Declaration (Decl);
            when Iir_Kind_Non_Object_Alias_Declaration =>
               Last_Decl := Decl;
               Decl := Get_Chain (Decl);
               Sem_Non_Object_Alias_Declaration (Last_Decl);
               if Attr_Spec_Chain /= Null_Iir then
                  while Last_Decl /= Decl loop
                     Check_Post_Attribute_Specification
                       (Attr_Spec_Chain, Last_Decl);
                     Last_Decl := Get_Chain (Last_Decl);
                  end loop;
               end if;
               goto Again;
            when Iir_Kind_File_Declaration =>
               Sem_File_Declaration (Decl);
            when Iir_Kind_Use_Clause =>
               Sem_Use_Clause (Decl);
            when Iir_Kind_Configuration_Specification =>
               null;
            when Iir_Kind_Disconnection_Specification =>
               Sem_Disconnect_Specification (Decl);
            when Iir_Kind_Group_Template_Declaration =>
               Sem_Group_Template_Declaration (Decl);
            when Iir_Kind_Group_Declaration =>
               Sem_Group_Declaration (Decl);
            when Iir_Kinds_Signal_Attribute =>
               --  Added by sem, so nothing to do.
               null;
            when Iir_Kind_Protected_Type_Body =>
               Sem_Protected_Type_Body (Decl);
            when others =>
               Error_Kind ("sem_declaration_chain", Decl);
         end case;
         if Attr_Spec_Chain /= Null_Iir then
            Check_Post_Attribute_Specification (Attr_Spec_Chain, Decl);
         end if;
         Last_Decl := Decl;
         Decl := Get_Chain (Decl);
      end  loop;
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
      if Flags.Warn_Unused then
         case Get_Kind (Decl) is
            when Iir_Kind_Entity_Declaration =>
               --  May be used in architecture.
               null;
            when Iir_Kind_Architecture_Declaration
              | Iir_Kind_Block_Statement
              | Iir_Kind_Generate_Statement =>
               --  Might be used in a configuration.
               --  FIXME: create a second level of warning.
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
                     Error_Msg_Sem ("missing value for constant declared at "
                                    & Disp_Location (El), Decl);
                  else
                     --  Remove from visibility the full declaration of the
                     --  constant.
                     --  FIXME: this is not a check!
                     Set_Deferred_Declaration (El, Null_Iir);
                  end if;
               end if;
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               if Get_Subprogram_Body (El) = Null_Iir then
                  Error_Msg_Sem ("missing body for " & Disp_Node (El)
                                 & " declared at "
                                 & Disp_Location (El), Decl);
               end if;
            when Iir_Kind_Type_Declaration =>
               declare
                  Def : Iir;
               begin
                  Def := Get_Type (El);
                  if Get_Kind (Def) = Iir_Kind_Incomplete_Type_Definition
                    and then Get_Type_Declarator (Def) = El
                  then
                     Error_Msg_Sem ("missing full type declaration for "
                                    & Disp_Node (El), El);
                  elsif Get_Kind (Def) = Iir_Kind_Protected_Type_Declaration
                    and then Get_Protected_Type_Body (Def) = Null_Iir
                  then
                     Error_Msg_Sem ("missing protected type body for "
                                    & Disp_Node (El), El);
                  end if;
               end;
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
                  if not Get_Use_Flag (El) then
                     Warning_Msg_Sem
                       (Disp_Node (El) & " is never referenced", El);
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
      It_Type: Iir;
      A_Range: Iir;
      Range_Type : Iir;
   begin
      Xref_Decl (Iterator);
      It_Type := Get_Type (Iterator);
      A_Range := Sem_Discrete_Range_Integer (It_Type);
      if A_Range = Null_Iir then
         Set_Type (Iterator, Create_Error_Type (Iterator));
         return;
      end if;
      if Get_Kind (A_Range) in Iir_Kinds_Type_And_Subtype_Definition then
         Range_Type := A_Range;
      else
         Range_Type := Get_Type (A_Range);
      end if;
      case Get_Kind (Range_Type) is
         when Iir_Kinds_Discrete_Type_Definition =>
            null;
         when others =>
            Error_Msg_Sem ("iterator is not of discrete type", A_Range);
            Set_Type (Iterator, Null_Iir);
            return;
      end case;

      Set_Type (Iterator, Range_To_Subtype_Definition (A_Range));
      Set_Base_Name (Iterator, Iterator);
      Set_Expr_Staticness (Iterator, Staticness);
   end Sem_Iterator;
end Sem_Decls;
