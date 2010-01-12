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
with Evaluation; use Evaluation;
with Iirs_Utils; use Iirs_Utils;
with Libraries;
with Errorout; use Errorout;
with Flags; use Flags;
with Name_Table;
with Std_Package; use Std_Package;
with Types; use Types;
with Iir_Chains; use Iir_Chains;
with Std_Names;
with Sem;
with Sem_Scopes; use Sem_Scopes;
with Sem_Expr; use Sem_Expr;
with Sem_Stmts; use Sem_Stmts;
with Sem_Decls; use Sem_Decls;
with Sem_Assocs; use Sem_Assocs;
with Sem_Types;
with Sem_Psl;
with Xrefs; use Xrefs;

package body Sem_Names is
   --  Finish the semantization of NAME using RES as named entity.
   --  This is called when the semantization is finished and an uniq
   --  interpretation has been determined (RES).
   --
   --  Error messages are emitted here.
   procedure Finish_Sem_Name (Name : Iir; Res : Iir);

   procedure Error_Overload (Expr: Iir) is
   begin
      Error_Msg_Sem ("can't resolve overload for " & Disp_Node (Expr), Expr);
   end Error_Overload;

   procedure Disp_Overload_List (List : Iir_List; Loc : Iir)
   is
      El : Iir;
   begin
      Error_Msg_Sem ("possible interpretations are:", Loc);
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         case Get_Kind (El) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration
              | Iir_Kind_Implicit_Function_Declaration
              | Iir_Kind_Implicit_Procedure_Declaration =>
               Error_Msg_Sem (Disp_Subprg (El), El);
            when Iir_Kind_Function_Call =>
               El := Get_Implementation (El);
               Error_Msg_Sem (Disp_Subprg (El), El);
            when others =>
               Error_Msg_Sem (Disp_Node (El), El);
         end case;
      end loop;
   end Disp_Overload_List;

   -- Create an overload list.
   -- must be destroyed with free_iir.
   function Get_Overload_List return Iir_Overload_List is
   begin
      return Create_Iir (Iir_Kind_Overload_List);
   end Get_Overload_List;

   function Create_Overload_List (List : Iir_List) return Iir_Overload_List
   is
      Res : Iir_Overload_List;
   begin
      Res := Create_Iir (Iir_Kind_Overload_List);
      Set_Overload_List (Res, List);
      return Res;
   end Create_Overload_List;

   function Simplify_Overload_List (List : Iir_List) return Iir
   is
      Res : Iir;
      L1 : Iir_List;
   begin
      case Get_Nbr_Elements (List) is
         when 0 =>
            L1 := List;
            Destroy_Iir_List (L1);
            return Null_Iir;
         when 1 =>
            L1 := List;
            Res := Get_First_Element (List);
            Destroy_Iir_List (L1);
            return Res;
         when others =>
            return Create_Overload_List (List);
      end case;
   end Simplify_Overload_List;

   -- Return true if AN_IIR is an overload list.
   function Is_Overload_List (An_Iir: Iir) return Boolean is
   begin
      return Get_Kind (An_Iir) = Iir_Kind_Overload_List;
   end Is_Overload_List;

   --  From the list LIST of function or enumeration literal, extract the
   --  list of (return) types.
   --  If there is only one type, return it.
   --  If there is no types, return NULL.
   --  Otherwise, return the list as an overload list.
   function Create_List_Of_Types (List : Iir_List)
     return Iir
   is
      Res_List : Iir_List;
      Decl : Iir;
   begin
      --  Create the list of possible return types.
      Res_List := Create_Iir_List;
      for I in Natural loop
         Decl := Get_Nth_Element (List, I);
         exit when Decl = Null_Iir;
         case Get_Kind (Decl) is
            when Iir_Kinds_Function_Declaration =>
               Add_Element (Res_List, Get_Return_Type (Decl));
            when Iir_Kind_Enumeration_Literal
              | Iir_Kind_Function_Call
              | Iir_Kind_Indexed_Name =>
               Add_Element (Res_List, Get_Type (Decl));
            when others =>
               Error_Kind ("create_list_of_types", Decl);
         end case;
      end loop;
      return Simplify_Overload_List (Res_List);
   end Create_List_Of_Types;

   --  Add new interpretation DECL to RES.
   --  Create an overload_list if necessary.
   --  Before the first call, RES should be set to NULL_IIR.
   procedure Add_Result (Res : in out Iir; Decl : Iir)
   is
      Nres : Iir;
      Nres_List : Iir_List;
   begin
      if Decl = Null_Iir then
         return;
      end if;
      if Res = Null_Iir then
         Res := Decl;
      elsif Is_Overload_List (Res) then
         Append_Element (Get_Overload_List (Res), Decl);
      else
         Nres_List := Create_Iir_List;
         Nres := Create_Overload_List (Nres_List);
         Append_Element (Nres_List, Res);
         Append_Element (Nres_List, Decl);
         Res := Nres;
      end if;
   end Add_Result;

   --  Move elements of result list LIST to result list RES.
   --  Destroy LIST if necessary.
   procedure Add_Result_List (Res : in out Iir; List : Iir);
   pragma Unreferenced (Add_Result_List);

   procedure Add_Result_List (Res : in out Iir; List : Iir)
   is
      El : Iir;
      List_List : Iir_List;
      Res_List : Iir_List;
   begin
      if Res = Null_Iir then
         Res := List;
      elsif List = Null_Iir then
         null;
      elsif not Is_Overload_List (List) then
         Add_Result (Res, List);
      else
         if not Is_Overload_List (Res) then
            El := Res;
            Res := Get_Overload_List;
            Append_Element (Get_Overload_List (Res), El);
         end if;
         List_List := Get_Overload_List (List);
         Res_List := Get_Overload_List (Res);
         for I in Natural loop
            El := Get_Nth_Element (List_List, I);
            exit when El = Null_Iir;
            Append_Element (Res_List, El);
         end loop;
         Free_Iir (List);
      end if;
   end Add_Result_List;

   --  Free interpretations of LIST except KEEP.
   procedure Sem_Name_Free_Result (List : Iir; Keep : Iir)
   is
      procedure Sem_Name_Free (El : Iir) is
      begin
         case Get_Kind (El) is
            when Iir_Kind_Function_Call
              | Iir_Kind_Indexed_Name =>
               --  FIXME: recursion ?
               Free_Iir (El);
            when Iir_Kinds_Function_Declaration
              | Iir_Kinds_Procedure_Declaration
              | Iir_Kind_Enumeration_Literal =>
               null;
            when others =>
               Error_Kind ("sem_name_free", El);
         end case;
      end Sem_Name_Free;

      El : Iir;
      List_List : Iir_List;
   begin
      if List = Null_Iir then
         return;
      elsif not Is_Overload_List (List) then
         if List /= Keep then
            Sem_Name_Free (List);
         end if;
      else
         List_List := Get_Overload_List (List);
         for I in Natural loop
            El := Get_Nth_Element (List_List, I);
            exit when El = Null_Iir;
            if El /= Keep then
               Sem_Name_Free (El);
            end if;
         end loop;
         Free_Iir (List);
      end if;
   end Sem_Name_Free_Result;

   --  Find all named declaration whose identifier is ID in DECL_LIST and
   --  return it.
   --  The result can be NULL (if no such declaration exist),
   --  a declaration, or an overload_list containing all declarations.
   function Find_Declarations_In_List
     (Decl: Iir; Name : Iir_Selected_Name; Keep_Alias : Boolean)
     return Iir
   is
      Res: Iir := Null_Iir;

      --  If indentifier of DECL is ID, then add DECL in the result.
      procedure Handle_Decl (Decl : Iir; Id : Name_Id) is
      begin
         --  Use_clauses may appear in a declaration list.
         case Get_Kind (Decl) is
            when Iir_Kind_Use_Clause
              | Iir_Kind_Anonymous_Type_Declaration =>
               return;
            when Iir_Kind_Non_Object_Alias_Declaration =>
               if Get_Identifier (Decl) = Id then
                  if Keep_Alias then
                     Add_Result (Res, Decl);
                  else
                     Add_Result (Res, Get_Name (Decl));
                  end if;
               end if;
            when others =>
               if Get_Identifier (Decl) = Id then
                  Add_Result (Res, Decl);
               end if;
         end case;
      end Handle_Decl;

      procedure Iterator_Decl is new Sem_Scopes.Iterator_Decl
        (Arg_Type => Name_Id, Handle_Decl => Handle_Decl);
      --procedure Iterator_Decl_List is new Sem_Scopes.Iterator_Decl_List
      --  (Arg_Type => Name_Id, Handle_Decl => Iterator_Decl);
      procedure Iterator_Decl_Chain is new Sem_Scopes.Iterator_Decl_Chain
        (Arg_Type => Name_Id, Handle_Decl => Iterator_Decl);

      Id : Name_Id;
      Decl_Body : Iir;
   begin
      Id := Get_Suffix_Identifier (Name);
      case Get_Kind (Decl) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            Iterator_Decl_Chain (Get_Interface_Declaration_Chain (Decl), Id);
         when Iir_Kind_Entity_Declaration =>
            Iterator_Decl_Chain (Get_Generic_Chain (Decl), Id);
            Iterator_Decl_Chain (Get_Port_Chain (Decl), Id);
         when Iir_Kind_Architecture_Declaration =>
            null;
         when Iir_Kind_Generate_Statement =>
            null;
         when Iir_Kind_Package_Declaration =>
            null;
         when Iir_Kind_Block_Statement =>
            declare
               Header : Iir;
            begin
               Header := Get_Block_Header (Decl);
               if Header /= Null_Iir then
                  Iterator_Decl_Chain (Get_Generic_Chain (Header), Id);
                  Iterator_Decl_Chain (Get_Port_Chain (Header), Id);
               end if;
            end;
         when Iir_Kind_For_Loop_Statement =>
            Handle_Decl (Get_Iterator_Scheme (Decl), Id);
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement =>
            null;
         when others =>
            Error_Kind ("find_declarations_in_list", Decl);
      end case;

      case Get_Kind (Decl) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            Decl_Body := Get_Subprogram_Body (Decl);
            Iterator_Decl_Chain
              (Get_Declaration_Chain (Decl_Body), Id);
            Iterator_Decl_Chain
              (Get_Sequential_Statement_Chain (Decl_Body), Id);
         when Iir_Kind_Architecture_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Block_Statement =>
            Iterator_Decl_Chain (Get_Declaration_Chain (Decl), Id);
            Iterator_Decl_Chain (Get_Concurrent_Statement_Chain (Decl), Id);
         when Iir_Kind_Package_Declaration =>
            Iterator_Decl_Chain (Get_Declaration_Chain (Decl), Id);
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement =>
            Iterator_Decl_Chain (Get_Declaration_Chain (Decl), Id);
            Iterator_Decl_Chain (Get_Sequential_Statement_Chain (Decl), Id);
         when Iir_Kind_For_Loop_Statement =>
            null;
         when others =>
            Error_Kind ("find_declarations_in_list", Decl);
      end case;
      --if Res = Null_Iir then
      --   Error_Msg_Sem ("""" & Name_Table.Image (Id) & """ not defined in "
      --                  & Disp_Node (Decl), Name);
      --end if;
      return Res;
   end Find_Declarations_In_List;

   --  Create an implicit_dereference node if PREFIX is of type access.
   --  Return PREFIX otherwise.
   --  PARENT is used if an implicit dereference node is created, to copy
   --  location from.
   function Insert_Implicit_Dereference (Prefix : Iir; Parent : Iir)
     return Iir
   is
      Prefix_Type : Iir;
      Res : Iir_Implicit_Dereference;
   begin
      Prefix_Type := Get_Type (Prefix);

      case Get_Kind (Prefix_Type) is
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            null;
         when others =>
            return Prefix;
      end case;
      Check_Read (Prefix);
      Res := Create_Iir (Iir_Kind_Implicit_Dereference);
      Location_Copy (Res, Parent);
      Set_Type (Res, Get_Designated_Type (Prefix_Type));
      Set_Prefix (Res, Prefix);
      Set_Base_Name (Res, Res);
      Set_Expr_Staticness (Res, None);
      return Res;
   end Insert_Implicit_Dereference;

   --  If PREFIX is a function specification that cannot be converted to a
   --  function call (because of lack of association), return FALSE.
   function Maybe_Function_Call (Prefix : Iir) return Boolean
   is
      Inter : Iir;
   begin
      if Get_Kind (Prefix) not in Iir_Kinds_Function_Declaration then
         return True;
      end if;
      Inter := Get_Interface_Declaration_Chain (Prefix);
      while Inter /= Null_Iir loop
         if Get_Default_Value (Inter) = Null_Iir then
            return False;
         end if;
         Inter := Get_Chain (Inter);
      end loop;
      return True;
   end Maybe_Function_Call;

   procedure Name_To_Method_Object (Call : Iir; Name : Iir)
   is
      Prefix : Iir;
      Obj : Iir;
   begin
      if Get_Kind (Name) = Iir_Kind_Selected_Name then
         Prefix := Get_Prefix (Name);
         Obj := Get_Named_Entity (Prefix);
         if Obj /= Null_Iir
           and then
           (Get_Kind (Obj) = Iir_Kind_Variable_Declaration
            or Get_Kind (Obj) = Iir_Kind_Variable_Interface_Declaration)
           and then Get_Type (Obj) /= Null_Iir
         then
            if Get_Kind (Get_Type (Obj)) /= Iir_Kind_Protected_Type_Declaration
            then
               Error_Msg_Sem ("type of the prefix should be a protected type",
                              Prefix);
               return;
            end if;
            Set_Method_Object (Call, Obj);
         end if;
      end if;
      Set_Implementation (Call, Get_Named_Entity (Name));
   end Name_To_Method_Object;

   --  NAME is the name of the function (and not the parenthesis name)
   function Sem_As_Function_Call (Name : Iir; Spec : Iir; Assoc_Chain : Iir)
                                 return Iir_Function_Call
   is
      Call : Iir_Function_Call;
   begin
      --  Check.
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol =>
            null;
         when others =>
            Error_Kind ("sem_as_function_call", Name);
      end case;

      Call := Create_Iir (Iir_Kind_Function_Call);
      Location_Copy (Call, Name);
      Name_To_Method_Object (Call, Name);
      Set_Implementation (Call, Spec);
      Set_Parameter_Association_Chain (Call, Assoc_Chain);
      Set_Type (Call, Get_Return_Type (Spec));
      Set_Base_Name (Call, Call);
      return Call;
   end Sem_As_Function_Call;

   --  If PREFIX is a function specification, then return a function call,
   --  else return PREFIX.
   function Maybe_Insert_Function_Call (Name : Iir; Spec : Iir) return Iir
   is
   begin
      if Get_Kind (Spec) in Iir_Kinds_Function_Declaration then
         return Sem_As_Function_Call (Name, Spec, Null_Iir);
      else
         return Spec;
      end if;
   end Maybe_Insert_Function_Call;

   --  If PTR_TYPE is not NULL_IIR, then return an implciti dereference to
   --  PREFIX, else return PREFIX.
   function Maybe_Insert_Dereference (Prefix : Iir; Ptr_Type : Iir) return Iir
   is
      Id : Iir;
   begin
      if Ptr_Type /= Null_Iir then
         Id := Create_Iir (Iir_Kind_Implicit_Dereference);
         Location_Copy (Id, Prefix);
         Set_Type (Id, Get_Designated_Type (Ptr_Type));
         Set_Prefix (Id, Prefix);
         Set_Base_Name (Id, Id);
         return Id;
      else
         return Prefix;
      end if;
   end Maybe_Insert_Dereference;

   procedure Finish_Sem_Indexed_Name (Expr : Iir)
   is
      Prefix : Iir;
      Prefix_Type : Iir;
      Index_Subtype : Iir;
      Index_List : Iir_List;
      Index : Iir;
      Expr_Staticness : Iir_Staticness;
   begin
      Prefix := Get_Prefix (Expr);
      Prefix_Type := Get_Type (Prefix);
      Expr_Staticness := Locally;

      Index_List := Get_Index_List (Expr);
      -- LRM93 §6.4: there must be one such expression for each index
      -- position of the array and each expression must be of the
      -- type of the corresponding index.
      -- Loop on the indexes.
      for I in Natural loop
         Index_Subtype :=
           Get_Nth_Element (Get_Index_Subtype_List (Prefix_Type), I);
         exit when Index_Subtype = Null_Iir;
         Index := Get_Nth_Element (Index_List, I);
         -- The index_subtype can be an unconstrained index type.
         Index := Check_Is_Expression (Index, Index);
         if Index /= Null_Iir then
            Index := Sem_Expression (Index, Get_Base_Type (Index_Subtype));
         end if;
         if Index /= Null_Iir then
            if Get_Expr_Staticness (Index) = Locally
              and then Get_Type_Staticness (Index_Subtype) = Locally
            then
               Index := Eval_Expr_Check (Index, Index_Subtype);
            end if;
            Replace_Nth_Element (Get_Index_List (Expr), I, Index);
            Expr_Staticness := Min (Expr_Staticness,
                                    Get_Expr_Staticness (Index));
         else
            Expr_Staticness := None;
         end if;
      end loop;

      Set_Type (Expr, Get_Element_Subtype (Prefix_Type));

      --  An indexed name cannot be locally static.
      Set_Expr_Staticness
        (Expr, Min (Globally, Min (Expr_Staticness,
                                   Get_Expr_Staticness (Prefix))));

      -- LRM93 §6.1:
      -- a name is said to be a static name iff:
      -- The name is an indexed name whose prefix is a static name
      -- and every expression that appears as part of the name is a
      -- static expression.
      --
      -- a name is said to be a locally static name iif:
      -- The name is an indexed name whose prefix is a locally
      -- static name and every expression that appears as part
      -- of the name is a locally static expression.
      Set_Name_Staticness (Expr, Min (Expr_Staticness,
                                      Get_Name_Staticness (Prefix)));

      Set_Base_Name (Expr, Get_Base_Name (Prefix));
   end Finish_Sem_Indexed_Name;

   procedure Finish_Sem_Dereference (Res : Iir)
   is
   begin
      Set_Base_Name (Res, Res);
      Check_Read (Get_Prefix (Res));
      Set_Expr_Staticness (Res, None);
      Set_Name_Staticness (Res, None);
   end Finish_Sem_Dereference;

   procedure Finish_Sem_Slice_Name (Name : Iir_Slice_Name)
   is
      -- The prefix of the slice
      Prefix: Iir;
      Prefix_Type: Iir;
      Prefix_Base_Type : Iir;
      Prefix_Bt : Iir;
      Index_List: Iir_List;
      Index_Type: Iir;
      Suffix: Iir;
      Slice_Type : Iir;
      Expr_Type : Iir;
      Staticness : Iir_Staticness;
      Suffix_Rng : Iir;
      Prefix_Rng : Iir;
   begin
      -- Set a type to the prefix.
      Prefix := Get_Prefix (Name);
      Prefix_Type := Get_Type (Prefix);
      Set_Base_Name (Name, Get_Base_Name (Prefix));

      -- LRM93 §6.5: the prefix of an indexed name must be appropriate
      -- for an array type.
      Prefix_Bt := Get_Base_Type (Prefix_Type);
      if Get_Kind (Prefix_Bt) /= Iir_Kind_Array_Type_Definition then
         Error_Msg_Sem ("slice can only be applied to an array", Name);
         return;
      end if;

      -- LRM93 §6.5:
      -- The prefix of a slice must be appropriate for a
      -- one-dimensionnal array object.
      Index_List := Get_Index_Subtype_List (Prefix_Type);
      if Get_Nbr_Elements (Index_List) /= 1 then
         Error_Msg_Sem ("slice prefix must be an unidimensional array", Name);
         return;
      end if;

      Index_Type := Get_First_Element (Index_List);
      Prefix_Rng := Eval_Range (Index_Type);

      --  LRM93 6.5
      --  It is an error if either the bounds of the discrete range does not
      --  belong to the index range of the prefixing array, *unless* the slice
      --  is a null slice.
      --
      --  LRM93 6.5
      --  The slice is a null slice if the discrete range is a null range.

      -- LRM93 §6.5:
      -- The bounds of the discrete range [...] must be of the
      -- type of the index of the array.
      Suffix := Sem_Discrete_Range_Expression
        (Get_Suffix (Name), Index_Type, False);
      if Suffix = Null_Iir then
         return;
      end if;
      Set_Suffix (Name, Suffix);

      -- LRM93 §6.5:
      -- It is an error if the direction of the discrete range is not
      -- the same as that of the index range of the array denoted
      -- by the prefix of the slice name.

      -- Check this only if the type is a constrained type.
      Suffix_Rng := Eval_Range (Suffix);
      if Get_Kind (Prefix_Type) = Iir_Kind_Array_Subtype_Definition
        and then Get_Index_Constraint_Flag (Prefix_Type)
        and then Prefix_Rng /= Null_Iir
        and then Suffix_Rng /= Null_Iir
        and then Get_Direction (Suffix_Rng) /= Get_Direction (Prefix_Rng)
      then
         if False and then Flags.Vhdl_Std = Vhdl_87 then
            -- emit a warning for a null slice.
            Warning_Msg_Sem
              ("direction mismatch results in a null slice", Name);
         end if;
         Error_Msg_Sem ("direction of the range mismatch", Name);
      end if;

      --  LRM93 §7.4.1
      --  A slice is never a locally static expression.
      Staticness := Get_Discrete_Range_Staticness (Suffix);
      Set_Expr_Staticness
        (Name, Min (Min (Staticness, Get_Expr_Staticness (Prefix)), Globally));
      Set_Name_Staticness
        (Name, Min (Staticness, Get_Name_Staticness (Prefix)));

      --  The type of the slice is a subtype of the base type whose
      --  range contraint is the slice itself.
      if Get_Kind (Suffix) in Iir_Kinds_Discrete_Type_Definition then
         Slice_Type := Suffix;
      else
         case Get_Kind (Get_Base_Type (Index_Type)) is
            when Iir_Kind_Integer_Type_Definition =>
               Slice_Type := Create_Iir (Iir_Kind_Integer_Subtype_Definition);
            when Iir_Kind_Enumeration_Type_Definition =>
               Slice_Type :=
                 Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
            when others =>
               Error_Kind ("sem_expr: slice_name", Get_Base_Type (Index_Type));
         end case;
         Set_Range_Constraint (Slice_Type, Suffix);
         Set_Type_Staticness (Slice_Type, Staticness);
         Set_Base_Type (Slice_Type, Get_Base_Type (Index_Type));
         Set_Location (Slice_Type, Get_Location (Suffix));
      end if;

      Expr_Type := Create_Iir (Iir_Kind_Array_Subtype_Definition);
      Set_Location (Expr_Type, Get_Location (Suffix));
      Set_Index_Subtype_List (Expr_Type, Create_Iir_List);
      Prefix_Base_Type := Get_Base_Type (Prefix_Type);
      Set_Base_Type (Expr_Type, Prefix_Base_Type);
      Set_Signal_Type_Flag (Expr_Type,
                            Get_Signal_Type_Flag (Prefix_Base_Type));
      Append_Element (Get_Index_Subtype_List (Expr_Type), Slice_Type);
      Set_Element_Subtype (Expr_Type, Get_Element_Subtype (Prefix_Type));
      if Get_Kind (Prefix_Type) /= Iir_Kind_Array_Type_Definition then
         Set_Resolution_Function
           (Expr_Type, Get_Resolution_Function (Prefix_Type));
      else
         Set_Resolution_Function (Expr_Type, Null_Iir);
      end if;
      Set_Type_Staticness
        (Expr_Type, Min (Get_Type_Staticness (Prefix_Type),
                         Get_Type_Staticness (Slice_Type)));
      Set_Type (Name, Expr_Type);
      Set_Index_Constraint_Flag (Expr_Type, True);
      Set_Constraint_State (Expr_Type, Fully_Constrained);
      if Is_Signal_Object (Prefix) then
         Sem_Types.Set_Type_Has_Signal (Expr_Type);
      end if;
   end Finish_Sem_Slice_Name;

   procedure Finish_Sem_Function_Call (Call : Iir)
   is
      Rtype : Iir;
   begin
      Set_Name_Staticness (Call, None);
      --  FIXME: modify sem_subprogram_call to avoid such a type swap.
      Rtype := Get_Type (Call);
      Set_Type (Call, Null_Iir);
      if Sem_Subprogram_Call (Call, Null_Iir) = Null_Iir then
         Set_Type (Call, Rtype);
      end if;
   end Finish_Sem_Function_Call;

   procedure Finish_Sem_Array_Attribute (Attr : Iir; Param : Iir)
   is
      Parameter : Iir;
      Prefix_Type : Iir;
      Index_Type : Iir;
      Prefix : Iir;
      Staticness : Iir_Staticness;
   begin
      --  LRM93 14.1
      --  Parameter: A locally static expression of type universal_integer, the
      --  value of which must not exceed the dimensionality of A.  If omitted,
      --  it defaults to 1.
      if Param = Null_Iir then
         Parameter := Universal_Integer_One;
      else
         Parameter := Sem_Expression
           (Param, Universal_Integer_Type_Definition);
         if Parameter = Null_Iir then
            Parameter := Universal_Integer_One;
         else
            if Get_Expr_Staticness (Parameter) /= Locally then
               Error_Msg_Sem ("parameter must be locally static", Parameter);
               Parameter := Universal_Integer_One;
            end if;
         end if;
      end if;
      Prefix := Get_Prefix (Attr);
      Prefix_Type := Get_Type (Prefix);
      declare
         Dim : Iir_Int64;
         Indexes_List : Iir_List;
      begin
         Indexes_List := Get_Index_Subtype_List (Prefix_Type);
         Dim := Get_Value (Parameter);
         if Dim < 1 or else Dim > Iir_Int64 (Get_Nbr_Elements (Indexes_List))
         then
            Error_Msg_Sem ("parameter value out of bound", Attr);
            Parameter := Universal_Integer_One;
            Dim := 1;
         end if;
         Index_Type := Get_Nth_Element (Indexes_List, Natural (Dim - 1));
      end;

      case Get_Kind (Attr) is
         when Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute =>
            Set_Type (Attr, Index_Type);
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            Set_Type (Attr, Index_Type);
         when Iir_Kind_Length_Array_Attribute =>
            Set_Type (Attr, Convertible_Integer_Type_Definition);
         when Iir_Kind_Ascending_Array_Attribute =>
            Set_Type (Attr, Boolean_Type_Definition);
         when others =>
            raise Internal_Error;
      end case;

      Set_Parameter (Attr, Parameter);
      if Get_Kind (Prefix_Type) = Iir_Kind_Array_Subtype_Definition then
         Set_Index_Subtype (Attr, Index_Type);
      end if;

      --  LRM 7.4.1
      --  A locally static range is either [...], or a range of the first form
      --  whose prefix denotes either a locally static subtype or an object
      --  that is of a locally static subtype.

      --  LRM 7.4.2
      --  A globally static range is either [...], or a range of the first form
      --  whose prefix denotes either a globally static subtype or an object
      --  that is of a globally static subtype.
      --
      --  A globally static subtype is either a globally static scalar subtype,
      --  a globally static array subtype, [...]
      --
      --  A globally static array subtype is a constrained array subtype
      --  formed by imposing on an unconstrained array type a globally static
      --  index constraint.
      Staticness := Get_Type_Staticness (Prefix_Type);
      if Flags.Vhdl_Std = Vhdl_93c
        and then Get_Kind (Prefix) not in Iir_Kinds_Type_Declaration
      then
         --  For 93c:
         --  if the prefix is a static expression, the staticness of the
         --   expression may be higher than the staticness of the type
         --   (eg: generic whose type is an unconstrained array).
         --   Also consider expression staticness.
         Staticness := Iir_Staticness'Max (Staticness,
                                           Get_Expr_Staticness (Prefix));
      end if;
      Set_Expr_Staticness (Attr, Staticness);
   end Finish_Sem_Array_Attribute;

   procedure Finish_Sem_Scalar_Type_Attribute (Attr : Iir; Param : Iir)
   is
      Prefix_Type : Iir;
      Prefix_Bt : Iir;
      Parameter : Iir;
      Param_Type : Iir;
   begin
      if Param = Null_Iir then
         Error_Msg_Sem (Disp_Node (Attr) & " requires a parameter", Attr);
         return;
      end if;

      Prefix_Type := Get_Type (Get_Prefix (Attr));
      Prefix_Bt := Get_Base_Type (Prefix_Type);

      case Get_Kind (Attr) is
         when Iir_Kind_Pos_Attribute =>
            --  LRM93 14.1
            --  Parameter: An expression whose type is the base type of T.
            Parameter := Sem_Expression (Param, Prefix_Bt);
         when Iir_Kind_Val_Attribute =>
            --  LRM93 14.1
            --  Parameter: An expression of any integer type.
            Param_Type := Get_Type (Param);
            if Is_Overload_List (Param_Type) then
               Parameter := Sem_Expression
                 (Param, Universal_Integer_Type_Definition);
            else
               if Get_Kind (Get_Base_Type (Param_Type))
                 /= Iir_Kind_Integer_Type_Definition
               then
                  Error_Msg_Sem ("parameter must be an integer", Attr);
                  return;
               end if;
               Parameter := Param;
            end if;
         when Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute =>
            --  LRM93 14.1
            --  Parameter: An expression whose type is the base type of T.
            Parameter := Sem_Expression (Param, Prefix_Bt);
         when Iir_Kind_Image_Attribute =>
            --  LRM93 14.1
            --  Parameter: An expression whose type is the base type of T.
            Parameter := Sem_Expression (Param, Prefix_Bt);
         when Iir_Kind_Value_Attribute =>
            --  Parameter: An expression of type string.
            Parameter := Sem_Expression (Param, String_Type_Definition);
         when others =>
            raise Internal_Error;
      end case;
      if Parameter = Null_Iir then
         Set_Parameter (Attr, Param);
         Set_Expr_Staticness (Attr, None);
         return;
      end if;
      Set_Parameter (Attr, Parameter);
      Set_Expr_Staticness (Attr, Min (Get_Type_Staticness (Prefix_Type),
                                      Get_Expr_Staticness (Parameter)));
   end Finish_Sem_Scalar_Type_Attribute;

   procedure Finish_Sem_Signal_Attribute (Attr : Iir; Parameter : Iir)
   is
      Param : Iir;
   begin
      if Parameter = Null_Iir then
         return;
      end if;
      if Get_Kind (Attr)= Iir_Kind_Transaction_Attribute then
         Error_Msg_Sem ("'transaction does not allow a parameter", Attr);
      else
         Param := Sem_Expression (Parameter, Time_Subtype_Definition);
         Set_Parameter (Attr, Param);
      end if;
   end Finish_Sem_Signal_Attribute;

   function Is_Type_Abstract_Numeric (Atype : Iir) return Boolean is
   begin
      case Get_Kind (Atype) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Floating_Type_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Type_Abstract_Numeric;

   function Are_Types_Closely_Related (Type1, Type2 : Iir) return Boolean
   is
      Base_Type1 : Iir;
      Base_Type2 : Iir;
      Ant1, Ant2 : Boolean;
      Index_List1, Index_List2 : Iir_List;
      El1, El2 : Iir;
   begin
      Base_Type1 := Get_Base_Type (Type1);
      Base_Type2 := Get_Base_Type (Type2);

      --  LRM 7.3.5
      --  In particular, a type is closely related to itself.
      if Base_Type1 = Base_Type2 then
         return True;
      end if;

      --  LRM 7.3.5
      --  a) Abstract Numeric Types: Any abstract numeric type is closely
      --     related to any other abstract numeric type.
      Ant1 := Is_Type_Abstract_Numeric (Type1);
      Ant2 := Is_Type_Abstract_Numeric (Type2);
      if Ant1 and Ant2 then
         return True;
      end if;
      if Ant1 or Ant2 then
         return False;
      end if;

      --  LRM 7.3.5
      --  b) Array Types: Two array types are closely related if and only if
      --     The types have the same dimensionality; For each index position,
      --     the index types are either the same or are closely related; and
      --     The element types are the same.
      --
      --  No other types are closely related.
      if not (Get_Kind (Base_Type1) = Iir_Kind_Array_Type_Definition
              and then Get_Kind (Base_Type2) = Iir_Kind_Array_Type_Definition)
      then
         return False;
      end if;
      Index_List1 := Get_Index_Subtype_List (Base_Type1);
      Index_List2 := Get_Index_Subtype_List (Base_Type2);
      if Get_Nbr_Elements (Index_List1) /= Get_Nbr_Elements (Index_List2) then
         return False;
      end if;
      if Get_Base_Type (Get_Element_Subtype (Base_Type1))
        /= Get_Base_Type (Get_Element_Subtype (Base_Type2))
      then
         return False;
      end if;
      for I in Natural loop
         El1 := Get_Nth_Element (Index_List1, I);
         exit when El1 = Null_Iir;
         El2 := Get_Nth_Element (Index_List2, I);
         if not Are_Types_Closely_Related (El1, El2) then
            return False;
         end if;
      end loop;
      return True;
   end Are_Types_Closely_Related;

   procedure Finish_Sem_Type_Conversion (Conv: Iir_Type_Conversion)
   is
      Expr: Iir;
      Staticness : Iir_Staticness;
   begin
      -- LRM93 7.3.5
      -- Furthermore, the operand of a type conversion is not allowed to be
      -- the literal null, an allocator, an aggregate, or a string literal.
      Expr := Get_Expression (Conv);
      case Get_Kind (Expr) is
         when Iir_Kind_Null_Literal
           | Iir_Kind_Aggregate
           | Iir_Kind_String_Literal
           | Iir_Kind_Bit_String_Literal =>
            Error_Msg_Sem
              (Disp_Node (Expr) & " cannot be a type conversion operand",
               Expr);
            return;
         when others =>
            -- LRM93 7.3.5
            -- The type of the operand of a type conversion must be
            -- determinable independent of the context (in particular,
            -- independent of the target type).
            Expr := Sem_Expression_Universal (Expr);
            if Expr = Null_Iir then
               return;
            end if;
            if Get_Kind (Expr) in Iir_Kinds_Allocator then
               Error_Msg_Sem
                 (Disp_Node (Expr) & " cannot be a type conversion operand",
                  Expr);
            end if;
      end case;

      Set_Expression (Conv, Expr);

      --  LRM93 7.4.1 Locally Static Primaries.
      --  9. a type conversion whose expression is a locally static expression.
      --  LRM93 7.4.2 Globally Static Primaries.
      --  14. a type conversion whose expression is a globally static
      --      expression.
      if Expr /= Null_Iir then
         Staticness := Get_Expr_Staticness (Expr);
         if Flags.Vhdl_Std = Vhdl_87 then
            Staticness := Min (Globally, Staticness);
         end if;
         Set_Expr_Staticness (Conv, Staticness);

         if not Are_Types_Closely_Related (Get_Type (Conv), Get_Type (Expr))
         then
            --  FIXME: should explain why the types are not closely related.
            Error_Msg_Sem
              ("conversion not allowed between not closely related types",
               Conv);
            --  Avoid error storm in evaluation.
            Set_Expr_Staticness (Conv, None);
         else
            Check_Read (Expr);
         end if;
      end if;
   end Finish_Sem_Type_Conversion;

   procedure Finish_Sem_Function_Specification (Name : Iir; Spec : Iir)
   is
      Res : Iir;
   begin
      if not Maybe_Function_Call (Spec) then
         Error_Msg_Sem (Disp_Node (Spec) & " requires parameters", Name);
         Set_Named_Entity (Name, Null_Iir);
         return;
      end if;
      Res := Maybe_Insert_Function_Call (Name, Spec);
      if Get_Kind (Res) /= Iir_Kind_Function_Call then
         raise Internal_Error;
      end if;
      Finish_Sem_Function_Call (Res);
      Set_Named_Entity (Name, Res);
   end Finish_Sem_Function_Specification;

   procedure Finish_Sem_Implicits (Name : Iir; Pfx : Iir)
   is
      Name_Pfx : Iir;
   begin
      case Get_Kind (Pfx) is
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Attribute_Value =>
            null;
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Slice_Name =>
            Name_Pfx := Get_Prefix (Name);
            if Is_Overload_List (Name_Pfx) then
               Finish_Sem_Name (Name_Pfx, Pfx);
            end if;
         when Iir_Kind_Implicit_Dereference =>
            Finish_Sem_Implicits (Name, Get_Prefix (Pfx));
            Finish_Sem_Dereference (Pfx);
         when Iir_Kind_Dereference =>
            null;
         when Iir_Kind_Function_Call =>
            if Get_Name_Staticness (Pfx) = Unknown then
               Finish_Sem_Function_Call (Pfx);
            else
               Name_Pfx := Get_Prefix (Name);
               if Is_Overload_List (Name_Pfx) then
                  Finish_Sem_Name (Name_Pfx, Pfx);
               end if;
            end if;
         when others =>
            Error_Kind ("finish_sem_implicits", Pfx);
      end case;
   end Finish_Sem_Implicits;

   --  OBJ is an 'impure' object (variable, signal or file) referenced at
   --  location LOC.
   --  Check the pure rules.
   procedure Sem_Check_Pure (Loc : Iir; Obj : Iir)
   is
      procedure Update_Impure_Depth (Subprg_Spec : Iir; Depth : Iir_Int32)
      is
         Bod : Iir;
      begin
         Bod := Get_Subprogram_Body (Subprg_Spec);
         if Bod = Null_Iir then
            return;
         end if;
         if Depth < Get_Impure_Depth (Bod) then
            Set_Impure_Depth (Bod, Depth);
         end if;
      end Update_Impure_Depth;

      procedure Error_Pure (Subprg : Iir; Obj : Iir)
      is
      begin
         Error_Msg_Sem
           ("reference to " & Disp_Node (Obj) & " violate pure rule for "
            & Disp_Node (Subprg), Loc);
      end Error_Pure;

      Subprg : constant Iir := Sem_Stmts.Get_Current_Subprogram;
      Subprg_Body : Iir;
      Parent : Iir;
   begin
      --  Apply only in subprograms.
      if Subprg = Null_Iir then
         return;
      end if;
      case Get_Kind (Subprg) is
         when Iir_Kinds_Process_Statement =>
            return;
         when Iir_Kind_Procedure_Declaration =>
            --  Exit now if already known as impure.
            if Get_Purity_State (Subprg) = Impure then
               return;
            end if;
         when Iir_Kind_Function_Declaration =>
            --  Exit now if impure.
            if Get_Pure_Flag (Subprg) = False then
               return;
            end if;
         when others =>
            Error_Kind ("sem_check_pure", Subprg);
      end case;

      --  Not all objects are impure.
      case Get_Kind (Obj) is
         when Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            null;
         when Iir_Kind_File_Declaration =>
            --  LRM 93 2.2
            --  If a pure function is the parent of a given procedure, then
            --  that procedure must not contain a reference to an explicitly
            --  declared file object [...]
            --
            --  A pure function must not contain a reference to an explicitly
            --  declared file.
            if Flags.Vhdl_Std > Vhdl_93c then
               if Get_Kind (Subprg) = Iir_Kind_Function_Declaration then
                  Error_Pure (Subprg, Obj);
               else
                  Set_Purity_State (Subprg, Impure);
                  Set_Impure_Depth (Get_Subprogram_Body (Subprg),
                                    Iir_Depth_Impure);
               end if;
            end if;
            return;
         when others =>
            return;
      end case;

      --  OBJ is declared in the immediate declarative part of the subprogram.
      Parent := Get_Parent (Obj);
      Subprg_Body := Get_Subprogram_Body (Subprg);
      if Parent = Subprg or else Parent = Subprg_Body then
         return;
      end if;

      --  Function.
      if Get_Kind (Subprg) = Iir_Kind_Function_Declaration then
         Error_Pure (Subprg, Obj);
         return;
      end if;

      case Get_Kind (Parent) is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kinds_Process_Statement
           | Iir_Kind_Protected_Type_Body =>
            --  The procedure is impure.
            Set_Purity_State (Subprg, Impure);
            Set_Impure_Depth (Subprg_Body, Iir_Depth_Impure);
            return;
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Update_Impure_Depth
              (Subprg,
               Get_Subprogram_Depth (Get_Subprogram_Specification (Parent)));
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            Update_Impure_Depth (Subprg, Get_Subprogram_Depth (Parent));
         when others =>
            Error_Kind ("sem_check_pure(2)", Parent);
      end case;
   end Sem_Check_Pure;

   --  Set All_Sensitized_State to False iff OBJ is a signal declaration
   --  and the current subprogram is in a package body.
   procedure Sem_Check_All_Sensitized (Obj : Iir)
   is
      Subprg : Iir;
   begin
      --  We cares only of signals.
      if Get_Kind (Obj) /= Iir_Kind_Signal_Declaration then
         return;
      end if;
      --  We cares only of subprograms.  Give up if we are in a process.
      Subprg := Sem_Stmts.Get_Current_Subprogram;
      if Subprg = Null_Iir
        or else Get_Kind (Subprg) not in Iir_Kinds_Subprogram_Declaration
      then
         return;
      end if;
      if Get_Kind (Get_Library_Unit (Sem.Get_Current_Design_Unit))
        = Iir_Kind_Package_Body
      then
         Set_All_Sensitized_State (Subprg, Invalid_Signal);
      else
         Set_All_Sensitized_State (Subprg, Read_Signal);
      end if;
   end Sem_Check_All_Sensitized;

   procedure Finish_Sem_Name (Name : Iir; Res : Iir)
   is
      Pfx : Iir;
   begin
      case Get_Kind (Res) is
         when Iir_Kind_Design_Unit =>
            return;
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration =>
            Set_Base_Name (Name, Res);
            return;
         when Iir_Kind_Type_Conversion =>
            Finish_Sem_Type_Conversion (Res);
            return;
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Slice_Name
           | Iir_Kind_Dereference =>
            null;
         when Iir_Kind_Function_Call =>
            Finish_Sem_Function_Call (Res);
            return;
         when Iir_Kinds_Function_Declaration
           | Iir_Kinds_Procedure_Declaration =>
            --declare
            --   Nres : Iir;
            --begin
            --   Nres := Sem_As_Function_Call (Res, Null_Iir, Name);
            --  Set_Named_Entity (Name, Nres);
            --  Finish_Sem_Function_Call (Nres);
            --end;
            return;
         when Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Range_Array_Attribute =>
            Finish_Sem_Array_Attribute (Res, Null_Iir);
            return;
--          when Iir_Kind_Pos_Attribute =>
--             if Get_Parameter (Res) = Null_Iir then
--                Finish_Sem_Scalar_Type_Attribute (Res, Null_Iir);
--             end if;
--             return;
         when Iir_Kind_Psl_Expression =>
            return;
         when others =>
            Error_Kind ("finish_sem_name", Res);
      end case;

      Pfx := Get_Prefix (Res);
      Finish_Sem_Implicits (Name, Pfx);

      case Get_Kind (Res) is
         when Iir_Kind_Indexed_Name =>
            Finish_Sem_Indexed_Name (Res);
         when Iir_Kind_Slice_Name =>
            Finish_Sem_Slice_Name (Res);
         when Iir_Kind_Selected_Element =>
            Set_Name_Staticness (Res, Get_Name_Staticness (Pfx));
            Set_Expr_Staticness (Res, Get_Expr_Staticness (Pfx));
            Set_Base_Name (Res, Get_Base_Name (Pfx));
         when Iir_Kind_Dereference =>
            Finish_Sem_Dereference (Res);
         when others =>
            Error_Kind ("finish_sem_name(2)", Res);
      end case;
   end Finish_Sem_Name;

   --  LRM93 §6.2
   --  The evaluation of a simple name has no other effect than to determine
   --  the named entity denoted by the name.
   --
   --  NAME may be a string literal too.
   --  GHDL: set interpretation of NAME (possibly an overload list).
   --  If SOFT is TRUE, then no error message is reported in case of failure.
   procedure Sem_Simple_Name (Name : Iir; Keep_Alias : Boolean; Soft : Boolean)
   is
      Interpretation: Name_Interpretation_Type;
      Res: Iir;
      Res_List : Iir_List;
      N : Natural;
   begin
      Interpretation := Get_Interpretation (Get_Identifier (Name));

      if not Valid_Interpretation (Interpretation) then
         if not Soft then
            Error_Msg_Sem
              ("no declaration for """ & Image_Identifier (Name) & """", Name);
         end if;
         Res := Error_Mark;
      elsif not Valid_Interpretation (Get_Next_Interpretation (Interpretation))
      then
         -- not overloaded.
         Res := Get_Declaration (Interpretation);
         if not Keep_Alias
           and then Get_Kind (Res) = Iir_Kind_Non_Object_Alias_Declaration
         then
            Res := Get_Name (Res);
         end if;

         if not Get_Visible_Flag (Res) then
            if not Soft then
               Error_Msg_Sem (Disp_Node (Res) & " is not visible here", Name);
            end if;
            --  Even if a named entity was found, return an error_mark.
            --  Indeed, the named entity found is certainly the one being
            --  semantized, and the semantization may be uncomplete.
            Res := Error_Mark;
         end if;
      else
         Res_List := Create_Iir_List;
         N := 0;
         --  The SEEN_FLAG is used to get only one meaning which can be reached
         --  through several pathes (such as aliases).
         while Valid_Interpretation (Interpretation) loop
            Res := Get_Declaration (Interpretation);
            if not Keep_Alias
              and then Get_Kind (Res) = Iir_Kind_Non_Object_Alias_Declaration
            then
               Res := Get_Name (Res);
            end if;
            if not Get_Seen_Flag (Res) then
               Set_Seen_Flag (Res, True);
               N := N + 1;
               Append_Element (Res_List, Res);
            end if;
            Interpretation := Get_Next_Interpretation (Interpretation);
         end loop;
         for I in 0 .. N - 1 loop
            Res := Get_Nth_Element (Res_List, I);
            Set_Seen_Flag (Res, False);
         end loop;
         Res := Create_Overload_List (Res_List);
      end if;
      Set_Base_Name (Name, Res);
      Set_Named_Entity (Name, Res);
   end Sem_Simple_Name;

   --  LRM93 §6.3
   --  Selected Names.
   procedure Sem_Selected_Name (Name: Iir; Keep_Alias : Boolean)
   is
      Prefix: Iir;
      Suffix: Name_Id;
      Prefix_Loc : Location_Type;
      Res : Iir;

      procedure Sem_As_Expanded_Name (Sub_Name : Iir)
      is
         Sub_Res : Iir;
      begin
         if Get_Is_Within_Flag (Sub_Name) then
            Sub_Res := Find_Declarations_In_List (Sub_Name, Name, Keep_Alias);
            if Sub_Res /= Null_Iir then
               Add_Result (Res, Sub_Res);
            end if;
         end if;
      end Sem_As_Expanded_Name;

      --  LRM93 §6.3
      --  For a selected name that is used to denote a record element,
      --  the suffix must be a simple name denoting an element of a
      --  record object or value.  The prefix must be appropriate for the
      --  type of this object or value.
      procedure Sem_As_Selected_Element (Sub_Name : Iir)
      is
         Base_Type : Iir;
         Ptr_Type : Iir;
         Rec_El : Iir;
         R : Iir;
         Se : Iir;
      begin
         --  FIXME: if not is_expr (sub_name) return.
         Base_Type := Get_Base_Type (Get_Type (Sub_Name));
         if Get_Kind (Base_Type) = Iir_Kind_Access_Type_Definition
         then
            Ptr_Type := Base_Type;
            Base_Type :=
              Get_Base_Type (Get_Designated_Type (Base_Type));
         else
            Ptr_Type := Null_Iir;
         end if;

         if Get_Kind (Base_Type) /= Iir_Kind_Record_Type_Definition then
            return;
         end if;

         Rec_El := Find_Name_In_List
           (Get_Elements_Declaration_List (Base_Type), Suffix);
         if Rec_El = Null_Iir then
            return;
         end if;

         if not Maybe_Function_Call (Sub_Name) then
            return;
         end if;

         R := Maybe_Insert_Function_Call (Name, Sub_Name);
         R := Maybe_Insert_Dereference (R, Ptr_Type);

         Se := Create_Iir (Iir_Kind_Selected_Element);
         Location_Copy (Se, Name);
         Set_Prefix (Se, R);
         Set_Type (Se, Get_Type (Rec_El));
         Set_Selected_Element (Se, Rec_El);
         Set_Base_Name (Se, Get_Base_Name (R));
         Set_Base_Name (Name, Get_Base_Name (R));
         Add_Result (Res, Se);
      end Sem_As_Selected_Element;

      procedure Error_Selected_Element (Prefix_Type : Iir)
      is
         Base_Type : Iir;
      begin
         Base_Type := Get_Base_Type (Prefix_Type);
         if Get_Kind (Base_Type) = Iir_Kind_Access_Type_Definition then
            Base_Type := Get_Base_Type (Get_Designated_Type (Base_Type));
         end if;
         if Get_Kind (Base_Type) /= Iir_Kind_Record_Type_Definition then
            Error_Msg_Sem
              (Disp_Node (Prefix) & " does not designate a record", Name);
         else
            Error_Msg_Sem
              ("no element """ & Name_Table.Image (Suffix)
               & """ in " & Disp_Node (Base_Type), Name);
         end if;
      end Error_Selected_Element;

      procedure Sem_As_Method_Call (Sub_Name : Iir)
      is
         Prot_Type : Iir;
         Method : Iir;
      begin
         Prot_Type := Get_Type (Sub_Name);
         Method := Find_Name_In_Chain
           (Get_Declaration_Chain (Prot_Type), Suffix);
         if Method = Null_Iir then
            Error_Msg_Sem
              ("no method " & Name_Table.Image (Suffix) & " in "
               & Disp_Node (Prot_Type), Name);
            return;
         else
            Add_Result (Res, Method);
         end if;

--          case Get_Kind (Method) is
--             when Iir_Kind_Function_Declaration =>
--                Call := Create_Iir (Iir_Kind_Function_Call);
--                Set_Type (Call, Get_Return_Type (Method));
--                Set_Base_Name (Call, Call);
--             when Iir_Kind_Procedure_Declaration =>
--                Call := Create_Iir (Iir_Kind_Procedure_Call);
--             when others =>
--                Error_Kind ("sem_as_method_call", Method);
--          end case;
--          Location_Copy (Call, Sub_Name);
--          Set_Implementation (Call, Method);
--          --Set_Parameter_Association_Chain (Call, Xx);
--          Add_Result (Res, Call);
      end Sem_As_Method_Call;

   begin
      Prefix := Get_Prefix (Name);
      Prefix_Loc := Get_Location (Prefix);
      Sem_Name (Prefix, False);
      Prefix := Get_Named_Entity (Prefix);
      if Prefix = Error_Mark then
         Set_Named_Entity (Name, Prefix);
         return;
      end if;
      Suffix := Get_Suffix_Identifier (Name);

      Res := Null_Iir;

      --  FIXME: do better.
      --
      case Get_Kind (Prefix) is
         when Iir_Kind_Overload_List =>
            --  LRM93 6.3
            --  If, according to the visibility rules, there is at
            --  least one possible interpretation of the prefix of a
            --  selected name as the name of an enclosing entity
            --  interface, architecture, subprogram, block statement,
            --  process statement, generate statement, or loop
            --  statement, then the only interpretations considered are
            --  those of the immediately preceding paragraph.
            --
            --  In this case, the selected name is always interpreted
            --  as an expanded name.  In particular, no interpretations
            --  of the prefix as a function call are considered.
            declare
               Prefix_List : Iir_List;
               El : Iir;
            begin
               Prefix_List := Get_Overload_List (Prefix);
               for I in Natural loop
                  El := Get_Nth_Element (Prefix_List, I);
                  exit when El = Null_Iir;
                  Sem_As_Expanded_Name (El);
               end loop;
               if Res /= Null_Iir then
                  for I in Natural loop
                     El := Get_Nth_Element (Prefix_List, I);
                     exit when El = Null_Iir;
                     Sem_As_Selected_Element (El);
                  end loop;
               end if;
            end;
            if Res = Null_Iir then
               Error_Msg_Sem ("no suffix """ & Name_Table.Image (Suffix)
                              & """ for overloaded selected name", Name);
            end if;
         when Iir_Kind_Library_Declaration =>
            --  LRM93 6.3
            --  An expanded name denotes a primary unit constained in a design
            --  library if the prefix denotes the library and the suffix is the
            --  simple name if a primary unit whose declaration is contained
            --  in that library.
            --  An expanded name is not allowed for a secondary unit,
            --  particularly for an architecture body.
            --  GHDL: FIXME: error message more explicite
            Res := Libraries.Load_Primary_Unit (Prefix, Suffix, Name);
            if Res = Null_Iir then
               Error_Msg_Sem
                 ("primary unit """ & Name_Table.Image (Suffix)
                  & """ not found in " & Disp_Node (Prefix), Name);
            else
               Sem.Add_Dependence (Res);
            end if;
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Design_Unit
--            | Iir_Kind_Architecture_Declaration
--            | Iir_Kind_Entity_Declaration
--            | Iir_Kind_Package_Declaration
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_For_Loop_Statement =>
            --  LRM93 §6.3
            --  An expanded name denotes a named entity declared immediatly
            --  within a named construct if the prefix that is an entity
            --  interface, an architecture, a subprogram, a block statement,
            --  a process statement, a generate statement, or a loop
            --  statement, and the suffix is the simple name, character
            --  literal, or operator symbol of an named entity whose
            --  declaration occurs immediatly within that construct.
            if Get_Kind (Prefix) = Iir_Kind_Design_Unit then
               Libraries.Load_Design_Unit (Prefix, Name);
               Sem.Add_Dependence (Prefix);
               Prefix := Get_Library_Unit (Prefix);
               --  Modified only for xrefs, since a design_unit points to
               --  the first context clause, while a library unit points to
               --  the identifier.
               Set_Named_Entity (Get_Prefix (Name), Prefix);
            end if;

            Res := Find_Declarations_In_List (Prefix, Name, Keep_Alias);

            if Res = Null_Iir then
               Error_Msg_Sem
                 ("no declaration for """ & Name_Table.Image (Suffix)
                  & """ in " & Disp_Node (Prefix), Name);
            else
               --  LRM93 §6.3
               --  This form of expanded name is only allowed within the
               --  construct itself.
               if Get_Kind (Prefix) /= Iir_Kind_Package_Declaration
                 and then not Get_Is_Within_Flag (Prefix)
               then
                  Error_Msg_Sem
                    ("this expanded name is only allowed within the construct",
                     Prefix_Loc);
                  --  Hum, keep res.
               end if;
            end if;
         when Iir_Kind_Function_Declaration =>
            Sem_As_Expanded_Name (Prefix);
            if Res = Null_Iir then
               Sem_As_Selected_Element (Prefix);
            end if;
            if Res = Null_Iir then
               Error_Selected_Element (Get_Return_Type (Prefix));
            end if;
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Function_Call =>
            if Get_Kind (Get_Type (Prefix))
              = Iir_Kind_Protected_Type_Declaration
            then
               Sem_As_Method_Call (Prefix);
            else
               Sem_As_Selected_Element (Prefix);
               if Res = Null_Iir then
                  Error_Selected_Element (Get_Type (Prefix));
               end if;
            end if;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Component_Instantiation_Statement =>
            Error_Msg_Sem
              (Disp_Node (Prefix) & " cannot be selected by name", Prefix_Loc);

         when others =>
            Error_Kind ("sem_selected_name(2)", Prefix);
      end case;
      if Res = Null_Iir then
         Res := Error_Mark;
      elsif not Is_Overload_List (Res) then
         --  Finish sem
         Finish_Sem_Name (Name, Res);
      end if;
      Set_Named_Entity (Name, Res);
   end Sem_Selected_Name;

   --  If ASSOC_LIST has one element, which is an expression without formal,
   --  return the actual, else return NULL_IIR.
   function Get_One_Actual (Assoc_Chain : Iir) return Iir
   is
      Assoc : Iir;
   begin
      if Assoc_Chain = Null_Iir or else Get_Chain (Assoc_Chain) /= Null_Iir
      then
         return Null_Iir;
      end if;
      Assoc := Assoc_Chain;
      if Get_Kind (Assoc) /= Iir_Kind_Association_Element_By_Expression then
         return Null_Iir;
      end if;
      if Get_Formal (Assoc) /= Null_Iir then
         return Null_Iir;
      end if;
      return Get_Actual (Assoc);
   end Get_One_Actual;

   function Slice_Or_Index (Actual : Iir) return Iir_Kind
   is
   begin
      --  But it may be a slice name.
      case Get_Kind (Actual) is
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Range_Expression =>
            return Iir_Kind_Slice_Name;
         when others =>
            if Is_Range_Attribute_Name (Actual) then
               return Iir_Kind_Slice_Name;
            end if;
      end case;
      --  By default, this is an indexed name.
      return Iir_Kind_Indexed_Name;
   end Slice_Or_Index;

   function Sem_Index_Specification (Name : Iir_Parenthesis_Name; Itype : Iir)
                                    return Iir
   is
      Actual : Iir;
      Kind : Iir_Kind;
      Res : Iir;
   begin
      Actual := Get_One_Actual (Get_Association_Chain (Name));
      if Actual = Null_Iir then
         Error_Msg_Sem ("only one index specification is allowed", Name);
         return Null_Iir;
      end if;
      case Get_Kind (Actual) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Sem_Name (Actual, False);
            Actual := Get_Named_Entity (Actual);
            --  FIXME: semantization to be finished.
            --Maybe_Finish_Sem_Name (Actual);
         when others =>
            null;
      end case;
      Kind := Slice_Or_Index (Actual);
      Res := Create_Iir (Kind);
      Location_Copy (Res, Name);
      case Kind is
         when Iir_Kind_Indexed_Name =>
            Actual := Sem_Expression (Actual, Itype);
            if Actual = Null_Iir then
               return Null_Iir;
            end if;
            Check_Read (Actual);
            if Get_Expr_Staticness (Actual) < Globally then
               Error_Msg_Sem ("index must be a static expression", Name);
            end if;
            Set_Index_List (Res, Create_Iir_List);
            Append_Element (Get_Index_List (Res), Actual);
         when Iir_Kind_Slice_Name =>
            Actual := Sem_Discrete_Range_Expression (Actual, Itype, False);
            if Actual = Null_Iir then
               return Null_Iir;
            end if;
            if Get_Discrete_Range_Staticness (Actual) < Globally then
               Error_Msg_Sem ("index must be a static expression", Name);
            end if;
            Set_Suffix (Res, Actual);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Sem_Index_Specification;

   procedure Sem_Parenthesis_Name (Name : Iir_Parenthesis_Name)
   is
      Prefix: Iir;
      Prefix_Name : Iir;
      Res : Iir;
      Assoc_Chain : Iir;

      Slice_Index_Kind : Iir_Kind;

      procedure Index_Or_Not
      is
         El : Iir;
      begin
         Slice_Index_Kind := Iir_Kind_Error;
         El := Assoc_Chain;
         while El /= Null_Iir loop
            case Get_Kind (El) is
               when Iir_Kind_Association_Element_By_Expression =>
                  if Get_Formal (El) /= Null_Iir then
                     return;
                  end if;
               when others =>
                  --  Only expression are allowed.
                  return;
            end case;
            El := Get_Chain (El);
         end loop;
         Slice_Index_Kind := Iir_Kind_Indexed_Name;
      end Index_Or_Not;

      --  If FINISH is TRUE, then display error message in case of error.
      function Sem_As_Indexed_Or_Slice_Name (Sub_Name : Iir; Finish : Boolean)
        return Iir
      is
         Base_Type : Iir;
         Ptr_Type : Iir;
         P : Iir;
         R : Iir;
      begin
         if Slice_Index_Kind = Iir_Kind_Error then
            if Finish then
               Error_Msg_Sem ("prefix is not a function name", Name);
            end if;
            --  No way.
            return Null_Iir;
         end if;

         --  Only prefixes can be indexed or sliced.
         --  Catch errors such as slice of a type conversion.
         if not Is_Object_Name (Sub_Name)
           and then Get_Kind (Sub_Name) not in Iir_Kinds_Function_Declaration
         then
            if Finish then
               Error_Msg_Sem ("prefix is not a name (found "
                              & Disp_Node (Sub_Name) & ")", Name);
            end if;
            return Null_Iir;
         end if;

         --  Extract type of prefix, handle possible implicit deference.
         Base_Type := Get_Base_Type (Get_Type (Sub_Name));
         if Get_Kind (Base_Type) = Iir_Kind_Access_Type_Definition
         then
            Ptr_Type := Base_Type;
            Base_Type :=
              Get_Base_Type (Get_Designated_Type (Base_Type));
         else
            Ptr_Type := Null_Iir;
         end if;

         if Get_Kind (Base_Type) /= Iir_Kind_Array_Type_Definition then
            if Finish then
               Error_Msg_Sem ("type of prefix is not an array", Name);
            end if;
            return Null_Iir;
         end if;
         if Get_Nbr_Elements (Get_Index_Subtype_List (Base_Type)) /=
           Get_Chain_Length (Assoc_Chain)
         then
            if Finish then
               Error_Msg_Sem
                 ("number of indexes mismatches array dimension", Name);
            end if;
            return Null_Iir;
         end if;

         if not Maybe_Function_Call (Sub_Name) then
            if Finish then
               --  Should not happen.
               raise Internal_Error;
            end if;
            return Null_Iir;
         end if;

         P := Maybe_Insert_Function_Call (Prefix_Name, Sub_Name);
         P := Maybe_Insert_Dereference (P, Ptr_Type);

         R := Create_Iir (Slice_Index_Kind);
         Location_Copy (R, Name);
         Set_Prefix (R, P);

         case Slice_Index_Kind is
            when Iir_Kind_Slice_Name =>
               Set_Suffix (R, Get_Actual (Assoc_Chain));
               Set_Type (R, Get_Type (P));
            when Iir_Kind_Indexed_Name =>
               declare
                  Idx_El : Iir;
                  Idx_List : Iir_List;
               begin
                  Idx_List := Create_Iir_List;
                  Set_Index_List (R, Idx_List);
                  Idx_El := Assoc_Chain;
                  while Idx_El /= Null_Iir loop
                     Append_Element (Idx_List, Get_Actual (Idx_El));
                     Idx_El := Get_Chain (Idx_El);
                  end loop;
               end;
               Set_Type (R, Get_Element_Subtype (Base_Type));
            when others =>
               raise Internal_Error;
         end case;

         return R;
      end Sem_As_Indexed_Or_Slice_Name;

      Actual : Iir;
      Actual_Expr : Iir;
   begin
      -- The prefix is a function name, a type mark or an array.
      Prefix_Name := Get_Prefix (Name);
      Sem_Name (Prefix_Name, False);
      Prefix := Get_Named_Entity (Prefix_Name);
      if Prefix = Error_Mark then
         Set_Named_Entity (Name, Error_Mark);
         return;
      end if;
      Res := Null_Iir;

      Assoc_Chain := Get_Association_Chain (Name);
      Actual := Get_One_Actual (Assoc_Chain);

      if Actual /= Null_Iir
        and then
        (Get_Kind (Actual) = Iir_Kind_Range_Expression
         or else
         (Get_Kind (Actual) = Iir_Kind_Attribute_Name
          and then (Get_Attribute_Identifier (Actual) = Std_Names.Name_Range
                    or else
                    Get_Attribute_Identifier (Actual)
                    = Std_Names.Name_Reverse_Range)))
      then
         --  A slice.
         Slice_Index_Kind := Iir_Kind_Slice_Name;
         Add_Result (Res, Sem_As_Indexed_Or_Slice_Name (Prefix, True));
      elsif Actual /= Null_Iir
        and then (Get_Kind (Prefix) = Iir_Kind_Type_Declaration
                  or else Get_Kind (Prefix) = Iir_Kind_Subtype_Declaration)
      then
         --  A type conversion
         Res := Create_Iir (Iir_Kind_Type_Conversion);
         Location_Copy (Res, Name);
         Set_Type_Mark (Res, Prefix);
         Set_Type (Res, Get_Type (Prefix));
         Set_Expression (Res, Actual);
      else
         if Actual /= Null_Iir
           and then (Get_Kind (Actual) = Iir_Kind_Simple_Name
                     or Get_Kind (Actual) = Iir_Kind_Selected_Name)
         then
            Sem_Name (Actual, False);
            Actual_Expr := Get_Named_Entity (Actual);
            if Actual_Expr = Error_Mark then
               Set_Named_Entity (Name, Actual_Expr);
               return;
            end if;
            --  Decides between sliced or indexed name to actual.
            Slice_Index_Kind := Slice_Or_Index (Actual_Expr);
         else
            Index_Or_Not;
         end if;

         if Slice_Index_Kind /= Iir_Kind_Slice_Name then
            if Sem_Actual_Of_Association_Chain (Assoc_Chain) = False then
               Actual := Null_Iir;
            else
               Actual := Get_One_Actual (Assoc_Chain);
            end if;
         end if;

         case Get_Kind (Prefix) is
            when Iir_Kind_Overload_List =>
               declare
                  El : Iir;
                  Used : Boolean;
                  R : Iir;
                  Match : Boolean;
                  Prefix_List : Iir_List;
               begin
                  Prefix_List := Get_Overload_List (Prefix);
                  for I in Natural loop
                     El := Get_Nth_Element (Prefix_List, I);
                     exit when El = Null_Iir;
                     Used := False;
                     if Get_Kind (El) in Iir_Kinds_Function_Declaration then
                        Sem_Association_Chain
                          (Get_Interface_Declaration_Chain (El),
                           Assoc_Chain, False, Missing_Parameter, Name, Match);
                        if Match then
                           Add_Result
                             (Res, Sem_As_Function_Call (Prefix_Name, El,
                                                         Assoc_Chain));
                           Used := True;
                        end if;
                     end if;
                     if Get_Kind (El) not in Iir_Kinds_Procedure_Declaration
                     then
                        R := Sem_As_Indexed_Or_Slice_Name (El, False);
                        if R /= Null_Iir then
                           Add_Result (Res, R);
                           Used := True;
                        end if;
                     end if;
                     if not Used then
                        Sem_Name_Free_Result (El, Null_Iir);
                     end if;
                  end loop;
               end;
               if Res = Null_Iir then
                  Error_Msg_Sem
                    ("prefix is neither a function name "
                     & "nor can it be sliced or indexed", Name);
               end if;
            when Iir_Kinds_Function_Declaration =>
               Add_Result (Res, Sem_As_Function_Call (Prefix_Name,
                                                      Prefix, Assoc_Chain));
               Add_Result (Res, Sem_As_Indexed_Or_Slice_Name (Prefix, False));
               if Res = Null_Iir then
                  Error_Msg_Sem
                    ("prefix is neither a function name "
                     & "nor can it be sliced or indexed", Name);
               end if;

            when Iir_Kinds_Object_Declaration
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Slice_Name
              | Iir_Kind_Dereference
              | Iir_Kind_Implicit_Dereference
              | Iir_Kind_Selected_Element
              | Iir_Kind_Attribute_Value
              | Iir_Kind_Function_Call =>
               Add_Result (Res, Sem_As_Indexed_Or_Slice_Name (Prefix, True));

            when Iir_Kinds_Array_Attribute =>
               if Actual /= Null_Iir then
                  Finish_Sem_Array_Attribute (Prefix, Actual);
                  Set_Named_Entity (Name, Prefix);
               else
                  Error_Msg_Sem ("bad attribute parameter", Name);
                  Set_Named_Entity (Name, Error_Mark);
               end if;
               return;

            when Iir_Kinds_Scalar_Type_Attribute
              | Iir_Kind_Image_Attribute
              | Iir_Kind_Value_Attribute =>
               if Actual /= Null_Iir then
                  Finish_Sem_Scalar_Type_Attribute (Prefix, Actual);
                  Set_Named_Entity (Name, Prefix);
               else
                  Error_Msg_Sem ("bad attribute parameter", Name);
                  Set_Named_Entity (Name, Error_Mark);
               end if;
               return;

            when Iir_Kind_Type_Declaration
              | Iir_Kind_Subtype_Declaration =>
               Error_Msg_Sem
                 ("subprogram name is a type mark (missing apostrophe)", Name);

            when Iir_Kind_Stable_Attribute
              | Iir_Kind_Quiet_Attribute
              | Iir_Kind_Delayed_Attribute =>
               if Actual /= Null_Iir then
                  Finish_Sem_Signal_Attribute (Prefix, Actual);
                  Set_Named_Entity (Name, Prefix);
               else
                  Error_Msg_Sem ("bad attribute parameter", Name);
                  Set_Named_Entity (Name, Error_Mark);
               end if;
               return;

            when Iir_Kinds_Procedure_Declaration =>
               Error_Msg_Sem ("function name is a procedure", Name);

            when Iir_Kinds_Process_Statement
              | Iir_Kind_Component_Declaration
              | Iir_Kind_Type_Conversion =>
               Error_Msg_Sem
                 (Disp_Node (Prefix) & " cannot be indexed or sliced", Name);
               Res := Null_Iir;

            when Iir_Kind_Psl_Declaration =>
               Res := Sem_Psl.Sem_Psl_Name (Name);

            when others =>
               Error_Kind ("sem_parenthesis_name", Prefix);
         end case;
      end if;

      if Res = Null_Iir then
         Res := Error_Mark;
      elsif not Is_Overload_List (Res) then
         Finish_Sem_Name (Name, Res);
      end if;
      Set_Named_Entity (Name, Res);
   end Sem_Parenthesis_Name;

   procedure Sem_Selected_By_All_Name (Name : Iir_Selected_By_All_Name)
   is
      Prefix : Iir;
      Res : Iir;

      procedure Sem_As_Selected_By_All_Name (Sub_Name : Iir)
      is
         Base_Type : Iir;
         R : Iir;
      begin
         Base_Type := Get_Base_Type (Get_Type (Sub_Name));
         if Get_Kind (Base_Type) /= Iir_Kind_Access_Type_Definition then
            return;
         end if;

         R := Create_Iir (Iir_Kind_Dereference);
         Location_Copy (R, Name);
         Set_Prefix (R, Sub_Name);
         --  FIXME: access subtype.
         Set_Type (R, Get_Designated_Type (Base_Type));
         Add_Result (Res, R);
      end Sem_As_Selected_By_All_Name;
   begin
      Prefix := Get_Prefix (Name);
      Sem_Name (Prefix, True);
      Prefix := Get_Named_Entity (Prefix);
      if Prefix = Null_Iir then
         return;
      end if;
      Res := Null_Iir;
      case Get_Kind (Prefix) is
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Indexed_Name =>
            Sem_As_Selected_By_All_Name (Prefix);
         when Iir_Kind_Error =>
            Set_Named_Entity (Name, Error_Mark);
            return;
         when others =>
            Error_Kind ("sem_selected_by_all_name", Prefix);
      end case;
      if Res = Null_Iir then
         Error_Msg_Sem ("prefix is not an access", Name);
         Res := Error_Mark;
      elsif not Is_Overload_List (Res) then
         Finish_Sem_Name (Name, Res);
      end if;
      Set_Named_Entity (Name, Res);
   end Sem_Selected_By_All_Name;

   function Sem_Base_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix : Iir;
      Res : Iir;
      Base_Type : Iir;
      Type_Decl : Iir;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));
      case Get_Kind (Prefix) is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            null;
         when others =>
            Error_Msg_Sem
              ("prefix of 'base attribute must be a type or a subtype", Attr);
            return Error_Mark;
      end case;
      Res := Create_Iir (Iir_Kind_Base_Attribute);
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      Base_Type := Get_Base_Type (Get_Type (Prefix));
      Type_Decl := Get_Type_Declarator (Base_Type);
      if Get_Kind (Type_Decl) = Iir_Kind_Anonymous_Type_Declaration then
         Base_Type := Get_Subtype_Definition (Type_Decl);
      end if;
      Set_Type (Res, Base_Type);
      return Res;
   end Sem_Base_Attribute;

   function Sem_User_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix : Iir;
      Value : Iir;
      Attr_Id : Name_Id;
      Spec : Iir_Attribute_Specification;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));

      --  LRM93 6.6
      --  If the attribute name denotes an alias, then the attribute name
      --  denotes an attribute of the aliased name and not the alias itself,
      --  except when the attribute designator denotes any of the predefined
      --  attributes 'simple_name, 'path_name, or 'instance_name.
      if Get_Kind (Prefix) = Iir_Kind_Object_Alias_Declaration then
         --  GHDL: according to 4.3.3, the name cannot be an alias.
         Prefix := Get_Name (Prefix);
      end if;

      --  LRM93 6.6
      --  If the attribute designator denotes a user-defined attribute, the
      --  prefix cannot denote a subelement or a slice of an object.
      case Get_Kind (Prefix) is
         when Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            Error_Msg_Sem ("prefix of user defined attribute cannot be an "
                           & "object subelement", Attr);
            return Error_Mark;
         when Iir_Kind_Dereference =>
            Error_Msg_Sem ("prefix of user defined attribute cannot be an "
                           & "anonymous object", Attr);
            return Error_Mark;
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Design_Unit
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kinds_Function_Declaration
           | Iir_Kinds_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Unit_Declaration
           | Iir_Kinds_Sequential_Statement
           | Iir_Kinds_Concurrent_Statement =>
            --  FIXME: to complete
            null;
         when others =>
            Error_Kind ("sem_user_attribute", Prefix);
      end case;

      Attr_Id := Get_Attribute_Identifier (Attr);
      Value := Get_Attribute_Value_Chain (Prefix);
      while Value /= Null_Iir loop
         Spec := Get_Attribute_Specification (Value);
         exit when Get_Identifier (Get_Attribute_Designator (Spec)) = Attr_Id;
         Value := Get_Chain (Value);
      end loop;
      if Value = Null_Iir then
         Error_Msg_Sem
           (Disp_Node (Prefix) & " was not annotated with attribute '"
            & Name_Table.Image (Attr_Id) & ''', Attr);
         if Attr_Id = Std_Names.Name_First or Attr_Id = Std_Names.Name_Last
         then
            --  Nice (?) message for Ada users.
            Error_Msg_Sem
              ("(you may use 'high, 'low, 'left or 'right attribute)", Attr);
         end if;
         return Error_Mark;
      end if;

      Xref_Ref (Attr, Value);

      return Value;
   end Sem_User_Attribute;

   function Sem_Scalar_Type_Attribute (Attr : Iir_Attribute_Name)
     return Iir
   is
      use Std_Names;
      Prefix_Name : Iir;
      Prefix : Iir;
      Prefix_Type : Iir;
      Res : Iir;
      Id : Name_Id;
   begin
      Id := Get_Attribute_Identifier (Attr);
      Prefix_Name := Get_Prefix (Attr);
      Prefix := Get_Named_Entity (Prefix_Name);
      --  LRM93 14.1
      --  Prefix: Any discrete or physical type of subtype T.
      case Get_Kind (Prefix) is
         when Iir_Kinds_Type_Declaration
           | Iir_Kind_Base_Attribute =>
            null;
         when others =>
            Error_Msg_Sem ("prefix of '" & Name_Table.Image (Id)
                           & " attribute must be a type", Attr);
            return Error_Mark;
      end case;
      Prefix_Type := Get_Type (Prefix);
      case Id is
         when Name_Image
           | Name_Value =>
            if Get_Kind (Prefix_Type) not in Iir_Kinds_Scalar_Type_Definition
            then
               Error_Msg_Sem
                 ("prefix of '" & Name_Table.Image (Id)
                  & " attribute must be a scalar type", Attr);
               Error_Msg_Sem
                 ("found " & Disp_Node (Prefix_Type)
                  & " defined at " & Disp_Location (Prefix_Type), Attr);
               return Error_Mark;
            end if;
         when others =>
            case Get_Kind (Prefix_Type) is
               when Iir_Kinds_Discrete_Type_Definition
                 | Iir_Kind_Physical_Subtype_Definition
                 | Iir_Kind_Physical_Type_Definition =>
                  null;
               when others =>
                  Error_Msg_Sem
                    ("prefix of '" & Name_Table.Image (Id)
                     & " attribute must be discrete or physical type", Attr);
                  Error_Msg_Sem
                    ("found " & Disp_Node (Prefix_Type)
                     & " defined at " & Disp_Location (Prefix_Type), Attr);
                  return Error_Mark;
            end case;
      end case;

      --  Create the resulting node.
      case Get_Attribute_Identifier (Attr) is
         when Name_Pos =>
            Res := Create_Iir (Iir_Kind_Pos_Attribute);
         when Name_Val =>
            Res := Create_Iir (Iir_Kind_Val_Attribute);
         when Name_Succ =>
            Res := Create_Iir (Iir_Kind_Succ_Attribute);
         when Name_Pred =>
            Res := Create_Iir (Iir_Kind_Pred_Attribute);
         when Name_Leftof =>
            Res := Create_Iir (Iir_Kind_Leftof_Attribute);
         when Name_Rightof =>
            Res := Create_Iir (Iir_Kind_Rightof_Attribute);
         when Name_Image =>
            Res := Create_Iir (Iir_Kind_Image_Attribute);
         when Name_Value =>
            Res := Create_Iir (Iir_Kind_Value_Attribute);
         when others =>
            raise Internal_Error;
      end case;
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);

      case Get_Attribute_Identifier (Attr) is
         when Name_Pos =>
            --  LRM93 14.1
            --  Result type: universal_integer.
            Set_Type (Res, Convertible_Integer_Type_Definition);
         when Name_Val =>
            --  LRM93 14.1
            --  Result type: the base type of T
            Set_Type (Res, Get_Base_Type (Prefix_Type));
         when Name_Succ
           | Name_Pred
           | Name_Leftof
           | Name_Rightof =>
            --  LRM93 14.1
            --  Result type: the base type of T.
            Set_Type (Res, Get_Base_Type (Prefix_Type));
         when Name_Image =>
            --  LRM93 14.1
            --  Result type: type string
            Set_Type (Res, String_Type_Definition);
         when Name_Value =>
            --  LRM93 14.1
            --  Result type: the base type of T.
            Set_Type (Res, Get_Base_Type (Prefix_Type));
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Sem_Scalar_Type_Attribute;

   --  Sem attributes whose prefix is a type or a subtype.
   function Sem_Predefined_Type_Attribute (Attr : Iir_Attribute_Name)
     return Iir
   is
      use Std_Names;
      Res : Iir;
      Prefix : Iir;
      Prefix_Type : Iir;
   begin
      case Get_Attribute_Identifier (Attr) is
         when Name_Left =>
            Res := Create_Iir (Iir_Kind_Left_Type_Attribute);
         when Name_Right =>
            Res := Create_Iir (Iir_Kind_Right_Type_Attribute);
         when Name_High =>
            Res := Create_Iir (Iir_Kind_High_Type_Attribute);
         when Name_Low =>
            Res := Create_Iir (Iir_Kind_Low_Type_Attribute);
         when Name_Ascending =>
            Res := Create_Iir (Iir_Kind_Ascending_Type_Attribute);
         when Name_Range
           | Name_Reverse_Range =>
            Error_Msg_Sem
              ("prefix of range attribute must be an array type or object",
               Attr);
            return Error_Mark;
         when others =>
            raise Internal_Error;
      end case;
      Location_Copy (Res, Attr);
      Prefix := Get_Named_Entity (Get_Prefix (Attr));
      Set_Prefix (Res, Prefix);
      Prefix_Type := Get_Type (Prefix);
      case Get_Attribute_Identifier (Attr) is
         when Name_Ascending =>
            --  LRM93 14.1
            --  Result Type: type boolean.
            Set_Type (Res, Boolean_Type_Definition);
         when others =>
            --  LRM 14.1
            --  Result Type: Same type as T.
            Set_Type (Res, Prefix_Type);
      end case;
      Set_Expr_Staticness (Res, Get_Type_Staticness (Prefix_Type));
      return Res;
   end Sem_Predefined_Type_Attribute;

   --  Called for attributes Length, Left, Right, High, Low, Range,
   --  Reverse_Range, Ascending.
   function Sem_Array_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      use Std_Names;
      Prefix: Iir;
      Prefix_Type : Iir;
      Res : Iir;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));

      --  LRM93 14.1
      --  Prefix: Any prefix A that is appropriate for an array object, or an
      --  alias thereof, or that denotes a constrained array subtype.
      case Get_Kind (Prefix) is
         when Iir_Kind_Dereference
           | Iir_Kinds_Object_Declaration
           | Iir_Kind_Function_Call
           | Iir_Kind_Selected_Element
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Image_Attribute =>
            --  FIXME: list of expr.
            Prefix_Type := Get_Type (Prefix);
            case Get_Kind (Prefix_Type) is
               when Iir_Kind_Access_Type_Definition
                 | Iir_Kind_Access_Subtype_Definition =>
                  declare
                     Designated_Type : Iir;
                  begin
                     Designated_Type :=
                       Get_Designated_Type (Get_Base_Type (Prefix_Type));
                     Prefix := Insert_Implicit_Dereference (Prefix, Attr);
                     Prefix_Type := Designated_Type;
                  end;
               when Iir_Kinds_Array_Type_Definition =>
                  null;
               when others =>
                  Error_Msg_Sem ("object prefix must be an array", Attr);
                  return Error_Mark;
            end case;
--          when Iir_Kind_Array_Subtype_Definition =>
--             Prefix_Type := Prefix;
--          when Iir_Kind_Array_Type_Definition =>
--             Error_Type;
--             return Null_Iir;
         when Iir_Kind_Subtype_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Base_Attribute =>
            Prefix_Type := Get_Type (Prefix);
            if not Is_Fully_Constrained_Type (Prefix_Type) then
               Error_Msg_Sem ("prefix type is not constrained", Attr);
               --  We continue using the unconstrained array type.
               --  At least, this type is valid; and even if the array was
               --  constrained, the base type would be the same.
            end if;
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Process_Statement =>
            Error_Msg_Sem
              (Disp_Node (Prefix) & " is not an appropriate prefix for '"
               & Name_Table.Image (Get_Attribute_Identifier (Attr))
               & " attribute",
               Attr);
            return Error_Mark;
         when others =>
            Error_Msg_Sem ("prefix must denote an array object or type", Attr);
            return Error_Mark;
      end case;

      case Get_Kind (Prefix_Type) is
         when Iir_Kinds_Scalar_Type_Definition =>
            --  FIXME: check prefix is a scalar type or subtype.
            return Sem_Predefined_Type_Attribute (Attr);
         when Iir_Kinds_Array_Type_Definition =>
            null;
         when others =>
            Error_Msg_Sem
              ("prefix of '"
               & Name_Table.Image (Get_Attribute_Identifier (Attr))
               & " attribute must denote a constrained array subtype",
               Attr);
            return Error_Mark;
      end case;

      case Get_Attribute_Identifier (Attr) is
         when Name_Left =>
            Res := Create_Iir (Iir_Kind_Left_Array_Attribute);
         when Name_Right =>
            Res := Create_Iir (Iir_Kind_Right_Array_Attribute);
         when Name_High =>
            Res := Create_Iir (Iir_Kind_High_Array_Attribute);
         when Name_Low =>
            Res := Create_Iir (Iir_Kind_Low_Array_Attribute);
         when Name_Range =>
            Res := Create_Iir (Iir_Kind_Range_Array_Attribute);
         when Name_Reverse_Range =>
            Res := Create_Iir (Iir_Kind_Reverse_Range_Array_Attribute);
         when Name_Length =>
            Res := Create_Iir (Iir_Kind_Length_Array_Attribute);
            Set_Type (Res, Convertible_Integer_Type_Definition);
         when Name_Ascending =>
            Res := Create_Iir (Iir_Kind_Ascending_Array_Attribute);
            Set_Type (Res, Boolean_Type_Definition);
         when others =>
            raise Internal_Error;
      end case;
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      return Res;
   end Sem_Array_Attribute;

   function Sem_Signal_Signal_Attribute
     (Attr : Iir_Attribute_Name; Kind : Iir_Kind)
     return Iir
   is
      Res : Iir;
      Prefix : Iir;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));
      Res := Create_Iir (Kind);
      if Kind = Iir_Kind_Delayed_Attribute then
         Set_Type (Res, Get_Type (Prefix));
      elsif Kind = Iir_Kind_Transaction_Attribute then
         Set_Type (Res, Bit_Type_Definition);
      else
         Set_Type (Res, Boolean_Type_Definition);
      end if;
      Set_Base_Name (Res, Res);
--       Param := Get_Suffix (Attr);
--       if Param /= Null_Iir then
--          if Kind = Iir_Kind_Transaction_Attribute then
--             Error_Msg_Sem ("'transaction does not allow a parameter", Attr);
--             Param := Null_Iir;
--          else
--             Param := Sem_Expression
--               (Param, Time_Subtype_Definition);
--             Set_Parameter (Res, Param);
--          end if;
--       end if;

      if Get_Kind (Prefix) = Iir_Kind_Signal_Interface_Declaration then
         --  LRM93 2.1.1.2 / LRM08 4.2.2.3
         --
         --  It is an error if signal-valued attributes 'STABLE , 'QUIET,
         --  'TRANSACTION, and 'DELAYED of formal signal paramaters of any
         --  mode are read within a subprogram.
         case Get_Kind (Get_Parent (Prefix)) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Error_Msg_Sem
                 ("'" & Name_Table.Image (Get_Attribute_Identifier (Attr)) &
                  " is not allowed for a signal parameter", Attr);
            when others =>
               null;
         end case;
      end if;
      Sem_Stmts.Add_Declaration_For_Implicit_Signal (Res);
      return Res;
   end Sem_Signal_Signal_Attribute;

   function Sem_Signal_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      use Std_Names;
      Prefix: Iir;
      Res : Iir;
      Base : Iir;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));
      Base := Get_Object_Prefix (Prefix);
      case Get_Kind (Base) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kinds_Signal_Attribute =>
            null;
         when others =>
            Error_Msg_Sem
              ("prefix of '"
               & Name_Table.Image (Get_Attribute_Identifier (Attr))
               & " attribute must denote a signal", Attr);
            return Error_Mark;
      end case;
      case Get_Attribute_Identifier (Attr) is
         when Name_Stable =>
            Res := Sem_Signal_Signal_Attribute
              (Attr, Iir_Kind_Stable_Attribute);
         when Name_Quiet =>
            Res := Sem_Signal_Signal_Attribute
              (Attr, Iir_Kind_Quiet_Attribute);
         when Name_Delayed =>
            Res := Sem_Signal_Signal_Attribute
              (Attr, Iir_Kind_Delayed_Attribute);
         when Name_Transaction =>
            Res := Sem_Signal_Signal_Attribute
              (Attr, Iir_Kind_Transaction_Attribute);
         when Name_Event =>
            Res := Create_Iir (Iir_Kind_Event_Attribute);
            Set_Type (Res, Boolean_Type_Definition);
         when Name_Active =>
            Res := Create_Iir (Iir_Kind_Active_Attribute);
            Set_Type (Res, Boolean_Type_Definition);
         when Name_Last_Value =>
            Res := Create_Iir (Iir_Kind_Last_Value_Attribute);
            Set_Type (Res, Get_Type (Prefix));
         when Name_Last_Event =>
            Res := Create_Iir (Iir_Kind_Last_Event_Attribute);
            Set_Type (Res, Time_Type_Definition);
         when Name_Last_Active =>
            Res := Create_Iir (Iir_Kind_Last_Active_Attribute);
            Set_Type (Res, Time_Type_Definition);
         when Name_Driving_Value =>
            Res := Create_Iir (Iir_Kind_Driving_Value_Attribute);
            Set_Type (Res, Get_Type (Prefix));
            --  FIXME: check restrictions.
         when Name_Driving =>
            Res := Create_Iir (Iir_Kind_Driving_Attribute);
            Set_Type (Res, Boolean_Type_Definition);
            --  FIXME: check restrictions.
         when others =>
            --  Not yet implemented attribute, or really an internal error.
            raise Internal_Error;
      end case;
      Location_Copy (Res, Attr);

      --  LRM 4.3.2
      --  The value of an object is said to be read when one of the following
      --  conditions is satisfied:
      --  [...]
      --  * When the object is a signal and the value of any of its predefined
      --    attributes 'STABLE, 'QUIET, 'DELAYED, 'TRANSACTION, 'EVENT,
      --    'ACTIVE, 'LAST_EVENT, 'LAST_ACTIVE, or 'LAST_VALUE is read.

      --  LRM 14.1
      --  S'Driving Restrictions:
      --  S'Driving_Value Restrictions:
      --  This attribute is available only from within a process, a
      --  concurrent statement with an equivalent process, or a subprogram.
      --  If the prefix denotes a port, it is an error if the port does not
      --  have a mode of INOUT, OUT or BUFFER.  It is also an error if the
      --  attribute name appears in a subprogram body that is not a declarative
      --  item contained within a process statement and the prefix is not a
      --  formal parameter of the given subprogram or of a parent of that
      --  subprogram.  Finally, it is an error if the prefix denotes a
      --  subprogram formal parameter whose mode is not INOUT or OUT, or if
      --  S'Driving is False at the time of the evaluation of S'Driving_Value.
      case Get_Kind (Res) is
         when Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute =>
            Check_Read (Prefix);
         when Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute =>
            --  FIXME: complete checks.
            if Get_Current_Concurrent_Statement = Null_Iir then
               Error_Msg_Sem
                 ("'driving or 'driving_value is available only within a "
                  & "concurrent statement", Attr);
            else
               case Get_Kind (Get_Current_Concurrent_Statement) is
                  when Iir_Kinds_Process_Statement
                    | Iir_Kind_Concurrent_Conditional_Signal_Assignment
                    | Iir_Kind_Concurrent_Selected_Signal_Assignment
                    | Iir_Kind_Concurrent_Procedure_Call_Statement =>
                     null;
                  when others =>
                     Error_Msg_Sem
                       ("'driving or 'driving_value not available within "
                        & "this concurrent statement", Attr);
               end case;
            end if;

            case Get_Kind (Base) is
               when Iir_Kind_Signal_Declaration =>
                  null;
               when Iir_Kind_Signal_Interface_Declaration =>
                  case Get_Mode (Base) is
                     when Iir_Buffer_Mode
                       | Iir_Inout_Mode
                       | Iir_Out_Mode =>
                        null;
                     when others =>
                        Error_Msg_Sem
                          ("mode of 'driving or 'driving_value prefix must "
                           & "be out, inout or buffer", Attr);
                  end case;
               when others =>
                  Error_Msg_Sem
                    ("bad prefix for 'driving or 'driving_value", Attr);
            end case;
         when others =>
            null;
      end case;

      --  According to LRM 7.4, signal attributes are not static expressions
      --  since the prefix (a signal) is not a static expression.
      Set_Expr_Staticness (Res, None);

      --  LRM 6.1
      --  A name is said to be a static name if and only if at least one of
      --  the following conditions holds:
      --  [...]
      --  -  The name is a attribute name whose prefix is a static signal name
      --     and whose suffix is one of the predefined attributes 'DELAYED,
      --     'STABLE, 'QUIET or 'TRANSACTION.
      --  According to LRM 6.1, attributes are not static names.
      if Flags.Vhdl_Std = Vhdl_93c or Flags.Vhdl_Std >= Vhdl_02 then
         case Get_Kind (Res) is
            when Iir_Kind_Stable_Attribute
              | Iir_Kind_Quiet_Attribute
              | Iir_Kind_Delayed_Attribute
              | Iir_Kind_Transaction_Attribute =>
               Set_Name_Staticness (Res, Get_Name_Staticness (Prefix));
            when others =>
               Set_Name_Staticness (Res, None);
         end case;
      else
         Set_Name_Staticness (Res, None);
      end if;

      Set_Prefix (Res, Prefix);

      --  Set has_active_flag when activity is read.
      case Get_Kind (Res) is
         when Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Active_Attribute =>
            Set_Has_Active_Flag (Base, True);
         when others =>
            null;
      end case;

      return Res;
   end Sem_Signal_Attribute;

   --  'Simple_name, 'instance_name and 'path_name.
   function Sem_Name_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      use Std_Names;
      Prefix: Iir;
      Res : Iir;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));
      --  LRM 14.1  Predefined attributes
      --  E'SIMPLE_NAME
      --    Prefix: Any named entity as defined in 5.1
      --  E'INSTANCE_NAME
      --    Prefix: Any named entity other than the local ports and generics
      --       of a component declaration.
      --  E'PATH_NAME
      --    Prefix: Any named entity other than the local ports and generics
      --       of a component declaration.
      case Get_Kind (Prefix) is
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Design_Unit
           | Iir_Kind_Non_Object_Alias_Declaration =>
            null;

         when Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Constant_Interface_Declaration =>
            if Get_Attribute_Identifier (Attr) /= Name_Simple_Name
              and then Get_Kind (Get_Parent (Prefix))
              = Iir_Kind_Component_Declaration
            then
               Error_Msg_Sem
                 ("local ports or generics of a component cannot be a prefix",
                  Attr);
            end if;
         when others =>
            Error_Msg_Sem (Disp_Node (Prefix) & " is not a named entity",
                           Attr);
      end case;

      case Get_Attribute_Identifier (Attr) is
         when Name_Simple_Name =>
            Res := Create_Iir (Iir_Kind_Simple_Name_Attribute);
            Eval_Simple_Name (Get_Identifier (Prefix));
            Set_Simple_Name_Identifier (Res, Name_Table.Get_Identifier);
            Set_Type (Res, Create_Unidim_Array_By_Length
                      (String_Type_Definition,
                       Iir_Int64 (Name_Table.Name_Length),
                       Attr));
            Set_Expr_Staticness (Res, Locally);

         when Name_Path_Name =>
            Res := Create_Iir (Iir_Kind_Path_Name_Attribute);
            Set_Expr_Staticness (Res, Globally);
            Set_Type (Res, String_Type_Definition);

         when Name_Instance_Name =>
            Res := Create_Iir (Iir_Kind_Instance_Name_Attribute);
            Set_Expr_Staticness (Res, Globally);
            Set_Type (Res, String_Type_Definition);

         when others =>
            raise Internal_Error;
      end case;

      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      return Res;
   end Sem_Name_Attribute;

   procedure Sem_Attribute (Attr : Iir_Attribute_Name)
   is
      use Std_Names;
      Prefix : Iir;
      Res : Iir;
      Sig : Iir_Signature;
   begin
      --  LRM93 6.6  Attribute names
      --  The meaning of the prefix of an attribute name must be determinable
      --  independently of the attribute designator and independently of the
      --  fact that it is the prefix of an attribute.
      Prefix := Get_Prefix (Attr);

      --  LRM93 6.6
      --  If the prefix of an attribute name denotes an alias, then the
      --  attribute name denotes an attribute of the aliased name and not the
      --  alias itself, except when the attribute designator denotes any of
      --  the predefined attributes 'Simple_Name, 'Path_Name or 'Instance_Name.
      --  If the prefix of an attribute name denotes an alias and the
      --  attribute designator denotes any of the predefined attributes
      --  'Simple_Name, 'Path_Name or 'Instance_Name, then the attribute name
      --  denotes the attribute of the alias and not of the aliased name.
      if Flags.Vhdl_Std > Vhdl_87
        and then Get_Attribute_Identifier (Attr) in Name_Id_Name_Attributes
      then
         Sem_Name (Prefix, True);
      else
         Sem_Name (Prefix, False);
      end if;

      Prefix := Get_Named_Entity (Prefix);
      if Prefix = Error_Mark then
         Set_Named_Entity (Attr, Prefix);
         return;
      end if;

      --  LRM93 6.6
      --  A signature may follow the prefix if and only if the prefix denotes
      --  a subprogram or enumeration literal, or an alias thereof.
      --  In this case, the signature is required to match (see Section 2.3.2)
      --  the parameter and result type profile of exactly one visible
      --  subprogram or enumeration literal, as is appropriate to the prefix.
      -- GHDL: this is done by Sem_Signature.
      Sig := Get_Signature (Attr);
      if Sig /= Null_Iir then
         Prefix := Sem_Signature (Prefix, Sig);
         if Prefix = Null_Iir then
            Set_Named_Entity (Attr, Error_Mark);
            return;
         end if;
         Set_Named_Entity (Get_Prefix (Attr), Prefix);
      end if;

      if Get_Kind (Prefix) = Iir_Kind_Overload_List then
         Error_Msg_Sem ("prefix of attribute is overloaded", Attr);
         Set_Named_Entity (Attr, Error_Mark);
         return;
      end if;

      case Get_Attribute_Identifier (Attr) is
         when Name_Base =>
            Res := Sem_Base_Attribute (Attr);
         when Name_Image
           | Name_Value =>
            if Flags.Vhdl_Std > Vhdl_87 then
               Res := Sem_Scalar_Type_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Pos
           | Name_Val
           | Name_Succ
           | Name_Pred
           | Name_Rightof
           | Name_Leftof =>
            Res := Sem_Scalar_Type_Attribute (Attr);

         when Name_Length
           | Name_Left
           | Name_Right
           | Name_High
           | Name_Low
           | Name_Range
           | Name_Reverse_Range =>
            Res := Sem_Array_Attribute (Attr);

         when Name_Ascending =>
            if Flags.Vhdl_Std > Vhdl_87 then
               Res := Sem_Array_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Stable
           | Name_Event
           | Name_Last_Value
           | Name_Delayed
           | Name_Quiet
           | Name_Transaction
           | Name_Active
           | Name_Last_Active
           | Name_Last_Event =>
            Res := Sem_Signal_Attribute (Attr);

         when Name_Driving
           | Name_Driving_Value =>
            if Flags.Vhdl_Std > Vhdl_87 then
               Res := Sem_Signal_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Simple_Name
           | Name_Path_Name
           | Name_Instance_Name =>
            if Flags.Vhdl_Std > Vhdl_87 then
               Res := Sem_Name_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when others =>
            Res := Sem_User_Attribute (Attr);
      end case;

      if Res = Null_Iir then
         Error_Kind ("sem_attribute", Attr);
      end if;
      Set_Named_Entity (Attr, Res);
   end Sem_Attribute;

   --  LRM93 §6
   procedure Sem_Name (Name : Iir; Keep_Alias : Boolean)
   is
   begin
      --  Exit now if NAME was already semantized.
      if Get_Named_Entity (Name) /= Null_Iir then
         return;
      end if;

      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Operator_Symbol =>
            --  String_Literal may be a symbol_operator.
            Sem_Simple_Name (Name, Keep_Alias, False);
         when Iir_Kind_Selected_Name =>
            Sem_Selected_Name (Name, Keep_Alias);
         when Iir_Kind_Parenthesis_Name =>
            Sem_Parenthesis_Name (Name);
         when Iir_Kind_Selected_By_All_Name =>
            Sem_Selected_By_All_Name (Name);
         when Iir_Kind_Attribute_Name =>
            Sem_Attribute (Name);
         when others =>
            Error_Kind ("sem_name", Name);
      end case;
   end Sem_Name;

   --  Finish semantisation of NAME, if necessary.
   procedure Maybe_Finish_Sem_Name (Name : Iir)
   is
      Expr : Iir;
   begin
      Expr := Get_Named_Entity (Name);
      case Get_Kind (Expr) is
         when Iir_Kind_Error =>
            null;
         when Iir_Kinds_Object_Declaration =>
            Set_Base_Name (Name, Expr);
            Sem_Check_Pure (Name, Expr);
            Sem_Check_All_Sensitized (Expr);
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference =>
            declare
               E : Iir;
            begin
               --  Get over implicit and explicit dereferences.
               E := Expr;
               loop
                  E := Get_Base_Name (E);
                  if Get_Kind (E) in Iir_Kinds_Dereference then
                     E := Get_Prefix (E);
                  else
                     exit;
                  end if;
               end loop;
               Sem_Check_Pure (Name, E);
               Sem_Check_All_Sensitized (E);
            end;
         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Unit_Declaration =>
            null;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            null;
         when Iir_Kind_Function_Call
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Type_Conversion =>
            null;
         when Iir_Kinds_Type_Attribute =>
            null;
         when Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute =>
            null;
         when Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute =>
            null;
         when Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute =>
            if Get_Parameter (Expr) = Null_Iir then
               Finish_Sem_Signal_Attribute (Expr, Null_Iir);
            end if;
         when Iir_Kinds_Array_Attribute =>
            if Get_Parameter (Expr) = Null_Iir then
               Finish_Sem_Array_Attribute (Expr, Null_Iir);
            end if;
         when Iir_Kinds_Scalar_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute =>
            if Get_Parameter (Expr) = Null_Iir then
               Finish_Sem_Scalar_Type_Attribute (Expr, Null_Iir);
            end if;
         when Iir_Kind_Implicit_Dereference =>
            --  Should not happen.
            raise Internal_Error;
         when Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Function_Declaration =>
            Finish_Sem_Function_Specification (Name, Expr);
         when Iir_Kind_Psl_Expression =>
            null;
         when others =>
            Error_Kind ("maybe_finish_sem_name", Expr);
      end case;
   end Maybe_Finish_Sem_Name;

   procedure Sem_Name_Soft (Name : Iir)
   is
   begin
      --  Exit now if NAME was already semantized.
      if Get_Named_Entity (Name) /= Null_Iir then
         return;
      end if;

      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Operator_Symbol =>
            --  String_Literal may be a symbol_operator.
            Sem_Simple_Name (Name, False, True);
         when others =>
            Error_Kind ("sem_name_soft", Name);
      end case;
   end Sem_Name_Soft;

   procedure Sem_Name_Clean (Name : Iir)
   is
      N : Iir;
      Next_N : Iir;
      Named_Entity : Iir;
      Atype : Iir;
   begin
      N := Name;
      while N /= Null_Iir loop
         case Get_Kind (N) is
            when Iir_Kind_Simple_Name
              | Iir_Kind_Operator_Symbol =>
               Next_N := Null_Iir;
            when others =>
               Error_Kind ("sem_name_clean", N);
         end case;

         --  Clear and free overload lists of Named_entity and type.
         Named_Entity := Get_Named_Entity (N);
         Set_Named_Entity (N, Null_Iir);
         if Named_Entity /= Null_Iir
           and then Is_Overload_List (Named_Entity)
         then
            Free_Iir (Named_Entity);
         end if;

         Atype := Get_Type (N);
         Set_Type (N, Null_Iir);
         if Atype /= Null_Iir
           and then Is_Overload_List (Atype)
         then
            Free_Iir (Atype);
         end if;

         N := Next_N;
      end loop;
   end Sem_Name_Clean;

   --  Remove procedure specification from LIST.
   function Remove_Procedures_From_List (Expr : Iir) return Iir
   is
      El : Iir;
      P : Natural;
      List : Iir_List;
   begin
      if not Is_Overload_List (Expr) then
         return Expr;
      end if;
      List := Get_Overload_List (Expr);
      P := 0;
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         case Get_Kind (El) is
            when Iir_Kinds_Procedure_Declaration =>
               null;
            when Iir_Kinds_Function_Declaration =>
               if Maybe_Function_Call (El) then
                  Replace_Nth_Element (List, P, El);
                  P := P + 1;
               end if;
            when others =>
               Replace_Nth_Element (List, P, El);
               P := P + 1;
         end case;
      end loop;
      case P is
         when 0 =>
            Free_Iir (Expr);
            return Null_Iir;
         when 1 =>
            El := Get_First_Element (List);
            Free_Iir (Expr);
            return El;
         when others =>
            Set_Nbr_Elements (List, P);
            return Expr;
      end case;
   end Remove_Procedures_From_List;

   --  Convert name EXPR to an expression (ie, create function call).
   --  A_TYPE is the expected type of the expression.
   --  Returns NULL_IIR in case of error.
   function Name_To_Expression (Name : Iir; A_Type : Iir) return Iir
   is
      Ret_Type : Iir;
      Res_Type : Iir;
      Expr : Iir;
      Expr_List : Iir_List;
      Res : Iir;
      El : Iir;
   begin
      Expr := Get_Named_Entity (Name);
      if Get_Kind (Expr) = Iir_Kind_Error then
         return Null_Iir;
      end if;
      if Check_Is_Expression (Expr, Name) = Null_Iir then
         return Null_Iir;
      end if;

      --  Note: EXPR may contain procedure names...
      Expr := Remove_Procedures_From_List (Expr);
      Set_Named_Entity (Name, Expr);
      if Expr = Null_Iir then
         Error_Msg_Sem ("procedure name " & Disp_Node (Name)
                        & " cannot be used as expression", Name);
         return Null_Iir;
      end if;

      if not Is_Overload_List (Expr) then
         Maybe_Finish_Sem_Name (Name);
         Expr := Get_Named_Entity (Name);
         if Expr = Null_Iir then
            return Null_Iir;
         end if;
         if A_Type /= Null_Iir then
            Res_Type := Get_Type (Expr);
            if Res_Type = Null_Iir then
               return Null_Iir;
            end if;
            if not Are_Basetypes_Compatible (Get_Base_Type (Res_Type), A_Type)
            then
               Error_Not_Match (Expr, A_Type, Name);
               return Null_Iir;
            end if;
            --  Fall through.
         end if;
      else
         --  EXPR is an overloaded name.
         Expr_List := Get_Overload_List (Expr);

         if A_Type /= Null_Iir then
            --  Find the name returning A_TYPE.
            Res := Null_Iir;
            for I in Natural loop
               El := Get_Nth_Element (Expr_List, I);
               exit when El = Null_Iir;
               if Are_Basetypes_Compatible (Get_Base_Type (Get_Type (El)),
                                            A_Type)
               then
                  Add_Result (Res, El);
               end if;
            end loop;
            if Res = Null_Iir then
               Error_Not_Match (Name, A_Type, Name);
               return Null_Iir;
            elsif Is_Overload_List (Res) then
               Error_Overload (Name);
               Disp_Overload_List (Get_Overload_List (Res), Name);
               return Null_Iir;
            else
               Sem_Name_Free_Result (Expr, Res);
               Set_Named_Entity (Name, Res);
               Finish_Sem_Name (Name, Res);
               Maybe_Finish_Sem_Name (Name);
               Expr := Get_Named_Entity (Name);
               --  Fall through.
            end if;
         else
            --  Create a list of type.
            Ret_Type := Create_List_Of_Types (Expr_List);
            if Ret_Type = Null_Iir or else not Is_Overload_List (Ret_Type) then
               --  There is either no types or one type for
               --  several meanings.
               Error_Overload (Name);
               Disp_Overload_List (Expr_List, Name);
               --Free_Iir (Ret_Type);
               return Null_Iir;
            end if;
            Set_Type (Name, Ret_Type);
            return Name;
         end if;
      end if;

      --  NAME has only one meaning, which is EXPR.
      Xref_Name (Name);
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            --Set_Base_Name (Name, Get_Base_Name (Expr));
            Set_Type (Name, Get_Type (Expr));
            Set_Expr_Staticness (Name, Get_Expr_Staticness (Expr));
            --Set_Name_Staticness (Name, Get_Name_Staticness (Expr));
            return Name;
         when Iir_Kind_Parenthesis_Name
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Selected_By_All_Name =>
            Free_Iir (Name);
            return Eval_Expr_If_Static (Expr);
         when others =>
            Error_Kind ("name_to_expression", Name);
      end case;
   end Name_To_Expression;

   function Is_Object_Name (Name : Iir) return Boolean
   is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Function_Call
           | Iir_Kinds_Signal_Attribute =>
            return True;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return False;
         when others =>
            return False;
      end case;
   end Is_Object_Name;

   function Name_To_Object (Name : Iir) return Iir
   is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Function_Call
           | Iir_Kinds_Signal_Attribute =>
            return Name;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Name_To_Object (Get_Named_Entity (Name));
         when others =>
            return Null_Iir;
      end case;
   end Name_To_Object;

   -- Find a uniq declaration for a name.
   function Find_Declaration (Name: Iir; Kind: Decl_Kind_Type)
     return Iir
   is
      procedure Error (Res : Iir; Str : String)
      is
      begin
         Error_Msg_Sem (Str & " expected, found " & Disp_Node (Res), Name);
      end Error;

      function Check_Kind (Res: Iir; Kind : Iir_Kind; Str: String)
        return Iir
      is
         Res_Kind : Iir_Kind;
      begin
         Res_Kind := Get_Kind (Res);
         if Res_Kind /= Kind then
            Error (Res, Str);
            return Null_Iir;
         else
            return Res;
         end if;
      end Check_Kind;

      function Check_Kind_Unit (Res: Iir; Kind : Iir_Kind; Str: String)
        return Iir
      is
         Res_Kind : Iir_Kind;
      begin
         if Get_Kind (Res) /= Iir_Kind_Design_Unit then
            Error (Res, Str);
            return Null_Iir;
         end if;

         Res_Kind := Get_Kind (Get_Library_Unit (Res));
         if Res_Kind /= Kind then
            Error (Res, Str);
            return Null_Iir;
         else
            return Res;
         end if;
      end Check_Kind_Unit;

      Res: Iir;
   begin
      Sem_Name (Name, False);
      Res := Get_Named_Entity (Name);

      if Res = Error_Mark then
         --  A message must have been displayed.
         --  FIXME: is it the case for find_selected_declarations ???
         --  Error_Msg_Sem ("identifier is not defined", Name);
         return Null_Iir;
      end if;

      Xref_Name (Name);

      case Kind is
         when Decl_Type
           | Decl_Incomplete_Type =>
            case Get_Kind (Res) is
               when Iir_Kind_Type_Declaration
                 | Iir_Kind_Subtype_Declaration =>
                  Res := Get_Type (Res);
                  --  Note: RES cannot be NULL_IIR, this is just to be more
                  --  bullet-proof.
                  if Kind /= Decl_Incomplete_Type
                    and then
                    (Res = Null_Iir or else
                     Get_Kind (Res) = Iir_Kind_Incomplete_Type_Definition)
                  then
                     Error_Msg_Sem
                       ("invalid use of an incomplete type definition", Name);
                  end if;
               when others =>
                  Error_Msg_Sem
                    ("type expected, found " & Disp_Node (Res), Name);
                  return Null_Iir;
            end case;
         when Decl_Component =>
            Res := Check_Kind (Res, Iir_Kind_Component_Declaration,
                               "component");
         when Decl_Unit =>
            null;
         when Decl_Label =>
            null;
         when Decl_Entity =>
            Res := Check_Kind_Unit
              (Res, Iir_Kind_Entity_Declaration, "entity");
         when Decl_Configuration =>
            Res := Check_Kind_Unit (Res, Iir_Kind_Configuration_Declaration,
                                    "configuration");
         when Decl_Group_Template =>
            Res := Check_Kind (Res, Iir_Kind_Group_Template_Declaration,
                               "group template");
         when Decl_Attribute =>
            Res := Check_Kind (Res, Iir_Kind_Attribute_Declaration,
                               "attribute");
      end case;
      return Res;
   end Find_Declaration;
end Sem_Names;
