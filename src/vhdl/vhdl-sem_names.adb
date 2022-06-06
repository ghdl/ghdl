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
with Flags; use Flags;
with Name_Table;
with Libraries;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Utils; use Vhdl.Utils;
with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Types; use Types;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Std_Names;
with Vhdl.Sem;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Vhdl.Sem_Scopes; use Vhdl.Sem_Scopes;
with Vhdl.Sem_Expr; use Vhdl.Sem_Expr;
with Vhdl.Sem_Stmts; use Vhdl.Sem_Stmts;
with Vhdl.Sem_Decls; use Vhdl.Sem_Decls;
with Vhdl.Sem_Assocs; use Vhdl.Sem_Assocs;
with Vhdl.Sem_Specs;
with Vhdl.Sem_Types;
with Vhdl.Sem_Psl;
with Vhdl.Xrefs; use Vhdl.Xrefs;

package body Vhdl.Sem_Names is
   --  Finish the analyze of NAME using RES as named entity.
   --  This is called when the analyze is finished and an uniq
   --  interpretation has been determined (RES).
   --
   --  Error messages are emitted here.
   function Finish_Sem_Name (Name : Iir; Res : Iir) return Iir;

   --  Return the fully analyzed name of NAME.
   function Name_To_Analyzed_Name (Name : Iir) return Iir;

   procedure Error_Overload (Expr: Iir) is
   begin
      if Is_Error (Expr) then
         --  Avoid error storm.
         return;
      end if;
      Error_Msg_Sem (+Expr, "can't resolve overload for %n", +Expr);
   end Error_Overload;

   procedure Disp_Overload_List (List : Iir_List; Loc : Iir)
   is
      El : Iir;
      It : List_Iterator;
   begin
      Error_Msg_Sem (+Loc, "possible interpretations are:");
      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         case Get_Kind (El) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Error_Msg_Sem (+El, Disp_Subprg (El));
            when Iir_Kind_Function_Call =>
               El := Get_Implementation (El);
               Error_Msg_Sem (+El, Disp_Subprg (El));
            when others =>
               Error_Msg_Sem (+El, "%n", +El);
         end case;
         Next (It);
      end loop;
   end Disp_Overload_List;

   -- Create an overload list.
   -- must be destroyed with free_iir.
   function Get_Overload_List return Iir_Overload_List
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Overload_List);
      return Res;
   end Get_Overload_List;

   function Create_Overload_List (List : Iir_List) return Iir_Overload_List
   is
      Res : Iir_Overload_List;
   begin
      Res := Get_Overload_List;
      Set_Overload_List (Res, List);
      return Res;
   end Create_Overload_List;

   procedure Free_Overload_List (N : in out Iir_Overload_List)
   is
      List : Iir_List;
   begin
      List := Get_Overload_List (N);
      Destroy_Iir_List (List);
      Free_Iir (N);
      N := Null_Iir;
   end Free_Overload_List;

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

   function Is_Defined_Type (Atype : Iir) return Boolean is
   begin
      return Atype /= Null_Iir
        and then not Kind_In (Get_Kind (Atype),
                              Iir_Kind_Overload_List,
                              Iir_Kind_Wildcard_Type_Definition);
   end Is_Defined_Type;

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
      It : List_Iterator;
   begin
      --  Create the list of possible return types.
      Res_List := Create_Iir_List;
      It := List_Iterate (List);
      while Is_Valid (It) loop
         Decl := Get_Element (It);
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration =>
               Add_Element (Res_List, Get_Return_Type (Decl));
            when Iir_Kind_Enumeration_Literal
              | Iir_Kind_Function_Call
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Slice_Name
              | Iir_Kind_Selected_Element
              | Iir_Kind_Dereference =>
               Add_Element (Res_List, Get_Type (Decl));
            when others =>
               Error_Kind ("create_list_of_types", Decl);
         end case;
         Next (It);
      end loop;
      return Simplify_Overload_List (Res_List);
   end Create_List_Of_Types;

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

   --  Extract from overload list RES the function call without implicit
   --  conversion.  Return Null_Iir if there is no function call, or if there
   --  is an expressions that isn't a function call, or if there is more than
   --  one function call without implicit conversion.
   --  Cf Sem_Expr.Get_Non_Implicit_Subprogram
   function Extract_Call_Without_Implicit_Conversion (Res : Iir) return Iir
   is
      pragma Assert (Is_Overload_List (Res));
      List : constant Iir_List := Get_Overload_List (Res);
      It : List_Iterator;
      Call : Iir;
      El : Iir;
      Imp : Iir;
      Inter : Iir;
   begin
      Call := Null_Iir;
      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         if Get_Kind (El) = Iir_Kind_Function_Call then
            Imp := Get_Implementation (El);
            Inter := Get_Interface_Declaration_Chain (Imp);
            if Get_Type (Inter) = Universal_Integer_Type_Definition
              or else Get_Type (Inter) = Universal_Real_Type_Definition
            then
               --  The type of the first interface is a universal type.  So,
               --  there were no implicit conversions.  Once there is an
               --  implicit conversion, the only way to 'convert' to a
               --  universal type is through T'Pos, which has to be resolved.
               --  Note: there are no interface of convertible types.
               --  GHDL: this is not proven...
               if Call /= Null_Iir then
                  --  More than one call without implicit conversion.
                  return Null_Iir;
               else
                  Call := El;
               end if;
            end if;
         else
            return Null_Iir;
         end if;
         Next (It);
      end loop;

      return Call;
   end Extract_Call_Without_Implicit_Conversion;

   --  Move elements of result list LIST to result list RES.
   --  Destroy LIST if necessary.
   procedure Add_Result_List (Res : in out Iir; List : Iir);
   pragma Unreferenced (Add_Result_List);

   procedure Add_Result_List (Res : in out Iir; List : Iir)
   is
      El : Iir;
      List_List : Iir_List;
      Res_List : Iir_List;
      It : List_Iterator;
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
         It := List_Iterate (List_List);
         while Is_Valid (It) loop
            Append_Element (Res_List, Get_Element (It));
            Next (It);
         end loop;
         Free_Iir (List);
      end if;
   end Add_Result_List;

   --  Free interpretations of LIST except KEEP (which can be Null_Iir to free
   --  the whole list).
   procedure Sem_Name_Free_Result (List : Iir; Keep : Iir)
   is
      procedure Sem_Name_Free (El : Iir) is
      begin
         case Get_Kind (El) is
            when Iir_Kind_Function_Call
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Selected_Element =>
               Sem_Name_Free (Get_Prefix (El));
               Free_Iir (El);
            when Iir_Kind_Attribute_Name =>
               Free_Iir (El);
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration
              | Iir_Kind_Enumeration_Literal =>
               null;
            when Iir_Kind_Interface_Function_Declaration
              | Iir_Kind_Interface_Procedure_Declaration =>
               null;
            when Iir_Kinds_Denoting_Name =>
               null;
            when others =>
               Error_Kind ("sem_name_free", El);
         end case;
      end Sem_Name_Free;

      El : Iir;
      List_List : Iir_List;
      It : List_Iterator;
   begin
      if List = Null_Iir then
         return;
      elsif not Is_Overload_List (List) then
         if List /= Keep then
            Sem_Name_Free (List);
         end if;
      else
         List_List := Get_Overload_List (List);
         It := List_Iterate (List_List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            if El /= Keep then
               Sem_Name_Free (El);
            end if;
            Next (It);
         end loop;
         Free_Iir (List);
      end if;
   end Sem_Name_Free_Result;

   procedure Free_Parenthesis_Name (Name : Iir; Res : Iir)
   is
      Chain, Next_Chain : Iir;
   begin
      pragma Assert (Get_Kind (Res) /= Iir_Kind_Function_Call);
      Chain := Get_Association_Chain (Name);
      while Chain /= Null_Iir loop
         Next_Chain := Get_Chain (Chain);
         Free_Iir (Chain);
         Chain := Next_Chain;
      end loop;
      Free_Iir (Name);
   end Free_Parenthesis_Name;

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
                     Add_Result (Res, Get_Named_Entity (Get_Name (Decl)));
                  end if;
               end if;
            when others =>
               --  Consider only visible declarations (case of an implicit
               --  declaration that is overriden by explicit one).
               if Get_Identifier (Decl) = Id and Get_Visible_Flag (Decl) then
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

      Id : constant Name_Id := Get_Identifier (Name);
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            Iterator_Decl_Chain (Get_Interface_Declaration_Chain (Decl), Id);
         when Iir_Kind_Entity_Declaration =>
            Iterator_Decl_Chain (Get_Generic_Chain (Decl), Id);
            Iterator_Decl_Chain (Get_Port_Chain (Decl), Id);
         when Iir_Kind_Architecture_Body =>
            null;
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement =>
            null;
         when Iir_Kind_Package_Declaration =>
            declare
               Header : constant Iir := Get_Package_Header (Decl);
            begin
               if Is_Valid (Header)
                 and then Get_Is_Within_Flag (Decl)
               then
                  Iterator_Decl_Chain (Get_Generic_Chain (Header), Id);
               end if;
            end;
         when Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration =>
            --  Generics are not visible in selected name.
            null;
            --  Iterator_Decl_Chain (Get_Generic_Chain (Decl), Id);
         when Iir_Kind_Block_Statement =>
            declare
               Header : constant Iir := Get_Block_Header (Decl);
            begin
               if Header /= Null_Iir then
                  Iterator_Decl_Chain (Get_Generic_Chain (Header), Id);
                  Iterator_Decl_Chain (Get_Port_Chain (Header), Id);
               end if;
            end;
         when Iir_Kind_For_Loop_Statement =>
            Handle_Decl (Get_Parameter_Specification (Decl), Id);
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement =>
            null;
         when others =>
            Error_Kind ("find_declarations_in_list", Decl);
      end case;

      case Get_Kind (Decl) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            declare
               Decl_Body : constant Iir := Get_Subprogram_Body (Decl);
            begin
               Iterator_Decl_Chain
                 (Get_Declaration_Chain (Decl_Body), Id);
               Iterator_Decl_Chain
                 (Get_Sequential_Statement_Chain (Decl_Body), Id);
            end;
         when Iir_Kind_Architecture_Body
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Block_Statement =>
            Iterator_Decl_Chain (Get_Declaration_Chain (Decl), Id);
            Iterator_Decl_Chain (Get_Concurrent_Statement_Chain (Decl), Id);
         when Iir_Kind_For_Generate_Statement =>
            declare
               Bod : constant Iir := Get_Generate_Statement_Body (Decl);
            begin
               Iterator_Decl_Chain (Get_Declaration_Chain (Bod), Id);
               Iterator_Decl_Chain (Get_Concurrent_Statement_Chain (Bod), Id);
            end;
         when Iir_Kind_If_Generate_Statement =>
            declare
               Clause : Iir;
               Bod : Iir;
            begin
               --  Look only in the current generate_statement_body
               Clause := Decl;
               while Clause /= Null_Iir loop
                  Bod := Get_Generate_Statement_Body (Clause);
                  if Get_Is_Within_Flag (Bod) then
                     Iterator_Decl_Chain
                       (Get_Declaration_Chain (Bod), Id);
                     Iterator_Decl_Chain
                       (Get_Concurrent_Statement_Chain (Bod), Id);
                     exit;
                  end if;
                  Clause := Get_Generate_Else_Clause (Clause);
               end loop;
            end;
         when Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration =>
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
      if Get_Kind (Prefix) /= Iir_Kind_Function_Declaration then
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
      Obj_Alias : Iir;
      Obj_Type : Iir;
   begin
      if Get_Kind (Name) /= Iir_Kind_Selected_Name then
         return;
      end if;

      Prefix := Get_Prefix (Name);
      Obj := Get_Named_Entity (Prefix);
      if Obj = Null_Iir then
         return;
      end if;
      if Get_Kind (Obj) = Iir_Kind_Object_Alias_Declaration then
         Obj_Alias := Get_Named_Entity (Get_Name (Obj));
      else
         Obj_Alias := Obj;
      end if;

      if Kind_In (Obj_Alias, Iir_Kind_Variable_Declaration,
                  Iir_Kind_Interface_Variable_Declaration)
      then
         Obj_Type := Get_Type (Obj_Alias);
         if Obj_Type = Null_Iir then
            return;
         end if;
         if Get_Kind (Obj_Type) /= Iir_Kind_Protected_Type_Declaration
         then
            Error_Msg_Sem
              (+Prefix, "type of the prefix should be a protected type");
            return;
         end if;
         Set_Method_Object (Call, Obj_Alias);
         Set_Use_Flag (Obj, True);
      end if;
   end Name_To_Method_Object;

   --  NAME is the name of the function (and not the parenthesis name)
   function Sem_As_Function_Call (Name : Iir; Spec : Iir; Assoc_Chain : Iir)
                                 return Iir_Function_Call
   is
      Call : Iir_Function_Call;
   begin
      --  Check.
      pragma Assert (Get_Kind (Name) in Iir_Kinds_Denoting_Name);

      Call := Create_Iir (Iir_Kind_Function_Call);
      Location_Copy (Call, Name);
      if Get_Kind (Name) = Iir_Kind_Parenthesis_Name then
         Set_Prefix (Call, Get_Prefix (Name));
      else
         Set_Prefix (Call, Name);
      end if;
      Name_To_Method_Object (Call, Name);
      Set_Implementation (Call, Spec);
      Set_Parameter_Association_Chain (Call, Assoc_Chain);
      Set_Type (Call, Get_Return_Type (Spec));
      Set_Base_Name (Call, Call);
      return Call;
   end Sem_As_Function_Call;

   --  If SPEC is a function specification, then return a function call,
   --  else return SPEC.
   function Maybe_Insert_Function_Call (Name : Iir; Spec : Iir) return Iir
   is
   begin
      if Get_Kind (Spec) = Iir_Kind_Function_Declaration then
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
      Prefix : constant Iir := Get_Prefix (Expr);
      Prefix_Type : constant Iir := Get_Type (Prefix);
      Index_List : constant Iir_Flist := Get_Index_List (Expr);
      Index_Subtype : Iir;
      Index : Iir;
      Expr_Staticness : Iir_Staticness;
   begin
      Expr_Staticness := Locally;

      -- LRM93 6.4: there must be one such expression for each index
      -- position of the array and each expression must be of the
      -- type of the corresponding index.
      -- Loop on the indexes.
      for I in Flist_First .. Flist_Last (Index_List) loop
         Index := Get_Nth_Element (Index_List, I);
         Index_Subtype := Get_Index_Type (Prefix_Type, I);
         -- The index_subtype can be an unconstrained index type.
         Index := Check_Is_Expression (Index, Index);
         if Index /= Null_Iir then
            Index := Sem_Expression (Index, Get_Base_Type (Index_Subtype));
         end if;
         if Index /= Null_Iir then
            Check_Read (Index);
            if Get_Expr_Staticness (Index) = Locally
              and then Get_Type_Staticness (Index_Subtype) = Locally
            then
               Index := Eval_Expr_Check (Index, Index_Subtype);
            end if;
            Set_Nth_Element (Index_List, I, Index);
            Expr_Staticness := Min (Expr_Staticness,
                                    Get_Expr_Staticness (Index));
         else
            Expr_Staticness := None;
         end if;
      end loop;

      Set_Type (Expr, Get_Element_Subtype (Prefix_Type));

      -- LRM93 6.1
      -- a name is said to be a static name iff:
      -- The name is an indexed name whose prefix is a static name
      -- and every expression that appears as part of the name is a
      -- static expression.
      --
      -- a name is said to be a locally static name iif:
      -- The name is an indexed name whose prefix is a locally
      -- static name and every expression that appears as part
      -- of the name is a locally static expression.
      Set_Name_Staticness
        (Expr, Min (Expr_Staticness, Get_Name_Staticness (Prefix)));

      --  An indexed name cannot be locally static.
      if Flags.Vhdl_Std < Vhdl_08 then
         Expr_Staticness := Min (Globally, Expr_Staticness);
      end if;
      Set_Expr_Staticness
        (Expr, Min (Expr_Staticness, Get_Expr_Staticness (Prefix)));

      Set_Base_Name (Expr, Get_Base_Name (Prefix));
   end Finish_Sem_Indexed_Name;

   procedure Finish_Sem_Dereference (Res : Iir) is
   begin
      Set_Base_Name (Res, Res);
      Check_Read (Get_Prefix (Res));
      Set_Expr_Staticness (Res, None);
      Set_Name_Staticness (Res, None);
   end Finish_Sem_Dereference;

   procedure Finish_Sem_Slice_Name (Name : Iir_Slice_Name)
   is
      -- The prefix of the slice
      Prefix : constant Iir := Get_Prefix (Name);
      Prefix_Type : constant Iir := Get_Type (Prefix);
      Prefix_Base_Type : Iir;
      Prefix_Bt : constant Iir := Get_Base_Type (Prefix_Type);
      Index_List: Iir_Flist;
      Index_Type: Iir;
      Suffix: Iir;
      Slice_Type : Iir;
      Expr_Type : Iir;
      Staticness : Iir_Staticness;
      Prefix_Rng : Iir;
      Suffix_Rng : Iir;
   begin
      Set_Base_Name (Name, Get_Base_Name (Prefix));

      --  LRM93 6.5: the prefix of an indexed name must be appropriate
      --  for an array type.
      if Get_Kind (Prefix_Bt) /= Iir_Kind_Array_Type_Definition then
         Error_Msg_Sem (+Name, "slice can only be applied to an array");
         return;
      end if;

      --  LRM93 6.5:
      --  The prefix of a slice must be appropriate for a
      --  one-dimensionnal array object.
      Index_List := Get_Index_Subtype_List (Prefix_Type);
      if Get_Nbr_Elements (Index_List) /= 1 then
         Error_Msg_Sem
           (+Name, "slice prefix must be an one-dimensional array");
         return;
      end if;

      Index_Type := Get_Index_Type (Index_List, 0);
      Prefix_Rng := Eval_Static_Range (Index_Type);

      --  LRM93 6.5
      --  It is an error if either the bounds of the discrete range does not
      --  belong to the index range of the prefixing array, *unless* the slice
      --  is a null slice.
      --
      --  LRM93 6.5
      --  The slice is a null slice if the discrete range is a null range.

      --  LRM93 6.5:
      --  The bounds of the discrete range [...] must be of the
      --  type of the index of the array.
      Suffix := Get_Suffix (Name);
      Suffix := Sem_Discrete_Range (Suffix, Index_Type, False);
      if Suffix = Null_Iir then
         return;
      end if;
      case Get_Kind (Suffix) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Staticness := Get_Type_Staticness (Get_Type (Suffix));
            Suffix_Rng := Get_Range_Constraint (Get_Type (Suffix));
         when Iir_Kinds_Scalar_Subtype_Definition =>
            Staticness := Get_Type_Staticness (Suffix);
            Suffix_Rng := Get_Range_Constraint (Suffix);
         when Iir_Kind_Range_Expression
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            Suffix := Eval_Range_If_Static (Suffix);
            Suffix_Rng := Suffix;
            Staticness := Get_Expr_Staticness (Suffix);
         when others =>
            Error_Kind ("finish_sem_slice_name", Suffix);
      end case;
      Set_Suffix (Name, Suffix);

      --  LRM93 6.5:
      --  It is an error if the direction of the discrete range is not
      --  the same as that of the index range of the array denoted
      --  by the prefix of the slice name.

      --  Check this only if the type is a constrained type.
      if Get_Kind (Prefix_Type) = Iir_Kind_Array_Subtype_Definition
        and then Get_Index_Constraint_Flag (Prefix_Type)
        and then Staticness = Locally
        and then Prefix_Rng /= Null_Iir
        and then Get_Direction (Suffix_Rng) /= Get_Direction (Prefix_Rng)
      then
         if False and then Flags.Vhdl_Std = Vhdl_87 then
            -- emit a warning for a null slice.
            Warning_Msg_Sem (Warnid_Runtime_Error, +Name,
                             "direction mismatch results in a null slice");

         end if;
         Error_Msg_Sem (+Name, "direction of the range mismatch");
      end if;

      --  LRM93 7.4.1
      --  A slice is never a locally static expression.
      Set_Expr_Staticness
        (Name, Min (Min (Staticness, Get_Expr_Staticness (Prefix)), Globally));
      Set_Name_Staticness
        (Name, Min (Staticness, Get_Name_Staticness (Prefix)));

      Expr_Type := Create_Iir (Iir_Kind_Array_Subtype_Definition);
      Set_Location (Expr_Type, Get_Location (Suffix));

      --  The type of the slice is a subtype of the base type whose
      --  range contraint is the slice itself.
      case Get_Kind (Suffix) is
         when Iir_Kinds_Denoting_Name =>
            Slice_Type := Get_Type (Suffix);
         when Iir_Kinds_Scalar_Subtype_Definition =>
            Slice_Type := Suffix;
         when others =>
            case Get_Kind (Get_Base_Type (Index_Type)) is
               when Iir_Kind_Integer_Type_Definition =>
                  Slice_Type :=
                    Create_Iir (Iir_Kind_Integer_Subtype_Definition);
               when Iir_Kind_Enumeration_Type_Definition =>
                  Slice_Type :=
                    Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
               when others =>
                  Error_Kind
                    ("sem_expr: slice_name", Get_Base_Type (Index_Type));
            end case;
            Set_Range_Constraint (Slice_Type, Suffix_Rng);
            Set_Is_Ref (Slice_Type, True);
            Set_Type_Staticness (Slice_Type, Staticness);
            Set_Parent_Type (Slice_Type, Get_Base_Type (Index_Type));
            Set_Location (Slice_Type, Get_Location (Suffix));

            --  Attach the new index subtype to the array subtype.
            Index_List := Create_Iir_Flist (1);
            Set_Index_Constraint_List (Expr_Type, Index_List);
            Set_Nth_Element (Index_List, 0, Slice_Type);
      end case;

      Index_List := Create_Iir_Flist (1);
      Set_Index_Subtype_List (Expr_Type, Index_List);
      Set_Nth_Element (Index_List, 0, Slice_Type);
      Prefix_Base_Type := Get_Base_Type (Prefix_Type);
      Set_Parent_Type (Expr_Type, Prefix_Base_Type);
      Set_Signal_Type_Flag (Expr_Type,
                            Get_Signal_Type_Flag (Prefix_Base_Type));
      Set_Element_Subtype (Expr_Type, Get_Element_Subtype (Prefix_Type));
      if Get_Kind (Prefix_Type) = Iir_Kind_Array_Subtype_Definition then
         Set_Resolution_Indication
           (Expr_Type, Sem_Types.Copy_Resolution_Indication (Prefix_Type));
      else
         Set_Resolution_Indication (Expr_Type, Null_Iir);
      end if;
      Set_Type_Staticness
        (Expr_Type, Min (Get_Type_Staticness (Prefix_Type),
                         Get_Type_Staticness (Slice_Type)));
      Set_Type (Name, Expr_Type);
      Set_Slice_Subtype (Name, Expr_Type);
      Set_Index_Constraint_Flag (Expr_Type, True);
      Set_Constraint_State (Expr_Type, Fully_Constrained);
      if Is_Signal_Object (Prefix) then
         Sem_Types.Set_Type_Has_Signal (Expr_Type);
      end if;
   end Finish_Sem_Slice_Name;

   --  PREFIX is the name denoting the function declaration, and its analysis
   --  is already finished.
   procedure Finish_Sem_Function_Call (Call : Iir; Prefix : Iir)
   is
      Rtype : Iir;
   begin
      Set_Prefix (Call, Prefix);
      Set_Implementation (Call, Get_Named_Entity (Prefix));

      --  LRM08 8.1 Names
      --  The name is a simple name or selected name that does NOT denote a
      --  function call [...]
      --
      --  GHDL: so function calls are never static names.
      Set_Name_Staticness (Call, None);

      --  FIXME: modify sem_subprogram_call to avoid such a type swap.
      Rtype := Get_Type (Call);
      Set_Type (Call, Null_Iir);
      if Sem_Subprogram_Call (Call, Null_Iir) = Null_Iir then
         Set_Type (Call, Rtype);
      end if;
   end Finish_Sem_Function_Call;

   function Function_Declaration_To_Call (Name : Iir) return Iir
   is
      Expr : Iir;
   begin
      Expr := Get_Named_Entity (Name);
      if Maybe_Function_Call (Expr) then
         Expr := Sem_As_Function_Call (Name, Expr, Null_Iir);
         pragma Assert (Get_Kind (Expr) = Iir_Kind_Function_Call);
         Finish_Sem_Function_Call (Expr, Name);
         return Expr;
      else
         Error_Msg_Sem (+Name, "%n requires parameters", +Expr);
         Set_Type (Name, Get_Type (Expr));
         Set_Expr_Staticness (Name, None);
         Set_Named_Entity (Name, Create_Error_Expr (Expr, Get_Type (Expr)));
         return Name;
      end if;
   end Function_Declaration_To_Call;

   function Sem_Type_Mark (Name : Iir; Incomplete : Boolean := False)
                          return Iir
   is
      Atype : Iir;
      Res : Iir;
      Decl : Iir;
   begin
      --  The name must not have been analyzed.
      pragma Assert (Get_Type (Name) = Null_Iir);

      if Is_Error (Name) then
         Set_Type (Name, Name);
         return Name;
      end if;

      case Get_Kind (Name) is
         when Iir_Kinds_Name
           | Iir_Kind_Attribute_Name =>
            null;
         when others =>
            Error_Msg_Sem (+Name, "name expected for a type mark");
            return Create_Error_Type (Name);
      end case;

      --  Analyze the name (if not already done).
      Res := Get_Named_Entity (Name);
      if Res = Null_Iir then
         Sem_Name (Name);
         Res := Get_Named_Entity (Name);
      end if;
      if Res /= Null_Iir and then Is_Overload_List (Res) then
         Error_Msg_Sem (+Name, "name does not denote a type mark");
         return Create_Error_Type (Name);
      end if;
      Res := Finish_Sem_Name (Name);

      --  LRM87 14.1 Predefined attributes
      if Get_Kind (Res) = Iir_Kind_Base_Attribute then
         Error_Msg_Sem
           (+Name, "'Base attribute cannot be used as a type mark");
      end if;

      Atype := Name_To_Type_Definition (Res);

      if Is_Error (Atype) then
         if Get_Kind (Res) in Iir_Kinds_Denoting_Name then
            Set_Named_Entity (Res, Atype);
         else
            return Create_Error_Type (Name);
         end if;
      elsif not Incomplete then
         if Get_Kind (Atype) = Iir_Kind_Incomplete_Type_Definition then
            Error_Msg_Sem
              (+Name, "invalid use of an incomplete type definition");
            Atype := Create_Error_Type (Name);
            Set_Named_Entity (Res, Atype);
         end if;
      end if;

      Set_Type (Res, Atype);

      if Get_Kind (Res) in Iir_Kinds_Denoting_Name then
         Decl := Get_Named_Entity (Res);
         if Kind_In (Decl,
                     Iir_Kind_Type_Declaration, Iir_Kind_Subtype_Declaration)
         then
            Set_Use_Flag (Decl, True);
         end if;
      end if;

      return Res;
   end Sem_Type_Mark;

   --  Return Globally if the prefix of NAME is a globally static name.
   function Get_Object_Type_Staticness (Name : Iir) return Iir_Staticness
   is
      Base : constant Iir := Get_Base_Name (Name);
      Parent : Iir;
   begin
      if Get_Kind (Base) in Iir_Kinds_Dereference then
         --  A dereferenced object is never static.
         return None;
      end if;

      Parent := Get_Parent (Base);
      loop
         case Get_Kind (Parent) is
            when Iir_Kind_Entity_Declaration
              | Iir_Kind_Architecture_Body
              | Iir_Kind_Block_Statement
              | Iir_Kind_Block_Header
              | Iir_Kind_Component_Declaration
              | Iir_Kinds_Process_Statement
              | Iir_Kind_Generate_Statement_Body
              | Iir_Kind_Design_Unit =>
               --  Globally static.
               return Globally;
            when Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Body
              | Iir_Kind_Package_Instantiation_Declaration
              | Iir_Kind_Protected_Type_Body =>
               --  Possibly nested construct.
               Parent := Get_Parent (Parent);
            when Iir_Kinds_Subprogram_Declaration
              | Iir_Kinds_Subprogram_Body
              | Iir_Kinds_Interface_Subprogram_Declaration =>
               --  Not globally static.
               return None;
            when others =>
               Error_Kind ("get_object_type_staticness", Parent);
         end case;
      end loop;
   end Get_Object_Type_Staticness;

   procedure Finish_Sem_Array_Attribute
     (Attr_Name : Iir; Attr : Iir; Param : Iir)
   is
      Parameter : Iir;
      Prefix_Type : Iir;
      Index_Type : Iir;
      Prefix : Iir;
      Prefix_Name : Iir;
      Staticness : Iir_Staticness;
   begin
      --  LRM93 14.1
      --  Parameter: A locally static expression of type universal_integer, the
      --  value of which must not exceed the dimensionality of A.  If omitted,
      --  it defaults to 1.
      if Param = Null_Iir then
         Parameter := Null_Iir;
      else
         Parameter := Sem_Expression
           (Param, Universal_Integer_Type_Definition);
         if Parameter /= Null_Iir then
            if Get_Expr_Staticness (Parameter) /= Locally then
               Error_Msg_Sem (+Parameter, "parameter must be locally static");
            end if;
         else
            --  Don't forget there is a parameter, so the attribute cannot
            --  be reanalyzed with a default parameter.
            Parameter := Error_Mark;
         end if;
      end if;

      --  See Sem_Array_Attribute_Name for comments about the prefix.
      Prefix_Name := Get_Prefix (Attr_Name);
      if Is_Type_Name (Prefix_Name) /= Null_Iir then
         Prefix := Sem_Type_Mark (Prefix_Name);
      else
         Prefix := Finish_Sem_Name (Prefix_Name, Get_Prefix (Attr));
         --  Convert function declaration to call.
         if Get_Kind (Prefix) in Iir_Kinds_Denoting_Name
           and then
           (Get_Kind (Get_Named_Entity (Prefix))
              = Iir_Kind_Function_Declaration)
         then
            Prefix := Function_Declaration_To_Call (Prefix);
         end if;
         if not Is_Object_Name (Prefix) then
            Error_Msg_Sem_Relaxed
              (Attr, Warnid_Attribute,
               "prefix of array attribute must be an object name");
         end if;
      end if;
      Set_Prefix (Attr, Prefix);

      Prefix_Type := Get_Type (Prefix);
      if Is_Error (Prefix_Type) then
         return;
      end if;

      declare
         Dim : Int64;
         Indexes_List : constant Iir_Flist :=
           Get_Index_Subtype_List (Prefix_Type);
      begin
         if Is_Null (Parameter)
           or else Get_Expr_Staticness (Parameter) /= Locally
         then
            Dim := 1;
         else
            Dim := Get_Value (Parameter);
         end if;
         if Dim < 1 or else Dim > Int64 (Get_Nbr_Elements (Indexes_List))
         then
            Error_Msg_Sem (+Attr, "parameter value out of bound");
            Dim := 1;
         end if;
         Index_Type := Get_Index_Type (Indexes_List, Natural (Dim - 1));
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

      pragma Assert (Get_Parameter (Attr) = Null_Iir);

      Set_Parameter (Attr, Parameter);

      --  If the corresponding type is known, save it so that it is not
      --  necessary to extract it from the object.
      if Get_Kind (Prefix_Type) = Iir_Kind_Array_Subtype_Definition
        and then Get_Constraint_State (Prefix_Type) = Fully_Constrained
      then
         Set_Index_Subtype (Attr, Index_Type);
      end if;

      --  LRM08 9.4.2 Locally static primaries
      --  g) A predefined attribute that is a function, [other than ... and
      --     other than ...], whose prefix is either a locally static subtype
      --     or is an object that is of a locally static subtype, and whose
      --     actual parameter (if any) is a locally static expression.
      --
      --  LRM08 9.4.3 Globally static primaries
      --  l) A predefined attribute that is a function, [other than ... and
      --     other than ...], whose prefix is appropriate for a globally
      --     static attribute, and whose actual parameter (if any) is a
      --     globally static expression.
      --
      --  A prefix is appropriate for a globally static attribute if it denotes
      --  a signal, a constant, a type or subtype, a globally static function
      --  call, a variable that is not of an access type, or a variable of an
      --  access type whose designated subtype is fully constrained.

      --  LRM93 7.4.1
      --  A locally static range is either [...], or a range of the first form
      --  whose prefix denotes either a locally static subtype or an object
      --  that is of a locally static subtype.

      --  LRM93 7.4.2
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
      if Is_Object_Name (Prefix) then
         Staticness := Iir_Staticness'Max
           (Staticness, Get_Object_Type_Staticness (Prefix));
      end if;
      Set_Expr_Staticness (Attr, Staticness);
   end Finish_Sem_Array_Attribute;

   procedure Finish_Sem_Scalar_Type_Attribute
     (Attr_Name : Iir; Attr : Iir; Param : Iir)
   is
      Prefix : Iir;
      Prefix_Type : Iir;
      Prefix_Bt : Iir;
      Parameter : Iir;
      Param_Type : Iir;
   begin
      if Param = Null_Iir then
         Error_Msg_Sem (+Attr, "%n requires a parameter", +Attr);
         return;
      end if;

      Prefix := Finish_Sem_Name (Get_Prefix (Attr));
      Free_Iir (Attr_Name);
      Set_Prefix (Attr, Prefix);

      Prefix_Type := Get_Type (Prefix);
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
                  Error_Msg_Sem (+Attr, "parameter must be an integer");
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
      pragma Assert (Get_Parameter (Attr) = Null_Iir);
      if Parameter = Null_Iir then
         Set_Parameter (Attr, Param);
         Set_Expr_Staticness (Attr, None);
         return;
      end if;
      Set_Parameter (Attr, Parameter);
      Set_Expr_Staticness (Attr, Min (Get_Type_Staticness (Prefix_Type),
                                      Get_Expr_Staticness (Parameter)));
      Set_Name_Staticness (Attr, Get_Expr_Staticness (Attr));
   end Finish_Sem_Scalar_Type_Attribute;

   --  Finish analysis of attributes for signals.
   procedure Finish_Sem_Signal_Attribute (Attr : Iir)
   is
      Prefix : constant Iir := Get_Prefix (Attr);
   begin
      --  According to LRM 7.4, signal attributes are not static expressions
      --  since the prefix (a signal) is not a static expression.
      Set_Expr_Staticness (Attr, None);

      --  For all signal attributes:
      --
      --  LRM93 14.1 Predefined attributes
      --  Prefix: any signal denoted by the static signal name S.
      if Get_Name_Staticness (Prefix) < Globally then
         Error_Msg_Sem
           (+Attr, "prefix of %n must be a static name", +Attr);
      end if;

      --  LRM02 6.1 / LRM08 8.1
      --  A name is said to be a static name if and only if at least one of
      --  the following conditions holds:
      --  [...]
      --  -  The name is a attribute name whose prefix is a static signal name
      --     and whose suffix is one of the predefined attributes 'DELAYED,
      --     'STABLE, 'QUIET or 'TRANSACTION.
      --  According to LRM 6.1, attributes are not static names.
      if Flag_Relaxed_Rules or Flags.Vhdl_Std >= Vhdl_02 then
         case Get_Kind (Attr) is
            when Iir_Kind_Stable_Attribute
              | Iir_Kind_Quiet_Attribute
              | Iir_Kind_Delayed_Attribute
              | Iir_Kind_Transaction_Attribute =>
               Set_Name_Staticness (Attr, Get_Name_Staticness (Prefix));
            when others =>
               Set_Name_Staticness (Attr, None);
         end case;
      else
         Set_Name_Staticness (Attr, None);
      end if;
   end Finish_Sem_Signal_Attribute;

   --  Finish analysis of attributes that are signals for signals
   procedure Finish_Sem_Signal_Attribute_Signal (Attr : Iir; Parameter : Iir)
   is
      pragma Assert (Parameter /= Null_Iir);
      Param : Iir;
   begin
      if Get_Kind (Attr) = Iir_Kind_Transaction_Attribute then
         Error_Msg_Sem (+Attr, "'transaction does not allow a parameter");
         return;
      end if;

      Param := Sem_Expression (Parameter, Time_Subtype_Definition);
      if Param /= Null_Iir then
         --  LRM93 14.1
         --  Parameter: A static expression of type TIME [that evaluate
         --  to a nonnegative value.]
         if Get_Expr_Staticness (Param) = None then
            Error_Msg_Sem
              (+Param, "parameter of signal attribute must be static");
         end if;
         Set_Parameter (Attr, Param);
      end if;
   end Finish_Sem_Signal_Attribute_Signal;

   procedure Sem_Quantity_Attribute_Parameters
     (Attr : Iir; Params : Iir_Array; Params_Type : Iir_Array; Min : Natural)
   is
      Param : Iir;
   begin
      pragma Assert (Params'First = 1);
      pragma Assert (Params_Type'First = 1);
      pragma Assert (Params_Type'Last = Params'Last);
      for I in Params'Range loop
         Param := Params (I);
         if Param = Null_Iir then
            if I <= Min then
               Error_Msg_Sem
                 (+Attr, "not enough parameters for the attribute");
            end if;
            return;
         end if;
         if Params_Type (I) = Null_Iir then
            Error_Msg_Sem (+Attr, "too many parameters for the attribute");
            return;
         end if;

         Param := Sem_Expression (Param, Params_Type (I));
         if Param /= Null_Iir then
            if Get_Expr_Staticness (Param) < Globally then
               Error_Msg_Sem
                 (+Param, "parameter must be a static expression");
            end if;
            Set_Attribute_Parameter (Attr, I, Param);
         end if;
      end loop;
   end Sem_Quantity_Attribute_Parameters;

   procedure Finish_Sem_Quantity_Attribute
     (Attr_Name : Iir; Attr : Iir; Params : Iir_Array)
   is
      Prefix : Iir;
      Param : Iir;
   begin
      Prefix := Get_Prefix (Attr_Name);
      Set_Prefix (Attr, Prefix);
      Free_Iir (Attr_Name);

      case Get_Kind (Attr) is
         when Iir_Kind_Quantity_Delayed_Attribute =>
            --  AMS-LRM17 16.2.6
            --  Q'DELAYED[(T)]
            --  Parameter:
            --    T: A static expression of type REAL that evaluates to a
            --     non-negative number.  If omitted, defaults to 0.0.
            Sem_Quantity_Attribute_Parameters
              (Attr, Params, (1 => Real_Type_Definition,
                              2 .. 4 => Null_Iir), 0);
         when Iir_Kind_Above_Attribute =>
            pragma Assert (Params'First = 1 and Params'Last = 1);
            if Params (1) = Null_Iir then
               Error_Msg_Sem (+Attr, "'above requires a parameter");
            else
               --  FIXME: AMS-LRM17 16.2.6
               --  Any quantity appearing in the expression shall be denoted by
               --  a static name.
               Param := Sem_Expression (Params (1), Get_Type (Prefix));
               if Param /= Null_Iir then
                  Set_Parameter (Attr, Param);
               end if;
            end if;
         when Iir_Kind_Ramp_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute =>
            pragma Assert (Params'First = 1 and Params'Last = 4);
            --  AMS-LRM17 16.2.6
            --  S'RAMP [(TRISE [, TFALL])]
            --  Parameters:
            --    TRISE: a static expression of a floating-point type that
            --      evaluates to a nonnegative value.  If omitted, it
            --      defaults to 0.
            --    TFALL: a static expression of a floating-point type that
            --      evaluates to a nonnegative value.  If omitted, it
            --      defaults to the value of TRISE.
            --
            --  S'SLEW [(RISING_SLOP [, FALLING_SLOPE])]
            --  Parameters:
            --    RISING_SLOPE: a static expression of type REAL that
            --     evaluates to a positive value.  If omitted, it defaults
            --     to REAL'HIGH, which is interpreted as an infinite slope.
            --    FALLING_SLOPE: a static expression of type REAL that
            --     evaluates to a negative value.  If omitted, it defaults
            --     to the negative of RISING_SLOPE.  The value REAL'LOW is
            --     interpreted as a negative infinite slope.
            --
            --  Q'SLEW [(MAX_RISING_SLOPE [, MAX_FALLING_SLOPE])]
            --  Parameters:
            --    MAX_RISING_SLOPE: a static expression of type REAL that
            --     evaluates to a positive value.  If omitted, it defaults
            --     to REAL'HIGH, which is interpreted as an infinite slope.
            --    MAX_FALLING_SLOPE: a static expression of type REAL that
            --     evaluates to a negative value.  If omitted, it defaults
            --     to the negative of MAX_RISING_SLOPE.  The value REAL'LOW
            --     is interpreted as a negative infinite slope.
            Sem_Quantity_Attribute_Parameters
              (Attr, Params, (1 => Real_Type_Definition,
                              2 => Real_Type_Definition,
                              3 | 4 => Null_Iir), 1);
         when Iir_Kind_Zoh_Attribute =>
            --  AMS-LRM17 16.2.6
            --  Q'LTF(T [, INITIAL_DELAY)
            --  Parameters:
            --    T: A static expression of type REAL that evaluates to a
            --     positive value.  This is the sampling period.
            --    INITIAL_DELAY: A static expression of type REAL that
            --     evaluates to a non-negative value.  The first sampling will
            --     occur after INITIAL_DELAY seconds.  If omitted, it defaults
            --     to 0.0.
            Sem_Quantity_Attribute_Parameters
              (Attr, Params, (1 => Real_Type_Definition,
                              2 => Real_Type_Definition,
                              3 | 4 => Null_Iir), 1);
         when Iir_Kind_Ltf_Attribute =>
            --  AMS-LRM17 16.2.6
            --  Q'LTF(NUM, DEN)
            --  Parameters:
            --    NUM: a static expression of type REAL_VECTOR that contains
            --     the numerator coefficients.
            --    DEN: a static expression of type REAL_VECTOR that contains
            --     the denominator coefficients.
            Sem_Quantity_Attribute_Parameters
              (Attr, Params, (1 => Real_Vector_Type_Definition,
                              2 => Real_Vector_Type_Definition,
                              3 | 4 => Null_Iir), 2);
         when Iir_Kind_Ztf_Attribute =>
            --  AMS-LRM17 16.2.6
            --  Q'ZTF(NUM, DEN, T, [, INITIAL_DELAY])
            --  Parameters:
            --    NUM: A static expression of type REAL_VECTOR with the
            --     numerator coefficients.
            --    DEN: A static expression of type REAL_VECTOR with the
            --     denominator coefficients.
            --    T: A static expression of type REAL that evaluates to a
            --     positive value, which is the sampling period.
            --    INITIAL_DELAY: A static expression of type REAL that
            --     evaluates to a non-negative value, which is the time of the
            --     first sampling.  If omitted, it defaults to 0.0
            Sem_Quantity_Attribute_Parameters
              (Attr, Params, (1 => Real_Vector_Type_Definition,
                              2 => Real_Vector_Type_Definition,
                              3 => Real_Type_Definition,
                              4 => Real_Type_Definition), 3);
         when others =>
            Error_Kind ("finish_sem_quantity_attribute", Attr);
      end case;
   end Finish_Sem_Quantity_Attribute;

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
      Base_Type1 : constant Iir := Get_Base_Type (Type1);
      Base_Type2 : constant Iir := Get_Base_Type (Type2);
      Ant1, Ant2 : Boolean;
      Index_List1, Index_List2 : Iir_Flist;
      El1, El2 : Iir;
   begin
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
      for I in Flist_First .. Flist_Last (Index_List1) loop
         El1 := Get_Index_Type (Index_List1, I);
         El2 := Get_Index_Type (Index_List2, I);
         if not Are_Types_Closely_Related (El1, El2) then
            return False;
         end if;
      end loop;
      return True;
   end Are_Types_Closely_Related;

   function Sem_Type_Conversion
     (Name : Iir; Type_Mark : Iir; Actual : Iir; In_Formal : Boolean)
     return Iir
   is
      Conv_Type : constant Iir := Get_Type (Type_Mark);
      Conv : Iir_Type_Conversion;
      Expr : Iir;
      Staticness : Iir_Staticness;
   begin
      Conv := Create_Iir (Iir_Kind_Type_Conversion);
      Location_Copy (Conv, Name);
      Set_Type_Mark (Conv, Type_Mark);
      Set_Type (Conv, Conv_Type);
      Set_Expression (Conv, Actual);

      --  Default staticness in case of error.
      Set_Expr_Staticness (Conv, None);

      --  Bail out if no actual (or invalid one).
      if Actual = Null_Iir then
         return Conv;
      end if;

      --  LRM93 7.3.5
      --  Furthermore, the operand of a type conversion is not allowed to be
      --  the literal null, an allocator, an aggregate, or a string literal.
      case Get_Kind (Actual) is
         when Iir_Kind_Null_Literal
           | Iir_Kind_Aggregate
           | Iir_Kind_String_Literal8
           | Iir_Kinds_Allocator =>
            Error_Msg_Sem
              (+Actual, "%n cannot be a type conversion operand", +Actual);
            return Conv;
         when Iir_Kind_Range_Expression =>
            --  Try to nicely handle expression like NAME (A to B).
            Error_Msg_Sem
              (+Actual, "subtype indication not allowed in an expression");
            return Conv;
         when Iir_Kind_Error =>
            return Conv;
         when others =>
            null;
      end case;

      --  LRM93 7.3.5
      --  The type of the operand of a type conversion must be
      --  determinable independent of the context (in particular,
      --  independent of the target type).
      Expr := Sem_Expression_Universal (Actual);
      if Expr = Null_Iir then
         return Conv;
      end if;
      Set_Expression (Conv, Expr);

      --  LRM93 7.4.1 Locally Static Primaries.
      --  9. a type conversion whose expression is a locally static expression.
      --  LRM93 7.4.2 Globally Static Primaries.
      --  14. a type conversion whose expression is a globally static
      --      expression.
      Staticness := Get_Expr_Staticness (Expr);

      --  If the type mark is not locally static, the expression cannot
      --  be locally static.  This was clarified in VHDL 08, but a type
      --  mark that denotes an unconstrained array type, does not prevent
      --  the expression from being static.
      if Get_Kind (Conv_Type) not in Iir_Kinds_Array_Type_Definition
        or else Get_Constraint_State (Conv_Type) = Fully_Constrained
      then
         Staticness := Min (Staticness, Get_Type_Staticness (Conv_Type));
      end if;

      --  LRM87 7.4 Static Expressions
      --  A type conversion is not a locally static expression.
      if Flags.Vhdl_Std = Vhdl_87 then
         Staticness := Min (Globally, Staticness);
      end if;
      Set_Expr_Staticness (Conv, Staticness);

      if not Are_Types_Closely_Related (Conv_Type, Get_Type (Expr)) then
         --  FIXME: should explain why the types are not closely related.
         Error_Msg_Sem
           (+Conv, "conversion allowed only between closely related types");
         --  Avoid error storm in evaluation.
         Set_Expr_Staticness (Conv, None);
      else
         --  Unless the type conversion appears in the formal part of an
         --  association, the expression must be readable.
         if not In_Formal then
            Check_Read (Expr);
         end if;
      end if;
      return Conv;
   end Sem_Type_Conversion;

   --  OBJ is an 'impure' object (variable, signal or file) referenced at
   --  location LOC.
   --  Check the pure rules (LRM08 4 Subprograms and packages,
   --  LRM08 4.3 Subprograms bodies).
   procedure Sem_Check_Pure (Loc : Iir; Obj : Iir)
   is
      procedure Update_Impure_Depth (Subprg_Spec : Iir; Depth : Iir_Int32)
      is
         Bod : constant Iir := Get_Subprogram_Body (Subprg_Spec);
      begin
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
         Error_Msg_Sem_Relaxed
           (Loc, Warnid_Pure,
            "reference to %n violate pure rule for %n", (+Obj, +Subprg));
      end Error_Pure;

      Subprg : constant Iir := Sem_Stmts.Get_Current_Subprogram;
      Subprg_Body : Iir;
      Parent : Iir;
      Decl : Iir;
   begin
      --  Apply only in subprograms.
      if Subprg = Null_Iir then
         return;
      end if;
      case Get_Kind (Subprg) is
         when Iir_Kinds_Process_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement =>
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

      --  Follow aliases.
      if Get_Kind (Obj) = Iir_Kind_Object_Alias_Declaration then
         Decl := Get_Object_Prefix (Get_Name (Obj));
      else
         Decl := Obj;
      end if;

      --  Not all objects are impure.
      case Get_Kind (Decl) is
         when Iir_Kind_Object_Alias_Declaration =>
            raise Program_Error;
         when Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            null;
         when Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            --  When referenced as a formal name (FIXME: this is an
            --  approximation), the rules don't apply.
            if not Get_Is_Within_Flag (Get_Parent (Decl)) then
               return;
            end if;
         when Iir_Kind_File_Declaration =>
            --  LRM 93 2.2
            --  If a pure function is the parent of a given procedure, then
            --  that procedure must not contain a reference to an explicitly
            --  declared file object [...]
            --
            --  A pure function must not contain a reference to an explicitly
            --  declared file.
            if Get_Kind (Subprg) = Iir_Kind_Function_Declaration then
               Error_Pure (Subprg, Obj);
            else
               Set_Purity_State (Subprg, Impure);
               Set_Impure_Depth (Get_Subprogram_Body (Subprg),
                                 Iir_Depth_Impure);
            end if;
            return;
         when others =>
            return;
      end case;

      --  DECL is declared in the immediate declarative part of the subprogram.
      Parent := Get_Parent (Decl);
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
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body
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
      case Get_Kind (Obj) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            null;
         when others =>
            return;
      end case;
      --  We cares only of subprograms.  Give up if we are in a process.
      Subprg := Sem_Stmts.Get_Current_Subprogram;
      if Subprg = Null_Iir
        or else Get_Kind (Subprg) not in Iir_Kinds_Subprogram_Declaration
      then
         return;
      end if;
      if Get_Kind (Obj) = Iir_Kind_Interface_Signal_Declaration
        and then Get_Parent (Obj) = Subprg
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

   --  Free overload list of NAME but keep RES interpretation.
   procedure Free_Old_Entity_Name (Name : Iir; Res : Iir)
   is
      Old_Res : constant Iir := Get_Named_Entity (Name);
   begin
      if Old_Res /= Null_Iir and then Old_Res /= Res then
         pragma Assert (Is_Overload_List (Old_Res));
         Sem_Name_Free_Result (Old_Res, Res);
      end if;
      Set_Named_Entity (Name, Res);
   end Free_Old_Entity_Name;

   function Finish_Sem_Denoting_Name (Name : Iir; Res : Iir) return Iir is
   begin
      case Iir_Kinds_Denoting_Name (Get_Kind (Name)) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Operator_Symbol =>
            Set_Base_Name (Name, Res);
            Xref_Ref (Name, Res);
            return Name;
         when Iir_Kind_Selected_Name =>
            declare
               Prefix, Res_Prefix : Iir;
               Old_Res : Iir;
            begin
               Xref_Ref (Name, Res);
               Prefix := Name;
               Res_Prefix := Res;
               loop
                  Prefix := Get_Prefix (Prefix);
                  Res_Prefix := Get_Parent (Res_Prefix);

                  --  Get the parent for expanded_name, may skip some parents.
                  case Get_Kind (Res_Prefix) is
                     when Iir_Kind_Design_Unit =>
                        Res_Prefix :=
                          Get_Library (Get_Design_File (Res_Prefix));
                     when others =>
                        null;
                  end case;

                  pragma Assert (Get_Kind (Prefix) in Iir_Kinds_Denoting_Name);
                  Xref_Ref (Prefix, Res_Prefix);

                  --  Cannot use Free_Old_Entity_Name as a prefix may not be
                  --  the parent (for protected subprogram calls).
                  Old_Res := Get_Named_Entity (Prefix);
                  if Is_Overload_List (Old_Res) then
                     Free_Iir (Old_Res);
                     Set_Named_Entity (Prefix, Res_Prefix);
                  end if;

                  exit when Get_Kind (Prefix) /= Iir_Kind_Selected_Name;
               end loop;
            end;
            return Name;
         when Iir_Kind_Reference_Name =>
            --  Not in the sources.
            raise Internal_Error;
      end case;
   end Finish_Sem_Denoting_Name;

   function Finish_Sem_Name_1 (Name : Iir; Res : Iir) return Iir
   is
      Prefix : Iir;
      Name_Prefix : Iir;
      Name_Res : Iir;
   begin
      case Get_Kind (Res) is
         when Iir_Kinds_Library_Unit =>
            return Finish_Sem_Denoting_Name (Name, Res);
         when Iir_Kinds_Sequential_Statement
           | Iir_Kinds_Concurrent_Statement =>
            --  Label or part of an expanded name (for process, block
            --  and generate).
            return Finish_Sem_Denoting_Name (Name, Res);
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            Name_Res := Finish_Sem_Denoting_Name (Name, Res);
            Set_Base_Name (Name_Res, Res);
            Set_Name_Staticness (Name_Res, Get_Name_Staticness (Res));
            Set_Expr_Staticness (Name_Res, Get_Expr_Staticness (Res));
            Sem_Check_Pure (Name_Res, Res);
            Sem_Check_All_Sensitized (Res);
            Set_Type (Name_Res, Get_Type (Res));
            return Name_Res;
         when Iir_Kind_Terminal_Declaration
           | Iir_Kind_Interface_Terminal_Declaration =>
            Name_Res := Finish_Sem_Denoting_Name (Name, Res);
            Set_Base_Name (Name_Res, Res);
            Set_Name_Staticness (Name_Res, Get_Name_Staticness (Res));
            Sem_Check_Pure (Name_Res, Res);
            return Name_Res;
         when Iir_Kind_Attribute_Value =>
            pragma Assert (Get_Kind (Name) = Iir_Kind_Attribute_Name);
            Prefix := Finish_Sem_Name (Get_Prefix (Name));
            Set_Prefix (Name, Prefix);
            if Get_Is_Forward_Ref (Prefix) then
               Set_Base_Name (Prefix, Null_Iir);
            end if;
            Set_Base_Name (Name, Res);
            Set_Type (Name, Get_Type (Res));
            Set_Name_Staticness (Name, Get_Name_Staticness (Res));
            Set_Expr_Staticness (Name, Get_Expr_Staticness (Res));
            return Name;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Interface_Type_Declaration =>
            Name_Res := Finish_Sem_Denoting_Name (Name, Res);
            Set_Base_Name (Name_Res, Res);
            return Name_Res;
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            Name_Res := Finish_Sem_Denoting_Name (Name, Res);
            Set_Type (Name_Res, Get_Return_Type (Res));
            return Name_Res;
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return Finish_Sem_Denoting_Name (Name, Res);
         when Iir_Kind_Type_Conversion =>
            pragma Assert (Get_Kind (Name) = Iir_Kind_Parenthesis_Name);
            Set_Type_Mark (Res, Sem_Type_Mark (Get_Prefix (Name)));
            Free_Parenthesis_Name (Name, Res);
            return Res;
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Slice_Name
           | Iir_Kind_Dereference =>
            --  Fall through.
            null;
         when Iir_Kind_Implicit_Dereference =>
            --  The name may not have a prefix.
            Prefix := Finish_Sem_Name_1 (Name, Get_Prefix (Res));
            Set_Prefix (Res, Prefix);
            Finish_Sem_Dereference (Res);
            return Res;
         when Iir_Kind_Function_Call =>
            case Get_Kind (Name) is
               when Iir_Kind_Parenthesis_Name =>
                  --  Usual case.
                  Prefix := Finish_Sem_Name
                    (Get_Prefix (Name), Get_Implementation (Res));
                  Finish_Sem_Function_Call (Res, Prefix);
                  Free_Iir (Name);
               when Iir_Kinds_Denoting_Name =>
                  --  Call without association list.
                  Prefix := Finish_Sem_Name (Name, Get_Implementation (Res));
                  Finish_Sem_Function_Call (Res, Prefix);
               when others =>
                  Error_Kind ("Finish_Sem_Name(function call)", Name);
            end case;
            return Res;
         when Iir_Kinds_Array_Attribute =>
            if Get_Parameter (Res) = Null_Iir then
               Finish_Sem_Array_Attribute (Name, Res, Null_Iir);
            end if;
            if Get_Kind (Name) = Iir_Kind_Attribute_Name then
               Free_Iir (Name);
            else
               Free_Iir (Get_Prefix (Name));
               Free_Parenthesis_Name (Name, Res);
            end if;
            return Res;
         when Iir_Kinds_Scalar_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute =>
            if Get_Parameter (Res) = Null_Iir then
               Finish_Sem_Scalar_Type_Attribute (Name, Res, Null_Iir);
            else
               Free_Parenthesis_Name (Name, Res);
            end if;
            return Res;
         when Iir_Kind_Across_Attribute
            | Iir_Kind_Through_Attribute
            | Iir_Kind_Nature_Reference_Attribute =>
            null;
         when Iir_Kinds_Signal_Value_Attribute =>
            null;
         when Iir_Kinds_Signal_Attribute =>
            --  Cannot use the common code below for the prefix, because
            --  the parenthesis_name is absorbed as a parameter.
            Prefix := Get_Prefix (Res);
            Name_Prefix := Get_Prefix (Name);
            if Get_Kind (Name) = Iir_Kind_Parenthesis_Name then
               --  Skip the parenthesis name.
               Prefix := Finish_Sem_Name_1 (Get_Prefix (Name_Prefix), Prefix);
               Set_Prefix (Res, Prefix);
               --  But free it.
               Free_Parenthesis_Name (Name, Res);
            else
               pragma Assert (Get_Parameter (Res) = Null_Iir);
               Prefix := Finish_Sem_Name (Name_Prefix, Prefix);
               Set_Prefix (Res, Prefix);
               Free_Iir (Name);
            end if;
            Finish_Sem_Signal_Attribute (Res);
            return Res;
         when Iir_Kind_Above_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute =>
            if Get_Parameter (Res) = Null_Iir then
               --  Not finished.  Need to emit an error message.
               Finish_Sem_Quantity_Attribute (Name, Res, (1 => Null_Iir));
            else
               Free_Parenthesis_Name (Name, Res);
            end if;
            return Res;
         when Iir_Kind_Dot_Attribute
            | Iir_Kind_Integ_Attribute =>
            --  Already finished.
            return Res;
         when Iir_Kinds_Type_Attribute
            | Iir_Kind_Subtype_Attribute
            | Iir_Kind_Element_Attribute
            | Iir_Kind_Base_Attribute =>
            pragma Assert (Get_Kind (Name) = Iir_Kind_Attribute_Name);
            Free_Iir (Name);
            return Res;
         when Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute =>
            Free_Iir (Name);
            return Res;
         when Iir_Kinds_External_Name =>
            pragma Assert (Name = Res);
            return Res;
         when Iir_Kind_Psl_Expression =>
            return Res;
         when Iir_Kind_Psl_Declaration =>
            return Name;
         when Iir_Kind_Element_Declaration =>
            --  Certainly an error!
            return Name;
         when Iir_Kind_Error =>
            return Name;
         when others =>
            Error_Kind ("finish_sem_name_1", Res);
      end case;

      --  The name has a prefix, finish it.
      Prefix := Get_Prefix (Res);
      Name_Prefix := Get_Prefix (Name);
      Prefix := Finish_Sem_Name_1 (Name_Prefix, Prefix);
      Set_Prefix (Res, Prefix);

      case Get_Kind (Res) is
         when Iir_Kind_Indexed_Name =>
            Finish_Sem_Indexed_Name (Res);
            Free_Parenthesis_Name (Name, Res);
         when Iir_Kind_Slice_Name =>
            Finish_Sem_Slice_Name (Res);
            Free_Parenthesis_Name (Name, Res);
         when Iir_Kind_Selected_Element =>
            pragma Assert (Get_Kind (Name) = Iir_Kind_Selected_Name);
            Xref_Ref (Res, Get_Named_Entity (Res));
            Set_Name_Staticness (Res, Get_Name_Staticness (Prefix));
            Set_Expr_Staticness (Res, Get_Expr_Staticness (Prefix));
            Set_Base_Name (Res, Get_Base_Name (Prefix));
            Free_Iir (Name);
         when Iir_Kind_Dereference =>
            pragma Assert (Get_Kind (Name) = Iir_Kind_Selected_By_All_Name);
            Finish_Sem_Dereference (Res);
            Free_Iir (Name);
         when Iir_Kind_Subtype_Attribute
            | Iir_Kind_Through_Attribute
            | Iir_Kind_Across_Attribute
            | Iir_Kind_Nature_Reference_Attribute =>
            Sem_Name_Free_Result (Name, Res);
         when Iir_Kinds_Signal_Value_Attribute =>
            Sem_Name_Free_Result (Name, Res);
            Finish_Sem_Signal_Attribute (Res);
         when others =>
            Error_Kind ("finish_sem_name_1(2)", Res);
      end case;
      return Res;
   end Finish_Sem_Name_1;

   function Finish_Sem_Name (Name : Iir; Res : Iir) return Iir is
   begin
      if Get_Kind (Res) /= Iir_Kind_Implicit_Dereference then
         --  There is no corresponding name for implicit_dereference (because
         --  it is implicit).
         --  Free overload list (but keep RES interpretation) for other cases.
         Free_Old_Entity_Name (Name, Res);
      end if;

      return Finish_Sem_Name_1 (Name, Res);
   end Finish_Sem_Name;

   function Finish_Sem_Name (Name : Iir) return Iir is
   begin
      return Finish_Sem_Name_1 (Name, Get_Named_Entity (Name));
   end Finish_Sem_Name;

   --  LRM93 6.2
   --  The evaluation of a simple name has no other effect than to determine
   --  the named entity denoted by the name.
   --
   --  NAME may be a simple name, a strig literal or a character literal.
   --  GHDL: set interpretation of NAME (possibly an overload list) or
   --  error_mark for unknown names.
   --  If SOFT is TRUE, then no error message is reported in case of failure.
   procedure Sem_Simple_Name (Name : Iir; Keep_Alias : Boolean; Soft : Boolean)
   is
      Id : constant Name_Id := Get_Identifier (Name);
      Interpretation: Name_Interpretation_Type;
      Res: Iir;
      Res_List : Iir_List;
      Res_It : List_Iterator;
      N : Natural;
   begin
      Interpretation := Get_Interpretation (Id);

      if not Valid_Interpretation (Interpretation) then
         --  Unknown name.
         if not Soft then
            Interpretation := Get_Interpretation_Raw (Id);
            if Valid_Interpretation (Interpretation)
              and then Is_Conflict_Declaration (Interpretation)
            then
               Error_Msg_Sem
                 (+Name, "no declaration for %i (due to conflicts)", +Name);
            else
               Error_Msg_Sem (+Name, "no declaration for %i", +Name);
            end if;
         end if;
         Res := Error_Mark;
      elsif not Valid_Interpretation (Get_Next_Interpretation (Interpretation))
      then
         --  One simple interpretation.
         Res := Get_Declaration (Interpretation);

         --  For a design unit, return the library unit
         if Get_Kind (Res) = Iir_Kind_Design_Unit then
            --  FIXME: should replace interpretation ?
            Load_Design_Unit (Res, Name);
            Sem.Add_Dependence (Res);
            Res := Get_Library_Unit (Res);
         end if;

         --  Check visibility.
         if not Get_Visible_Flag (Res) then
            if Flag_Relaxed_Rules
              and then Get_Kind (Res) in Iir_Kinds_Object_Declaration
              and then Valid_Interpretation (Get_Under_Interpretation (Id))
            then
               Res := Get_Declaration (Get_Under_Interpretation (Id));
            else
               if not Soft then
                  Error_Msg_Sem (+Name, "%n is not visible here", +Res);
               end if;
               --  Even if a named entity was found, return an error_mark.
               --  Indeed, the named entity found is certainly the one being
               --  analyzed, and the analyze may be uncomplete.
               Res := Error_Mark;
            end if;
         end if;

         if not Keep_Alias
           and then Get_Kind (Res) = Iir_Kind_Non_Object_Alias_Declaration
         then
            Res := Get_Named_Entity (Get_Name (Res));
         end if;
      else
         --  Name is overloaded.
         Res_List := Create_Iir_List;
         N := 0;
         --  The SEEN_FLAG is used to get only one meaning which can be reached
         --  through several paths (such as aliases).
         while Valid_Interpretation (Interpretation) loop
            if Keep_Alias then
               Res := Get_Declaration (Interpretation);
            else
               Res := Get_Non_Alias_Declaration (Interpretation);
            end if;
            if not Get_Seen_Flag (Res) then
               Set_Seen_Flag (Res, True);
               N := N + 1;
               Append_Element (Res_List, Res);
            end if;
            Interpretation := Get_Next_Interpretation (Interpretation);
         end loop;

         --  FIXME: there can be only one element (a function and its alias!).

         --  Clear SEEN_FLAG.
         Res_It := List_Iterate (Res_List);
         while Is_Valid (Res_It) loop
            Set_Seen_Flag (Get_Element (Res_It), False);
            Next (Res_It);
         end loop;

         Res := Create_Overload_List (Res_List);
      end if;

      Set_Named_Entity (Name, Res);
   end Sem_Simple_Name;

   --  LRM93 6.3
   --  Selected Names.
   procedure Sem_Selected_Name
     (Name: Iir; Keep_Alias : Boolean := False; Soft : Boolean := False)
   is
      Suffix : constant Name_Id := Get_Identifier (Name);
      Prefix_Name : constant Iir := Get_Prefix (Name);
      Prefix_Loc : constant Location_Type := Get_Location (Prefix_Name);

      Prefix: Iir;
      Res : Iir;

      --  Analyze SUB_NAME.NAME as an expanded name (ie, NAME is declared
      --  within SUB_NAME).  This is possible only if the expanded name is
      --  analyzed within the context of SUB_NAME.
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

      --  LRM93 6.3
      --  For a selected name that is used to denote a record element,
      --  the suffix must be a simple name denoting an element of a
      --  record object or value.  The prefix must be appropriate for the
      --  type of this object or value.
      --
      --  Analyze SUB_NAME.NAME as a selected element.
      procedure Sem_As_Selected_Element (Sub_Name : Iir)
      is
         Name_Type : Iir;
         Ptr_Type : Iir;
         Rec_El : Iir;
         R : Iir;
         Se : Iir;
      begin
         Name_Type := Get_Type (Sub_Name);
         if Kind_In (Name_Type, Iir_Kind_Access_Type_Definition,
                     Iir_Kind_Access_Subtype_Definition)
         then
            Ptr_Type := Name_Type;
            Name_Type := Get_Designated_Type (Name_Type);
         else
            Ptr_Type := Null_Iir;
         end if;

         --  Only records have elements.
         if not Kind_In (Name_Type, Iir_Kind_Record_Type_Definition,
                         Iir_Kind_Record_Subtype_Definition)
         then
            return;
         end if;

         Rec_El := Find_Name_In_Flist
           (Get_Elements_Declaration_List (Name_Type), Suffix);
         if Rec_El = Null_Iir then
            --  No such element in the record type.
            return;
         end if;

         if not Maybe_Function_Call (Sub_Name) then
            return;
         end if;

         R := Maybe_Insert_Function_Call (Prefix_Name, Sub_Name);
         R := Maybe_Insert_Dereference (R, Ptr_Type);

         Se := Create_Iir (Iir_Kind_Selected_Element);
         Location_Copy (Se, Name);
         Set_Prefix (Se, R);
         Set_Type (Se, Get_Type (Rec_El));
         Set_Identifier (Se, Suffix);
         Set_Named_Entity (Se, Rec_El);
         Set_Base_Name (Se, Get_Object_Prefix (R, False));
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
              (+Name, "%n does not designate a record", +Prefix);
         else
            Error_Msg_Sem
              (+Name, "no element %i in %n", (+Suffix, +Base_Type));
         end if;
      end Error_Selected_Element;

      procedure Sem_As_Protected_Item (Sub_Name : Iir)
      is
         Prot_Type : constant Iir := Get_Type (Sub_Name);
         Method : Iir;
      begin
         --  LRM98 12.3 Visibility
         --  s) For a subprogram declared immediately within a given protected
         --     type declaration: at the place of the suffix in a selected
         --     name whose prefix denotes an object of the protected type.
         Method := Get_Declaration_Chain (Prot_Type);
         while Method /= Null_Iir loop
            case Get_Kind (Method) is
               when Iir_Kind_Function_Declaration |
                 Iir_Kind_Procedure_Declaration =>
                  if Get_Identifier (Method) = Suffix then
                     Add_Result (Res, Method);
                  end if;
               when Iir_Kind_Attribute_Specification
                 | Iir_Kind_Use_Clause =>
                  null;
               when others =>
                  --  Declarations not allowed in protected types.
                  --  Just ignore them.
                  null;
            end case;
            Method := Get_Chain (Method);
         end loop;
      end Sem_As_Protected_Item;

      procedure Error_Protected_Item (Prot_Type : Iir) is
      begin
         Error_Msg_Sem (+Name, "no method %i in %n", (+Suffix, +Prot_Type));
      end Error_Protected_Item;

      --  Check if a synopsys package can be imported.
      procedure Check_Synopsys_Package (Lib : Iir)
      is
         use Std_Names;
      begin
         if Get_Identifier (Lib) /= Name_Ieee then
            return;
         end if;

         case Suffix is
            when Name_Std_Logic_Arith
              | Name_Std_Logic_Signed
              | Name_Std_Logic_Unsigned =>
               --  Synopsys package.
               null;
            when Name_Std_Logic_Textio =>
               if Vhdl_Std >= Vhdl_08 then
                  --  Standard ieee package in vhdl-08
                  return;
               end if;
            when others =>
               --  Not a synopsys package.
               return;
         end case;
         if Get_Identifier
           (Get_Library (Get_Design_File (Sem.Get_Current_Design_Unit)))
           = Name_Ieee
         then
            --  No error when referenced from an ieee package.  That could
            --  happen only for synopsys packages, so an error will be
            --  emitted when the user references the first synopsys package.
            return;
         end if;
         Error_Msg_Sem
           (+Name, "use of synopsys package %i needs the -fsynopsys option",
            +Suffix);
      end Check_Synopsys_Package;
   begin
      --  Analyze prefix.
      if Soft then
         Sem_Name_Soft (Prefix_Name);
      else
         Sem_Name (Prefix_Name);
      end if;
      Prefix := Get_Named_Entity (Prefix_Name);
      if Is_Error (Prefix) then
         Set_Named_Entity (Name, Prefix);
         return;
      end if;

      Res := Null_Iir;

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
               It : List_Iterator;
               El : Iir;
            begin
               --  So, first try as expanded name.
               Prefix_List := Get_Overload_List (Prefix);
               It := List_Iterate (Prefix_List);
               while Is_Valid (It) loop
                  El := Get_Element (It);
                  case Get_Kind (El) is
                     when Iir_Kind_Function_Call
                       | Iir_Kind_Selected_Element =>
                        --  Not an expanded name.
                        null;
                     when others =>
                        Sem_As_Expanded_Name (El);
                  end case;
                  Next (It);
               end loop;

               --  If no expanded name are found, try as selected element.
               if Res = Null_Iir then
                  It := List_Iterate (Prefix_List);
                  while Is_Valid (It) loop
                     El := Get_Element (It);
                     case Get_Kind (El) is
                        when Iir_Kind_Procedure_Declaration =>
                           --  A procedure cannot be the prefix of a selected
                           --  element.
                           null;
                        when others =>
                           Sem_As_Selected_Element (El);
                     end case;
                     Next (It);
                  end loop;
               end if;
            end;
            if Res = Null_Iir and then not Soft then
               Error_Msg_Sem
                 (+Name, "no suffix %i for overloaded selected name", +Suffix);
            end if;
         when Iir_Kind_Library_Declaration =>
            --  LRM93 6.3
            --  An expanded name denotes a primary unit constained in a design
            --  library if the prefix denotes the library and the suffix is the
            --  simple name if a primary unit whose declaration is contained
            --  in that library.
            --  An expanded name is not allowed for a secondary unit,
            --  particularly for an architecture body.
            --  GHDL: FIXME: error message more explicit
            Res := Load_Primary_Unit (Prefix, Suffix, Name);
            if Res /= Null_Iir then
               if not Soft and then not Flag_Synopsys then
                  Check_Synopsys_Package (Prefix);
               end if;
               Sem.Add_Dependence (Res);
               if Get_Kind (Res) = Iir_Kind_Design_Unit then
                  Res := Get_Library_Unit (Res);
               end if;
            elsif not Soft then
               Error_Msg_Sem
                 (+Name, "unit %i not found in %n", (+Suffix, +Prefix));
            end if;
         when Iir_Kind_Process_Statement
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_For_Loop_Statement =>
            --  LRM93 6.3
            --  An expanded name denotes a named entity declared immediatly
            --  within a named construct if the prefix that is an entity
            --  interface, an architecture, a subprogram, a block statement,
            --  a process statement, a generate statement, or a loop
            --  statement, and the suffix is the simple name, character
            --  literal, or operator symbol of an named entity whose
            --  declaration occurs immediatly within that construct.
            if Get_Kind (Prefix) = Iir_Kind_Design_Unit then
               Load_Design_Unit (Prefix, Name);
               Sem.Add_Dependence (Prefix);
               Prefix := Get_Library_Unit (Prefix);
               --  Modified only for xrefs, since a design_unit points to
               --  the first context clause, while a library unit points to
               --  the identifier.
               Set_Named_Entity (Get_Prefix (Name), Prefix);
            end if;

            Res := Find_Declarations_In_List (Prefix, Name, Keep_Alias);

            if Res = Null_Iir then
               if not Soft then
                  Error_Msg_Sem
                    (+Name, "no declaration for %i in %n", (+Suffix, +Prefix));
               end if;
            else
               --  LRM93 6.3
               --  This form of expanded name is only allowed within the
               --  construct itself.
               --  FIXME: LRM08 12.3 Visibility h)
               if not Kind_In (Prefix,
                               Iir_Kind_Package_Declaration,
                               Iir_Kind_Package_Instantiation_Declaration)
                 and then not Get_Is_Within_Flag (Prefix)
               then
                  if not Soft then
                     Error_Msg_Sem
                       (+Prefix_Loc,
                        "an expanded name is only allowed "
                          & "within the construct");
                  end if;
                  --  Hum, keep res.
               elsif Get_Kind (Prefix) = Iir_Kind_Package_Declaration
                 and then not Get_Is_Within_Flag (Prefix)
                 and then Is_Uninstantiated_Package (Prefix)
               then
                  --  LRM08 12.3 f) Visibility
                  --  For a declaration given in a package declaration, other
                  --  than in a package declaration that defines an
                  --  uninstantiated package: [...]
                  if not Soft then
                     Error_Msg_Sem
                       (+Prefix_Loc,
                        "cannot refer a declaration in an "
                          & "uninstantiated package");
                  end if;
               end if;
            end if;
         when Iir_Kind_Function_Declaration =>
            Sem_As_Expanded_Name (Prefix);
            if Res = Null_Iir then
               Sem_As_Selected_Element (Prefix);
            end if;
            if Res = Null_Iir and then not Soft then
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
               Sem_As_Protected_Item (Prefix);
               if Res = Null_Iir and then not Soft then
                  Error_Protected_Item (Prefix);
               end if;
            else
               Sem_As_Selected_Element (Prefix);
               if Res = Null_Iir and then not Soft then
                  Error_Selected_Element (Get_Type (Prefix));
               end if;
            end if;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Slice_Name
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Enumeration_Literal =>
            if not Soft then
               Error_Msg_Sem
                 (+Prefix_Loc, "%n cannot be selected by name", +Prefix);
            end if;

         when Iir_Kind_Error =>
            --  Let's propagate the error.
            null;

         when others =>
            Error_Kind ("sem_selected_name(2)", Prefix);
      end case;
      if Res = Null_Iir then
         Res := Error_Mark;
      end if;
      Set_Named_Entity (Name, Res);
   end Sem_Selected_Name;

   --  Extract actuals from ASSOC_CHAIN.  Report errors.
   procedure Extract_Attribute_Parameters
     (Assoc_Chain : Iir; Actuals : out Iir_Array)
   is
      Assoc : Iir;
   begin
      pragma Assert (Assoc_Chain /= Null_Iir);

      Assoc := Assoc_Chain;
      for I in Actuals'Range loop
         if Assoc = Null_Iir then
            Actuals (I) := Null_Iir;
         else
            --  Not 'open' association element ?
            if Get_Kind (Assoc) /= Iir_Kind_Association_Element_By_Expression
            then
               Error_Msg_Sem (+Assoc, "'open' is not an attribute parameter");
               Actuals (Actuals'First) := Null_Iir;
               return;
            end if;

            --  Not an association (ie no formal) ?
            if Get_Formal (Assoc) /= Null_Iir then
               Error_Msg_Sem
                 (+Assoc, "formal not allowed for attribute parameter");
               Actuals (Actuals'First) := Null_Iir;
               return;
            end if;

            Actuals (I) := Get_Actual (Assoc);

            Assoc := Get_Chain (Assoc);
         end if;
      end loop;

      if Assoc /= Null_Iir then
         Error_Msg_Sem (+Assoc, "too many parameters for the attribute");
      end if;
   end Extract_Attribute_Parameters;

   --  If ASSOC_ASSOC has one element, which is an expression without formal,
   --  return the actual, else return NULL_IIR.
   function Get_One_Actual (Assoc_Chain : Iir) return Iir
   is
      Assoc : Iir;
   begin
      --  Only one actual ?
      if Assoc_Chain = Null_Iir or else Get_Chain (Assoc_Chain) /= Null_Iir
      then
         return Null_Iir;
      end if;

      --  Not 'open' association element ?
      Assoc := Assoc_Chain;
      if Get_Kind (Assoc) /= Iir_Kind_Association_Element_By_Expression then
         return Null_Iir;
      end if;

      --  Not an association (ie no formal) ?
      if Get_Formal (Assoc) /= Null_Iir then
         return Null_Iir;
      end if;

      return Get_Actual (Assoc);
   end Get_One_Actual;

   function Slice_Or_Index (Actual : Iir) return Iir_Kind is
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

   --  Check whether association chain ASSOCS may be interpreted as indexes.
   function Index_Or_Not (Assocs : Iir) return Iir_Kind
   is
      El : Iir;
   begin
      El := Assocs;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Association_Element_By_Expression =>
               if Get_Formal (El) /= Null_Iir then
                  return Iir_Kind_Error;
               end if;
            when others =>
               --  Only expression are allowed.
               return Iir_Kind_Error;
         end case;
         El := Get_Chain (El);
      end loop;
      return Iir_Kind_Indexed_Name;
   end Index_Or_Not;

   function Sem_Index_Specification (Name : Iir_Parenthesis_Name; Itype : Iir)
                                    return Iir
   is
      Actual : Iir;
      Kind : Iir_Kind;
      Res : Iir;
   begin
      --  FIXME: reuse Sem_Name for the whole analysis ?

      Actual := Get_One_Actual (Get_Association_Chain (Name));
      if Actual = Null_Iir then
         Error_Msg_Sem (+Name, "only one index specification is allowed");
         return Null_Iir;
      end if;
      case Get_Kind (Actual) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Sem_Name (Actual);
            Kind := Slice_Or_Index (Get_Named_Entity (Actual));
            --  FIXME: analyze to be finished.
            --Maybe_Finish_Sem_Name (Actual);
         when Iir_Kind_Subtype_Definition
           | Iir_Kind_Range_Expression =>
            Kind := Iir_Kind_Slice_Name;
         when others =>
            Kind := Slice_Or_Index (Actual);
      end case;

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
               Error_Msg_Sem (+Name, "index must be a static expression");
            end if;
            Set_Index_List (Res, Create_Iir_Flist (1));
            Set_Nth_Element (Get_Index_List (Res), 0, Actual);
         when Iir_Kind_Slice_Name =>
            Actual := Sem_Discrete_Range (Actual, Itype, False);
            if Actual = Null_Iir then
               return Null_Iir;
            end if;
            Set_Suffix (Res, Actual);
            Actual := Get_Range_From_Discrete_Range (Actual);
            if Get_Expr_Staticness (Actual) < Globally then
               Error_Msg_Sem (+Name, "index must be a static expression");
            end if;
         when others =>
            raise Internal_Error;
      end case;
      Free_Parenthesis_Name (Name, Res);
      return Res;
   end Sem_Index_Specification;

   procedure Sem_Parenthesis_Name (Name : Iir_Parenthesis_Name)
   is
      Prefix_Name : constant Iir := Get_Prefix (Name);
      Prefix: Iir;
      Res : Iir;
      Res_Prefix : Iir;
      Assoc_Chain : Iir;

      Slice_Index_Kind : Iir_Kind;

      --  If FINISH is TRUE, then display error message in case of error.
      function Sem_As_Indexed_Or_Slice_Name (Sub_Name : Iir; Finish : Boolean)
        return Iir
      is
         Arr_Type : Iir;
         Ptr_Type : Iir;
         P : Iir;
         R : Iir;
      begin
         if Slice_Index_Kind = Iir_Kind_Error then
            if Finish then
               Error_Msg_Sem (+Name, "prefix is not a function name");
            end if;
            --  No way.
            return Null_Iir;
         end if;

         --  Only values can be indexed or sliced.
         --  Catch errors such as slice of a type conversion.
         if Name_To_Value (Sub_Name) = Null_Iir
           and then not Is_Function_Declaration (Sub_Name)
         then
            if Finish then
               Error_Msg_Sem
                 (+Name, "prefix is not an array value (found %n)", +Sub_Name);
            end if;
            return Null_Iir;
         end if;

         --  Extract type of prefix, handle possible implicit deference.
         Arr_Type := Get_Type (Sub_Name);
         if Kind_In (Arr_Type, Iir_Kind_Access_Type_Definition,
                     Iir_Kind_Access_Subtype_Definition)
         then
            --  FIXME: use base type until full support of access subtypes.
            Ptr_Type := Get_Base_Type (Arr_Type);
            Arr_Type := Get_Designated_Type (Arr_Type);
         else
            Ptr_Type := Null_Iir;
         end if;

         if not Kind_In (Arr_Type, Iir_Kind_Array_Type_Definition,
                         Iir_Kind_Array_Subtype_Definition)
         then
            if Finish and then not Is_Error (Arr_Type) then
               Error_Msg_Sem (+Name, "type of prefix is not an array");
            end if;
            return Null_Iir;
         end if;
         if Get_Nbr_Elements (Get_Index_Subtype_List (Arr_Type)) /=
           Get_Chain_Length (Assoc_Chain)
         then
            if Finish then
               Error_Msg_Sem
                 (+Name, "number of indexes mismatches array dimension");
            end if;
            return Null_Iir;
         end if;

         --  For indexed names, discard type incompatibilities between indexes
         --  and array type indexes.
         --  The FINISH = True case will be handled by Finish_Sem_Indexed_Name.
         if Slice_Index_Kind = Iir_Kind_Indexed_Name and then not Finish then
            declare
               Type_Index_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (Arr_Type);
               Type_Index : Iir;
               Assoc : Iir;
            begin
               Assoc := Assoc_Chain;
               for I in Natural loop
                  --  Assoc and Type_Index_List have the same length as this
                  --  was checked just above.
                  exit when Assoc = Null_Iir;
                  if Get_Kind (Assoc)
                    /= Iir_Kind_Association_Element_By_Expression
                  then
                     return Null_Iir;
                  end if;
                  Type_Index := Get_Index_Type (Type_Index_List, I);
                  if Is_Expr_Compatible (Type_Index, Get_Actual (Assoc))
                    = Not_Compatible
                  then
                     return Null_Iir;
                  end if;
                  Assoc := Get_Chain (Assoc);
               end loop;
            end;
         end if;

         if not Maybe_Function_Call (Sub_Name) then
            if Finish then
               Error_Msg_Sem (+Name, "missing parameters for function call");
            end if;
            return Null_Iir;
         end if;

         P := Maybe_Insert_Function_Call (Prefix_Name, Sub_Name);
         P := Maybe_Insert_Dereference (P, Ptr_Type);

         R := Create_Iir (Slice_Index_Kind);
         Location_Copy (R, Name);
         Set_Prefix (R, P);
         Set_Base_Name (R, Get_Object_Prefix (P));

         case Slice_Index_Kind is
            when Iir_Kind_Slice_Name =>
               Set_Suffix (R, Get_Actual (Assoc_Chain));
               Set_Type (R, Get_Base_Type (Get_Type (P)));
            when Iir_Kind_Indexed_Name =>
               declare
                  Idx_El : Iir;
                  Idx_List : Iir_List;
               begin
                  Idx_List := Create_Iir_List;
                  Idx_El := Assoc_Chain;
                  while Idx_El /= Null_Iir loop
                     Append_Element (Idx_List, Get_Actual (Idx_El));
                     Idx_El := Get_Chain (Idx_El);
                  end loop;
                  Set_Index_List (R, List_To_Flist (Idx_List));
               end;
               Set_Type (R, Get_Element_Subtype (Arr_Type));
            when others =>
               raise Internal_Error;
         end case;

         return R;
      end Sem_As_Indexed_Or_Slice_Name;

      --  Sem parenthesis name when the prefix is a function declaration.
      --  Can be either a function call (and the expression is the actual) or
      --  a slice/index of the result of a call without actual.
      procedure Sem_Parenthesis_Function (Sub_Name : Iir)
      is
         Used : Boolean;
         R : Iir;
         Match : Compatibility_Level;
         Call : Iir;
      begin
         Used := False;

         --  A function call.
         if Is_Function_Declaration (Sub_Name) then
            Sem_Association_Chain
              (Get_Interface_Declaration_Chain (Sub_Name),
               Assoc_Chain, False, Missing_Parameter, Name, Match);
            if Match /= Not_Compatible then
               Call := Sem_As_Function_Call
                 (Prefix_Name, Sub_Name, Assoc_Chain);
               Add_Result (Res, Call);
               Add_Result (Res_Prefix, Sub_Name);
               Used := True;
            end if;
         end if;

         --  A slice/index of a call (without parameters).
         if not Is_Procedure_Declaration (Sub_Name) then
            R := Sem_As_Indexed_Or_Slice_Name (Sub_Name, False);
            if R /= Null_Iir then
               Add_Result (Res, R);
               Add_Result (Res_Prefix, Sub_Name);
               Used := True;
            end if;
         end if;

         if not Used then
            Sem_Name_Free_Result (Sub_Name, Null_Iir);
         end if;
      end Sem_Parenthesis_Function;

      procedure Error_Parenthesis_Function (Spec : Iir)
      is
         Match : Compatibility_Level;
      begin
         Error_Msg_Sem (+Name, "cannot match %n with actuals", +Prefix);
         --  Display error message.
         Sem_Association_Chain
           (Get_Interface_Declaration_Chain (Spec),
            Assoc_Chain, True, Missing_Parameter, Name, Match);
      end Error_Parenthesis_Function;

      Actual : Iir;
      Actual_Expr : Iir;
   begin
      --  The prefix is a function name, a type mark or an array.
      Sem_Name (Prefix_Name);
      Prefix := Get_Named_Entity (Prefix_Name);
      if Prefix = Error_Mark then
         Set_Named_Entity (Name, Error_Mark);
         return;
      end if;
      Res := Null_Iir;

      Assoc_Chain := Get_Association_Chain (Name);
      Actual := Get_One_Actual (Assoc_Chain);

      if Kind_In (Prefix,
                  Iir_Kind_Type_Declaration, Iir_Kind_Subtype_Declaration)
      then
         --  A type conversion.  The prefix is a type mark.
         declare
            In_Formal : Boolean;
         begin
            if Actual = Null_Iir then
               --  More than one actual.  Keep only the first.
               Error_Msg_Sem
                 (+Name, "type conversion allows only one expression");
               In_Formal := False;
            else
               In_Formal := Get_In_Formal_Flag (Assoc_Chain);
            end if;

            --  This is certainly the easiest case: the prefix is not
            --  overloaded, so the result can be computed.
            Set_Named_Entity
              (Name, Sem_Type_Conversion (Name, Prefix, Actual, In_Formal));
         end;
         return;
      end if;

      --  Select between slice or indexed name.
      Actual_Expr := Null_Iir;
      if Actual /= Null_Iir then
         --  Only one actual: can be a slice or an index
         case Get_Kind (Actual) is
            when Iir_Kinds_Name
              | Iir_Kind_Attribute_Name =>
               --  Maybe a discrete range name.
               Sem_Name (Actual);
               Actual_Expr := Get_Named_Entity (Actual);
               if Actual_Expr = Error_Mark then
                  Set_Named_Entity (Name, Actual_Expr);
                  return;
               end if;
               --  Decides between sliced or indexed name to actual.
               Slice_Index_Kind := Slice_Or_Index (Actual_Expr);
            when Iir_Kind_Range_Expression
              | Iir_Kind_Subtype_Definition =>
               --  This can only be a slice.
               Slice_Index_Kind := Iir_Kind_Slice_Name;
            when others =>
               --  Any other expression: an indexed name.
               Slice_Index_Kind := Iir_Kind_Indexed_Name;
         end case;
      else
         --  More than one actual: an indexed name.

         --  FIXME: improve error message for multi-dim slice ?
         Slice_Index_Kind := Index_Or_Not (Assoc_Chain);
      end if;

      --  Analyze actuals if not already done (done for slices).
      if Slice_Index_Kind /= Iir_Kind_Slice_Name then
         if Sem_Actual_Of_Association_Chain (Assoc_Chain) = False then
            Actual := Null_Iir;
         else
            Actual := Get_One_Actual (Assoc_Chain);
         end if;
      end if;

      Res_Prefix := Null_Iir;

      case Get_Kind (Prefix) is
         when Iir_Kind_Overload_List =>
            declare
               El : Iir;
               Prefix_List : Iir_List;
               It : List_Iterator;
            begin
               Prefix_List := Get_Overload_List (Prefix);
               It := List_Iterate (Prefix_List);
               while Is_Valid (It) loop
                  El := Get_Element (It);
                  Sem_Parenthesis_Function (El);
                  Next (It);
               end loop;
               --  Some prefixes may have been removed, replace with the
               --  rebuilt prefix list.
               Free_Overload_List (Prefix);
               Set_Named_Entity (Prefix_Name, Res_Prefix);
            end;
            if Res = Null_Iir then
               Error_Msg_Sem
                 (+Name, "no overloaded function found matching %n",
                  +Prefix_Name);
            end if;
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            Sem_Parenthesis_Function (Prefix);
            Set_Named_Entity (Prefix_Name, Res_Prefix);
            if Res = Null_Iir then
               Error_Parenthesis_Function (Prefix);
            end if;

         when Iir_Kinds_Object_Declaration
            | Iir_Kind_Indexed_Name
            | Iir_Kind_Slice_Name
            | Iir_Kind_Dereference
            | Iir_Kind_Implicit_Dereference
            | Iir_Kind_Selected_Element
            | Iir_Kind_Attribute_Value
            | Iir_Kind_Simple_Name_Attribute
            | Iir_Kind_Function_Call =>
            Add_Result (Res, Sem_As_Indexed_Or_Slice_Name (Prefix, True));

         when Iir_Kinds_Array_Attribute =>
            if Actual /= Null_Iir then
               Finish_Sem_Array_Attribute (Prefix_Name, Prefix, Actual);
               Set_Named_Entity (Name, Prefix);
            else
               Error_Msg_Sem (+Name, "bad attribute parameter");
               Set_Named_Entity (Name, Error_Mark);
            end if;
            return;

         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            Error_Msg_Sem
              (+Name, "subprogram name is a type mark (missing apostrophe)");

         when Iir_Kinds_Scalar_Type_Attribute
            | Iir_Kind_Image_Attribute
            | Iir_Kind_Value_Attribute =>
            if Get_Parameter (Prefix) /= Null_Iir then
               --  Attribute already has a parameter, the expression
               --  is either a slice or an index.
               Add_Result
                 (Res, Sem_As_Indexed_Or_Slice_Name (Prefix, True));
            elsif Actual /= Null_Iir then
               Finish_Sem_Scalar_Type_Attribute (Prefix_Name, Prefix, Actual);
               Res := Prefix;
            else
               Error_Msg_Sem (+Name, "bad attribute parameter");
               Res := Error_Mark;
            end if;

         when Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute =>
            if Get_Parameter (Prefix) /= Null_Iir then
               --  Attribute is complete, so it is now index or slice.
               Add_Result (Res, Sem_As_Indexed_Or_Slice_Name (Prefix, True));
            elsif Actual /= Null_Iir then
               Finish_Sem_Signal_Attribute_Signal (Prefix, Actual);
               Res := Prefix;
            else
               Error_Msg_Sem (+Name, "bad attribute parameter");
               Res := Error_Mark;
            end if;

         when Iir_Kind_Ramp_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute =>
            declare
               Params : Iir_Array (1 .. 4);
            begin
               --  Try to extract 2 actuals from the name association list.
               --  Emit a message in case of error.
               Extract_Attribute_Parameters (Assoc_Chain, Params);

               if Params (1) /= Null_Iir then
                  --  If ok, finish analysis.
                  Finish_Sem_Quantity_Attribute (Prefix_Name, Prefix, Params);
               else
                  Prefix := Error_Mark;
               end if;
               --  The meaning of the parenthesis name is the attribute (as
               --  the actuals have been moved to the attribute node).
               Set_Named_Entity (Name, Prefix);
               return;
            end;

         when Iir_Kind_Above_Attribute =>
            if Actual /= Null_Iir then
               Finish_Sem_Quantity_Attribute
                 (Prefix_Name, Prefix, (1 => Actual));
            else
               Error_Msg_Sem (+Name, "bad attribute parameter");
               Prefix := Error_Mark;
            end if;
            Set_Named_Entity (Name, Prefix);
            return;

         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            Error_Msg_Sem (+Name, "cannot call %n in an expression",
                           +Prefix);

         when Iir_Kinds_Sequential_Statement
            | Iir_Kinds_Concurrent_Statement
            | Iir_Kind_Component_Declaration
            | Iir_Kind_Type_Conversion
            | Iir_Kind_Unit_Declaration
            | Iir_Kind_Enumeration_Literal
            | Iir_Kind_Attribute_Declaration
            | Iir_Kinds_Library_Unit
            | Iir_Kind_Library_Declaration
            | Iir_Kinds_Type_Attribute
            | Iir_Kind_Nature_Declaration
            | Iir_Kind_Subnature_Declaration
            | Iir_Kind_Group_Declaration
            | Iir_Kind_Group_Template_Declaration =>
            Error_Msg_Sem (+Name, "%n cannot be indexed or sliced", +Prefix);
            Res := Null_Iir;

         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            Res := Sem_Psl.Sem_Psl_Name (Name);

         when Iir_Kind_Error =>
            --  Continue with the error.
            Res := Prefix;

         when others =>
            Error_Kind ("sem_parenthesis_name", Prefix);
      end case;

      if Res = Null_Iir then
         Res := Error_Mark;
      end if;
      Set_Named_Entity (Name, Res);
   end Sem_Parenthesis_Name;

   procedure Sem_Selected_By_All_Name (Name : Iir_Selected_By_All_Name)
   is
      Prefix : Iir;
      Prefix_Name : Iir;
      Res : Iir;

      procedure Sem_As_Selected_By_All_Name (Sub_Name : Iir)
      is
         Base_Type : Iir;
         R, R1 : Iir;
      begin
         --  Only accept prefix of access type.
         Base_Type := Get_Base_Type (Get_Type (Sub_Name));
         if Get_Kind (Base_Type) /= Iir_Kind_Access_Type_Definition then
            return;
         end if;

         if not Maybe_Function_Call (Sub_Name) then
            return;
         end if;

         R1 := Maybe_Insert_Function_Call (Get_Prefix (Name), Sub_Name);

         R := Create_Iir (Iir_Kind_Dereference);
         Location_Copy (R, Name);
         Set_Prefix (R, R1);
         --  FIXME: access subtype.
         Set_Type (R, Get_Designated_Type (Base_Type));
         Add_Result (Res, R);
      end Sem_As_Selected_By_All_Name;
   begin
      Prefix := Get_Prefix (Name);
      Sem_Name (Prefix);
      Prefix_Name := Prefix;
      Prefix := Get_Named_Entity (Prefix);
      if Prefix = Null_Iir then
         return;
      end if;
      Res := Null_Iir;

      case Get_Kind (Prefix) is
         when Iir_Kind_Overload_List =>
            declare
               Prefix_List : constant Iir_List := Get_Overload_List (Prefix);
               It : List_Iterator;
            begin
               It := List_Iterate (Prefix_List);
               while Is_Valid (It) loop
                  Sem_As_Selected_By_All_Name (Get_Element (It));
                  Next (It);
               end loop;
            end;
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Function_Call =>
            Sem_As_Selected_By_All_Name (Prefix);
         when Iir_Kind_Function_Declaration =>
            Prefix := Sem_As_Function_Call (Name => Prefix_Name,
                                            Spec => Prefix,
                                            Assoc_Chain => Null_Iir);
            Sem_As_Selected_By_All_Name (Prefix);
         when Iir_Kind_Library_Declaration
            | Iir_Kind_Package_Declaration =>
            Error_Msg_Sem (+Name, "%n cannot be selected by all", +Prefix);
            Set_Named_Entity (Name, Error_Mark);
            return;
         when Iir_Kind_Error =>
            Set_Named_Entity (Name, Error_Mark);
            return;
         when others =>
            Error_Kind ("sem_selected_by_all_name", Prefix);
      end case;
      if Res = Null_Iir then
         Error_Msg_Sem (+Name, "prefix type is not an access type");
         Res := Error_Mark;
      end if;
      Set_Named_Entity (Name, Res);
   end Sem_Selected_By_All_Name;

   function Sem_Base_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix_Name : Iir;
      Prefix_Type : Iir;
      Res : Iir;
      Base_Type : Iir;
      Type_Decl : Iir;
   begin
      Prefix_Name := Finish_Sem_Name (Get_Prefix (Attr));
      Prefix_Type := Name_To_Type_Definition (Prefix_Name);
      if not Is_Error (Prefix_Type) then
         Base_Type := Get_Base_Type (Prefix_Type);
         --  Get the first subtype.  FIXME: ref?
         Type_Decl := Get_Type_Declarator (Base_Type);
         if Get_Kind (Type_Decl) = Iir_Kind_Anonymous_Type_Declaration then
            Base_Type := Get_Subtype_Definition (Type_Decl);
         end if;
      else
         Base_Type := Prefix_Type;
      end if;
      Res := Create_Iir (Iir_Kind_Base_Attribute);
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix_Name);
      Set_Type (Res, Base_Type);
      return Res;
   end Sem_Base_Attribute;

   function Sem_User_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix : Iir;
      Value : Iir;
      Attr_Id : Name_Id;
   begin
      Prefix := Get_Named_Entity (Prefix_Name);

      --  LRM93 6.6
      --  If the attribute name denotes an alias, then the attribute name
      --  denotes an attribute of the aliased name and not the alias itself,
      --  except when the attribute designator denotes any of the predefined
      --  attributes 'simple_name, 'path_name, or 'instance_name.
      if Get_Kind (Prefix) = Iir_Kind_Object_Alias_Declaration then
         --  GHDL: according to 4.3.3, the name cannot be an alias.
         Prefix := Strip_Denoting_Name (Get_Name (Prefix));
      end if;

      --  LRM93 6.6
      --  If the attribute designator denotes a user-defined attribute, the
      --  prefix cannot denote a subelement or a slice of an object.
      case Get_Kind (Prefix) is
         when Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Element =>
            Error_Msg_Sem (+Attr, "prefix of user defined attribute cannot be "
                             & "an object subelement");
            return Error_Mark;
         when Iir_Kind_Dereference =>
            Error_Msg_Sem (+Attr, "prefix of user defined attribute cannot be "
                             & "an anonymous object");
            return Error_Mark;
         when Iir_Kind_Attribute_Declaration =>
            Error_Msg_Sem (+Attr, "prefix of user defined attribute cannot be "
                             & "an attribute");
            return Error_Mark;
         when Iir_Kind_Function_Call
           | Iir_Kind_Type_Conversion
           | Iir_Kinds_Attribute =>
            Error_Msg_Sem (+Attr, "invalid prefix for user defined attribute");
            return Error_Mark;
         when Iir_Kinds_Object_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kinds_Library_Unit =>
            --  FIXME: complete
            null;
         when Iir_Kinds_Sequential_Statement
           | Iir_Kinds_Concurrent_Statement =>
            --  May appear textually before the statement.
            Set_Is_Forward_Ref (Prefix_Name, True);
         when others =>
            Error_Kind ("sem_user_attribute", Prefix);
      end case;

      Attr_Id := Get_Identifier (Attr);
      Value := Sem_Specs.Find_Attribute_Value (Prefix, Attr_Id);
      if Value = Null_Iir then
         Error_Msg_Sem (+Attr, "%n was not annotated with attribute %i",
                        (+Prefix, +Attr_Id));
         if Attr_Id = Std_Names.Name_First or Attr_Id = Std_Names.Name_Last
         then
            --  Nice (?) message for Ada users.
            Error_Msg_Sem
              (+Attr, "(you may use 'high, 'low, 'left or 'right attribute)");
         end if;
         return Error_Mark;
      end if;

      Xref_Ref (Attr, Value);

      if Get_Static_Attribute_Flag (Get_Attribute_Specification (Value))
        and then not Get_Is_Within_Flag (Prefix)
        and then Get_Expr_Staticness (Value) /= Locally
      then
         Error_Msg_Sem
           (+Attr, "non-locally static attribute cannot be referenced here");
         return Error_Mark;
      end if;

      return Value;
   end Sem_User_Attribute;

   --  The prefix of scalar type attributes is a type name (or 'base), and
   --  therefore isn't overloadable.  So at the end of the function, the
   --  analyze is finished.
   function Sem_Scalar_Type_Attribute (Attr : Iir_Attribute_Name)
                                      return Iir
   is
      use Std_Names;
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Id : constant Name_Id := Get_Identifier (Attr);
      Prefix_Type : Iir;
      Res : Iir;
   begin
      --  LRM93 14.1
      --  Prefix: Any discrete or physical type of subtype T.
      Prefix_Type :=
        Name_To_Type_Definition (Name_To_Analyzed_Name (Prefix_Name));
      Set_Type (Prefix_Name, Prefix_Type);
      if Is_Error (Prefix_Type) then
         --Error_Msg_Sem
         --(+Attr, "prefix of %i attribute must be a type", +Id);
         return Error_Mark;
      end if;

      case Id is
         when Name_Image
           | Name_Value =>
            if Get_Kind (Prefix_Type)
              not in Iir_Kinds_Scalar_Type_And_Subtype_Definition
            then
               Report_Start_Group;
               Error_Msg_Sem
                 (+Attr, "prefix of %i attribute must be a scalar type", +Id);
               Error_Msg_Sem
                 (+Attr, "found %n defined at %l",
                  (+Prefix_Type, +Prefix_Type));
               Report_End_Group;
               return Error_Mark;
            end if;
         when others =>
            case Get_Kind (Prefix_Type) is
               when Iir_Kinds_Discrete_Type_Definition
                 | Iir_Kind_Physical_Subtype_Definition
                 | Iir_Kind_Physical_Type_Definition =>
                  null;
               when others =>
                  Report_Start_Group;
                  Error_Msg_Sem
                    (+Attr, "prefix of %i"
                       & " attribute must be discrete or physical type", +Id);
                  Error_Msg_Sem
                    (+Attr, "found %n defined at %l",
                     (+Prefix_Type, +Prefix_Type));
                  Report_End_Group;
                  return Error_Mark;
            end case;
      end case;

      --  Create the resulting node.
      case Get_Identifier (Attr) is
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
      Set_Prefix (Res, Prefix_Name);
      Set_Base_Name (Res, Res);

      case Get_Identifier (Attr) is
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

   --  Analyze attributes whose prefix is a type or a subtype and result is
   --  a value (not a function).
   function Sem_Predefined_Type_Attribute (Attr : Iir_Attribute_Name)
     return Iir
   is
      use Std_Names;
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Id : constant Name_Id := Get_Identifier (Attr);
      Res : Iir;
      Prefix : Iir;
      Prefix_Type : Iir;
   begin
      case Id is
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
              (+Attr,
               "prefix of range attribute must be an array type or object");
            return Error_Mark;
         when others =>
            Error_Msg_Sem (+Attr, "attribute %i not valid on this type", +Id);
            return Error_Mark;
      end case;
      Location_Copy (Res, Attr);
      Set_Base_Name (Res, Res);

      Prefix := Get_Named_Entity (Prefix_Name);
      case Get_Kind (Prefix) is
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            Prefix := Finish_Sem_Name (Prefix_Name, Prefix);
            Prefix_Type := Get_Type (Prefix);
            Set_Expr_Staticness (Res, Get_Expr_Staticness (Prefix));
         when Iir_Kind_Base_Attribute =>
            --  Base_Attribute is already finished.
            pragma Assert (Get_Kind (Prefix_Name) = Iir_Kind_Attribute_Name);
            Free_Iir (Prefix_Name);
            Prefix_Type := Get_Type (Prefix);
            Set_Expr_Staticness (Res, Get_Type_Staticness (Prefix_Type));
         when others =>
            Prefix := Sem_Type_Mark (Prefix_Name);
            Prefix_Type := Get_Type (Prefix);
            Set_Expr_Staticness (Res, Get_Type_Staticness (Prefix_Type));
      end case;
      Set_Prefix (Res, Prefix);

      case Get_Identifier (Attr) is
         when Name_Ascending =>
            --  LRM93 14.1
            --  Result Type: type boolean.
            Set_Type (Res, Boolean_Type_Definition);
         when others =>
            --  LRM 14.1
            --  Result Type: Same type as T.
            Set_Type (Res, Prefix_Type);
      end case;
      return Res;
   end Sem_Predefined_Type_Attribute;

   --  Called for attributes Length, Left, Right, High, Low, Range,
   --  Reverse_Range, Ascending.
   --  FIXME: handle overload
   function Sem_Array_Attribute_Name (Attr : Iir_Attribute_Name) return Iir
   is
      use Std_Names;
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix_Type : Iir;
      Prefix : Iir;
      Res : Iir;
      Res_Type : Iir;
   begin
      Prefix := Get_Named_Entity (Prefix_Name);

      --  LRM93 14.1
      --  Prefix: Any prefix A that is appropriate for an array object, or an
      --  alias thereof, or that denotes a constrained array subtype.
      --
      --  LRM08 16.2 Predefined attributes.
      --  Prefix of A'Left[(N)], A'Right[(N)]... :
      --  Any prefix A that is appropriate for an array object, or an alias
      --  thereof, or that denotes a constrained an array subtype whose index
      --  ranges are defined by a constraint.
      --
      --  GHDL: the prefix cannot be a function call, as the result is not
      --  an object and it doesn't denote a subtype.  References are:
      --
      --  LRM08 6.4 Objects:
      --  An object is a named entity [...]
      --  In addition the following are objects, but are not named
      --  entities[...]
      --
      --  LRM08 6 Declarations
      --  the name is said to denote the associated entity.
      case Get_Kind (Prefix) is
         when Iir_Kind_Dereference
           | Iir_Kinds_Object_Declaration
           | Iir_Kind_Function_Call
           | Iir_Kind_Function_Declaration
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
                  Error_Msg_Sem (+Attr, "object prefix must be an array");
                  return Error_Mark;
            end case;
         when Iir_Kind_Subtype_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute =>
            Prefix_Type := Get_Type (Prefix);
            if not Is_Fully_Constrained_Type (Prefix_Type) then
               Error_Msg_Sem (+Attr, "prefix type is not constrained");
               --  We continue using the unconstrained array type.
               --  At least, this type is valid; and even if the array was
               --  constrained, the base type would be the same.
            end if;
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            --  For names such as pfx'Range'Left.
            --  Finish_Sem_Array_Attribute (Prefix_Name, Prefix, Null_Iir);
            Prefix_Type := Get_Type (Prefix);
         when Iir_Kind_Process_Statement =>
            Error_Msg_Sem
              (+Attr, "%n is not an appropriate prefix for %i attribute",
               (+Prefix, +Attr));
            return Error_Mark;
         when others =>
            Error_Msg_Sem
              (+Attr, "prefix must denote an array object or type");
            return Error_Mark;
      end case;

      case Get_Kind (Prefix_Type) is
         when Iir_Kinds_Scalar_Type_And_Subtype_Definition =>
            --  Note: prefix is a scalar type or subtype.
            return Sem_Predefined_Type_Attribute (Attr);
         when Iir_Kinds_Array_Type_Definition =>
            null;
         when others =>
            Error_Msg_Sem (+Attr, "prefix of %i attribute must denote a "
                             & "constrained array subtype", +Attr);
            return Error_Mark;
      end case;

      --  Type of the attribute.  This is correct unless there is a parameter,
      --  and furthermore 'range and 'reverse_range has to be handled
      --  specially because the result is a range and not a value.
      Res_Type := Get_Index_Type (Get_Index_Subtype_List (Prefix_Type), 0);

      --  Create the node for the attribute.
      case Get_Identifier (Attr) is
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
            --  FIXME: Error if ambiguous
            Res_Type := Convertible_Integer_Type_Definition;
         when Name_Ascending =>
            Res := Create_Iir (Iir_Kind_Ascending_Array_Attribute);
            --  FIXME: Error if ambiguous
            Res_Type := Boolean_Type_Definition;
         when others =>
            raise Internal_Error;
      end case;
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      Set_Type (Res, Res_Type);
      return Res;
   end Sem_Array_Attribute_Name;

   --  For 'Subtype
   function Sem_Subtype_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix_Name : Iir;
      Attr_Type   : Iir;
      Res         : Iir;
   begin
      Prefix_Name := Get_Prefix (Attr);
      Prefix_Name := Finish_Sem_Name (Prefix_Name);
      Set_Prefix (Attr, Prefix_Name);

      --  LRM08 16.2 Predefined attributes
      --  Prefix: Any prefix O that is appropriate for an object, or an alias
      --  thereof
      if not Is_Object_Name (Prefix_Name) then
         Error_Msg_Sem (+Attr, "prefix must denote an object");
         return Error_Mark;
      end if;

      --  The type defined by 'subtype is always constrained.  Create
      --  a subtype if it is not.
      Attr_Type := Get_Type (Prefix_Name);
      if False and not Is_Fully_Constrained_Type (Attr_Type) then
         Attr_Type := Sem_Types.Build_Constrained_Subtype (Attr_Type, Attr);
      end if;

      Res := Create_Iir (Iir_Kind_Subtype_Attribute);
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix_Name);
      Set_Type (Res, Attr_Type);

      Set_Base_Name (Res, Res);
      Set_Name_Staticness (Res, Get_Name_Staticness (Prefix_Name));
      Set_Type_Staticness (Res, Get_Type_Staticness (Attr_Type));

      return Res;
   end Sem_Subtype_Attribute;

   --  For 'Element
   function Sem_Element_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix_Name  : Iir;
      Attr_Type    : Iir;
      Attr_Subtype : Iir;
      Res          : Iir;
   begin
      Prefix_Name := Get_Prefix (Attr);
      Prefix_Name := Finish_Sem_Name (Prefix_Name);
      Set_Prefix (Attr, Prefix_Name);

      --  LRM08 16.2 Predefined attributes
      --  Prefix: Any prefix A that is appropriate for an array object, or an
      --  alias thereof, or that denotes an array subtype
      if (Get_Kind (Get_Base_Name (Prefix_Name))
          in Iir_Kinds_Object_Declaration)
      then
         Attr_Type := Get_Type (Prefix_Name);
      elsif (Get_Kind (Get_Base_Name (Prefix_Name))
          in Iir_Kinds_Type_Declaration)
      then
         Attr_Type := Get_Type (Get_Base_Name (Prefix_Name));
      else
         Error_Msg_Sem (+Attr, "prefix must denote an object or a type");
      end if;

      if False and not Is_Array_Type (Attr_Type) then
         Error_Msg_Sem (+Attr, "prefix must denote an array");
      end if;

      --  The type defined by 'element is always constrained.  Create
      --  a subtype if it is not.
      Attr_Subtype := Get_Element_Subtype (Attr_Type);
      if False and not Is_Fully_Constrained_Type (Attr_Subtype) then
         Attr_Subtype :=
             Sem_Types.Build_Constrained_Subtype (Attr_Subtype, Attr);
      end if;

      Res := Create_Iir (Iir_Kind_Element_Attribute);
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix_Name);
      Set_Type (Res, Attr_Subtype);

      Set_Base_Name (Res, Res);
      Set_Name_Staticness (Res, Get_Name_Staticness (Prefix_Name));
      Set_Type_Staticness (Res, Get_Type_Staticness (Attr_Subtype));

      return Res;
   end Sem_Element_Attribute;

   --  For 'Across or 'Through
   function Sem_Nature_Type_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix : Iir;
      Prefix_Nature : Iir;
      Res : Iir;
      Attr_Type : Iir;
   begin
      Prefix := Get_Named_Entity (Prefix_Name);

      --  LRM08 16.2 Predefined attributes
      --  Prefix: Any nature or subnature N.
      case Get_Kind (Prefix) is
         when Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration =>
            null;
         when others =>
            Error_Msg_Sem (+Attr, "prefix must denote a nature");
            return Error_Mark;
      end case;

      Prefix_Nature := Get_Nature (Prefix);

      case Get_Identifier (Attr) is
         when Std_Names.Name_Across =>
            Res := Create_Iir (Iir_Kind_Across_Attribute);
            Attr_Type := Get_Across_Type (Prefix_Nature);
         when Std_Names.Name_Through =>
            Res := Create_Iir (Iir_Kind_Through_Attribute);
            Attr_Type := Get_Across_Type (Prefix_Nature);
         when others =>
            raise Internal_Error;
      end case;
      pragma Assert (Attr_Type /= Null_Iir);

      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      Set_Type (Res, Attr_Type);

      Set_Base_Name (Res, Get_Base_Name (Prefix_Name));
      Set_Name_Staticness (Res, Get_Name_Staticness (Prefix_Name));
      Set_Type_Staticness (Res, Get_Type_Staticness (Attr_Type));

      return Res;
   end Sem_Nature_Type_Attribute;

   --  For 'Reference
   function Sem_Nature_Reference_Attribute (Attr : Iir_Attribute_Name)
                                           return Iir
   is
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix : Iir;
      Res : Iir;
   begin
      Prefix := Get_Named_Entity (Prefix_Name);

      --  AMS-LRM17 16.2.6 Predefined analog attributes
      --  Prefix: Any nature of subnature N.
      case Get_Kind (Prefix) is
         when Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration =>
            null;
         when others =>
            Error_Msg_Sem (+Attr, "prefix must denote a nature");
            return Error_Mark;
      end case;

      Res := Create_Iir (Iir_Kind_Nature_Reference_Attribute);
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      Set_Nature (Res, Get_Nature (Prefix));

      Set_Base_Name (Res, Get_Base_Name (Prefix_Name));
      Set_Name_Staticness (Res, Get_Name_Staticness (Prefix_Name));

      return Res;
   end Sem_Nature_Reference_Attribute;

   function Sem_Quantity_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      use Std_Names;
      Name_Prefix : constant Iir := Get_Prefix (Attr);
      Prefix: Iir;
      Res : Iir;
      Res_Type : Iir;
   begin
      Prefix := Get_Named_Entity (Name_Prefix);
      Prefix := Finish_Sem_Name_1 (Name_Prefix, Prefix);
      if not Is_Quantity_Name (Prefix) then
         Error_Msg_Sem
           (+Attr, "prefix of %i attribute must denote a quantity", +Attr);
      end if;

      Res_Type := Get_Type (Prefix);
      case Get_Identifier (Attr) is
         when Name_Above =>
            Res := Create_Iir (Iir_Kind_Above_Attribute);
            Res_Type := Boolean_Type_Definition;
         when Name_Dot =>
            Res := Create_Iir (Iir_Kind_Dot_Attribute);
         when Name_Integ =>
            Res := Create_Iir (Iir_Kind_Integ_Attribute);
         when Name_Zoh =>
            Res := Create_Iir (Iir_Kind_Zoh_Attribute);
         when Name_Ltf =>
            Res := Create_Iir (Iir_Kind_Ltf_Attribute);
         when Name_Ztf =>
            Res := Create_Iir (Iir_Kind_Ztf_Attribute);
         when Name_Delayed =>
            Res := Create_Iir (Iir_Kind_Quantity_Delayed_Attribute);
         when others =>
            --  Not yet implemented attribute, or really an internal error.
            raise Internal_Error;
      end case;

      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      Set_Type (Res, Res_Type);

      --  AMS-LRM17 16.2.6 Predefined analog an mixed-signal attributes
      --  Prefix: Any quantity denoted by the static name Q.
      if Get_Name_Staticness (Prefix) < Globally then
         Error_Msg_Sem
           (+Res, "prefix of %i attribute must be a static name", +Res);
      end if;

      --  According to LRM 7.4, signal attributes are not static expressions
      --  since the prefix (a signal) is not a static expression.
      Set_Expr_Staticness (Res, None);

      --  AMS-LRM17 8.1 Names
      --  A name is said to be a static name if and only if one of the
      --  following conditions holds:
      --  [...]
      --  -  The name is an attribute whose prefix is a static quantity name
      --     and whose suffix is one of the predefined attributes 'ABOVE, 'DOT,
      --     'INTEG, 'DELAYED, 'SLEW, 'LTF, 'ZOH, or 'ZTF.
      Set_Name_Staticness (Res, Globally);

      return Res;
   end Sem_Quantity_Attribute;

   function Sem_Slew_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix: Iir;
      Res : Iir;
      Res_Type : Iir;
   begin
      Prefix := Get_Named_Entity (Prefix_Name);
      if Is_Quantity_Name (Prefix) then
         Res := Create_Iir (Iir_Kind_Quantity_Slew_Attribute);
      elsif Is_Signal_Name (Prefix) then
         Res := Create_Iir (Iir_Kind_Signal_Slew_Attribute);
      else
         Error_Msg_Sem
           (+Attr,
            "prefix of 'slew must denote a quantity or a signal", +Attr);
         return Error_Mark;
      end if;

      --  AMS-VHDL17 16.2.6
      --  Prefix: Any signal denoted by the static name S whose scalar
      --    subelements are of a floating-point type.
      --
      --  GHDL: not necessary when the prefix is a quantity.
      Res_Type := Get_Type (Prefix);
      if not Sem_Types.Is_Nature_Type (Res_Type) then
         Error_Msg_Sem (+Attr, "prefix of 'slew must be of nature type");
      end if;

      if Get_Name_Staticness (Prefix) < Globally then
         Error_Msg_Sem (+Attr, "prefix of 'slew must be a static name");
      end if;

      Set_Type (Res, Res_Type);
      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix);
      Set_Expr_Staticness (Res, None);

      --  AMS-LRM17 8.1 Names
      --  A name is said to be a static name if and only if one of the
      --  following conditions holds:
      --  [...]
      --  -  The name is an attribute whose prefix is a static quantity name
      --     and whose suffix is one of the predefined attributes 'ABOVE, 'DOT,
      --     'INTEG, 'DELAYED, 'SLEW, 'LTF, 'ZOH, or 'ZTF.
      Set_Name_Staticness (Res, Globally);

      return Res;
   end Sem_Slew_Attribute;

   function Sem_Signal_Signal_Attribute
     (Attr : Iir_Attribute_Name; Kind : Iir_Kind)
     return Iir
   is
      Res : Iir;
      Prefix : Iir;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));

      --  Create the proper signal attribute node.
      Res := Create_Iir (Kind);
      Location_Copy (Res, Attr);
      if Kind = Iir_Kind_Delayed_Attribute then
         Set_Type (Res, Get_Type (Prefix));
      elsif Kind = Iir_Kind_Transaction_Attribute then
         Set_Type (Res, Bit_Type_Definition);
      else
         Set_Type (Res, Boolean_Type_Definition);
      end if;
      Set_Base_Name (Res, Res);

      if Get_Kind (Prefix) = Iir_Kind_Interface_Signal_Declaration then
         --  LRM93 2.1.1.2 / LRM08 4.2.2.3
         --
         --  It is an error if signal-valued attributes 'STABLE , 'QUIET,
         --  'TRANSACTION, and 'DELAYED of formal signal paramaters of any
         --  mode are read within a subprogram.
         case Get_Kind (Get_Parent (Prefix)) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Error_Msg_Sem
                 (+Attr, "%i is not allowed for a signal parameter", +Attr);
            when others =>
               null;
         end case;
      end if;

      --  Add a declaration for it.
      Sem_Decls.Add_Declaration_For_Implicit_Signal (Res);
      return Res;
   end Sem_Signal_Signal_Attribute;

   function Sem_Signal_Attribute (Attr : Iir_Attribute_Name) return Iir
   is
      use Std_Names;
      Id : constant Name_Id := Get_Identifier (Attr);
      Prefix: Iir;
      Res : Iir;
      Base : Iir;
   begin
      Prefix := Get_Named_Entity (Get_Prefix (Attr));
      Base := Get_Object_Prefix (Prefix);
      if AMS_Vhdl
        and then Id = Name_Delayed
        and then Is_Quantity_Name (Base)
      then
         return Sem_Quantity_Attribute (Attr);
      end if;

      case Get_Kind (Base) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kinds_Signal_Attribute =>
            null;
         when others =>
            Error_Msg_Sem
              (+Attr, "prefix of %i attribute must denote a signal", +Attr);
            return Error_Mark;
      end case;
      case Id is
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
         when Name_Ramp =>
            Res := Create_Iir (Iir_Kind_Ramp_Attribute);
            Set_Type (Res, Get_Type (Prefix));
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
                 (+Attr, "'driving or 'driving_value is available only "
                    & "within a concurrent statement");
            else
               case Get_Kind (Get_Current_Concurrent_Statement) is
                  when Iir_Kinds_Process_Statement
                    | Iir_Kind_Concurrent_Conditional_Signal_Assignment
                    | Iir_Kind_Concurrent_Selected_Signal_Assignment
                    | Iir_Kind_Concurrent_Procedure_Call_Statement =>
                     null;
                  when others =>
                     Error_Msg_Sem
                       (+Attr, "'driving or 'driving_value not available "
                          & "within this concurrent statement");
               end case;
            end if;

            case Get_Kind (Base) is
               when Iir_Kind_Signal_Declaration =>
                  null;
               when Iir_Kind_Interface_Signal_Declaration =>
                  case Get_Mode (Base) is
                     when Iir_Buffer_Mode
                       | Iir_Inout_Mode
                       | Iir_Out_Mode =>
                        null;
                     when others =>
                        Error_Msg_Sem
                          (+Attr, "mode of 'driving or 'driving_value prefix "
                             & "must be out, inout or buffer");
                  end case;
               when others =>
                  Error_Msg_Sem
                    (+Attr, "bad prefix for 'driving or 'driving_value");
            end case;
         when Iir_Kind_Ramp_Attribute =>
            null;
         when others =>
            null;
      end case;

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
      Prefix_Name : constant Iir := Get_Prefix (Attr);
      Prefix: Iir;
      Res : Iir;
      Attr_Type : Iir;
   begin
      Prefix := Get_Named_Entity (Prefix_Name);
      Set_Prefix (Attr, Finish_Sem_Name (Prefix_Name, Prefix));

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
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kinds_Library_Unit
           | Iir_Kind_Non_Object_Alias_Declaration =>
            null;

         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration =>
            if Get_Identifier (Attr) /= Name_Simple_Name
              and then Get_Kind (Get_Parent (Prefix))
              = Iir_Kind_Component_Declaration
            then
               Error_Msg_Sem
                 (+Attr,
                  "local ports or generics of a component cannot be a prefix");
            end if;

         when Iir_Kind_Subtype_Attribute
           | Iir_Kind_Base_Attribute =>
            declare
               Atype : constant Iir := Get_Type (Prefix);
            begin
               if Is_Anonymous_Type_Definition (Atype) then
                  Error_Msg_Sem (+Attr, "%n is not a named entity", +Prefix);
                  return Create_Error_Expr (Attr, String_Type_Definition);
               end if;
               Prefix := Get_Type_Declarator (Atype);
            end;
         when others =>
            Error_Msg_Sem (+Attr, "%n is not a named entity", +Prefix);
            return Create_Error_Expr (Attr, String_Type_Definition);
      end case;

      case Get_Identifier (Attr) is
         when Name_Simple_Name =>
            declare
               Id : constant Name_Id := Name_Table.Get_Identifier
                 (Eval_Simple_Name (Get_Identifier (Prefix)));
            begin
               Res := Create_Iir (Iir_Kind_Simple_Name_Attribute);
               Set_Simple_Name_Identifier (Res, Id);
               Attr_Type := Create_Unidim_Array_By_Length
                 (String_Type_Definition,
                  Int64 (Name_Table.Get_Name_Length (Id)),
                  Attr);
               Set_Simple_Name_Subtype (Res, Attr_Type);
               Set_Expr_Staticness (Res, Locally);
            end;

         when Name_Path_Name =>
            Res := Create_Iir (Iir_Kind_Path_Name_Attribute);
            Set_Expr_Staticness (Res, Globally);
            Attr_Type := String_Type_Definition;

         when Name_Instance_Name =>
            Res := Create_Iir (Iir_Kind_Instance_Name_Attribute);
            Set_Expr_Staticness (Res, Globally);
            Attr_Type := String_Type_Definition;

         when others =>
            raise Internal_Error;
      end case;

      Location_Copy (Res, Attr);
      Set_Prefix (Res, Prefix_Name);
      Set_Type (Res, Attr_Type);
      return Res;
   end Sem_Name_Attribute;

   procedure Sem_Attribute_Name (Attr : Iir_Attribute_Name)
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
        and then Get_Identifier (Attr) in Name_Id_Name_Attributes
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
      Sig := Get_Attribute_Signature (Attr);
      if Sig /= Null_Iir then
         Prefix := Sem_Signature (Prefix, Sig);
         if Prefix = Null_Iir then
            Set_Named_Entity (Attr, Error_Mark);
            return;
         end if;
         Set_Named_Entity (Get_Prefix (Attr), Prefix);
      end if;

      if Get_Kind (Prefix) = Iir_Kind_Overload_List then
         --  FIXME: this should be allowed.
         Error_Msg_Sem (+Attr, "prefix of attribute is overloaded");
         Set_Named_Entity (Attr, Error_Mark);
         return;
      end if;

      --  Set_Prefix (Attr, Finish_Sem_Name (Get_Prefix (Attr), Prefix));

      case Get_Identifier (Attr) is
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
            Res := Sem_Array_Attribute_Name (Attr);

         when Name_Ascending =>
            if Flags.Vhdl_Std > Vhdl_87 then
               Res := Sem_Array_Attribute_Name (Attr);
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

         when Name_Subtype =>
            if Flags.Vhdl_Std >= Vhdl_08 then
               Res := Sem_Subtype_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Element =>
            if Flags.Vhdl_Std >= Vhdl_08 then
               Res := Sem_Element_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Across
           | Name_Through =>
            if Flags.AMS_Vhdl then
               Res := Sem_Nature_Type_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Reference =>
            if Flags.AMS_Vhdl then
               Res := Sem_Nature_Reference_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Above
           | Name_Dot
           | Name_Integ
           | Name_Zoh
           | Name_Ltf
           | Name_Ztf =>
            if Flags.AMS_Vhdl then
               Res := Sem_Quantity_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Ramp =>
            if Flags.AMS_Vhdl then
               Res := Sem_Signal_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when Name_Slew =>
            if Flags.AMS_Vhdl then
               Res := Sem_Slew_Attribute (Attr);
            else
               Res := Sem_User_Attribute (Attr);
            end if;

         when others =>
            Res := Sem_User_Attribute (Attr);
      end case;

      if Res = Null_Iir then
         Error_Kind ("sem_attribute_name", Attr);
      end if;
      Set_Named_Entity (Attr, Res);
   end Sem_Attribute_Name;

   --  LRM93 6
   procedure Sem_Name (Name : Iir; Keep_Alias : Boolean := False) is
   begin
      --  Exit now if NAME was already analyzed.
      if Get_Named_Entity (Name) /= Null_Iir then
         return;
      end if;

      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Operator_Symbol =>
            --  String_Literal may be a operator_symbol.
            Sem_Simple_Name (Name, Keep_Alias, Soft => False);
         when Iir_Kind_Selected_Name =>
            Sem_Selected_Name (Name, Keep_Alias);
         when Iir_Kind_Parenthesis_Name =>
            Sem_Parenthesis_Name (Name);
         when Iir_Kind_Selected_By_All_Name =>
            Sem_Selected_By_All_Name (Name);
         when Iir_Kind_Attribute_Name =>
            Sem_Attribute_Name (Name);
         when Iir_Kinds_External_Name =>
            Sem_External_Name (Name);
         when others =>
            Error_Kind ("sem_name", Name);
      end case;
   end Sem_Name;

   procedure Sem_Name_Soft (Name : Iir)
   is
   begin
      --  Exit now if NAME was already analyzed.
      if Get_Named_Entity (Name) /= Null_Iir then
         return;
      end if;

      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Operator_Symbol =>
            --  String_Literal may be a operator_symbol.
            Sem_Simple_Name (Name, False, Soft => True);
         when Iir_Kind_Selected_Name =>
            Sem_Selected_Name (Name, Keep_Alias => False, Soft => True);
         when Iir_Kind_Parenthesis_Name =>
            --  FIXME: SOFT!!
            Sem_Parenthesis_Name (Name);
         when others =>
            Error_Kind ("sem_name_soft", Name);
      end case;
   end Sem_Name_Soft;

   procedure Sem_Name_Clean_1 (Name : Iir)
   is
      Named_Entity : Iir;
      Atype : Iir;
   begin
      if Name = Null_Iir then
         return;
      end if;

      --  Clear and free overload lists of Named_entity and type.
      Named_Entity := Get_Named_Entity (Name);
      Set_Named_Entity (Name, Null_Iir);
      if Named_Entity /= Null_Iir
        and then Is_Overload_List (Named_Entity)
      then
         Free_Iir (Named_Entity);
      end if;

      Atype := Get_Type (Name);
      Set_Type (Name, Null_Iir);
      if Atype /= Null_Iir
        and then Is_Overload_List (Atype)
      then
         Free_Iir (Atype);
      end if;
   end Sem_Name_Clean_1;

   procedure Sem_Name_Clean (Name : Iir) is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Operator_Symbol =>
            Sem_Name_Clean_1 (Name);
         when Iir_Kind_Parenthesis_Name
           | Iir_Kind_Selected_Name =>
            Sem_Name_Clean_1 (Get_Prefix (Name));
            Sem_Name_Clean_1 (Name);
         when others =>
            Error_Kind ("sem_name_clean", Name);
      end case;
   end Sem_Name_Clean;

   --  Remove procedure specification from LIST.
   function Remove_Procedures_From_List (Expr : Iir) return Iir
   is
      El : Iir;
      List : Iir_List;
      It : List_Iterator;
      New_List : Iir_List;
   begin
      if not Is_Overload_List (Expr) then
         return Expr;
      end if;
      List := Get_Overload_List (Expr);
      New_List := Create_Iir_List;
      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         case Get_Kind (El) is
            when Iir_Kind_Procedure_Declaration =>
               null;
            when Iir_Kind_Function_Declaration =>
               if Maybe_Function_Call (El) then
                  Append_Element (New_List, El);
               end if;
            when others =>
               Append_Element (New_List, El);
         end case;
         Next (It);
      end loop;
      case Get_Nbr_Elements (New_List) is
         when 0 =>
            Free_Iir (Expr);
            Destroy_Iir_List (New_List);
            return Null_Iir;
         when 1 =>
            Free_Iir (Expr);
            El := Get_First_Element (New_List);
            Destroy_Iir_List (New_List);
            return El;
         when others =>
            Set_Overload_List (Expr, New_List);
            Destroy_Iir_List (List);
            return Expr;
      end case;
   end Remove_Procedures_From_List;

   --  Return the fully analyzed name of NAME.
   function Name_To_Analyzed_Name (Name : Iir) return Iir is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Attribute_Name
           | Iir_Kind_Parenthesis_Name =>
            return Get_Named_Entity (Name);
         when others =>
            return Name;
      end case;
   end Name_To_Analyzed_Name;

   --  Return True iff the current design unit is package body textio.
   function Is_Current_Design_Unit_Textio_Body return Boolean
   is
      Unit : constant Iir := Sem.Get_Current_Design_Unit;
      Cur_Lib : constant Iir := Get_Library_Unit (Unit);
   begin
      if Get_Kind (Cur_Lib) /= Iir_Kind_Package_Body then
         return False;
      end if;
      if Get_Library (Get_Design_File (Unit)) /= Libraries.Std_Library then
         return False;
      end if;
      return Get_Identifier (Cur_Lib) = Std_Names.Name_Textio;
   end Is_Current_Design_Unit_Textio_Body;

   --  Convert name EXPR to an expression (ie, create function call).
   --  A_TYPE is the expected type of the expression.
   --  Returns an Error node in case of error.
   function Name_To_Expression (Name : Iir; A_Type : Iir) return Iir
   is
      Ret_Type : Iir;
      Res_Type : Iir;
      Expr : Iir;
      Expr_List : Iir_List;
      Expr_It : List_Iterator;
      Res : Iir;
      Res1 : Iir;
      El : Iir;
   begin
      Expr := Get_Named_Entity (Name);
      if Get_Kind (Expr) = Iir_Kind_Error then
         return Expr;
      end if;
      if Check_Is_Expression (Expr, Name) = Null_Iir then
         return Create_Error_Expr (Name, A_Type);
      end if;

      --  Note: EXPR may contain procedure names...
      Expr := Remove_Procedures_From_List (Expr);
      Set_Named_Entity (Name, Expr);
      if Expr = Null_Iir then
         Error_Msg_Sem (+Name, "%n cannot be used as expression", +Name);
         --  Note: this creates a loop.
         Res := Create_Error_Expr (Name, A_Type);
         Set_Named_Entity (Name, Res);
         return Res;
      end if;

      if not Is_Overload_List (Expr) then
         Res := Finish_Sem_Name (Name);
         pragma Assert (Res /= Null_Iir);
         if A_Type /= Null_Iir then
            Res_Type := Get_Type (Res);
            if Res_Type = Null_Iir then
               return Create_Error_Expr (Res, A_Type);
            end if;
            if Are_Basetypes_Compatible (Get_Base_Type (Res_Type), A_Type)
              = Not_Compatible
            then
               Error_Not_Match (Res, A_Type);
               return Create_Error_Expr (Res, A_Type);
            end if;
            --  Fall through.
         end if;
      else
         --  EXPR is an overloaded name.
         Expr_List := Get_Overload_List (Expr);

         if A_Type /= Null_Iir then
            --  Find the name returning A_TYPE.
            Res := Null_Iir;
            Expr_It := List_Iterate (Expr_List);
            while Is_Valid (Expr_It) loop
               El := Get_Element (Expr_It);
               if Are_Basetypes_Compatible (Get_Base_Type (Get_Type (El)),
                                            A_Type)
                 /= Not_Compatible
               then
                  Add_Result (Res, El);
               end if;
               Next (Expr_It);
            end loop;
            if Res = Null_Iir then
               --  Specific error message for a non-visible enumeration
               --  literal.
               if (Get_Kind (Get_Base_Type (A_Type))
                     = Iir_Kind_Enumeration_Type_Definition)
                 and then Kind_In (Name, Iir_Kind_Simple_Name,
                                   Iir_Kind_Character_Literal)
               then
                  Res := Find_Name_In_Flist (Get_Enumeration_Literal_List
                                               (Get_Base_Type (A_Type)),
                                             Get_Identifier (Name));
                  if Res /= Null_Iir then
                     Error_Msg_Sem
                       (+Name, "enumeration literal %i is not visible "
                          & "(add a use clause)", +Name);
                     --  Keep the literal as result.
                  end if;
               end if;
            end if;

            if Res = Null_Iir then
               Error_Not_Match (Name, A_Type);
               return Create_Error_Expr (Name, A_Type);
            elsif Is_Overload_List (Res) then
               Res1 := Extract_Call_Without_Implicit_Conversion (Res);
               if Res1 /= Null_Iir then
                  Free_Iir (Res);
                  Res := Res1;
               else
                  Report_Start_Group;
                  Error_Overload (Name);
                  Disp_Overload_List (Get_Overload_List (Res), Name);
                  Report_End_Group;
                  Free_Iir (Res);
                  return Create_Error_Expr (Name, A_Type);
               end if;
            end if;

            --  Free results
            Sem_Name_Free_Result (Expr, Res);

            Ret_Type := Get_Type (Name);
            if Ret_Type /= Null_Iir then
               pragma Assert (Is_Overload_List (Ret_Type));
               Free_Overload_List (Ret_Type);
            end if;
            --  Fall through.
         else
            --  Create a list of type.
            Ret_Type := Create_List_Of_Types (Expr_List);
            if Ret_Type = Null_Iir or else not Is_Overload_List (Ret_Type) then
               Res1 := Extract_Call_Without_Implicit_Conversion (Expr);
               if Res1 /= Null_Iir then
                  --  Found it.
                  Res := Res1;
                  --  Fall through
               else
                  --  There is either no types or one type for
                  --  several meanings.
                  Report_Start_Group;
                  Error_Overload (Name);
                  Disp_Overload_List (Expr_List, Name);
                  Report_End_Group;
                  --Free_Iir (Ret_Type);
                  return Create_Error_Expr (Name, A_Type);
               end if;
            else
               Set_Type (Name, Ret_Type);
               return Name;
            end if;
         end if;

         Set_Named_Entity (Name, Res);
         Res := Finish_Sem_Name (Name);
      end if;

      --  NAME has only one meaning, which is RES.
      case Get_Kind (Res) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Selected_Name =>
            Expr := Get_Named_Entity (Res);
            if Get_Kind (Expr) = Iir_Kind_Function_Declaration then
               return Function_Declaration_To_Call (Res);
            else
               Set_Type (Res, Get_Type (Expr));
               Set_Expr_Staticness (Res, Get_Expr_Staticness (Expr));
               --Set_Name_Staticness (Name, Get_Name_Staticness (Expr));
               --Set_Base_Name (Name, Get_Base_Name (Expr));

               --  For time physical literal (without a literal), check for
               --  resolution and mark the literal used.
               --  Don't check for textio body, as it uses all units and
               --  user code shouldn't be affected by it.
               if Get_Type (Expr) = Time_Type_Definition
                 and then not Is_Current_Design_Unit_Textio_Body
               then
                  pragma Assert (Get_Kind (Expr) = Iir_Kind_Unit_Declaration);
                  Set_Use_Flag (Expr, True);

                  --  See Sem_Physical_Literal.
                  if Get_Value (Get_Physical_Literal (Expr)) = 0 then
                     Error_Msg_Sem
                       (+Res, "physical unit %i is below the "
                          & "time resolution", +Expr);
                  end if;
               end if;

               if Get_Kind (Expr) = Iir_Kind_Enumeration_Literal then
                  Set_Use_Flag (Expr, True);
               end if;

               return Res;
            end if;
         when Iir_Kind_Function_Call
           | Iir_Kind_Selected_Element
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Attribute_Name =>
            return Eval_Expr_If_Static (Res);
         when Iir_Kind_Dereference =>
            --  Never static.
            return Res;
         when Iir_Kinds_Array_Attribute =>
            --  FIXME: exclude range and reverse_range.
            return Eval_Expr_If_Static (Res);
         when Iir_Kinds_Signal_Attribute
           | Iir_Kinds_Signal_Value_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_Dot_Attribute
           | Iir_Kind_Integ_Attribute
           | Iir_Kind_Ramp_Attribute
           | Iir_Kind_Zoh_Attribute
           | Iir_Kind_Ltf_Attribute
           | Iir_Kind_Ztf_Attribute
           | Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute =>
            --  Never static
            return Res;
         when Iir_Kinds_Type_Attribute
           | Iir_Kinds_Scalar_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute =>
            return Eval_Expr_If_Static (Res);
         when Iir_Kind_Parenthesis_Name
           | Iir_Kind_Selected_By_All_Name =>
            raise Internal_Error;
         when others =>
            Error_Kind ("name_to_expression", Res);
      end case;
   end Name_To_Expression;

   function Name_To_Range (Name : Iir) return Iir
   is
      Expr : Iir;
   begin
      Expr := Get_Named_Entity (Name);
      if Get_Kind (Expr) = Iir_Kind_Error then
         return Error_Mark;
      end if;

      case Get_Kind (Expr) is
         when Iir_Kind_Subtype_Declaration
           | Iir_Kind_Type_Declaration =>
            Expr := Sem_Type_Mark (Name);
            Set_Expr_Staticness
              (Expr, Get_Type_Staticness (Get_Type (Expr)));
            return Expr;
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            if Get_Parameter (Expr) = Null_Iir then
               Finish_Sem_Array_Attribute (Name, Expr, Null_Iir);
            end if;
            if Get_Kind (Name) = Iir_Kind_Attribute_Name then
               Free_Iir (Name);
            else
               Free_Iir (Get_Prefix (Name));
               Free_Parenthesis_Name (Name, Expr);
            end if;
            return Expr;
         when others =>
            Error_Msg_Sem (+Name, "%n doesn't denote a range", +Name);
            return Error_Mark;
      end case;
   end Name_To_Range;

   function Name_To_Type_Definition (Name : Iir) return Iir
   is
      Atype : Iir;
   begin
      case Get_Kind (Name) is
         when Iir_Kinds_Denoting_Name =>
            --  Common correct case.
            Atype := Get_Named_Entity (Name);
            case Get_Kind (Atype) is
               when Iir_Kind_Type_Declaration =>
                  return Get_Type_Definition (Atype);
               when Iir_Kind_Subtype_Declaration
                 | Iir_Kind_Interface_Type_Declaration =>
                  return Get_Type (Atype);
               when Iir_Kind_Error =>
                  return Atype;
               when others =>
                  Report_Start_Group;
                  Error_Msg_Sem
                    (+Name, "a type mark must denote a type or a subtype");
                  Error_Msg_Sem
                    (+Name, "(type mark denotes %n)", +Atype);
                  Report_End_Group;
                  return Create_Error_Type (Atype);
            end case;
         when Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Across_Attribute
           | Iir_Kind_Through_Attribute =>
            return Get_Type (Name);
         when Iir_Kinds_Expression_Attribute =>
            Error_Msg_Sem (+Name, "%n is not a valid type mark", +Name);
            return Create_Error_Type (Name);
         when others =>
            if not Is_Error (Name) then
               Error_Msg_Sem
                 (+Name, "a type mark must be a simple or expanded name");
            end if;
            return Create_Error_Type (Name);
      end case;
   end Name_To_Type_Definition;

   function Sem_Denoting_Name (Name: Iir) return Iir
   is
      Res: Iir;
   begin
      pragma Assert (Get_Kind (Name) in Iir_Kinds_Denoting_Name);

      Sem_Name (Name);
      Res := Get_Named_Entity (Name);

      case Get_Kind (Res) is
         when Iir_Kind_Error =>
            --  A message must have been displayed.
            return Name;
         when Iir_Kind_Overload_List =>
            Error_Overload (Name);
            Set_Named_Entity (Name, Create_Error_Name (Name));
            return Name;
         when Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kinds_Object_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Foreign_Module
           | Iir_Kinds_Subprogram_Declaration
           | Iir_Kind_Component_Declaration =>
            Res := Finish_Sem_Name (Name, Res);
            pragma Assert (Get_Kind (Res) in Iir_Kinds_Denoting_Name);
            return Res;
         when Iir_Kind_Selected_Element =>
            --  An error (to be diagnosticed by the caller).
            return Name;
         when others =>
            Error_Kind ("sem_denoting_name", Res);
      end case;
   end Sem_Denoting_Name;

   procedure Sem_External_Name (Name : Iir)
   is
      Atype : Iir;
   begin
      pragma Assert (Get_Type (Name) = Null_Iir);

      Atype := Get_Subtype_Indication (Name);

      Atype := Sem_Types.Sem_Subtype_Indication (Atype);
      Set_Subtype_Indication (Name, Atype);
      Atype := Get_Type_Of_Subtype_Indication (Atype);
      if Atype = Null_Iir then
         Atype := Create_Error_Type (Null_Iir);
      end if;

      Set_Type (Name, Atype);

      --  LRM08 8.1 Names
      --  A name is said to be a static name if and only if one of the
      --  following condition holds:
      --  - The name is an external name.
      Set_Name_Staticness (Name, Globally);

      Set_Expr_Staticness (Name, None);

      --  Consider the node as analyzed.
      Set_Named_Entity (Name, Name);
   end Sem_External_Name;

   function Sem_Terminal_Name (Name : Iir) return Iir
   is
      Res : Iir;
   begin
      Sem_Name (Name);
      Res := Get_Named_Entity (Name);

      case Get_Kind (Res) is
         when Iir_Kind_Error =>
            --  A message must have been displayed.
            return Name;
         when Iir_Kind_Overload_List =>
            Error_Overload (Res);
            Set_Named_Entity (Name, Create_Error_Name (Name));
            return Name;
         when Iir_Kind_Terminal_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Nature_Reference_Attribute =>
            Res := Finish_Sem_Name (Name, Res);
            return Res;
         when others =>
            Error_Class_Match (Name, "terminal");
            Set_Named_Entity (Name, Create_Error_Name (Name));
            return Name;
      end case;
   end Sem_Terminal_Name;

   procedure Error_Class_Match (Name : Iir; Class_Name : String)
   is
      Ent : constant Iir := Get_Named_Entity (Name);
   begin
      if Is_Error (Ent) then
         Error_Msg_Sem (+Name, Class_Name & " name expected");
      else
         Error_Msg_Sem (+Name, Class_Name & " name expected, found %n",
                        +Get_Named_Entity (Name));
      end if;
   end Error_Class_Match;
end Vhdl.Sem_Names;
