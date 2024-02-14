--  Expressions synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Types; use Types;
with Name_Table;
with Str_Table;
with Errorout;

with Netlists;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Scanner;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Sem_Expr;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Heap; use Elab.Vhdl_Heap;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;
with Elab.Vhdl_Insts;

with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Vhdl_Eval; use Synth.Vhdl_Eval;
with Synth.Errors; use Synth.Errors;

with Grt.Types;
with Grt.Vhdl_Types;
with Grt.Strings;
with Grt.To_Strings;
with Grt.Rstrings;

package body Elab.Vhdl_Expr is
   function Synth_Bounds_From_Length (Atype : Node; Len : Int32)
                                     return Bound_Type
   is
      Rng : constant Node := Get_Range_Constraint (Atype);
      Limit : Int32;
   begin
      Limit := Int32 (Eval_Pos (Get_Left_Limit (Rng)));
      case Get_Direction (Rng) is
         when Dir_To =>
            return (Dir => Dir_To,
                    Left => Limit,
                    Right => Limit + Len - 1,
                    Len => Uns32 (Len));
         when Dir_Downto =>
            return (Dir => Dir_Downto,
                    Left => Limit,
                    Right => Limit - Len + 1,
                    Len => Uns32 (Len));
      end case;
   end Synth_Bounds_From_Length;

   function Exec_Simple_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node) return Valtyp
   is
      Els : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
      Last : constant Natural := Flist_Last (Els);
      Aggr_Type : constant Node := Get_Type (Aggr);
      Res_Typ : Type_Acc;
      Val : Valtyp;
      Res : Valtyp;
   begin
      --  Allocate the result.
      Res_Typ := Synth_Subtype_Indication (Syn_Inst, Aggr_Type);
      pragma Assert (Get_Nbr_Dimensions (Aggr_Type) = 1);
      pragma Assert (Res_Typ.Abound.Len = Uns32 (Last + 1));

      Res := Create_Value_Memory (Res_Typ, Current_Pool);

      for I in Flist_First .. Last loop
         --  Elements are supposed to be static, so no need for enable.
         Val := Synth_Expression_With_Type
           (Syn_Inst, Get_Nth_Element (Els, I), Res_Typ.Arr_El);
         pragma Assert (Is_Static (Val.Val));
         Write_Value (Res.Val.Mem + Size_Type (I) * Res_Typ.Arr_El.Sz, Val);
      end loop;

      return Res;
   end Exec_Simple_Aggregate;

   function Exec_Subtype_Conversion (Vt : Valtyp;
                                     Dtype : Type_Acc;
                                     Bounds : Boolean;
                                     Loc : Node) return Valtyp is
   begin
      return Synth_Subtype_Conversion (null, Vt, Dtype, Bounds, Loc);
   end Exec_Subtype_Conversion;

   function Find_Name_In_Declaration_Chain (Parent : Node; Id : Name_Id)
                                           return Node
   is
      Item : Node;
   begin
      Item := Get_Declaration_Chain (Parent);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kinds_Specification
              | Iir_Kind_Attribute_Implicit_Declaration
              | Iir_Kinds_Subprogram_Body =>
               null;
            when others =>
               if Get_Identifier (Item) = Id then
                  return Item;
               end if;
         end case;
         Item := Get_Chain (Item);
      end loop;
      return Null_Node;
   end Find_Name_In_Declaration_Chain;

   function Synth_Pathname_Object (Loc_Inst : Synth_Instance_Acc;
                                   Name : Node;
                                   Cur_Inst : Synth_Instance_Acc;
                                   Path : Node) return Valtyp
   is
      use Errorout;
      Id : constant Name_Id := Get_Identifier (Path);
      Scope : constant Node := Get_Source_Scope (Cur_Inst);
      Obj : Node;
      Res : Valtyp;
      Name_Typ : Type_Acc;
   begin
      --  Object simple name.
      case Get_Kind (Scope) is
         when Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Block_Statement
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration =>
            Obj := Find_Name_In_Declaration_Chain (Scope, Id);
         when Iir_Kind_Architecture_Body =>
            Obj := Find_Name_In_Declaration_Chain (Scope, Id);
            if Obj = Null_Node then
               --  Try ports / generics
               declare
                  Ent : constant Node := Get_Entity (Scope);
               begin
                  Obj := Find_Name_In_Chain (Get_Port_Chain (Ent), Id);
                  if Obj = Null_Node then
                     Obj := Find_Name_In_Chain (Get_Generic_Chain (Ent), Id);
                  end if;
               end;
            end if;
         when others =>
            Error_Kind ("synth_pathname_object(1)", Scope);
      end case;

      --  LRM08 8.7 External names
      --  It is an error when evaluating an external name if the identified
      --  declarative region does not contain a declaration of an object
      --  whose simple name is the object simple name of the external
      --  pathname.
      if Obj = Null_Node then
         Error_Msg_Synth
           (Loc_Inst, Path, "cannot find object %i in %i", (+Id, +Scope));
         return No_Valtyp;
      end if;

      --  LRM08 8.7 External names
      --  It is also an error when evaluating an external name if the object
      --  denoted by an external constant name is not a constant, or if the
      --  object denoted by an external signal name is not a signal, or if
      --  the object denoted by an external variable name is not a variable.
      case Get_Kind (Obj) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            case Iir_Kinds_External_Name (Get_Kind (Name)) is
               when Iir_Kind_External_Signal_Name =>
                  Res := Get_Value (Cur_Inst, Obj);
               when Iir_Kind_External_Constant_Name
                 | Iir_Kind_External_Variable_Name =>
                  Error_Msg_Synth
                    (Loc_Inst, Path,
                     "denoted object name %i is a not a signal", +Obj);
                  return No_Valtyp;
            end case;
         when Iir_Kind_Variable_Declaration =>
            case Iir_Kinds_External_Name (Get_Kind (Name)) is
               when Iir_Kind_External_Variable_Name =>
                  Res := Get_Value (Cur_Inst, Obj);
               when Iir_Kind_External_Constant_Name
                 | Iir_Kind_External_Signal_Name =>
                  Error_Msg_Synth
                    (Loc_Inst, Path,
                     "denoted object name %i is a not a variable", +Obj);
                  return No_Valtyp;
            end case;
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Constant_Declaration =>
            case Iir_Kinds_External_Name (Get_Kind (Name)) is
               when Iir_Kind_External_Constant_Name =>
                  Res := Get_Value (Cur_Inst, Obj);
               when Iir_Kind_External_Variable_Name
                 | Iir_Kind_External_Signal_Name =>
                  Error_Msg_Synth
                    (Loc_Inst, Path,
                     "denoted object name %i is a not a constant", +Obj);
                  return No_Valtyp;
            end case;
         when others =>
            Error_Kind ("synth_pathname_object(2)", Obj);
      end case;

      --  LRM08 8.7 External names
      --  Moreover, it is an error if the base type of the object denoted by
      --  an external name is not the same as the base type mark in the
      --  subtype indication of the external name.
      declare
         Obj_Type : constant Node := Get_Type (Obj);
         Name_Type : constant Node := Get_Type (Name);
      begin
         if Get_Base_Type (Obj_Type) /= Get_Base_Type (Name_Type) then
            Error_Msg_Synth
              (Loc_Inst, Path, "external name and object have different type");
            return No_Valtyp;
         end if;

         Name_Typ := Synth_Subtype_Indication
           (Loc_Inst, Get_Subtype_Indication (Name));
      end;

      --  LRM08 8.7 External names
      --  If the subtype indication denotes a composite subtype, then the
      --  object denoted by the external name is viewed as if it were of the
      --  subtype specified by the subtype indication.  For each index range,
      --  if any, in the subtype, if the subtype defines the index range, the
      --  object is viewed with that index range; otherwise, the object
      --  is viewed with the index range of the object.  The view specified
      --  by the subtype shall include a matching element (see 9.2.3) for
      --  each element of the object denoted by the external name.
      --
      --  If the subtype indication denotes a scalar subtype, then the object
      --  denoted by the external name is viewed as if it were of the subtype
      --  specified by the subtype indication; moreover, it is a error when
      --  evaluating the external name if this subtype does not have the same
      --  bounds and direction as the subtype of the object denoted by the
      --  external name.

      case Name_Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete =>
            if Name_Typ.Drange /= Res.Typ.Drange then
               Error_Msg_Synth
                 (Loc_Inst, Name, "bounds mismatch between name and object");
            end if;
         when Type_Float =>
            if Name_Typ.Frange /= Res.Typ.Frange then
               Error_Msg_Synth
                 (Loc_Inst, Name, "bounds mismatch between name and object");
            end if;
         when Type_Vector
           | Type_Unbounded_Vector
           | Type_Array
           | Type_Array_Unbounded
           | Type_Unbounded_Array
           | Type_Unbounded_Record
           | Type_Record =>
            Res := Synth_Subtype_Conversion
              (Loc_Inst, Res, Name_Typ, True, Name);
         when Type_Protected
           | Type_File
           | Type_Access =>
            null;
         when Type_Slice =>
            raise Internal_Error;
      end case;

      return Res;
   end Synth_Pathname_Object;

   function Synth_Pathname (Loc_Inst : Synth_Instance_Acc;
                            Name : Node;
                            Cur_Inst : Synth_Instance_Acc;
                            Path : Node) return Valtyp
   is
      use Errorout;
      Suffix : constant Node := Get_Pathname_Suffix (Path);
      Expr : constant Node := Get_Pathname_Expression (Path);
      Id : Name_Id;
      Scope : Node;
      Res : Node;
      Sub_Inst : Synth_Instance_Acc;
   begin
      if Suffix = Null_Node then
         --  Object simple name.
         return Synth_Pathname_Object (Loc_Inst, Name, Cur_Inst, Path);
      end if;

      Id := Get_Identifier (Path);
      Scope := Get_Source_Scope (Cur_Inst);

      --  Find name in concurrent statements.
      case Get_Kind (Scope) is
         when Iir_Kind_Architecture_Body
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement_Body =>
            Res := Find_Name_In_Chain
              (Get_Concurrent_Statement_Chain (Scope), Id);
         when Iir_Kind_Package_Declaration =>
            Res := Find_Name_In_Declaration_Chain (Scope, Id);
         when others =>
            Error_Kind ("synth_pathname(scope)", Scope);
      end case;
      if Res = Null_Node then
         Error_Msg_Synth
           (Loc_Inst, Path,
            "cannot find path element %i in %i", (+Id, +Scope));
         return No_Valtyp;
      end if;

      --  Check that expression is valid only for for-generate statement
      --  TODO: is it an error according to LRM08 ?
      if Expr /= Null_Node
        and then Get_Kind (Res) /= Iir_Kind_For_Generate_Statement
      then
         Error_Msg_Synth
           (Loc_Inst, Path,
            "index expression valid only for generate statements");
         return No_Valtyp;
      end if;

      --  Check if pathname element is elaborated.
      case Get_Kind (Res) is
         when Iir_Kind_Component_Instantiation_Statement
            | Iir_Kind_If_Generate_Statement
            | Iir_Kind_For_Generate_Statement
            | Iir_Kind_Package_Declaration
            | Iir_Kind_Block_Statement =>
            if not Is_Elaborated (Cur_Inst, Res) then
               Error_Msg_Synth
                 (Loc_Inst, Path, "%n is not yet elaborated", +Res);
               return No_Valtyp;
            end if;
         when Iir_Kind_Process_Statement =>
            --  And other concurrent statements...
            --  and other declarations
            null;
         when others =>
            Error_Kind ("synth_pathname(2a)", Res);
      end case;

      case Get_Kind (Res) is
         when Iir_Kind_Component_Instantiation_Statement =>
            Sub_Inst := Get_Sub_Instance (Cur_Inst, Res);
            if not Is_Entity_Instantiation (Res) then
               Sub_Inst := Get_Component_Instance (Sub_Inst);
               if Cur_Inst = null then
                  Error_Msg_Synth
                    (Loc_Inst, Path, "component for %i is not bound", +Res);
                  return No_Valtyp;
               end if;
            end if;
         when Iir_Kind_If_Generate_Statement =>
            Sub_Inst := Get_Sub_Instance (Cur_Inst, Res);
            if Sub_Inst = null then
               Error_Msg_Synth
                 (Loc_Inst, Path, "condition of generate statement is false");
               return No_Valtyp;
            end if;
         when Iir_Kind_For_Generate_Statement =>
            declare
               use Vhdl.Sem_Expr;
               Param : constant Node := Get_Parameter_Specification (Res);
               Param_Rng : Type_Acc;
               Idx : Valtyp;
               V : Int64;
               V_Off : Natural;
               Gen_Inst : Synth_Instance_Acc;
            begin
               --  LRM08 8.7 External names
               --  If the generate statement is a for generate statement, the
               --  pathname element shall include a static expression, [...]
               if Expr = Null_Node then
                  Error_Msg_Synth
                    (Loc_Inst, Path,
                     "expression required for a for-generate statement");
                  return No_Valtyp;
               end if;

               --  [...] the type of the expressoin shall be the same as the
               --  type of the generate parameter, [...]
               if Are_Nodes_Compatible (Expr, Param) = Not_Compatible then
                  Error_Msg_Synth
                    (Loc_Inst, Path,
                     "expression and generate parameter are not compatible");
                  return No_Valtyp;
               end if;

               --  [...] and the value of the expression shall belong to the
               --  discrete range specified for the generate parameter.
               Param_Rng := Get_Subtype_Object (Cur_Inst, Get_Type (Param));
               Idx := Synth_Expression_With_Type (Cur_Inst, Expr, Param_Rng);
               if Idx = No_Valtyp then
                  return No_Valtyp;
               end if;
               V := Read_Discrete (Idx);
               if not In_Range (Param_Rng.Drange, V) then
                  Error_Msg_Synth
                    (Loc_Inst, Path,
                     "expression not in the range of the generate parameter");
                  return No_Valtyp;
               end if;
               case Param_Rng.Drange.Dir is
                  when Dir_To =>
                     V_Off := Natural (V - Param_Rng.Drange.Left);
                  when Dir_Downto =>
                     V_Off := Natural (Param_Rng.Drange.Left - V);
               end case;
               Gen_Inst := Get_Sub_Instance (Cur_Inst, Res);
               Sub_Inst := Get_Generate_Sub_Instance (Gen_Inst, V_Off + 1);
            end;
         when Iir_Kind_Package_Declaration =>
            if Is_Uninstantiated_Package (Res) then
               Error_Msg_Synth
                 (Loc_Inst, Path,
                  "pathname element %i designates an uninstantiated package",
                  +Id);
               return No_Valtyp;
            end if;
            Sub_Inst := Get_Sub_Instance (Cur_Inst, Res);
         when Iir_Kind_Block_Statement =>
            Sub_Inst := Get_Sub_Instance (Cur_Inst, Res);
         when Iir_Kind_Process_Statement =>
            --  And other concurrent statements...
            --  and other declarations
            Error_Msg_Synth
              (Loc_Inst, Path,
               "pathname element %i does not denote a concurrent region",
               +Id);
            return No_Valtyp;
         when others =>
            Error_Kind ("synth_pathname(2)", Res);
      end case;
      return Synth_Pathname (Loc_Inst, Name, Sub_Inst, Suffix);
   end Synth_Pathname;

   function Exec_Absolute_Pathname
     (Syn_Inst : Synth_Instance_Acc; Name : Node; Path : Node) return Valtyp
   is
      Path_Inst : constant Synth_Instance_Acc := Elab.Vhdl_Insts.Top_Instance;
      Top_Arch : constant Node := Get_Source_Scope (Path_Inst);
      Top_Ent : constant Node := Get_Entity (Top_Arch);
      Suffix : constant Node := Get_Pathname_Suffix (Path);
   begin
      if Get_Identifier (Top_Ent) /= Get_Identifier (Suffix) then
         Error_Msg_Synth
           (Syn_Inst, Path,
            "expect %i instead of %i for root of absolute pathname",
            (+Top_Ent, +Suffix));
         return No_Valtyp;
      end if;

      return Synth_Pathname
        (Syn_Inst, Name, Path_Inst, Get_Pathname_Suffix (Suffix));
   end Exec_Absolute_Pathname;

   --  Find the corresponding concurrent region from SCOPE.
   function Exec_Pathname_Concurrent_Region (Scope : Synth_Instance_Acc)
                                            return Synth_Instance_Acc
   is
      Cur_Inst : Synth_Instance_Acc;
      Cur_Src : Node;
   begin
      --  LRM08 8.7 External names
      --  For a relative pathname, the innermost concurrent region is
      --  initially identifier, where a concurrent region is defined
      --  recursively to be
      --  - A block declarative region (including an external block and
      --    any block equivalent to a generate statement), or
      --  - A package declarative region (including a generic-mapped package
      --    equivalent to a package instantiation) declared immediately within
      --    a concurrent region.
      Cur_Inst := Scope;
      loop
         Cur_Src := Get_Source_Scope (Cur_Inst);
         if Cur_Src = Null_Node then
            --  Top level.
            return null;
         end if;
         case Get_Kind (Cur_Src) is
            when Iir_Kind_Architecture_Body
               | Iir_Kind_Entity_Declaration
               | Iir_Kind_Block_Statement
               | Iir_Kind_Generate_Statement_Body =>
               return Cur_Inst;
            when Iir_Kind_Component_Declaration =>
               --  The implicit block for the component.
               null;
            when Iir_Kind_Process_Statement
              | Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body =>
               null;
            when Iir_Kind_Package_Declaration =>
               --  Check if it is within a concurrent region.
               declare
                  Sub_Inst : constant Synth_Instance_Acc :=
                    Get_Instance_Parent (Cur_Inst);
               begin
                  if Exec_Pathname_Concurrent_Region (Sub_Inst) /= null then
                     return Cur_Inst;
                  else
                     return null;
                  end if;
               end;
            when others =>
               Error_Kind ("exec_pathname_concurrent_region", Cur_Src);
         end case;
         Cur_Inst := Get_Instance_Parent (Cur_Inst);
         pragma Assert (Cur_Inst /= null);
      end loop;
   end Exec_Pathname_Concurrent_Region;

   function Exec_Relative_Pathname
     (Syn_Inst : Synth_Instance_Acc; Name : Node; Path : Node) return Valtyp
   is
      Cur_Inst : Synth_Instance_Acc;
      Cur_Path : Node;
   begin
      Cur_Inst := Exec_Pathname_Concurrent_Region (Syn_Inst);
      if Cur_Inst = null then
         Error_Msg_Synth
           (Syn_Inst, Path, "external name is not within a concurrent region");
         return No_Valtyp;
      end if;

      --  LRM08 8.7 External names
      --  Then, for each occurrence of a circumflex accent followed by a dot,
      --  the innermost concurrent region, other than a block corresponding
      --  to a component instantiation statement, containing the previously
      --  identified declarative region replaces the previously identified
      --  declaration region as the identified declarative region.
      --  It is an error when evaluating the external name if, at any stage,
      --  there is no such containing declarative region, of if the containing
      --  declarative region is the declarative region of an uninstantiated
      --  package.

      Cur_Path := Path;
      while Get_Kind (Cur_Path) = Iir_Kind_Relative_Pathname loop
         Cur_Inst := Get_Instance_Parent (Cur_Inst);
         Cur_Inst := Exec_Pathname_Concurrent_Region (Cur_Inst);

         if Cur_Inst = null then
            Error_Msg_Synth
              (Syn_Inst, Path, "path already at top of hierarchy");
            return No_Valtyp;
         end if;

         Cur_Path := Get_Pathname_Suffix (Cur_Path);
      end loop;

      return Synth_Pathname (Syn_Inst, Name, Cur_Inst, Cur_Path);
   end Exec_Relative_Pathname;

   function Exec_Package_Pathname
     (Syn_Inst : Synth_Instance_Acc; Name : Node; Path : Node) return Valtyp
   is
      Lib_Id : constant Name_Id := Get_Identifier (Path);
      Pkg_Path : constant Node := Get_Pathname_Suffix (Path);
      Pkg_Id : constant Name_Id := Get_Identifier (Pkg_Path);
      Cur_Inst : Synth_Instance_Acc;
      It : Iterator_Top_Level_Type;
      N : Node;
   begin
      It := Iterator_Top_Level_Init;
      loop
         Iterate_Top_Level (It, Cur_Inst);
         exit when Cur_Inst = null;
         N := Get_Source_Scope (Cur_Inst);
         if Get_Identifier (N) = Pkg_Id
           and then
           Get_Identifier (Get_Library (Get_Design_File
                                          (Get_Design_Unit (N)))) = Lib_Id
         then
            return Synth_Pathname
              (Syn_Inst, Name, Cur_Inst, Get_Pathname_Suffix (Pkg_Path));
         end if;
      end loop;

      Error_Msg_Synth
        (Syn_Inst, Path, "cannot find package %i.%i in the design",
         (+Path, +Pkg_Path));
      return No_Valtyp;
   end Exec_Package_Pathname;


   function Exec_External_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                               return Valtyp
   is
      Path : Node;
   begin
      Path := Get_External_Pathname (Name);
      case Iir_Kinds_Pathname (Get_Kind (Path)) is
         when Iir_Kind_Absolute_Pathname =>
            return Exec_Absolute_Pathname (Syn_Inst, Name, Path);
         when Iir_Kind_Pathname_Element
           | Iir_Kind_Relative_Pathname =>
            return Exec_Relative_Pathname (Syn_Inst, Name, Path);
         when Iir_Kind_Package_Pathname =>
            return Exec_Package_Pathname (Syn_Inst, Name, Path);
      end case;
   end Exec_External_Name;

   --  Return True iff ID = S, case insensitive.
   function Match_Id (Id : Name_Id; M : Memory_Ptr; Len : Natural)
                     return Boolean is
   begin
      if Name_Table.Get_Name_Length (Id) /= Len then
         return False;
      end if;
      declare
         Img : constant String (1 .. Len) := Name_Table.Image (Id);
         C : Character;
      begin
         for I in Img'Range loop
            C := Read_Char (M + Size_Type (I - 1));
            C := Grt.Strings.To_Lower (C);
            if C /= Img (I) then
               return False;
            end if;
         end loop;
         return True;
      end;
   end Match_Id;

   --  V is the string whose value should be extracted from.  ETYPE and DTYPE
   --  are the type of the value.
   function Value_Attribute (V : Valtyp; Etype : Node; Dtype : Type_Acc)
                            return Valtyp
   is
      Btype : constant Node := Get_Base_Type (Etype);
      M : constant Memory_Ptr := V.Val.Mem;
      L : constant Uns32 := V.Typ.Abound.Len;
      Len : Uns32;
      First, Last : Size_Type;
      Val : Int64;
   begin
      --  LRM93 14.1 Predefined attributes.
      --  Leading and trailing whitespace are ignored.
      First := 0;
      Last := Size_Type (L - 1);
      while First <= Last loop
         exit when not Vhdl.Scanner.Is_Whitespace (Read_Char (M + First));
         First := First + 1;
      end loop;
      while Last >= First loop
         exit when not Vhdl.Scanner.Is_Whitespace (Read_Char (M + Last));
         Last := Last - 1;
      end loop;
      Len := Uns32 (Last - First + 1);

      case Get_Kind (Btype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            declare
               Id : Name_Id;
               En : Node;
            begin
               if Len = 3
                 and then Read_Char (M + First) = '''
                 and then Read_Char (M + First + 2) = '''
               then
                  Id := Name_Table.Get_Identifier (Read_Char (M + First + 1));
               else
                  declare
                     S : String (1 .. Natural (Len));
                     C : Character;
                  begin
                     for I in S'Range loop
                        C := Read_Char (M + First + Size_Type (I - 1));
                        C := Grt.Strings.To_Lower (C);
                        S (I) := C;
                     end loop;
                     Id := Name_Table.Get_Identifier_No_Create (S);
                  end;
               end if;
               En := Find_Name_In_Flist
                 (Get_Enumeration_Literal_List (Btype), Id);
               if En = Null_Node then
                  return No_Valtyp;
               end if;
               Val := Int64 (Get_Enum_Pos (En));
            end;
         when Iir_Kind_Integer_Type_Definition =>
            declare
               use Grt.To_Strings;
               use Grt.Types;
               use Grt.Vhdl_Types;
               Res : Value_I64_Result;
            begin
               Res := Value_I64 (To_Std_String_Basep (To_Address (M + First)),
                                 Ghdl_Index_Type (Len), 0);
               if Res.Status /= Value_Ok then
                  return No_Valtyp;
               end if;
               Val := Int64 (Res.Val);
            end;
         when Iir_Kind_Floating_Type_Definition =>
            declare
               use Grt.To_Strings;
               use Grt.Types;
               use Grt.Vhdl_Types;
               Res : Value_F64_Result;
            begin
               Res := Value_F64 (To_Std_String_Basep (To_Address (M + First)),
                                 Ghdl_Index_Type (Len), 0);
               if Res.Status /= Value_Ok then
                  return No_Valtyp;
               end if;
               return Create_Value_Float (Fp64 (Res.Val), Dtype);
            end;
         when Iir_Kind_Physical_Type_Definition =>
            declare
               use Grt.Types;
               use Grt.Vhdl_Types;
               use Grt.To_Strings;
               Is_Real : Boolean;
               Lit_Pos : Ghdl_Index_Type;
               Lit_End : Ghdl_Index_Type;
               Unit_Pos : Ghdl_Index_Type;
               Unit_F : Size_Type;
               Unit_Len : Natural;
               Mult : Int64;
               Unit : Iir;
               Unit_Id : Name_Id;
               Val_F : Grt.To_Strings.Value_F64_Result;
               Val_I : Grt.To_Strings.Value_I64_Result;
            begin
               Grt.To_Strings.Ghdl_Value_Physical_Split
                 (To_Std_String_Basep (To_Address (M)), Ghdl_Index_Type (L),
                  Is_Real, Lit_Pos, Lit_End, Unit_Pos);
               Unit_F := Size_Type (Unit_Pos);

               --  Find unit.
               Unit_Len := 0;
               for I in Unit_F .. Last loop
                  exit when Grt.Strings.Is_Whitespace (Read_Char (M + I));
                  Unit_Len := Unit_Len + 1;
               end loop;

               Unit := Get_Primary_Unit (Btype);
               while Unit /= Null_Iir loop
                  Unit_Id := Get_Identifier (Unit);
                  exit when Match_Id (Unit_Id, M + Unit_F, Unit_Len);
                  Unit := Get_Chain (Unit);
               end loop;

               if Unit = Null_Iir then
                  return No_Valtyp;
               end if;
               Mult := Get_Value (Get_Physical_Literal (Unit));

               if Is_Real then
                  Val_F := Value_F64 (To_Std_String_Basep (To_Address (M)),
                                      Lit_End, Ghdl_Index_Type (First));
                  if Val_F.Status /= Value_Ok then
                     return No_Valtyp;
                  end if;
                  Val := Int64 (Val_F.Val * Ghdl_F64 (Mult));
               else
                  Val_I := Value_I64 (To_Std_String_Basep (To_Address (M)),
                                      Lit_End, Ghdl_Index_Type (First));
                  if Val_I.Status /= Value_Ok then
                     return No_Valtyp;
                  end if;
                  Val := Int64 (Val_I.Val) * Mult;
               end if;
            end;

         when others =>
            raise Internal_Error;
      end case;
      return Create_Value_Discrete (Val, Dtype);
   end Value_Attribute;

   function Exec_Value_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                 return Valtyp
   is
      Param : constant Node := Get_Parameter (Attr);
      Etype : constant Node := Get_Type (Attr);
      V : Valtyp;
      Dtype : Type_Acc;
      Res : Valtyp;
   begin
      --  The value is supposed to be static.
      V := Synth_Expression (Syn_Inst, Param);
      if V = No_Valtyp then
         return No_Valtyp;
      end if;

      Dtype := Get_Subtype_Object (Syn_Inst, Etype);
      if not Is_Static (V.Val) then
         Error_Msg_Elab (Syn_Inst, Attr, "parameter of 'value must be static");
         return No_Valtyp;
      end if;

      Res := Value_Attribute (V, Etype, Dtype);
      if Res = No_Valtyp then
         Error_Msg_Synth (Syn_Inst, Attr, "incorrect 'value string");
      end if;
      return Res;
   end Exec_Value_Attribute;

   function Synth_Image_Attribute_Str (Val : Valtyp; Expr_Type : Iir)
                                      return String
   is
      use Grt.Types;
   begin
      case Get_Kind (Expr_Type) is
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            declare
               Str : String (1 .. 24);
               Last : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, Last, Ghdl_F64 (Read_Fp64 (Val)));
               return Str (Str'First .. Last);
            end;
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            declare
               Str : String (1 .. 21);
               First : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, First, Ghdl_I64 (Read_Discrete (Val)));
               return Str (First .. Str'Last);
            end;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            declare
               Lits : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Get_Base_Type (Expr_Type));
            begin
               return Name_Table.Image
                 (Get_Identifier
                    (Get_Nth_Element (Lits, Natural (Read_Discrete (Val)))));
            end;
         when Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            declare
               Str : String (1 .. 21);
               First : Natural;
               Id : constant Name_Id :=
                 Get_Identifier (Get_Primary_Unit (Get_Base_Type (Expr_Type)));
            begin
               Grt.To_Strings.To_String
                 (Str, First, Ghdl_I64 (Read_Discrete (Val)));
               return Str (First .. Str'Last) & ' ' & Name_Table.Image (Id);
            end;
         when others =>
            Error_Kind ("synth_image_attribute_str", Expr_Type);
      end case;
   end Synth_Image_Attribute_Str;

   function Exec_Image_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                 return Valtyp
   is
      Param : constant Node := Get_Parameter (Attr);
      Etype : constant Node := Get_Type (Attr);
      V : Valtyp;
      Dtype : Type_Acc;
      Res : Memtyp;
   begin
      --  The parameter is expected to be static.
      V := Synth_Expression (Syn_Inst, Param);
      if V = No_Valtyp then
         return No_Valtyp;
      end if;
      Dtype := Get_Subtype_Object (Syn_Inst, Etype);
      if not Is_Static (V.Val) then
         Error_Msg_Elab (Syn_Inst, Attr, "parameter of 'image must be static");
         return No_Valtyp;
      end if;

      Strip_Const (V);
      Res := String_To_Memtyp
        (Synth_Image_Attribute_Str (V, Get_Type (Param)), Dtype);
      return Create_Value_Memtyp (Res);
   end Exec_Image_Attribute;

   function Exec_Instance_Name_Attribute
     (Syn_Inst : Synth_Instance_Acc; Attr : Node) return Valtyp
   is
      Atype : constant Node := Get_Type (Attr);
      Atyp  : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Atype);
      Name  : constant Path_Instance_Name_Type :=
        Get_Path_Instance_Name_Suffix (Attr);
      Res : Memtyp;
   begin
      --  Return a truncated name, as the prefix is not completly known.
      Res := String_To_Memtyp (Name.Suffix, Atyp);
      return Create_Value_Memtyp (Res);
   end Exec_Instance_Name_Attribute;

   procedure Check_Matching_Bounds (Syn_Inst : Synth_Instance_Acc;
                                    L, R : Type_Acc; Loc : Node) is
   begin
      if not Are_Types_Equal (L, R) then
         Error_Msg_Elab (Syn_Inst, Loc, "non matching bounds");
      end if;
   end Check_Matching_Bounds;

   --  Return the bounds of a one dimensional array/vector type and the
   --  width of the element.
   procedure Get_Onedimensional_Array_Bounds
     (Typ : Type_Acc; Bnd : out Bound_Type; El_Typ : out Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Array
           | Type_Vector =>
            pragma Assert (Typ.Alast);
            El_Typ := Typ.Arr_El;
            Bnd := Typ.Abound;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Onedimensional_Array_Bounds;

   function Create_Onedimensional_Array_Subtype
     (Btyp : Type_Acc; Bnd : Bound_Type; El_Typ : Type_Acc) return Type_Acc
   is
      Res : Type_Acc;
   begin
      case Btyp.Kind is
         when Type_Vector =>
            pragma Assert (El_Typ.Kind in Type_Nets);
            Res := Create_Vector_Type (Bnd, False, Btyp.Arr_El);
         when Type_Unbounded_Vector =>
            pragma Assert (El_Typ.Kind in Type_Nets);
            Res := Create_Vector_Type (Bnd, False, Btyp.Uarr_El);
         when Type_Array =>
            pragma Assert (Btyp.Alast);
            pragma Assert (Is_Bounded_Type (Btyp.Arr_El));
            Res := Create_Array_Type (Bnd, False, True, Btyp.Arr_El);
         when Type_Unbounded_Array =>
            pragma Assert (Btyp.Ulast);
            pragma Assert (Is_Bounded_Type (El_Typ));
            Res := Create_Array_Type (Bnd, False, True, El_Typ);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Onedimensional_Array_Subtype;

   function Exec_Name_Subtype (Syn_Inst : Synth_Instance_Acc; Name : Node)
                              return Type_Acc is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Exec_Name_Subtype (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Parenthesis_Expression =>
            return Exec_Name_Subtype (Syn_Inst, Get_Expression (Name));
         when Iir_Kinds_Object_Declaration =>
            return Get_Value (Syn_Inst, Name).Typ;
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Name));
               Pfx : constant Node := Get_Prefix (Name);
               Res : Type_Acc;
            begin
               Res := Exec_Name_Subtype (Syn_Inst, Pfx);
               Res := Res.Rec.E (Idx + 1).Typ;
               return Res;
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Pfx : constant Node := Get_Prefix (Name);
               Res : Type_Acc;
            begin
               Res := Exec_Name_Subtype (Syn_Inst, Pfx);
               return Res.Arr_El;
            end;
         when Iir_Kind_Slice_Name =>
            declare
               use Netlists;
               Pfx_Typ : Type_Acc;
               Pfx_Bnd : Bound_Type;
               El_Typ : Type_Acc;
               Res_Bnd : Bound_Type;
               Sl_Off : Value_Offsets;
               Inp : Net;
               Err : Boolean;
            begin
               Pfx_Typ := Exec_Name_Subtype (Syn_Inst, Get_Prefix (Name));
               Get_Onedimensional_Array_Bounds (Pfx_Typ, Pfx_Bnd, El_Typ);
               Synth_Slice_Suffix (Syn_Inst, Name, Pfx_Bnd, Pfx_Typ.Wkind,
                                   El_Typ, Res_Bnd, Inp, Sl_Off, Err);
               if Err then
                  return null;
               end if;
               pragma Assert (Inp = No_Net);
               return Create_Onedimensional_Array_Subtype
                 (Pfx_Typ, Res_Bnd, El_Typ);
            end;
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            declare
               Val : Valtyp;
               Obj : Memtyp;
            begin
               --  Maybe do not dereference it if its type is known ?
               Val := Synth_Expression (Syn_Inst, Get_Prefix (Name));
               Obj := Elab.Vhdl_Heap.Synth_Dereference (Read_Access (Val));
               return Obj.Typ;
            end;
         when Iir_Kind_Function_Call =>
            declare
               Ret_Typ : Type_Acc;
               Val : Valtyp;
            begin
               Ret_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Name));
               if Is_Bounded_Type (Ret_Typ) then
                  return Ret_Typ;
               end if;

               --  Humm, is it an error ?
               Val := Synth.Vhdl_Expr.Synth_Expression (Syn_Inst, Name);
               return Val.Typ;
            end;

         when Iir_Kind_Enumeration_Literal
            | Iir_Kind_Unit_Declaration =>
            return Get_Subtype_Object (Syn_Inst, Get_Type (Name));

         when Iir_Kind_String_Literal8
           | Iir_Kind_Aggregate =>
            --  TODO: the value should be computed (once) and its type
            --  returned.
            return Synth_Subtype_Indication (Syn_Inst, Get_Type (Name));

         when Iir_Kind_Image_Attribute =>
            declare
               Val : Valtyp;
            begin
               Val := Synth.Vhdl_Expr.Synth_Expression (Syn_Inst, Name);
               return Val.Typ;
            end;

         when Iir_Kind_Element_Attribute =>
            declare
               Pfx : Type_Acc;
            begin
               Pfx := Exec_Name_Subtype (Syn_Inst, Get_Prefix (Name));
               return Pfx.Arr_El;
            end;

         when others =>
            Error_Kind ("exec_name_subtype", Name);
      end case;
   end Exec_Name_Subtype;

   function Exec_String_Literal (Syn_Inst : Synth_Instance_Acc;
                                 Str : Node;
                                 Str_Typ : Type_Acc) return Valtyp
   is
      pragma Assert (Get_Kind (Str) = Iir_Kind_String_Literal8);
      Id : constant String8_Id := Get_String8_Id (Str);
      Len : constant Int32 := Get_String_Length (Str);

      Str_Type : constant Node := Get_Type (Str);
      El_Type : Type_Acc;
      Bounds : Bound_Type;
      Res_Type : Type_Acc;
      Res : Valtyp;
      Pos : Nat8;
   begin
      case Str_Typ.Kind is
         when Type_Vector
           | Type_Array =>
            Bounds := Str_Typ.Abound;
            if Bounds.Len /= Uns32 (Len) then
               Error_Msg_Synth
                 (Syn_Inst, Str, "string length doesn't match constraints");
               return No_Valtyp;
            end if;
         when Type_Unbounded_Vector
            | Type_Unbounded_Array =>
            Bounds := Synth_Bounds_From_Length
              (Get_Index_Type (Str_Type, 0), Len);
         when others =>
            raise Internal_Error;
      end case;

      El_Type := Get_Array_Element (Str_Typ);
      if El_Type.Kind in Type_Nets then
         Res_Type := Create_Vector_Type (Bounds, True, El_Type);
      else
         Res_Type := Create_Array_Type (Bounds, True, True, El_Type);
      end if;
      Res := Create_Value_Memory (Res_Type, Current_Pool);

      --  Only U8 are handled.
      pragma Assert (El_Type.Sz = 1);

      --  From left to right.
      for I in 1 .. Bounds.Len loop
         -- FIXME: use literal from type ??
         Pos := Str_Table.Element_String8 (Id, Pos32 (I));
         Write_U8 (Res.Val.Mem + Size_Type (I - 1), Nat8'Pos (Pos));
      end loop;

      return Res;
   end Exec_String_Literal;

   function Exec_Path_Instance_Name_Attribute
     (Inst : Synth_Instance_Acc; Attr : Iir) return Memtyp
   is
      use Grt.Rstrings;
      use Name_Table;

      Is_Instance : constant Boolean :=
        Get_Kind (Attr) = Iir_Kind_Instance_Name_Attribute;

      Atype : constant Node := Get_Type (Attr);
      Str_Typ  : constant Type_Acc := Get_Subtype_Object (Inst, Atype);
      Name : constant Path_Instance_Name_Type :=
        Get_Path_Instance_Name_Suffix (Attr);
      Instance, Parent : Synth_Instance_Acc;
      Rstr : Rstring;
      Label, Stmt : Node;
   begin
      if Name.Path_Instance = Null_Iir then
         return String_To_Memtyp (Name.Suffix, Str_Typ);
      end if;

      Instance := Get_Instance_By_Scope
        (Inst, Get_Info_Scope (Name.Path_Instance));

      loop
         Parent := Get_Instance_Parent (Instance);
         if Parent = Root_Instance then
            Parent := null;
         end if;
         Label := Get_Source_Scope (Instance);
         Stmt := Get_Statement_Scope (Instance);

         case Get_Kind (Label) is
            when Iir_Kind_Entity_Declaration =>
               if Parent = null then
                  Prepend (Rstr, Image (Get_Identifier (Label)));
                  exit;
               end if;
            when Iir_Kind_Architecture_Body =>
               if Is_Instance then
                  Prepend (Rstr, ')');
                  Prepend (Rstr, Image (Get_Identifier (Label)));
                  Prepend (Rstr, '(');
               end if;

               if Is_Instance or else Parent = null then
                  Prepend (Rstr, Image (Get_Identifier (Get_Entity (Label))));
               end if;
               if Parent = null then
                  Prepend (Rstr, ':');
                  exit;
               end if;
            when Iir_Kind_Block_Statement =>
               Prepend (Rstr, Image (Get_Label (Label)));
               Prepend (Rstr, ':');
            when Iir_Kind_Generate_Statement_Body =>
               declare
                  Gen : constant Node := Get_Parent (Label);
               begin
                  case Iir_Kinds_Generate_Statement (Get_Kind (Gen)) is
                     when Iir_Kind_For_Generate_Statement =>
                        declare
                           It : constant Node :=
                             Get_Parameter_Specification (Gen);
                           Val : Valtyp;
                        begin
                           Val := Get_Value (Instance, It);
                           Prepend (Rstr, ')');
                           Prepend
                             (Rstr,
                              Synth_Image_Attribute_Str (Val, Get_Type (It)));
                           Prepend (Rstr, '(');
                        end;

                        --  Skip the for generate instance.
                        Parent := Get_Instance_Parent (Parent);

                     when Iir_Kind_If_Generate_Statement
                       | Iir_Kind_Case_Generate_Statement =>
                        null;
                  end case;

                  Prepend (Rstr, Image (Get_Label (Gen)));
                  Prepend (Rstr, ':');
               end;

            when Iir_Kind_Component_Declaration =>
               if Is_Instance then
                  Prepend (Rstr, '@');
               end if;
               Prepend (Rstr, Image (Get_Label (Stmt)));
               Prepend (Rstr, ':');
            when others =>
               Error_Kind ("Exec_Path_Instance_Name_Attribute",
                           Label);
         end case;
         Instance := Parent;
      end loop;
      declare
         Str1 : String (1 .. Length (Rstr));
         Len1 : Natural;
      begin
         Copy (Rstr, Str1, Len1);
         Free (Rstr);
         return String_To_Memtyp (Str1 & ':' & Name.Suffix, Str_Typ);
      end;
   end Exec_Path_Instance_Name_Attribute;
end Elab.Vhdl_Expr;
