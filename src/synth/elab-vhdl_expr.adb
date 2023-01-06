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

with Netlists;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Scanner;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Heap; use Elab.Vhdl_Heap;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;

with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Vhdl_Eval; use Synth.Vhdl_Eval;
with Synth.Errors; use Synth.Errors;

with Grt.Types;
with Grt.Vhdl_Types;
with Grt.Strings;
with Grt.To_Strings;
with Grt.Values;
with Grt.Vstrings;

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

   --  Return True iff ID = S, case insensitive.
   function Match_Id (Id : Name_Id; S : String) return Boolean is
   begin
      if Name_Table.Get_Name_Length (Id) /= S'Length then
         return False;
      end if;
      declare
         Img : constant String (S'Range) := Name_Table.Image (Id);
      begin
         for I in Img'Range loop
            if Grt.Strings.To_Lower (S (I)) /= Img (I) then
               return False;
            end if;
         end loop;
         return True;
      end;
   end Match_Id;

   function Exec_Value_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                 return Valtyp
   is
      Param : constant Node := Get_Parameter (Attr);
      Etype : constant Node := Get_Type (Attr);
      Btype : constant Node := Get_Base_Type (Etype);
      V : Valtyp;
      Dtype : Type_Acc;
   begin
      --  The value is supposed to be static.
      V := Synth_Expression (Syn_Inst, Param);
      if V = No_Valtyp then
         return No_Valtyp;
      end if;

      Dtype := Get_Subtype_Object (Syn_Inst, Etype);
      if not Is_Static (V.Val) then
         Error_Msg_Elab (+Attr, "parameter of 'value must be static");
         return No_Valtyp;
      end if;

      declare
         Value : constant String := Value_To_String (V);
         First, Last : Integer;
         Res_N : Node;
         Val : Int64;
      begin
         --  LRM93 14.1 Predefined attributes.
         --  Leading and trailing whitespace are ignored.
         First := Value'First;
         Last := Value'Last;
         while First <= Last loop
            exit when not Vhdl.Scanner.Is_Whitespace (Value (First));
            First := First + 1;
         end loop;
         while Last >= First loop
            exit when not Vhdl.Scanner.Is_Whitespace (Value (Last));
            Last := Last - 1;
         end loop;

         case Get_Kind (Btype) is
            when Iir_Kind_Enumeration_Type_Definition =>
               Res_N := Eval_Value_Attribute
                 (Value (First .. Last), Etype, Attr);
               Val := Int64 (Get_Enum_Pos (Res_N));
               Free_Iir (Res_N);
            when Iir_Kind_Integer_Type_Definition =>
               declare
                  use Grt.To_Strings;
                  use Grt.Types;
                  use Grt.Vhdl_Types;
                  Value1 : String renames Value (First .. Last);
                  Res : Value_I64_Result;
               begin
                  Res := Value_I64 (To_Std_String_Basep (Value1'Address),
                                    Value1'Length, 0);
                  if Res.Status = Value_Ok then
                     Val := Int64 (Res.Val);
                  else
                     Error_Msg_Synth
                       (Syn_Inst, Attr, "incorrect 'value string");
                     return No_Valtyp;
                  end if;
               end;
            when Iir_Kind_Physical_Type_Definition =>
               declare
                  use Grt.Types;
                  use Grt.Vhdl_Types;
                  Value1 : String renames Value (First .. Last);
                  F : constant Ghdl_Index_Type := Ghdl_Index_Type (First);
                  S : constant Std_String_Basep :=
                    To_Std_String_Basep (Value1'Address);
                  Len : constant Ghdl_Index_Type :=
                    Ghdl_Index_Type (Last - First + 1);
                  Is_Real : Boolean;
                  Lit_Pos : Ghdl_Index_Type;
                  Lit_End : Ghdl_Index_Type;
                  Unit_Pos : Ghdl_Index_Type;
                  Unit_F, Unit_L : Positive;
                  Mult : Int64;
                  Unit : Iir;
                  Unit_Id : Name_Id;
                  Val_F : Grt.To_Strings.Value_F64_Result;
               begin
                  Grt.Values.Ghdl_Value_Physical_Split
                    (S, Len, Is_Real, Lit_Pos, Lit_End, Unit_Pos);
                  Unit_F := Positive (Unit_Pos + 1);

                  --  Find unit.
                  Unit_L := Unit_F;
                  for I in Unit_F .. Last loop
                     exit when Grt.Strings.Is_Whitespace (Value1 (I));
                     Unit_L := I;
                  end loop;

                  Unit := Get_Primary_Unit (Btype);
                  while Unit /= Null_Iir loop
                     Unit_Id := Get_Identifier (Unit);
                     exit when Match_Id (Unit_Id, Value1 (Unit_F .. Unit_L));
                     Unit := Get_Chain (Unit);
                  end loop;

                  if Unit = Null_Iir then
                     Error_Msg_Synth (Syn_Inst, Attr, "incorrect unit name");
                     return No_Valtyp;
                  end if;
                  Mult := Get_Value (Get_Physical_Literal (Unit));

                  if Is_Real then
                     Val_F := Grt.To_Strings.Value_F64 (S, Lit_End, F);
                     Val := Int64 (Val_F.Val * Ghdl_F64 (Mult));
                  else
                     Val := Int64 (Grt.Values.Value_I64 (S, Lit_End, F))
                       * Mult;
                  end if;
               end;

            when others =>
               Error_Msg_Elab (+Attr, "unhandled type for 'value");
               return No_Valtyp;
         end case;
         return Create_Value_Discrete (Val, Dtype);
      end;
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
         Error_Msg_Elab (+Attr, "parameter of 'image must be static");
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

   procedure Check_Matching_Bounds (L, R : Type_Acc; Loc : Node) is
   begin
      if not Are_Types_Equal (L, R) then
         Error_Msg_Elab (+Loc, "non matching bounds");
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
            Res := Create_Vector_Type (Bnd, Btyp.Arr_El);
         when Type_Unbounded_Vector =>
            pragma Assert (El_Typ.Kind in Type_Nets);
            Res := Create_Vector_Type (Bnd, Btyp.Uarr_El);
         when Type_Array =>
            pragma Assert (Btyp.Alast);
            pragma Assert (Is_Bounded_Type (Btyp.Arr_El));
            Res := Create_Array_Type (Bnd, True, Btyp.Arr_El);
         when Type_Unbounded_Array =>
            pragma Assert (Btyp.Ulast);
            pragma Assert (Is_Bounded_Type (El_Typ));
            Res := Create_Array_Type (Bnd, True, El_Typ);
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
            begin
               Pfx_Typ := Exec_Name_Subtype (Syn_Inst, Get_Prefix (Name));
               Get_Onedimensional_Array_Bounds (Pfx_Typ, Pfx_Bnd, El_Typ);
               Synth_Slice_Suffix (Syn_Inst, Name, Pfx_Bnd, El_Typ,
                                   Res_Bnd, Inp, Sl_Off);
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
         Res_Type := Create_Vector_Type (Bounds, El_Type);
      else
         Res_Type := Create_Array_Type (Bounds, True, El_Type);
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
      use Grt.Vstrings;
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
