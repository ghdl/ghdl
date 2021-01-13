--  Interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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

with System;
with Ada.Unchecked_Conversion;
with Simple_IO; use Simple_IO;
with Types; use Types;
with Grt.Types; use Grt.Types;
with Flags; use Flags;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package;
with Vhdl.Evaluation;
with Vhdl.Utils; use Vhdl.Utils;
with Name_Table;
with Simul.File_Operation;
with Simul.Debugger; use Simul.Debugger;
with Std_Names;
with Str_Table;
with Files_Map;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Simul.Simulation; use Simul.Simulation;
with Grt.Astdio.Vhdl;
with Grt.Stdio;
with Grt.Options;
with Grt.Vstrings;
with Grt.To_Strings;
with Simul.Grt_Interface;
with Grt.Values;
with Grt.Errors;
with Grt.Std_Logic_1164;
with Grt.Lib;
with Grt.Strings;
with Vhdl.Sem_Inst;

package body Simul.Execution is

   function Execute_Function_Call
     (Block: Block_Instance_Acc; Expr: Iir; Imp : Iir)
     return Iir_Value_Literal_Acc;

   procedure Finish_Sequential_Statements
     (Proc : Process_State_Acc; Complex_Stmt : Iir);
   procedure Init_Sequential_Statements
     (Proc : Process_State_Acc; Complex_Stmt : Iir);
   procedure Update_Next_Statement (Proc : Process_State_Acc);

   -- Display a message when an assertion has failed.
   procedure Execute_Failed_Assertion (Msg : String;
                                       Report : String;
                                       Severity : Natural;
                                       Stmt: Iir);

   function Get_Instance_By_Scope
     (Instance: Block_Instance_Acc; Scope: Sim_Info_Acc)
     return Block_Instance_Acc is
   begin
      case Scope.Kind is
         when Kind_Block
           | Kind_Frame
           | Kind_Process =>
            declare
               Current : Block_Instance_Acc;
            begin
               Current := Instance;
               while Current /= null loop
                  if Current.Block_Scope = Scope then
                     return Current;
                  end if;
                  Current := Current.Up_Block;
               end loop;
               raise Internal_Error;
            end;
         when Kind_Protected =>
            declare
               Current : Block_Instance_Acc;
            begin
               Current := Instance;
               while Current /= null loop
                  if Current.Block_Scope = Scope
                    or Current.Uninst_Scope = Scope
                  then
                     return Current;
                  end if;
                  Current := Current.Up_Block;
               end loop;
               raise Internal_Error;
            end;
         when Kind_Package =>
            if Scope.Pkg_Parent = null then
               --  This is a scope for an uninstantiated package.
               declare
                  Current : Block_Instance_Acc;
               begin
                  Current := Instance;
                  while Current /= null loop
                     if Current.Uninst_Scope = Scope then
                        return Current;
                     end if;
                     Current := Current.Up_Block;
                  end loop;
                  raise Internal_Error;
               end;
            else
               --  Instantiated package.
               declare
                  Parent : Block_Instance_Acc;
               begin
                  Parent := Get_Instance_By_Scope (Instance, Scope.Pkg_Parent);
                  return Parent.Objects (Scope.Pkg_Slot).Instance;
               end;
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Instance_By_Scope;

   function Get_Instance_Object (Instance: Block_Instance_Acc; Obj : Iir)
                                return Iir_Value_Literal_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Obj);
      Obj_Inst : Block_Instance_Acc;
   begin
      Obj_Inst := Get_Instance_By_Scope (Instance, Info.Obj_Scope);
      return Obj_Inst.Objects (Info.Slot);
   end Get_Instance_Object;

   function Get_Info_For_Scope (Scope : Iir) return Sim_Info_Acc is
   begin
      --  The info for an architecture is in fact the entity.
      if Get_Kind (Scope) = Iir_Kind_Architecture_Body then
         return Get_Info (Get_Entity (Scope));
      else
         return Get_Info (Scope);
      end if;
   end Get_Info_For_Scope;

   procedure Create_Right_Bound_From_Length
     (Bounds : Iir_Value_Literal_Acc; Len : Iir_Index32) is
   begin
      pragma Assert (Bounds.Right = null);

      case Bounds.Left.Kind is
         when Iir_Value_E32 =>
            declare
               R : Ghdl_E32;
            begin
               case Bounds.Dir is
                  when Dir_To =>
                     R := Bounds.Left.E32 + Ghdl_E32 (Len - 1);
                  when Dir_Downto =>
                     R := Bounds.Left.E32 - Ghdl_E32 (Len - 1);
               end case;
               Bounds.Right := Create_E32_Value (R);
            end;
         when Iir_Value_I64 =>
            declare
               R : Ghdl_I64;
            begin
               case Bounds.Dir is
                  when Dir_To =>
                     R := Bounds.Left.I64 + Ghdl_I64 (Len - 1);
                  when Dir_Downto =>
                     R := Bounds.Left.I64 - Ghdl_I64 (Len - 1);
               end case;
               Bounds.Right := Create_I64_Value (R);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Right_Bound_From_Length;

   function Create_Bounds_From_Length (Block : Block_Instance_Acc;
                                       Atype : Iir;
                                       Len : Iir_Index32)
                                      return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
      Index_Bounds : Iir_Value_Literal_Acc;
   begin
      Index_Bounds := Execute_Bounds (Block, Atype);

      Res := Create_Range_Value (Left => Index_Bounds.Left,
                                 Right => null,
                                 Dir => Index_Bounds.Dir,
                                 Length => Len);

      if Len = 0 then
         --  Special case.
         Res.Right := Res.Left;
         case Res.Left.Kind is
            when Iir_Value_I64 =>
               case Index_Bounds.Dir is
                  when Dir_To =>
                     Res.Left := Create_I64_Value (Res.Right.I64 + 1);
                  when Dir_Downto =>
                     Res.Left := Create_I64_Value (Res.Right.I64 - 1);
               end case;
            when others =>
               raise Internal_Error;
         end case;
      else
         Create_Right_Bound_From_Length (Res, Len);
      end if;
      return Res;
   end Create_Bounds_From_Length;

   function Execute_High_Limit (Bounds : Iir_Value_Literal_Acc)
                                return Iir_Value_Literal_Acc is
   begin
      if Bounds.Dir = Dir_To then
         return Bounds.Right;
      else
         return Bounds.Left;
      end if;
   end Execute_High_Limit;

   function Execute_Low_Limit (Bounds : Iir_Value_Literal_Acc)
                               return Iir_Value_Literal_Acc is
   begin
      if Bounds.Dir = Dir_To then
         return Bounds.Left;
      else
         return Bounds.Right;
      end if;
   end Execute_Low_Limit;

   function Execute_Left_Limit (Bounds : Iir_Value_Literal_Acc)
                                return Iir_Value_Literal_Acc is
   begin
      return Bounds.Left;
   end Execute_Left_Limit;

   function Execute_Right_Limit (Bounds : Iir_Value_Literal_Acc)
                                 return Iir_Value_Literal_Acc is
   begin
      return Bounds.Right;
   end Execute_Right_Limit;

   function Execute_Length (Bounds : Iir_Value_Literal_Acc)
                           return Iir_Value_Literal_Acc is
   begin
      return Create_I64_Value (Ghdl_I64 (Bounds.Length));
   end Execute_Length;

   function Create_Enum_Value (Pos : Natural; Etype : Iir)
                              return Iir_Value_Literal_Acc
   is
      Base_Type : constant Iir := Get_Base_Type (Etype);
      Kind : constant Kind_Enum_Types := Get_Info (Base_Type).Kind;
   begin
      case Kind is
         when Kind_E8_Type
           | Kind_Log_Type =>
            return Create_E8_Value (Ghdl_E8 (Pos));
         when Kind_E32_Type =>
            return Create_E32_Value (Ghdl_E32 (Pos));
         when Kind_Bit_Type =>
            return Create_B1_Value (Ghdl_B1'Val (Pos));
      end case;
   end Create_Enum_Value;

   function String_To_Iir_Value (Str : String) return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      Res := Create_Array_Value (Str'Length, 1);
      Res.Bounds.D (1) := Create_Range_Value
        (Create_I64_Value (1),
         Create_I64_Value (Str'Length),
         Dir_To);
      for I in Str'Range loop
         Res.Val_Array.V (1 + Iir_Index32 (I - Str'First)) :=
           Create_E8_Value (Character'Pos (Str (I)));
      end loop;
      return Res;
   end String_To_Iir_Value;

   function Execute_Image_Attribute (Val : Iir_Value_Literal_Acc;
                                     Expr_Type : Iir)
                                    return String
   is
   begin
      case Get_Kind (Expr_Type) is
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            declare
               Str : String (1 .. 24);
               Last : Natural;
            begin
               Grt.To_Strings.To_String (Str, Last, Val.F64);
               return Str (Str'First .. Last);
            end;
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            declare
               Str : String (1 .. 21);
               First : Natural;
            begin
               Grt.To_Strings.To_String (Str, First, Val.I64);
               return Str (First .. Str'Last);
            end;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            declare
               Lits : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Get_Base_Type (Expr_Type));
               Pos : Natural;
            begin
               case Iir_Value_Enums (Val.Kind) is
                  when Iir_Value_B1 =>
                     Pos := Ghdl_B1'Pos (Val.B1);
                  when Iir_Value_E8 =>
                     Pos := Ghdl_E8'Pos (Val.E8);
                  when Iir_Value_E32 =>
                     Pos := Ghdl_E32'Pos (Val.E32);
               end case;
               return Name_Table.Image
                 (Get_Identifier (Get_Nth_Element (Lits, Pos)));
            end;
         when Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            declare
               Str : String (1 .. 21);
               First : Natural;
               Id : constant Name_Id :=
                 Get_Identifier (Get_Primary_Unit (Get_Base_Type (Expr_Type)));
            begin
               Grt.To_Strings.To_String (Str, First, Val.I64);
               return Str (First .. Str'Last) & ' ' & Name_Table.Image (Id);
            end;
         when others =>
            Error_Kind ("execute_image_attribute", Expr_Type);
      end case;
   end Execute_Image_Attribute;

   function Execute_Image_Attribute (Block: Block_Instance_Acc; Expr: Iir)
                                    return Iir_Value_Literal_Acc
   is
      Val : Iir_Value_Literal_Acc;
      Attr_Type : constant Iir := Get_Type (Get_Prefix (Expr));
   begin
      Val := Execute_Expression (Block, Get_Parameter (Expr));
      return String_To_Iir_Value
        (Execute_Image_Attribute (Val, Attr_Type));
   end Execute_Image_Attribute;

   function Execute_Path_Instance_Name_Attribute
     (Block : Block_Instance_Acc; Attr : Iir) return Iir_Value_Literal_Acc
   is
      use Vhdl.Evaluation;
      use Grt.Vstrings;
      use Name_Table;

      Name : constant Path_Instance_Name_Type :=
        Get_Path_Instance_Name_Suffix (Attr);
      Instance : Block_Instance_Acc;
      Rstr : Rstring;
      Is_Instance : constant Boolean :=
        Get_Kind (Attr) = Iir_Kind_Instance_Name_Attribute;
   begin
      if Name.Path_Instance = Null_Iir then
         return String_To_Iir_Value (Name.Suffix);
      end if;

      Instance := Get_Instance_By_Scope
        (Block, Get_Info_For_Scope (Name.Path_Instance));

      loop
         case Get_Kind (Instance.Label) is
            when Iir_Kind_Entity_Declaration =>
               if Instance.Parent = null then
                  Prepend (Rstr, Image (Get_Identifier (Instance.Label)));
                  exit;
               end if;
            when Iir_Kind_Architecture_Body =>
               if Is_Instance then
                  Prepend (Rstr, ')');
                  Prepend (Rstr, Image (Get_Identifier (Instance.Label)));
                  Prepend (Rstr, '(');
               end if;

               if Is_Instance or else Instance.Parent = null then
                  Prepend
                    (Rstr,
                     Image (Get_Identifier (Get_Entity (Instance.Label))));
               end if;
               if Instance.Parent = null then
                  Prepend (Rstr, ':');
                  exit;
               else
                  Instance := Instance.Parent;
               end if;
            when Iir_Kind_Block_Statement =>
               Prepend (Rstr, Image (Get_Label (Instance.Label)));
               Prepend (Rstr, ':');
               Instance := Instance.Parent;
            when Iir_Kind_Iterator_Declaration =>
               declare
                  Val : Iir_Value_Literal_Acc;
               begin
                  Val := Execute_Name (Instance, Instance.Label);
                  Prepend (Rstr, ')');
                  Prepend (Rstr, Execute_Image_Attribute
                             (Val, Get_Type (Instance.Label)));
                  Prepend (Rstr, '(');
               end;
               Instance := Instance.Parent;
            when Iir_Kind_Generate_Statement_Body =>
               Prepend (Rstr, Image (Get_Label (Get_Parent (Instance.Label))));
               Prepend (Rstr, ':');
               Instance := Instance.Parent;
            when Iir_Kind_Component_Instantiation_Statement =>
               if Is_Instance then
                  Prepend (Rstr, '@');
               end if;
               Prepend (Rstr, Image (Get_Label (Instance.Label)));
               Prepend (Rstr, ':');
               Instance := Instance.Parent;
            when others =>
               Error_Kind ("Execute_Path_Instance_Name_Attribute",
                           Instance.Label);
         end case;
      end loop;
      declare
         Str1 : String (1 .. Length (Rstr));
         Len1 : Natural;
      begin
         Copy (Rstr, Str1, Len1);
         Free (Rstr);
         return String_To_Iir_Value (Str1 & ':' & Name.Suffix);
      end;
   end Execute_Path_Instance_Name_Attribute;

   function Execute_Shift_Operator (Left : Iir_Value_Literal_Acc;
                                    Count : Ghdl_I64;
                                    Expr : Iir)
                                   return Iir_Value_Literal_Acc
   is
      Func : constant Iir_Predefined_Shift_Functions :=
        Get_Implicit_Definition (Get_Implementation (Expr));
      Cnt : Iir_Index32;
      Len : constant Iir_Index32 := Left.Bounds.D (1).Length;
      Dir_Left : Boolean;
      P : Iir_Index32;
      Res : Iir_Value_Literal_Acc;
      E : Iir_Value_Literal_Acc;
   begin
      --  LRM93 7.2.3
      --  That is, if R is 0 or if L is a null array, the return value is L.
      if Count = 0 or else Len = 0 then
         return Left;
      end if;

      case Func is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Rol =>
            Dir_Left := True;
         when Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sra
           | Iir_Predefined_Array_Ror =>
            Dir_Left := False;
      end case;
      if Count < 0 then
         Cnt := Iir_Index32 (-Count);
         Dir_Left := not Dir_Left;
      else
         Cnt := Iir_Index32 (Count);
      end if;

      case Func is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl =>
            E := Create_Enum_Value
              (0, Get_Element_Subtype (Get_Base_Type (Get_Type (Expr))));
         when Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra =>
            if Dir_Left then
               E := Left.Val_Array.V (Len);
            else
               E := Left.Val_Array.V (1);
            end if;
         when Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            Cnt := Cnt mod Len;
            if not Dir_Left then
               Cnt := (Len - Cnt) mod Len;
            end if;
      end case;

      Res := Create_Array_Value (1);
      Res.Bounds.D (1) := Left.Bounds.D (1);
      Create_Array_Data (Res, Len);
      P := 1;

      case Func is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra =>
            if Dir_Left then
               if Cnt < Len then
                  for I in Cnt .. Len - 1 loop
                     Res.Val_Array.V (P) := Left.Val_Array.V (I + 1);
                     P := P + 1;
                  end loop;
               else
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Res.Val_Array.V (P) := E;
                  P := P + 1;
               end loop;
            else
               if Cnt > Len then
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Res.Val_Array.V (P) := E;
                  P := P + 1;
               end loop;
               for I in Cnt .. Len - 1 loop
                  Res.Val_Array.V (P) := Left.Val_Array.V (I - Cnt + 1);
                  P := P + 1;
               end loop;
            end if;
         when Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            for I in 1 .. Len loop
               Res.Val_Array.V (P) := Left.Val_Array.V (Cnt + 1);
               P := P + 1;
               Cnt := Cnt + 1;
               if Cnt = Len then
                  Cnt := 0;
               end if;
            end loop;
      end case;
      return Res;
   end Execute_Shift_Operator;

   Hex_Chars : constant array (Natural range 0 .. 15) of Character :=
     "0123456789ABCDEF";

   function Execute_Bit_Vector_To_String (Val : Iir_Value_Literal_Acc;
                                          Log_Base : Natural)
                                         return Iir_Value_Literal_Acc
   is
      Base : constant Natural := 2 ** Log_Base;
      Blen : constant Natural := Natural (Val.Bounds.D (1).Length);
      Str : String (1 .. (Blen + Log_Base - 1) / Log_Base);
      Pos : Natural;
      V : Natural;
      N : Natural;
   begin
      V := 0;
      N := 1;
      Pos := Str'Last;
      for I in reverse Val.Val_Array.V'Range loop
         V := V + Ghdl_B1'Pos (Val.Val_Array.V (I).B1) * N;
         N := N * 2;
         if N = Base or else I = Val.Val_Array.V'First then
            Str (Pos) := Hex_Chars (V);
            Pos := Pos - 1;
            N := 1;
            V := 0;
         end if;
      end loop;
      return String_To_Iir_Value (Str);
   end Execute_Bit_Vector_To_String;

   procedure Assert_Std_Ulogic_Dc (Loc : Iir)
   is
   begin
      Execute_Failed_Assertion
        ("assertion",
         "STD_LOGIC_1164: '-' operand for matching ordering operator",
         1, Loc);
   end Assert_Std_Ulogic_Dc;

   procedure Check_Std_Ulogic_Dc (Loc : Iir; V : Grt.Std_Logic_1164.Std_Ulogic)
   is
      use Grt.Std_Logic_1164;
   begin
      if V = '-' then
         Assert_Std_Ulogic_Dc (Loc);
      end if;
   end Check_Std_Ulogic_Dc;

   --  EXPR is the expression whose implementation is an implicit function.
   function Execute_Implicit_Function (Block : Block_Instance_Acc;
                                       Expr: Iir;
                                       Left_Param : Iir;
                                       Right_Param : Iir;
                                       Res_Type : Iir)
                                      return Iir_Value_Literal_Acc
   is
      pragma Unsuppress (Overflow_Check);

      Imp : constant Iir := Strip_Denoting_Name (Get_Implementation (Expr));
      Func : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);

      --  Rename definition for monadic operations.
      Left, Right: Iir_Value_Literal_Acc;
      Operand : Iir_Value_Literal_Acc renames Left;
      Result: Iir_Value_Literal_Acc;

      procedure Eval_Right is
      begin
         Right := Execute_Expression (Block, Right_Param);
      end Eval_Right;

      --  Eval right argument, check left and right have same length,
      --  Create RESULT from left.
      procedure Eval_Array is
      begin
         Eval_Right;
         if Left.Bounds.D (1).Length /= Right.Bounds.D (1).Length then
            Error_Msg_Constraint (Expr);
         end if;
         --  Need to copy as the result is modified.
         Result := Unshare (Left, Expr_Pool'Access);
      end Eval_Array;
   begin
      --  Eval left operand.
      case Func is
         when Iir_Predefined_Now_Function =>
            Left := null;
         when Iir_Predefined_Bit_Rising_Edge
           | Iir_Predefined_Boolean_Rising_Edge
           | Iir_Predefined_Bit_Falling_Edge
           | Iir_Predefined_Boolean_Falling_Edge=>
            Operand := Execute_Name (Block, Left_Param, True);
         when others =>
            Left := Execute_Expression (Block, Left_Param);
      end case;
      Right := null;

      case Func is
         when Iir_Predefined_Error =>
            raise Internal_Error;

         when Iir_Predefined_Array_Array_Concat
           | Iir_Predefined_Element_Array_Concat
           | Iir_Predefined_Array_Element_Concat
           | Iir_Predefined_Element_Element_Concat =>
            Eval_Right;

            declare
               --  Type of the index.
               Idx_Type : constant Iir :=
                 Get_Nth_Element (Get_Index_Subtype_List (Res_Type), 0);

               -- Array length of the result.
               Len: Iir_Index32;

               -- Index into the result.
               Pos: Iir_Index32;
            begin
               -- Compute the length of the result.
               case Func is
                  when Iir_Predefined_Array_Array_Concat =>
                     Len := Left.Val_Array.Len + Right.Val_Array.Len;
                  when Iir_Predefined_Element_Array_Concat =>
                     Len := 1 + Right.Val_Array.Len;
                  when Iir_Predefined_Array_Element_Concat =>
                     Len := Left.Val_Array.Len + 1;
                  when Iir_Predefined_Element_Element_Concat =>
                     Len := 1 + 1;
                  when others =>
                     raise Program_Error;
               end case;

               if Func = Iir_Predefined_Array_Array_Concat
                 and then Left.Val_Array.Len = 0
               then
                  if Flags.Vhdl_Std = Vhdl_87 then
                     --  LRM87 7.2.3
                     --  [...], unless the left operand is a null array, in
                     --  which case the result of the concatenation is the
                     --  right operand.
                     return Right;
                  else
                     --  LRM93 7.2.4
                     --  If both operands are null arrays, then the result of
                     --  the concatenation is the right operand.
                     if Right.Val_Array.Len = 0 then
                        return Right;
                     end if;
                  end if;
               end if;

               if Flags.Vhdl_Std = Vhdl_87
                 and then (Func = Iir_Predefined_Array_Array_Concat
                             or Func = Iir_Predefined_Array_Element_Concat)
               then
                  --  LRM87 7.2.3 Adding Operators
                  --  The left bound if this result is the left bound of the
                  --  left operand, [...].  The direction of the result is the
                  --  direction of the left operand, unless the left operand
                  --  is a null array, in which case the direction of the
                  --  result is that of the right operand.
                  Result := Create_Array_Value (Len, 1);
                  Result.Bounds.D (1) := Create_Range_Value
                    (Left.Bounds.D (1).Left, null, Left.Bounds.D (1).Dir, Len);
                  Create_Right_Bound_From_Length (Result.Bounds.D (1), Len);
               else
                  --  Create the array result.
                  Result := Create_Array_Value (Len, 1);
                  Result.Bounds.D (1) := Create_Bounds_From_Length
                    (Block, Idx_Type, Len);
               end if;
               Check_Range_Constraints
                 (Block, Result.Bounds.D (1), Idx_Type, Expr);

               -- Fill the result: left.
               case Func is
                  when Iir_Predefined_Array_Array_Concat
                    | Iir_Predefined_Array_Element_Concat =>
                     for I in Left.Val_Array.V'Range loop
                        Result.Val_Array.V (I) := Left.Val_Array.V (I);
                     end loop;
                     Pos := Left.Val_Array.Len;
                  when Iir_Predefined_Element_Array_Concat
                    | Iir_Predefined_Element_Element_Concat =>
                     Result.Val_Array.V (1) := Left;
                     Pos := 1;
                  when others =>
                     raise Program_Error;
               end case;

               -- Note: here POS is equal to the position of the last element
               -- filled, or 0 if no elements were filled.

               --  Fill the result: right.
               case Func is
                  when Iir_Predefined_Array_Array_Concat
                    | Iir_Predefined_Element_Array_Concat =>
                     for I in Right.Val_Array.V'Range loop
                        Result.Val_Array.V (Pos + I) := Right.Val_Array.V (I);
                     end loop;
                  when Iir_Predefined_Array_Element_Concat
                    | Iir_Predefined_Element_Element_Concat =>
                     Result.Val_Array.V (Pos + 1) := Right;
                  when others =>
                     raise Program_Error;
               end case;
            end;

         when Iir_Predefined_Bit_And
           | Iir_Predefined_Boolean_And =>
            if Left.B1 = Lit_Enum_0.B1 then
               --  Short circuit operator.
               Result := Lit_Enum_0;
            else
               Eval_Right;
               Result := Boolean_To_Lit (Right.B1 = Lit_Enum_1.B1);
            end if;
         when Iir_Predefined_Bit_Nand
           | Iir_Predefined_Boolean_Nand =>
            if Left.B1 = Lit_Enum_0.B1 then
               --  Short circuit operator.
               Result := Lit_Enum_1;
            else
               Eval_Right;
               Result := Boolean_To_Lit (Right.B1 = Lit_Enum_0.B1);
            end if;
         when Iir_Predefined_Bit_Or
           | Iir_Predefined_Boolean_Or =>
            if Left.B1 = Lit_Enum_1.B1 then
               --  Short circuit operator.
               Result := Lit_Enum_1;
            else
               Eval_Right;
               Result := Boolean_To_Lit (Right.B1 = Lit_Enum_1.B1);
            end if;
         when Iir_Predefined_Bit_Nor
           | Iir_Predefined_Boolean_Nor =>
            if Left.B1 = Lit_Enum_1.B1 then
               --  Short circuit operator.
               Result := Lit_Enum_0;
            else
               Eval_Right;
               Result := Boolean_To_Lit (Right.B1 = Lit_Enum_0.B1);
            end if;
         when Iir_Predefined_Bit_Xor
           | Iir_Predefined_Boolean_Xor =>
            Eval_Right;
            Result := Boolean_To_Lit (Left.B1 /= Right.B1);
         when Iir_Predefined_Bit_Xnor
           | Iir_Predefined_Boolean_Xnor =>
            Eval_Right;
            Result := Boolean_To_Lit (Left.B1 = Right.B1);
         when Iir_Predefined_Bit_Not
           | Iir_Predefined_Boolean_Not =>
            Result := Boolean_To_Lit (Operand.B1 = Lit_Enum_0.B1);

         when Iir_Predefined_Bit_Condition =>
            Result := Boolean_To_Lit (Operand.B1 = Lit_Enum_1.B1);

         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra
           | Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            Eval_Right;
            Result := Execute_Shift_Operator (Left, Right.I64, Expr);

         when Iir_Predefined_Enum_Equality
           | Iir_Predefined_Integer_Equality
           | Iir_Predefined_Array_Equality
           | Iir_Predefined_Access_Equality
           | Iir_Predefined_Physical_Equality
           | Iir_Predefined_Floating_Equality
           | Iir_Predefined_Record_Equality
           | Iir_Predefined_Bit_Match_Equality
           | Iir_Predefined_Bit_Array_Match_Equality =>
            Eval_Right;
            Result := Boolean_To_Lit (Is_Equal (Left, Right));
         when Iir_Predefined_Enum_Inequality
           | Iir_Predefined_Integer_Inequality
           | Iir_Predefined_Array_Inequality
           | Iir_Predefined_Access_Inequality
           | Iir_Predefined_Physical_Inequality
           | Iir_Predefined_Floating_Inequality
           | Iir_Predefined_Record_Inequality
           | Iir_Predefined_Bit_Match_Inequality
           | Iir_Predefined_Bit_Array_Match_Inequality =>
            Eval_Right;
            Result := Boolean_To_Lit (not Is_Equal (Left, Right));
         when Iir_Predefined_Integer_Less
           | Iir_Predefined_Physical_Less
           | Iir_Predefined_Enum_Less =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) < Equal);
         when Iir_Predefined_Integer_Greater
           | Iir_Predefined_Physical_Greater
           | Iir_Predefined_Enum_Greater =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) > Equal);
         when Iir_Predefined_Integer_Less_Equal
           | Iir_Predefined_Physical_Less_Equal
           | Iir_Predefined_Enum_Less_Equal =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) <= Equal);
         when Iir_Predefined_Integer_Greater_Equal
           | Iir_Predefined_Physical_Greater_Equal
           | Iir_Predefined_Enum_Greater_Equal =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) >= Equal);

         when Iir_Predefined_Enum_Minimum
           | Iir_Predefined_Physical_Minimum =>
            Eval_Right;
            if Compare_Value (Left, Right) = Less then
               Result := Left;
            else
               Result := Right;
            end if;
         when Iir_Predefined_Enum_Maximum
           | Iir_Predefined_Physical_Maximum =>
            Eval_Right;
            if Compare_Value (Left, Right) = Less then
               Result := Right;
            else
               Result := Left;
            end if;

         when Iir_Predefined_Integer_Plus
           | Iir_Predefined_Physical_Plus =>
            Eval_Right;
            case Left.Kind is
               when Iir_Value_I64 =>
                  Result := Create_I64_Value (Left.I64 + Right.I64);
               when others =>
                  raise Internal_Error;
            end case;
         when Iir_Predefined_Integer_Minus
           | Iir_Predefined_Physical_Minus =>
            Eval_Right;
            case Left.Kind is
               when Iir_Value_I64 =>
                  Result := Create_I64_Value (Left.I64 - Right.I64);
               when others =>
                  raise Internal_Error;
            end case;
         when Iir_Predefined_Integer_Mul =>
            Eval_Right;
            case Left.Kind is
               when Iir_Value_I64 =>
                  Result := Create_I64_Value (Left.I64 * Right.I64);
               when others =>
                  raise Internal_Error;
            end case;
         when Iir_Predefined_Integer_Mod =>
            Eval_Right;
            case Left.Kind is
               when Iir_Value_I64 =>
                  if Right.I64 = 0 then
                     Error_Msg_Constraint (Expr);
                  end if;
                  Result := Create_I64_Value (Left.I64 mod Right.I64);
               when others =>
                  raise Internal_Error;
            end case;
         when Iir_Predefined_Integer_Rem =>
            Eval_Right;
            case Left.Kind is
               when Iir_Value_I64 =>
                  if Right.I64 = 0 then
                     Error_Msg_Constraint (Expr);
                  end if;
                  Result := Create_I64_Value (Left.I64 rem Right.I64);
               when others =>
                  raise Internal_Error;
            end case;
         when Iir_Predefined_Integer_Div =>
            Eval_Right;
            case Left.Kind is
               when Iir_Value_I64 =>
                  if Right.I64 = 0 then
                     Error_Msg_Constraint (Expr);
                  end if;
                  Result := Create_I64_Value (Left.I64 / Right.I64);
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Predefined_Integer_Absolute
           | Iir_Predefined_Physical_Absolute =>
            case Operand.Kind is
               when Iir_Value_I64 =>
                  Result := Create_I64_Value (abs Operand.I64);
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Predefined_Integer_Negation
           | Iir_Predefined_Physical_Negation =>
            case Operand.Kind is
               when Iir_Value_I64 =>
                  Result := Create_I64_Value (-Operand.I64);
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Predefined_Integer_Identity
           | Iir_Predefined_Physical_Identity =>
            case Operand.Kind is
               when Iir_Value_I64 =>
                  Result := Create_I64_Value (Operand.I64);
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Predefined_Integer_Exp =>
            Eval_Right;
            case Left.Kind is
               when Iir_Value_I64 =>
                  if Right.I64 < 0 then
                     Error_Msg_Constraint (Expr);
                  end if;
                  Result := Create_I64_Value (Left.I64 ** Natural (Right.I64));
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Predefined_Integer_Minimum =>
            Eval_Right;
            Result := Create_I64_Value (Ghdl_I64'Min (Left.I64, Right.I64));
         when Iir_Predefined_Integer_Maximum =>
            Eval_Right;
            Result := Create_I64_Value (Ghdl_I64'Max (Left.I64, Right.I64));

         when Iir_Predefined_Floating_Mul =>
            Eval_Right;
            Result := Create_F64_Value (Left.F64 * Right.F64);
         when Iir_Predefined_Floating_Div =>
            Eval_Right;
            Result := Create_F64_Value (Left.F64 / Right.F64);
         when Iir_Predefined_Floating_Minus =>
            Eval_Right;
            Result := Create_F64_Value (Left.F64 - Right.F64);
         when Iir_Predefined_Floating_Plus =>
            Eval_Right;
            Result := Create_F64_Value (Left.F64 + Right.F64);
         when Iir_Predefined_Floating_Exp =>
            Eval_Right;
            Result := Create_F64_Value (Left.F64 ** Integer (Right.I64));
         when Iir_Predefined_Floating_Identity =>
            Result := Create_F64_Value (Operand.F64);
         when Iir_Predefined_Floating_Negation =>
            Result := Create_F64_Value (-Operand.F64);
         when Iir_Predefined_Floating_Absolute =>
            Result := Create_F64_Value (abs (Operand.F64));
         when Iir_Predefined_Floating_Less =>
            Eval_Right;
            Result := Boolean_To_Lit (Left.F64 < Right.F64);
         when Iir_Predefined_Floating_Less_Equal =>
            Eval_Right;
            Result := Boolean_To_Lit (Left.F64 <= Right.F64);
         when Iir_Predefined_Floating_Greater =>
            Eval_Right;
            Result := Boolean_To_Lit (Left.F64 > Right.F64);
         when Iir_Predefined_Floating_Greater_Equal =>
            Eval_Right;
            Result := Boolean_To_Lit (Left.F64 >= Right.F64);

         when Iir_Predefined_Floating_Minimum =>
            Eval_Right;
            Result := Create_F64_Value (Ghdl_F64'Min (Left.F64, Right.F64));
         when Iir_Predefined_Floating_Maximum =>
            Eval_Right;
            Result := Create_F64_Value (Ghdl_F64'Max (Left.F64, Right.F64));

         when Iir_Predefined_Integer_Physical_Mul =>
            Eval_Right;
            Result := Create_I64_Value (Left.I64 * Right.I64);
         when Iir_Predefined_Physical_Integer_Mul =>
            Eval_Right;
            Result := Create_I64_Value (Left.I64 * Right.I64);
         when Iir_Predefined_Physical_Physical_Div =>
            Eval_Right;
            Result := Create_I64_Value (Left.I64 / Right.I64);
         when Iir_Predefined_Physical_Integer_Div =>
            Eval_Right;
            Result := Create_I64_Value (Left.I64 / Right.I64);
         when Iir_Predefined_Real_Physical_Mul =>
            Eval_Right;
            Result := Create_I64_Value
              (Ghdl_I64 (Left.F64 * Ghdl_F64 (Right.I64)));
         when Iir_Predefined_Physical_Real_Mul =>
            Eval_Right;
            Result := Create_I64_Value
              (Ghdl_I64 (Ghdl_F64 (Left.I64) * Right.F64));
         when Iir_Predefined_Physical_Real_Div =>
            Eval_Right;
            Result := Create_I64_Value
              (Ghdl_I64 (Ghdl_F64 (Left.I64) / Right.F64));

         when Iir_Predefined_Universal_I_R_Mul =>
            Eval_Right;
            Result := Create_F64_Value (Ghdl_F64 (Left.I64) * Right.F64);
         when Iir_Predefined_Universal_R_I_Mul =>
            Eval_Right;
            Result := Create_F64_Value (Left.F64 * Ghdl_F64 (Right.I64));

         when Iir_Predefined_TF_Array_And =>
            Eval_Array;
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 and Right.Val_Array.V (I).B1;
            end loop;
         when Iir_Predefined_TF_Array_Nand =>
            Eval_Array;
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 and Right.Val_Array.V (I).B1);
            end loop;
         when Iir_Predefined_TF_Array_Or =>
            Eval_Array;
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 or Right.Val_Array.V (I).B1;
            end loop;
         when Iir_Predefined_TF_Array_Nor =>
            Eval_Array;
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 or Right.Val_Array.V (I).B1);
            end loop;
         when Iir_Predefined_TF_Array_Xor =>
            Eval_Array;
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 xor Right.Val_Array.V (I).B1;
            end loop;
         when Iir_Predefined_TF_Array_Xnor =>
            Eval_Array;
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 xor Right.Val_Array.V (I).B1);
            end loop;

         when Iir_Predefined_TF_Array_Element_And =>
            Eval_Right;
            Result := Unshare (Left, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 and Right.B1;
            end loop;
         when Iir_Predefined_TF_Element_Array_And =>
            Eval_Right;
            Result := Unshare (Right, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 and Left.B1;
            end loop;

         when Iir_Predefined_TF_Array_Element_Or =>
            Eval_Right;
            Result := Unshare (Left, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 or Right.B1;
            end loop;
         when Iir_Predefined_TF_Element_Array_Or =>
            Eval_Right;
            Result := Unshare (Right, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 or Left.B1;
            end loop;

         when Iir_Predefined_TF_Array_Element_Xor =>
            Eval_Right;
            Result := Unshare (Left, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 xor Right.B1;
            end loop;
         when Iir_Predefined_TF_Element_Array_Xor =>
            Eval_Right;
            Result := Unshare (Right, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 Result.Val_Array.V (I).B1 xor Left.B1;
            end loop;

         when Iir_Predefined_TF_Array_Element_Nand =>
            Eval_Right;
            Result := Unshare (Left, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 and Right.B1);
            end loop;
         when Iir_Predefined_TF_Element_Array_Nand =>
            Eval_Right;
            Result := Unshare (Right, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 and Left.B1);
            end loop;

         when Iir_Predefined_TF_Array_Element_Nor =>
            Eval_Right;
            Result := Unshare (Left, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 or Right.B1);
            end loop;
         when Iir_Predefined_TF_Element_Array_Nor =>
            Eval_Right;
            Result := Unshare (Right, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 or Left.B1);
            end loop;

         when Iir_Predefined_TF_Array_Element_Xnor =>
            Eval_Right;
            Result := Unshare (Left, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 xor Right.B1);
            end loop;
         when Iir_Predefined_TF_Element_Array_Xnor =>
            Eval_Right;
            Result := Unshare (Right, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 :=
                 not (Result.Val_Array.V (I).B1 xor Left.B1);
            end loop;

         when Iir_Predefined_TF_Array_Not =>
            --  Need to copy as the result is modified.
            Result := Unshare (Operand, Expr_Pool'Access);
            for I in Result.Val_Array.V'Range loop
               Result.Val_Array.V (I).B1 := not Result.Val_Array.V (I).B1;
            end loop;

         when Iir_Predefined_TF_Reduction_And =>
            Result := Create_B1_Value (True);
            for I in Operand.Val_Array.V'Range loop
               Result.B1 := Result.B1 and Operand.Val_Array.V (I).B1;
            end loop;
         when Iir_Predefined_TF_Reduction_Nand =>
            Result := Create_B1_Value (True);
            for I in Operand.Val_Array.V'Range loop
               Result.B1 := Result.B1 and Operand.Val_Array.V (I).B1;
            end loop;
            Result.B1 := not Result.B1;
         when Iir_Predefined_TF_Reduction_Or =>
            Result := Create_B1_Value (False);
            for I in Operand.Val_Array.V'Range loop
               Result.B1 := Result.B1 or Operand.Val_Array.V (I).B1;
            end loop;
         when Iir_Predefined_TF_Reduction_Nor =>
            Result := Create_B1_Value (False);
            for I in Operand.Val_Array.V'Range loop
               Result.B1 := Result.B1 or Operand.Val_Array.V (I).B1;
            end loop;
            Result.B1 := not Result.B1;
         when Iir_Predefined_TF_Reduction_Xor =>
            Result := Create_B1_Value (False);
            for I in Operand.Val_Array.V'Range loop
               Result.B1 := Result.B1 xor Operand.Val_Array.V (I).B1;
            end loop;
         when Iir_Predefined_TF_Reduction_Xnor =>
            Result := Create_B1_Value (False);
            for I in Operand.Val_Array.V'Range loop
               Result.B1 := Result.B1 xor Operand.Val_Array.V (I).B1;
            end loop;
            Result.B1 := not Result.B1;

         when Iir_Predefined_Bit_Rising_Edge
           | Iir_Predefined_Boolean_Rising_Edge =>
            return Boolean_To_Lit
              (Execute_Event_Attribute (Operand)
                 and then Execute_Signal_Value (Operand).B1 = True);
         when Iir_Predefined_Bit_Falling_Edge
           | Iir_Predefined_Boolean_Falling_Edge =>
            return Boolean_To_Lit
              (Execute_Event_Attribute (Operand)
                 and then Execute_Signal_Value (Operand).B1 = False);

         when Iir_Predefined_Array_Greater =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) = Greater);

         when Iir_Predefined_Array_Greater_Equal =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) >= Equal);

         when Iir_Predefined_Array_Less =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) = Less);

         when Iir_Predefined_Array_Less_Equal =>
            Eval_Right;
            Result := Boolean_To_Lit (Compare_Value (Left, Right) <= Equal);

         when Iir_Predefined_Array_Minimum =>
            Eval_Right;
            if Compare_Value (Left, Right) = Less then
               Result := Left;
            else
               Result := Right;
            end if;
         when Iir_Predefined_Array_Maximum =>
            Eval_Right;
            if Compare_Value (Left, Right) = Less then
               Result := Right;
            else
               Result := Left;
            end if;

         when Iir_Predefined_Vector_Maximum =>
            declare
               El_St : constant Iir :=
                 Get_Return_Type (Get_Implementation (Expr));
               V : Iir_Value_Literal_Acc;
            begin
               Result := Execute_Low_Limit (Execute_Bounds (Block, El_St));
               for I in Left.Val_Array.V'Range loop
                  V := Left.Val_Array.V (I);
                  if Compare_Value (V, Result) = Greater then
                     Result := V;
                  end if;
               end loop;
            end;
         when Iir_Predefined_Vector_Minimum =>
            declare
               El_St : constant Iir :=
                 Get_Return_Type (Get_Implementation (Expr));
               V : Iir_Value_Literal_Acc;
            begin
               Result := Execute_High_Limit (Execute_Bounds (Block, El_St));
               for I in Left.Val_Array.V'Range loop
                  V := Left.Val_Array.V (I);
                  if Compare_Value (V, Result) = Less then
                     Result := V;
                  end if;
               end loop;
            end;

         when Iir_Predefined_Endfile =>
            Result := Boolean_To_Lit (File_Operation.Endfile (Left, Null_Iir));

         when Iir_Predefined_Now_Function =>
            Result := Create_I64_Value (Ghdl_I64 (Grt.Types.Current_Time));

         when Iir_Predefined_Integer_To_String
           | Iir_Predefined_Floating_To_String
           | Iir_Predefined_Physical_To_String =>
            Result := String_To_Iir_Value
              (Execute_Image_Attribute (Left, Get_Type (Left_Param)));

         when Iir_Predefined_Enum_To_String =>
            declare
               use Name_Table;
               Base_Type : constant Iir :=
                 Get_Base_Type (Get_Type (Left_Param));
               Lits : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Base_Type);
               Pos : constant Natural := Get_Enum_Pos (Left);
               Id : Name_Id;
            begin
               if Base_Type = Vhdl.Std_Package.Character_Type_Definition then
                  Result := String_To_Iir_Value ((1 => Character'Val (Pos)));
               else
                  Id := Get_Identifier (Get_Nth_Element (Lits, Pos));
                  if Is_Character (Id) then
                     Result := String_To_Iir_Value ((1 => Get_Character (Id)));
                  else
                     declare
                        Img : String := Image (Id);
                     begin
                        if Img (Img'First) = '\' then
                           --  Reformat extended identifiers for to_image.
                           pragma Assert (Img (Img'Last) = '\');
                           declare
                              Npos : Natural;
                              K : Natural;
                              C : Character;
                           begin
                              Npos := Img'First;
                              K := Npos + 1;
                              while K < Img'Last loop
                                 C := Img (K);
                                 Img (Npos) := C;
                                 Npos := Npos + 1;
                                 if C = '\' then
                                    K := K + 2;
                                 else
                                    K := K + 1;
                                 end if;
                              end loop;
                              Result := String_To_Iir_Value
                                (Img (Img'First .. Npos - 1));
                           end;
                        else
                           Result := String_To_Iir_Value (Img);
                        end if;
                     end;
                  end if;
               end if;
            end;

         when Iir_Predefined_Array_Char_To_String =>
            declare
               Lits : constant Iir_Flist :=
                 Get_Enumeration_Literal_List
                 (Get_Base_Type
                    (Get_Element_Subtype (Get_Type (Left_Param))));
               Str : String (1 .. Natural (Left.Bounds.D (1).Length));
               Pos : Natural;
            begin
               for I in Left.Val_Array.V'Range loop
                  Pos := Get_Enum_Pos (Left.Val_Array.V (I));
                  Str (Positive (I)) := Name_Table.Get_Character
                    (Get_Identifier (Get_Nth_Element (Lits, Pos)));
               end loop;
               Result := String_To_Iir_Value (Str);
            end;

         when Iir_Predefined_Bit_Vector_To_Hstring =>
            return Execute_Bit_Vector_To_String (Left, 4);

         when Iir_Predefined_Bit_Vector_To_Ostring =>
            return Execute_Bit_Vector_To_String (Left, 3);

         when Iir_Predefined_Real_To_String_Digits =>
            Eval_Right;
            declare
               Str : Grt.To_Strings.String_Real_Format;
               Last : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, Last, Left.F64, Ghdl_I32 (Right.I64));
               Result := String_To_Iir_Value (Str (1 .. Last));
            end;
         when Iir_Predefined_Real_To_String_Format =>
            Eval_Right;
            declare
               Format : String (1 .. Natural (Right.Val_Array.Len) + 1);
               Str : Grt.To_Strings.String_Real_Format;
               Last : Natural;
            begin
               for I in Right.Val_Array.V'Range loop
                  Format (Positive (I)) :=
                    Character'Val (Right.Val_Array.V (I).E8);
               end loop;
               Format (Format'Last) := ASCII.NUL;
               Grt.To_Strings.To_String
                 (Str, Last, Left.F64, To_Ghdl_C_String (Format'Address));
               Result := String_To_Iir_Value (Str (1 .. Last));
            end;
         when Iir_Predefined_Time_To_String_Unit =>
            Eval_Right;
            declare
               Str : Grt.To_Strings.String_Time_Unit;
               First : Natural;
               Unit : Iir;
            begin
               Unit := Get_Unit_Chain (Vhdl.Std_Package.Time_Type_Definition);
               while Unit /= Null_Iir loop
                  exit when Vhdl.Evaluation.Get_Physical_Value (Unit)
                    = Int64 (Right.I64);
                  Unit := Get_Chain (Unit);
               end loop;
               if Unit = Null_Iir then
                  Error_Msg_Exec
                    ("to_string for time called with wrong unit", Expr);
               end if;
               Grt.To_Strings.To_String (Str, First, Left.I64, Right.I64);
               Result := String_To_Iir_Value
                 (Str (First .. Str'Last) & ' '
                    & Name_Table.Image (Get_Identifier (Unit)));
            end;

         when Iir_Predefined_Std_Ulogic_Match_Equality =>
            Eval_Right;
            declare
               use Grt.Std_Logic_1164;
            begin
               Result := Create_E8_Value
                 (Std_Ulogic'Pos
                    (Match_Eq_Table (Std_Ulogic'Val (Left.E8),
                                     Std_Ulogic'Val (Right.E8))));
            end;
         when Iir_Predefined_Std_Ulogic_Match_Inequality =>
            Eval_Right;
            declare
               use Grt.Std_Logic_1164;
            begin
               Result := Create_E8_Value
                 (Std_Ulogic'Pos
                    (Not_Table (Match_Eq_Table (Std_Ulogic'Val (Left.E8),
                                                Std_Ulogic'Val (Right.E8)))));
            end;
         when Iir_Predefined_Std_Ulogic_Match_Ordering_Functions =>
            Eval_Right;
            declare
               use Grt.Std_Logic_1164;
               L : constant Std_Ulogic := Std_Ulogic'Val (Left.E8);
               R : constant Std_Ulogic := Std_Ulogic'Val (Right.E8);
               Res : Std_Ulogic;
            begin
               Check_Std_Ulogic_Dc (Expr, L);
               Check_Std_Ulogic_Dc (Expr, R);
               case Iir_Predefined_Std_Ulogic_Match_Ordering_Functions (Func)
                  is
                  when Iir_Predefined_Std_Ulogic_Match_Less =>
                     Res := Match_Lt_Table (L, R);
                  when Iir_Predefined_Std_Ulogic_Match_Less_Equal =>
                     Res := Or_Table (Match_Lt_Table (L, R),
                                      Match_Eq_Table (L, R));
                  when Iir_Predefined_Std_Ulogic_Match_Greater =>
                     Res := Not_Table (Or_Table (Match_Lt_Table (L, R),
                                                 Match_Eq_Table (L, R)));
                  when Iir_Predefined_Std_Ulogic_Match_Greater_Equal =>
                     Res := Not_Table (Match_Lt_Table (L, R));
               end case;
               Result := Create_E8_Value (Std_Ulogic'Pos (Res));
            end;

         when Iir_Predefined_Std_Ulogic_Array_Match_Equality
           | Iir_Predefined_Std_Ulogic_Array_Match_Inequality =>
            Eval_Right;
            if Left.Bounds.D (1).Length /= Right.Bounds.D (1).Length then
               Error_Msg_Constraint (Expr);
            end if;
            declare
               use Grt.Std_Logic_1164;
               Res : Std_Ulogic := '1';
               Le, Re : Std_Ulogic;
               Has_Match_Err : Boolean;
            begin
               Has_Match_Err := False;
               for I in Left.Val_Array.V'Range loop
                  Le := Std_Ulogic'Val (Left.Val_Array.V (I).E8);
                  Re := Std_Ulogic'Val (Right.Val_Array.V (I).E8);
                  if (Le = '-' or Re = '-') and then not Has_Match_Err then
                     Assert_Std_Ulogic_Dc (Expr);
                     Has_Match_Err := True;
                  end if;
                  Res := And_Table (Res, Match_Eq_Table (Le, Re));
               end loop;
               if Func = Iir_Predefined_Std_Ulogic_Array_Match_Inequality then
                  Res := Not_Table (Res);
               end if;
               Result := Create_E8_Value (Std_Ulogic'Pos (Res));
            end;

         when others =>
            Error_Msg_Elab (Expr, "execute_implicit_function: unimplemented " &
                              Iir_Predefined_Functions'Image (Func));
            raise Internal_Error;
      end case;
      return Result;
   exception
      when Constraint_Error =>
         Error_Msg_Constraint (Expr);
   end Execute_Implicit_Function;

   procedure Execute_Implicit_Procedure
     (Block: Block_Instance_Acc; Stmt: Iir_Procedure_Call)
   is
      Imp : constant Iir := Get_Implementation (Stmt);
      Assoc_Chain : constant Iir := Get_Parameter_Association_Chain (Stmt);
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);
      Assoc: Iir;
      Formal : Iir;
      Val : Iir;
      Args: Iir_Value_Literal_Array (0 .. 3);
      Expr_Mark : Mark_Type;
   begin
      Mark (Expr_Mark, Expr_Pool);
      Assoc := Assoc_Chain;
      Formal := Inter_Chain;
      for I in Iir_Index32 loop
         exit when Assoc = Null_Iir;
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Expression =>
               Val := Get_Actual (Assoc);
            when Iir_Kind_Association_Element_Open =>
               Val := Get_Default_Value (Formal);
            when others =>
               raise Internal_Error;
         end case;
         Args (I) := Execute_Expression (Block, Val);
         Assoc := Get_Chain (Assoc);
         Formal := Get_Chain (Formal);
      end loop;
      case Get_Implicit_Definition (Imp) is
         when Iir_Predefined_Deallocate =>
            if Args (0).Val_Access /= null then
               Free_Heap_Value (Args (0));
               Args (0).Val_Access := null;
            end if;
         when Iir_Predefined_File_Open =>
            File_Operation.File_Open
              (Args (0), Args (1), Args (2), Inter_Chain, Stmt);
         when Iir_Predefined_File_Open_Status =>
            File_Operation.File_Open_Status
              (Args (0), Args (1), Args (2), Args (3),
               Get_Chain (Inter_Chain), Stmt);
         when Iir_Predefined_Write =>
            if Get_Text_File_Flag (Get_Type (Inter_Chain)) then
               File_Operation.Write_Text (Args (0), Args (1));
            else
               File_Operation.Write_Binary (Args (0), Args (1));
            end if;
         when Iir_Predefined_Read_Length =>
            if Get_Text_File_Flag (Get_Type (Inter_Chain)) then
               File_Operation.Read_Length_Text
                 (Args (0), Args (1), Args (2));
            else
               File_Operation.Read_Length_Binary
                 (Args (0), Args (1), Args (2));
            end if;
         when Iir_Predefined_Read =>
            File_Operation.Read_Binary (Args (0), Args (1));
         when Iir_Predefined_Flush =>
            File_Operation.Flush (Args (0));
         when Iir_Predefined_File_Close =>
            if Get_Text_File_Flag (Get_Type (Inter_Chain)) then
               File_Operation.File_Close_Text (Args (0), Stmt);
            else
               File_Operation.File_Close_Binary (Args (0), Stmt);
            end if;
         when others =>
            Error_Kind ("execute_implicit_procedure",
                        Get_Implicit_Definition (Imp));
      end case;
      Release (Expr_Mark, Expr_Pool);
   end Execute_Implicit_Procedure;

   procedure Execute_Foreign_Procedure
     (Block: Block_Instance_Acc; Stmt: Iir_Procedure_Call)
   is
      Imp : constant Iir := Get_Implementation (Stmt);
      Assoc_Chain : constant Iir := Get_Parameter_Association_Chain (Stmt);
      Assoc: Iir;
      Args: Iir_Value_Literal_Array (0 .. 3) := (others => null);
      Expr_Mark : Mark_Type;
   begin
      Mark (Expr_Mark, Expr_Pool);
      Assoc := Assoc_Chain;
      for I in Args'Range loop
         exit when Assoc = Null_Iir;
         Args (I) := Execute_Expression (Block, Get_Actual (Assoc));
         Assoc := Get_Chain (Assoc);
      end loop;
      case Get_Identifier (Imp) is
         when Std_Names.Name_Untruncated_Text_Read =>
            File_Operation.Untruncated_Text_Read
              (Args (0), Args (1), Args (2));
         when Std_Names.Name_Control_Simulation =>
            --  FIXME: handle stop properly.
            --  FIXME: this is the only place where longjump is called.
            Grt.Lib.Ghdl_Control_Simulation
              (Args (0).B1, Args (1).B1, Std_Integer (Args (2).I64));
            --  Do not return.
         when Std_Names.Name_Textio_Write_Real =>
            File_Operation.Textio_Write_Real
              (Args (0), Args (1), Args (2).F64, Std_Integer (Args (3).I64));
         when others =>
            Error_Msg_Exec ("unsupported foreign procedure call", Stmt);
      end case;
      Release (Expr_Mark, Expr_Pool);
   end Execute_Foreign_Procedure;

   -- Compute the offset for INDEX into a range BOUNDS.
   -- EXPR is only used in case of error.
   function Get_Index_Offset
     (Index: Iir_Value_Literal_Acc;
      Bounds: Iir_Value_Literal_Acc;
      Expr: Iir)
      return Iir_Index32
   is
      Left_Pos, Right_Pos: Iir_Value_Literal_Acc;
   begin
      Left_Pos := Bounds.Left;
      Right_Pos := Bounds.Right;
      if Index.Kind /= Left_Pos.Kind or else Index.Kind /= Right_Pos.Kind then
         raise Internal_Error;
      end if;
      case Iir_Value_Discrete (Index.Kind) is
         when Iir_Value_B1 =>
            case Bounds.Dir is
               when Dir_To =>
                  if Index.B1 >= Left_Pos.B1 and then
                    Index.B1 <= Right_Pos.B1
                  then
                     -- to
                     return Ghdl_B1'Pos (Index.B1) - Ghdl_B1'Pos (Left_Pos.B1);
                  end if;
               when Dir_Downto =>
                  if Index.B1 <= Left_Pos.B1 and then
                    Index.B1 >= Right_Pos.B1
                  then
                     -- downto
                     return Ghdl_B1'Pos (Left_Pos.B1) - Ghdl_B1'Pos (Index.B1);
                  end if;
            end case;
         when Iir_Value_E8 =>
            case Bounds.Dir is
               when Dir_To =>
                  if Index.E8 >= Left_Pos.E8 and then
                    Index.E8 <= Right_Pos.E8
                  then
                     -- to
                     return Iir_Index32 (Index.E8 - Left_Pos.E8);
                  end if;
               when Dir_Downto =>
                  if Index.E8 <= Left_Pos.E8 and then
                    Index.E8 >= Right_Pos.E8
                  then
                     -- downto
                     return Iir_Index32 (Left_Pos.E8 - Index.E8);
                  end if;
            end case;
         when Iir_Value_E32 =>
            case Bounds.Dir is
               when Dir_To =>
                  if Index.E32 >= Left_Pos.E32 and then
                    Index.E32 <= Right_Pos.E32
                  then
                     -- to
                     return Iir_Index32 (Index.E32 - Left_Pos.E32);
                  end if;
               when Dir_Downto =>
                  if Index.E32 <= Left_Pos.E32 and then
                    Index.E32 >= Right_Pos.E32
                  then
                     -- downto
                     return Iir_Index32 (Left_Pos.E32 - Index.E32);
                  end if;
            end case;
         when Iir_Value_I64 =>
            case Bounds.Dir is
               when Dir_To =>
                  if Index.I64 >= Left_Pos.I64 and then
                    Index.I64 <= Right_Pos.I64
                  then
                     -- to
                     return Iir_Index32 (Index.I64 - Left_Pos.I64);
                  end if;
               when Dir_Downto =>
                  if Index.I64 <= Left_Pos.I64 and then
                    Index.I64 >= Right_Pos.I64
                  then
                     -- downto
                     return Iir_Index32 (Left_Pos.I64 - Index.I64);
                  end if;
            end case;
      end case;
      Error_Msg_Constraint (Expr);
      return 0;
   end Get_Index_Offset;

   --  Create an iir_value_literal of kind iir_value_array and of life LIFE.
   --  Allocate the array of bounds, and fill it from A_TYPE.
   --  Allocate the array of values.
   function Create_Array_Bounds_From_Type (Block : Block_Instance_Acc;
                                           A_Type : Iir;
                                           Create_Val_Array : Boolean)
                                          return Iir_Value_Literal_Acc
   is
      --  Only for constrained subtypes.
      pragma Assert (Get_Constraint_State (A_Type) = Fully_Constrained);

      Index_List : constant Iir_Flist := Get_Index_Subtype_List (A_Type);
      Res : Iir_Value_Literal_Acc;
      Len : Iir_Index32;
      Bound : Iir_Value_Literal_Acc;
   begin
      Res := Create_Array_Value (Iir_Index32 (Get_Nbr_Elements (Index_List)));
      Len := 1;
      for I in 1 .. Res.Bounds.Nbr_Dims loop
         Bound := Execute_Bounds
           (Block, Get_Nth_Element (Index_List, Natural (I - 1)));
         Len := Len * Bound.Length;
         Res.Bounds.D (I) := Bound;
      end loop;
      if Create_Val_Array then
         Create_Array_Data (Res, Len);
      end if;
      return Res;
   end Create_Array_Bounds_From_Type;

   --  Return the steps (ie, offset in the array when index DIM is increased
   --  by one) for array ARR and dimension DIM.
   function Get_Step_For_Dim (Arr: Iir_Value_Literal_Acc; Dim : Natural)
     return Iir_Index32
   is
      Bounds : Value_Bounds_Array_Acc renames Arr.Bounds;
      Res : Iir_Index32;
   begin
      Res := 1;
      for I in Iir_Index32 (Dim + 1) .. Bounds.Nbr_Dims loop
         Res := Res * Bounds.D (I).Length;
      end loop;
      return Res;
   end Get_Step_For_Dim;

   --  Create a literal for a string or a bit_string
   function String_To_Enumeration_Array_1 (Str: Iir; El_Type : Iir)
                                          return Iir_Value_Literal_Acc
   is
      pragma Assert (Get_Kind (Str) = Iir_Kind_String_Literal8);
      Id : constant String8_Id := Get_String8_Id (Str);
      Len : constant Iir_Index32 := Iir_Index32 (Get_String_Length (Str));

      El_Btype : constant Iir := Get_Base_Type (El_Type);

      Lit: Iir_Value_Literal_Acc;
      El : Iir_Value_Literal_Acc;

      Pos : Nat8;
   begin
      Lit := Create_Array_Value (Len, 1);

      for I in Lit.Val_Array.V'Range loop
         -- FIXME: use literal from type ??
         Pos := Str_Table.Element_String8 (Id, Pos32 (I));
         El := Create_Enum_Value (Natural (Pos), El_Btype);
         Lit.Val_Array.V (I) := El;
      end loop;

      return Lit;
   end String_To_Enumeration_Array_1;

   --  Create a literal for a string or a bit_string
   function Execute_String_Literal (Str: Iir; Block_Type : Block_Instance_Acc)
                                   return Iir_Value_Literal_Acc
   is
      Array_Type: constant Iir := Get_Type (Str);
      Index_Types : constant Iir_Flist := Get_Index_Subtype_List (Array_Type);
      Res : Iir_Value_Literal_Acc;
   begin
      --  Array must be unidimensional.
      pragma Assert (Get_Nbr_Elements (Index_Types) = 1);

      Res := String_To_Enumeration_Array_1
        (Str, Get_Element_Subtype (Array_Type));

      --  When created from static evaluation, a string may still have an
      --  unconstrained type.
      if Get_Constraint_State (Array_Type) /= Fully_Constrained then
         Res.Bounds.D (1) :=
           Create_Range_Value (Create_I64_Value (1),
                               Create_I64_Value (Ghdl_I64 (Res.Val_Array.Len)),
                               Dir_To,
                               Res.Val_Array.Len);
      else
         Res.Bounds.D (1) :=
           Execute_Bounds (Block_Type, Get_Nth_Element (Index_Types, 0));
      end if;

      --  The range may not be statically constant.
      if Res.Bounds.D (1).Length /= Res.Val_Array.Len then
         Error_Msg_Constraint (Str);
      end if;

      return Res;
   end Execute_String_Literal;

   --  Fill LENGTH elements of RES, starting at ORIG by steps of STEP.
   --  Use expressions from (BLOCK, AGGREGATE) to fill the elements.
   --  EL_TYPE is the type of the array element.
   procedure Fill_Array_Aggregate_1 (Block : Block_Instance_Acc;
                                     Aggregate : Iir;
                                     Res : Iir_Value_Literal_Acc;
                                     Orig : Iir_Index32;
                                     Step : Iir_Index32;
                                     Dim : Iir_Index32;
                                     Nbr_Dim : Iir_Index32;
                                     El_Type : Iir)
   is
      Value : Iir;
      Bound : constant Iir_Value_Literal_Acc := Res.Bounds.D (Dim);
      Length : constant Iir_Index32 := Bound.Length;

      procedure Set_Elem (Pos : Iir_Index32)
      is
         Val : Iir_Value_Literal_Acc;
      begin
         if Dim = Nbr_Dim then
            --  VALUE is an expression (which may be an aggregate, but not
            --  a sub-aggregate.
            Val := Execute_Expression_With_Type (Block, Value, El_Type);
            --  LRM93 7.3.2.2
            --  For a multi-dimensional aggregate of dimension n, a check
            --  is made that all (n-1)-dimensional subaggregates have the
            --  same bounds.
            --  GHDL: I have added an implicit array conversion, however
            --   it may be useful to allow cases like this:
            --     type str_array is array (natural range <>)
            --        of string (10 downto 1);
            --     constant floats : str_array :=
            --         ( "00000000.0", HT & "+1.5ABCDE");
            --   The subtype of the first sub-aggregate (0.0) is
            --   determinated by the context, according to rule 9 and 4
            --   of LRM93 7.3.2.2 and therefore is string (10 downto 1),
            --   while the subtype of the second sub-aggregate (HT & ...)
            --   is determinated by rules 1 and 2 of LRM 7.2.4, and is
            --   string (1 to 10).
            --   Unless an implicit conversion is used, according to the
            --   LRM, this should fail, but it makes no sens.
            --
            --   FIXME: Add a warning, a flag ?
            --Implicit_Array_Conversion (Block, Val, El_Type, Value);
            --Check_Constraints (Block, Val, El_Type, Value);
            Res.Val_Array.V (1 + Orig + Pos * Step) := Val;
         else
            case Get_Kind (Value) is
               when Iir_Kind_Aggregate =>
                  --  VALUE is a sub-aggregate.
                  Fill_Array_Aggregate_1 (Block, Value, Res,
                                          Orig + Pos * Step,
                                          Step / Res.Bounds.D (Dim + 1).Length,
                                          Dim + 1, Nbr_Dim, El_Type);
               when Iir_Kind_String_Literal8 =>
                  pragma Assert (Dim + 1 = Nbr_Dim);
                  Val := String_To_Enumeration_Array_1 (Value, El_Type);
                  if Val.Val_Array.Len /= Res.Bounds.D (Nbr_Dim).Length then
                     Error_Msg_Constraint (Value);
                  end if;
                  for I in Val.Val_Array.V'Range loop
                     Res.Val_Array.V (Orig + Pos * Step + I) :=
                       Val.Val_Array.V (I);
                  end loop;
               when others =>
                  Error_Kind ("fill_array_aggregate_1", Value);
            end case;
         end if;
      end Set_Elem;

      procedure Set_Elem_By_Expr (Expr : Iir)
      is
         Expr_Pos: Iir_Value_Literal_Acc;
      begin
         Expr_Pos := Execute_Expression (Block, Expr);
         Set_Elem (Get_Index_Offset (Expr_Pos, Bound, Expr));
      end Set_Elem_By_Expr;

      procedure Set_Elem_By_Range (Expr : Iir)
      is
         A_Range : Iir_Value_Literal_Acc;
         High, Low : Iir_Value_Literal_Acc;
      begin
         A_Range := Execute_Bounds (Block, Expr);
         if Is_Null_Range (A_Range) then
            return;
         end if;
         if A_Range.Dir = Dir_To then
            High := A_Range.Right;
            Low := A_Range.Left;
         else
            High := A_Range.Left;
            Low := A_Range.Right;
         end if;

         --  Locally modified (incremented)
         Low := Unshare (Low, Expr_Pool'Access);

         loop
            Set_Elem (Get_Index_Offset (Low, Bound, Expr));
            exit when Is_Equal (Low, High);
            Increment (Low);
         end loop;
      end Set_Elem_By_Range;

      Assoc : Iir;
      Pos : Iir_Index32;
   begin
      Assoc := Get_Association_Choices_Chain (Aggregate);
      Pos := 0;
      while Assoc /= Null_Iir loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  if Get_Element_Type_Flag (Assoc) then
                     if Pos >= Length then
                        Error_Msg_Constraint (Assoc);
                     end if;

                     Set_Elem (Pos);
                     Pos := Pos + 1;
                  else
                     declare
                        Val : Iir_Value_Literal_Acc;
                     begin
                        Val := Execute_Expression (Block, Value);
                        pragma Assert (Val.Kind = Iir_Value_Array);
                        pragma Assert (Val.Bounds.Nbr_Dims = 1);
                        for I in 1 .. Val.Val_Array.Len loop
                           if Pos >= Length then
                              Error_Msg_Constraint (Assoc);
                           end if;
                           Res.Val_Array.V (1 + Orig + Pos * Step) :=
                             Val.Val_Array.V (I);
                           Pos := Pos + 1;
                        end loop;
                     end;
                  end if;
               when Iir_Kind_Choice_By_Expression =>
                  Set_Elem_By_Expr (Get_Choice_Expression (Assoc));
               when Iir_Kind_Choice_By_Range =>
                  Set_Elem_By_Range (Get_Choice_Range (Assoc));
               when Iir_Kind_Choice_By_Others =>
                  for J in 1 .. Length loop
                     if Res.Val_Array.V (Orig + J * Step) = null then
                        Set_Elem (J - 1);
                     end if;
                  end loop;
                  return;
               when others =>
                  raise Internal_Error;
            end case;
            Assoc := Get_Chain (Assoc);
            exit when Assoc = Null_Iir;
            exit when not Get_Same_Alternative_Flag (Assoc);
         end loop;
      end loop;

      --  Check each elements have been set.
      --  FIXME: check directly with type.
      for J in 1 .. Length loop
         if Res.Val_Array.V (Orig + J * Step) = null then
            Error_Msg_Constraint (Aggregate);
         end if;
      end loop;
   end Fill_Array_Aggregate_1;

   --  Use expressions from (BLOCK, AGGREGATE) to fill RES.
   procedure Fill_Array_Aggregate (Block : Block_Instance_Acc;
                                   Aggregate : Iir;
                                   Res : Iir_Value_Literal_Acc)
   is
      Aggr_Type : constant Iir := Get_Type (Aggregate);
      El_Type : constant Iir := Get_Element_Subtype (Aggr_Type);
      Index_List : constant Iir_Flist := Get_Index_Subtype_List (Aggr_Type);
      Nbr_Dim : constant Iir_Index32 :=
        Iir_Index32 (Get_Nbr_Elements (Index_List));
      Step : Iir_Index32;
   begin
      Step := Get_Step_For_Dim (Res, 1);
      Fill_Array_Aggregate_1
        (Block, Aggregate, Res, 0, Step, 1, Nbr_Dim, El_Type);
   end Fill_Array_Aggregate;

   function Execute_Record_Aggregate (Block: Block_Instance_Acc;
                                      Aggregate: Iir;
                                      Aggregate_Type: Iir)
                                     return Iir_Value_Literal_Acc
   is
      List : constant Iir_Flist :=
        Get_Elements_Declaration_List (Get_Base_Type (Aggregate_Type));

      Res: Iir_Value_Literal_Acc;
      Expr : Iir;

      procedure Set_Expr (Pos : Iir_Index32) is
         El : constant Iir := Get_Nth_Element (List, Natural (Pos - 1));
      begin
         Res.Val_Record.V (Pos) :=
           Execute_Expression_With_Type (Block, Expr, Get_Type (El));
      end Set_Expr;

      Pos : Iir_Index32;
      Assoc: Iir;
      N_Expr : Iir;
   begin
      Res := Create_Record_Value (Iir_Index32 (Get_Nbr_Elements (List)));

      Assoc := Get_Association_Choices_Chain (Aggregate);
      Pos := 1;
      loop
         N_Expr := Get_Associated_Expr (Assoc);
         if N_Expr /= Null_Iir then
            Expr := N_Expr;
         end if;
         case Get_Kind (Assoc) is
            when Iir_Kind_Choice_By_None =>
               Set_Expr (Pos);
               Pos := Pos + 1;
            when Iir_Kind_Choice_By_Name =>
               Set_Expr (1 + Get_Element_Position
                           (Get_Named_Entity (Get_Choice_Name (Assoc))));
            when Iir_Kind_Choice_By_Others =>
               for I in Res.Val_Record.V'Range loop
                  if Res.Val_Record.V (I) = null then
                     Set_Expr (I);
                  end if;
               end loop;
            when others =>
               Error_Kind ("execute_record_aggregate", Assoc);
         end case;
         Assoc := Get_Chain (Assoc);
         exit when Assoc = Null_Iir;
      end loop;
      return Res;
   end Execute_Record_Aggregate;

   function Execute_Aggregate (Block: Block_Instance_Acc;
                               Aggregate: Iir;
                               Block_Type : Block_Instance_Acc;
                               Aggregate_Type: Iir)
                              return Iir_Value_Literal_Acc is
   begin
      case Get_Kind (Aggregate_Type) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            declare
               Res : Iir_Value_Literal_Acc;
            begin
               Res := Create_Array_Bounds_From_Type
                 (Block_Type, Aggregate_Type, True);
               Fill_Array_Aggregate (Block, Aggregate, Res);
               return Res;
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            return Execute_Record_Aggregate (Block, Aggregate, Aggregate_Type);
         when others =>
            Error_Kind ("execute_aggregate", Aggregate_Type);
      end case;
   end Execute_Aggregate;

   function Execute_Association_Expression
     (Actual_Instance : Block_Instance_Acc;
      Actual : Iir;
      Formal_Instance : Block_Instance_Acc)
     return Iir_Value_Literal_Acc
   is
   begin
      case Get_Kind (Actual) is
         when Iir_Kind_String_Literal8 =>
            return Execute_String_Literal (Actual, Formal_Instance);
         when Iir_Kind_Aggregate =>
            return Execute_Aggregate
              (Actual_Instance, Actual, Formal_Instance, Get_Type (Actual));
         when others =>
            null;
      end case;
      return Execute_Expression (Actual_Instance, Actual);
   end Execute_Association_Expression;


   function Execute_Simple_Aggregate (Block: Block_Instance_Acc; Aggr : Iir)
                                     return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
      List : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
   begin
      Res := Create_Array_Bounds_From_Type (Block, Get_Type (Aggr), True);
      for I in Res.Val_Array.V'Range loop
         Res.Val_Array.V (I) :=
           Execute_Expression (Block, Get_Nth_Element (List, Natural (I - 1)));
      end loop;
      return Res;
   end Execute_Simple_Aggregate;

   --  Fill LENGTH elements of RES, starting at ORIG by steps of STEP.
   --  Use expressions from (BLOCK, AGGREGATE) to fill the elements.
   --  EL_TYPE is the type of the array element.
   procedure Execute_Name_Array_Aggregate (Block : Block_Instance_Acc;
                                           Aggregate : Iir;
                                           Res : Iir_Value_Literal_Acc;
                                           Orig : Iir_Index32;
                                           Step : Iir_Index32;
                                           Dim : Iir_Index32;
                                           Nbr_Dim : Iir_Index32;
                                           El_Type : Iir)
   is
      Value : Iir;
      Bound : Iir_Value_Literal_Acc;

      procedure Set_Elem (Pos : Iir_Index32)
      is
         Val : Iir_Value_Literal_Acc;
         Is_Sig : Boolean;
      begin
         if Dim = Nbr_Dim then
            --  VALUE is an expression (which may be an aggregate, but not
            --  a sub-aggregate.
            Execute_Name_With_Base (Block, Value, null, Val, Is_Sig);
            Res.Val_Array.V (1 + Orig + Pos * Step) := Val;
         else
            --  VALUE is a sub-aggregate.
            Execute_Name_Array_Aggregate
              (Block, Value, Res,
               Orig + Pos * Step,
               Step / Res.Bounds.D (Dim + 1).Length,
               Dim + 1, Nbr_Dim, El_Type);
         end if;
      end Set_Elem;

      Assoc : Iir;
      Pos : Iir_Index32;
   begin
      Assoc := Get_Association_Choices_Chain (Aggregate);
      Bound := Res.Bounds.D (Dim);
      Pos := 0;
      while Assoc /= Null_Iir loop
         Value := Get_Associated_Expr (Assoc);
         case Get_Kind (Assoc) is
            when Iir_Kind_Choice_By_None =>
               null;
            when Iir_Kind_Choice_By_Expression =>
               declare
                  Expr_Pos: Iir_Value_Literal_Acc;
                  Val : constant Iir := Get_Expression (Assoc);
               begin
                  Expr_Pos := Execute_Expression (Block, Val);
                  Pos := Get_Index_Offset (Expr_Pos, Bound, Val);
               end;
            when others =>
               raise Internal_Error;
         end case;
         Set_Elem (Pos);
         Pos := Pos + 1;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Execute_Name_Array_Aggregate;

   function Execute_Record_Name_Aggregate (Block: Block_Instance_Acc;
                                           Aggregate: Iir;
                                           Aggregate_Type: Iir)
                                          return Iir_Value_Literal_Acc
   is
      List : constant Iir_Flist :=
        Get_Elements_Declaration_List (Get_Base_Type (Aggregate_Type));
      Res: Iir_Value_Literal_Acc;
      Expr : Iir;
      Pos : Iir_Index32;
      El_Pos : Iir_Index32;
      Is_Sig : Boolean;
      Assoc: Iir;
   begin
      Res := Create_Record_Value (Iir_Index32 (Get_Nbr_Elements (List)));
      Assoc := Get_Association_Choices_Chain (Aggregate);
      Pos := 0;
      loop
         Expr := Get_Associated_Expr (Assoc);
         if Expr = Null_Iir then
            --  List of choices is not allowed.
            raise Internal_Error;
         end if;
         case Get_Kind (Assoc) is
            when Iir_Kind_Choice_By_None =>
               El_Pos := Pos;
               Pos := Pos + 1;
            when Iir_Kind_Choice_By_Name =>
               El_Pos := Get_Element_Position (Get_Name (Assoc));
            when Iir_Kind_Choice_By_Others =>
               raise Internal_Error;
            when others =>
               Error_Kind ("execute_record_name_aggregate", Assoc);
         end case;
         Execute_Name_With_Base
           (Block, Expr, null, Res.Val_Record.V (1 + El_Pos), Is_Sig);
         Assoc := Get_Chain (Assoc);
         exit when Assoc = Null_Iir;
      end loop;
      return Res;
   end Execute_Record_Name_Aggregate;

   function Execute_Name_Aggregate (Block: Block_Instance_Acc;
                                    Aggregate: Iir;
                                    Aggregate_Type: Iir)
                                   return Iir_Value_Literal_Acc is
   begin
      case Get_Kind (Aggregate_Type) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            declare
               El_Type : constant Iir := Get_Element_Subtype (Aggregate_Type);
               Index_List : constant Iir_Flist :=
                 Get_Index_Subtype_List (Aggregate_Type);
               Nbr_Dim : constant Iir_Index32 :=
                 Iir_Index32 (Get_Nbr_Elements (Index_List));
               Res : Iir_Value_Literal_Acc;
               Step : Iir_Index32;
            begin
               pragma Assert
                 (Get_Constraint_State (Aggregate_Type) = Fully_Constrained);
               Res := Create_Array_Bounds_From_Type
                 (Block, Aggregate_Type, True);
               Step := Get_Step_For_Dim (Res, 1);
               Execute_Name_Array_Aggregate
                 (Block, Aggregate, Res, 0, Step, 1, Nbr_Dim, El_Type);
               return Res;
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            return Execute_Record_Name_Aggregate
              (Block, Aggregate, Aggregate_Type);
         when others =>
            Error_Kind ("execute_name_aggregate", Aggregate_Type);
      end case;
   end Execute_Name_Aggregate;

   --  Return the indexes range for prefix of ATTR.
   function Execute_Indexes (Block: Block_Instance_Acc; Attr : Iir)
                            return Iir_Value_Literal_Acc
   is
      Prefix : constant Iir := Strip_Denoting_Name (Get_Prefix (Attr));
      Dim : constant Natural :=
        Vhdl.Evaluation.Eval_Attribute_Parameter_Or_1 (Attr);
   begin
      case Get_Kind (Prefix) is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            declare
               Index : Iir;
            begin
               Index := Get_Nth_Element
                 (Get_Index_Subtype_List (Get_Type (Prefix)), Dim - 1);
               return Execute_Bounds (Block, Index);
            end;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            Error_Kind ("execute_indexes", Prefix);
         when others =>
            declare
               Orig : Iir_Value_Literal_Acc;
            begin
               Orig := Execute_Name (Block, Prefix, True);
               return Orig.Bounds.D (Iir_Index32 (Dim));
            end;
      end case;
   end Execute_Indexes;

   function Execute_Bounds (Block: Block_Instance_Acc; Prefix: Iir)
      return Iir_Value_Literal_Acc
   is
      Bound : Iir_Value_Literal_Acc;
   begin
      case Get_Kind (Prefix) is
         when Iir_Kind_Range_Expression =>
            declare
               Info : constant Sim_Info_Acc := Get_Info (Prefix);
            begin
               if Info = null then
                  Bound := Create_Range_Value
                    (Execute_Expression (Block, Get_Left_Limit (Prefix)),
                     Execute_Expression (Block, Get_Right_Limit (Prefix)),
                     Get_Direction (Prefix));
               elsif Info.Kind = Kind_Object then
                  Bound := Get_Instance_Object (Block, Prefix);
               else
                  raise Internal_Error;
               end if;
            end;

         when Iir_Kind_Subtype_Declaration =>
            return Execute_Bounds (Block, Get_Type (Prefix));

         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            --  FIXME: move this block before and avoid recursion.
            return Execute_Bounds (Block, Get_Range_Constraint (Prefix));

         when Iir_Kind_Range_Array_Attribute =>
            Bound := Execute_Indexes (Block, Prefix);
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Bound := Execute_Indexes (Block, Prefix);
            case Bound.Dir is
               when Dir_To =>
                  Bound := Create_Range_Value
                    (Bound.Right, Bound.Left, Dir_Downto, Bound.Length);
               when Dir_Downto =>
                  Bound := Create_Range_Value
                    (Bound.Right, Bound.Left, Dir_To, Bound.Length);
            end case;

         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Integer_Type_Definition =>
            return Execute_Bounds
              (Block,
               Get_Range_Constraint (Get_Type (Get_Type_Declarator (Prefix))));

         when Iir_Kinds_Denoting_Name =>
            return Execute_Bounds (Block, Get_Named_Entity (Prefix));

         when Iir_Kind_Subtype_Attribute =>
            return Execute_Bounds (Block, Get_Type (Prefix));

         when others =>
            -- Error_Kind ("execute_bounds", Get_Kind (Prefix));
            declare
               Prefix_Val: Iir_Value_Literal_Acc;
            begin
               Prefix_Val := Execute_Expression (Block, Prefix);
               Bound := Prefix_Val.Bounds.D (1);
            end;
      end case;
      if not Bound.Dir'Valid then
         raise Internal_Error;
      end if;
      return Bound;
   end Execute_Bounds;

   -- Perform type conversion as desribed in LRM93 7.3.5
   function Execute_Type_Conversion (Block: Block_Instance_Acc;
                                     Val : Iir_Value_Literal_Acc;
                                     Target_Type : Iir;
                                     Loc : Iir)
                                    return Iir_Value_Literal_Acc
   is
      Res: Iir_Value_Literal_Acc;
   begin
      Res := Val;
      case Get_Kind (Target_Type) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            case Iir_Value_Numerics (Res.Kind) is
               when Iir_Value_I64 =>
                  null;
               when Iir_Value_F64 =>
                  if Res.F64 > Ghdl_F64 (Int64'Last) or
                    Res.F64 < Ghdl_F64 (Int64'First)
                  then
                     Error_Msg_Constraint (Loc);
                  end if;
                  Res := Create_I64_Value (Ghdl_I64 (Res.F64));
            end case;
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            case Iir_Value_Numerics (Res.Kind) is
               when Iir_Value_F64 =>
                  null;
               when Iir_Value_I64 =>
                  Res := Create_F64_Value (Ghdl_F64 (Res.I64));
            end case;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            --  Must be same type.
            null;
         when Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            --  Same type.
            null;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            --  Same type.
            null;
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition =>
            --  LRM93 7.3.5
            --  if the type mark denotes an unconstrained array type and the
            --  operand is not a null array, then for each index position, the
            --  bounds of the result are obtained by converting the bounds of
            --  the operand to the corresponding index type of the target type.
            --
            --  LRM93 7.3.5
            --  If the type mark denotes a constrained array subtype, then the
            --  bounds of the result are those imposed by the type mark.
            if Get_Constraint_State (Target_Type) = Fully_Constrained then
               Implicit_Array_Conversion (Block, Res, Target_Type, Loc);
            else
               declare
                  Idx_List : constant Iir_Flist :=
                    Get_Index_Subtype_List (Target_Type);
                  Idx_Type : Iir;
               begin
                  Res := Create_Array_Value (Val.Bounds.Nbr_Dims);
                  Res.Val_Array := Val.Val_Array;
                  for I in Val.Bounds.D'Range loop
                     Idx_Type := Get_Index_Type (Idx_List, Natural (I - 1));
                     Res.Bounds.D (I) := Create_Range_Value
                       (Left => Execute_Type_Conversion
                          (Block, Val.Bounds.D (I).Left, Idx_Type, Loc),
                        Right => Execute_Type_Conversion
                          (Block, Val.Bounds.D (I).Right, Idx_Type, Loc),
                        Dir => Val.Bounds.D (I).Dir,
                        Length => Val.Bounds.D (I).Length);
                  end loop;
               end;
               end if;
         when others =>
            Error_Kind ("execute_type_conversion", Target_Type);
      end case;
      Check_Constraints (Block, Res, Target_Type, Loc);
      return Res;
   end Execute_Type_Conversion;

   --  Decrement VAL.
   --  May raise a constraint error using EXPR.
   function Execute_Dec (Val : Iir_Value_Literal_Acc; Expr : Iir)
     return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      case Iir_Value_Discrete (Val.Kind) is
         when Iir_Value_B1 =>
            if Val.B1 = False then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_B1_Value (False);
         when Iir_Value_E8 =>
            if Val.E8 = 0 then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_E8_Value (Val.E8 - 1);
         when Iir_Value_E32 =>
            if Val.E32 = 0 then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_E32_Value (Val.E32 - 1);
         when Iir_Value_I64 =>
            if Val.I64 = Ghdl_I64'First then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_I64_Value (Val.I64 - 1);
      end case;
      return Res;
   end Execute_Dec;

   --  Increment VAL.
   --  May raise a constraint error using EXPR.
   function Execute_Inc (Val : Iir_Value_Literal_Acc; Expr : Iir)
     return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      case Iir_Value_Discrete (Val.Kind) is
         when Iir_Value_B1 =>
            if Val.B1 = True then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_B1_Value (True);
         when Iir_Value_E32 =>
            if Val.E32 = Ghdl_E32'Last then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_E32_Value (Val.E32 + 1);
         when Iir_Value_E8 =>
            if Val.E8 = Ghdl_E8'Last then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_E8_Value (Val.E8 + 1);
         when Iir_Value_I64 =>
            if Val.I64 = Ghdl_I64'Last then
               Error_Msg_Constraint (Expr);
            end if;
            Res := Create_I64_Value (Val.I64 + 1);
      end case;
      return Res;
   end Execute_Inc;

   function Execute_Expression_With_Type (Block: Block_Instance_Acc;
                                          Expr: Iir;
                                          Expr_Type : Iir)
                                         return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      if Get_Kind (Expr) = Iir_Kind_Aggregate
        and then not Is_Fully_Constrained_Type (Get_Type (Expr))
      then
         return Execute_Aggregate (Block, Expr, Block, Expr_Type);
      else
         Res := Execute_Expression (Block, Expr);
         Implicit_Array_Conversion (Block, Res, Expr_Type, Expr);
         Check_Constraints (Block, Res, Expr_Type, Expr);
         return Res;
      end if;
   end Execute_Expression_With_Type;

   function Execute_Signal_Name
     (Block : Block_Instance_Acc; Expr : Iir; Kind : Signal_Slot)
     return Iir_Value_Literal_Acc
   is
      Base : constant Iir := Get_Object_Prefix (Expr, False);
      Info : constant Sim_Info_Acc := Get_Info (Base);
      Bblk : Block_Instance_Acc;
      Slot : Object_Slot_Type;
      Base_Val : Iir_Value_Literal_Acc;
      Res : Iir_Value_Literal_Acc;
      Is_Sig : Boolean;
   begin
      if Get_Kind (Base) = Iir_Kind_Object_Alias_Declaration then
         Bblk := Get_Instance_By_Scope (Block, Info.Obj_Scope);
         Base_Val := Execute_Signal_Name (Bblk, Get_Name (Base), Kind);
      else
         Bblk := Get_Instance_By_Scope (Block, Info.Obj_Scope);
         case Kind is
            when Signal_Sig =>
               Slot := Info.Slot;
            when Signal_Val =>
               Slot := Info.Slot + 1;
            when Signal_Init =>
               Slot := Info.Slot + 2;
         end case;
         Base_Val := Bblk.Objects (Slot);
      end if;
      Execute_Name_With_Base (Block, Expr, Base_Val, Res, Is_Sig);
      pragma Assert (Is_Sig);
      return Res;
   end Execute_Signal_Name;

   --  Indexed element will be at Pfx.Val_Array.V (Pos + 1)
   procedure Execute_Indexed_Name (Block: Block_Instance_Acc;
                                   Expr: Iir;
                                   Pfx : Iir_Value_Literal_Acc;
                                   Pos : out Iir_Index32)
   is
      pragma Assert (Get_Kind (Expr) = Iir_Kind_Indexed_Name);
      Index_List : constant Iir_Flist := Get_Index_List (Expr);
      Nbr_Dimensions : constant Iir_Index32 :=
        Iir_Index32 (Get_Nbr_Elements (Index_List));
      Index: Iir;
      Value: Iir_Value_Literal_Acc;
      Off : Iir_Index32;
   begin
      for I in 1 .. Nbr_Dimensions loop
         Index := Get_Nth_Element (Index_List, Natural (I - 1));
         Value := Execute_Expression (Block, Index);
         Off := Get_Index_Offset (Value, Pfx.Bounds.D (I), Expr);
         if I = 1 then
            Pos := Off;
         else
            Pos := Pos * Pfx.Bounds.D (I).Length + Off;
         end if;
      end loop;
   end Execute_Indexed_Name;

   --  Indexed element will be at Pfx.Val_Array.V (Pos)
   procedure Execute_Slice_Name (Prefix_Array: Iir_Value_Literal_Acc;
                                 Srange : Iir_Value_Literal_Acc;
                                 Low : out Iir_Index32;
                                 High : out Iir_Index32;
                                 Loc : Iir)
   is
      Index_Order : Order;
      -- Lower and upper bounds of the slice.
   begin
      pragma Assert (Prefix_Array /= null);

      --  LRM93 6.5
      --  It is an error if the direction of the discrete range is not
      --  the same as that of the index range of the array denoted by
      --  the prefix of the slice name.
      if Srange.Dir /= Prefix_Array.Bounds.D (1).Dir then
         Error_Msg_Exec ("slice direction mismatch", Loc);
      end if;

      --  LRM93 6.5
      --  It is an error if either of the bounds of the
      --  discrete range does not belong to the index range of the
      --  prefixing array, unless the slice is a null slice.
      Index_Order := Compare_Value (Srange.Left, Srange.Right);
      if (Srange.Dir = Dir_To and Index_Order = Greater)
        or (Srange.Dir = Dir_Downto and Index_Order = Less)
      then
         --  Null slice.
         Low := 1;
         High := 0;
      else
         Low := Get_Index_Offset
           (Srange.Left, Prefix_Array.Bounds.D (1), Loc);
         High := Get_Index_Offset
           (Srange.Right, Prefix_Array.Bounds.D (1), Loc);
      end if;
   end Execute_Slice_Name;

   procedure Execute_Name_With_Base (Block: Block_Instance_Acc;
                                     Expr: Iir;
                                     Base : Iir_Value_Literal_Acc;
                                     Res : out Iir_Value_Literal_Acc;
                                     Is_Sig : out Boolean) is
   begin
      --  Default value
      Is_Sig := False;

      case Get_Kind (Expr) is
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Transaction_Attribute =>
            Is_Sig := True;
            if Base /= null then
               Res := Base;
            else
               Res := Get_Instance_Object (Block, Expr);
            end if;

         when Iir_Kind_Object_Alias_Declaration =>
            --  FIXME: add a flag ?
            Is_Sig := Is_Signal_Object (Expr);
            if Base /= null then
               Res := Base;
            else
               Res := Get_Instance_Object (Block, Expr);
            end if;

         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kinds_Quantity_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            if Base /= null then
               Res := Base;
            else
               Res := Get_Instance_Object (Block, Expr);
            end if;

         when Iir_Kind_Indexed_Name =>
            declare
               Pfx : Iir_Value_Literal_Acc;
               Pos : Iir_Index32;
            begin
               Execute_Name_With_Base
                 (Block, Get_Prefix (Expr), Base, Pfx, Is_Sig);
               Execute_Indexed_Name (Block, Expr, Pfx, Pos);
               Res := Pfx.Val_Array.V (Pos + 1);
            end;

         when Iir_Kind_Slice_Name =>
            declare
               Prefix_Array: Iir_Value_Literal_Acc;
               Srange : Iir_Value_Literal_Acc;
               Low, High: Iir_Index32;
            begin
               Execute_Name_With_Base
                 (Block, Get_Prefix (Expr), Base, Prefix_Array, Is_Sig);

               Srange := Execute_Bounds (Block, Get_Suffix (Expr));
               Execute_Slice_Name (Prefix_Array, Srange, Low, High, Expr);

               Res := Create_Array_Value (High - Low + 1, 1);
               Res.Bounds.D (1) := Srange;
               for I in Low .. High loop
                  Res.Val_Array.V (1 + I - Low) :=
                    Prefix_Array.Val_Array.V (1 + I);
               end loop;
            end;

         when Iir_Kind_Selected_Element =>
            declare
               Prefix: Iir_Value_Literal_Acc;
               Pos: Iir_Index32;
            begin
               Execute_Name_With_Base
                 (Block, Get_Prefix (Expr), Base, Prefix, Is_Sig);
               Pos := Get_Element_Position (Get_Named_Entity (Expr));
               Res := Prefix.Val_Record.V (Pos + 1);
            end;

         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference =>
            declare
               Prefix: Iir_Value_Literal_Acc;
            begin
               Prefix := Execute_Name (Block, Get_Prefix (Expr));
               Res := Prefix.Val_Access;
               if Res = null then
                  Error_Msg_Exec ("deferencing null access", Expr);
               end if;
            end;

         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Attribute_Name =>
            Execute_Name_With_Base
              (Block, Get_Named_Entity (Expr), Base, Res, Is_Sig);

         when Iir_Kind_Function_Call =>
            --  A prefix can be an expression
            if Base /= null then
               raise Internal_Error;
            end if;
            Res := Execute_Expression (Block, Expr);

         when Iir_Kind_Aggregate =>
            Res := Execute_Name_Aggregate (Block, Expr, Get_Type (Expr));

         when Iir_Kind_Image_Attribute =>
            Res := Execute_Image_Attribute (Block, Expr);

         when Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute =>
            Res := Execute_Path_Instance_Name_Attribute (Block, Expr);

         when others =>
            Error_Kind ("execute_name_with_base", Expr);
      end case;
   end Execute_Name_With_Base;

   function Execute_Name (Block: Block_Instance_Acc;
                          Expr: Iir;
                          Ref : Boolean := False)
                         return Iir_Value_Literal_Acc
   is
      Res: Iir_Value_Literal_Acc;
      Is_Sig : Boolean;
   begin
      Execute_Name_With_Base (Block, Expr, null, Res, Is_Sig);
      if not Is_Sig or else Ref then
         return Res;
      else
         return Execute_Signal_Value (Res);
      end if;
   end Execute_Name;

   function Execute_Value_Attribute (Block: Block_Instance_Acc;
                                     Str_Val : Iir_Value_Literal_Acc;
                                     Expr: Iir)
                                    return Iir_Value_Literal_Acc
   is
      use Grt_Interface;
      use Name_Table;
      pragma Unreferenced (Block);

      Expr_Type : constant Iir := Get_Type (Expr);
      Res : Iir_Value_Literal_Acc;

      Str_Bnd : aliased Std_String_Bound := Build_Bound (Str_Val);
      Str_Str : aliased Std_String_Uncons (1 .. Str_Bnd.Dim_1.Length);
      Str : aliased Std_String := (To_Std_String_Basep (Str_Str'Address),
                                   To_Std_String_Boundp (Str_Bnd'Address));
   begin
      Set_Std_String_From_Iir_Value (Str, Str_Val);
      case Get_Kind (Expr_Type) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            Res := Create_I64_Value
              (Grt.Values.Ghdl_Value_I64 (Str'Unrestricted_Access));
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            Res := Create_F64_Value
              (Grt.Values.Ghdl_Value_F64 (Str'Unrestricted_Access));
         when Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            declare
               Is_Real : Boolean;
               Lit_Pos : Ghdl_Index_Type;
               Lit_End : Ghdl_Index_Type;
               Unit_Pos : Ghdl_Index_Type;
               Unit_Len : Ghdl_Index_Type;
               Mult : Ghdl_I64;
               Unit : Iir;
               Unit_Id : Name_Id;
            begin
               Grt.Values.Ghdl_Value_Physical_Split
                 (Str'Unrestricted_Access,
                  Is_Real, Lit_Pos, Lit_End, Unit_Pos);

               --  Find unit.
               Unit_Len := 0;
               Unit_Pos := Unit_Pos + 1;   --  From 0 based to 1 based
               for I in Unit_Pos .. Str_Bnd.Dim_1.Length loop
                  exit when Grt.Strings.Is_Whitespace (Str_Str (I));
                  Unit_Len := Unit_Len + 1;
                  Str_Str (I) := Grt.Strings.To_Lower (Str_Str (I));
               end loop;

               Unit := Get_Primary_Unit (Expr_Type);
               while Unit /= Null_Iir loop
                  Unit_Id := Get_Identifier (Unit);
                  exit when Get_Name_Length (Unit_Id) = Natural (Unit_Len)
                    and then Image (Unit_Id) =
                    String (Str_Str (Unit_Pos .. Unit_Pos + Unit_Len - 1));
                  Unit := Get_Chain (Unit);
               end loop;

               if Unit = Null_Iir then
                  Error_Msg_Exec ("incorrect unit name", Expr);
               end if;
               Mult := Ghdl_I64 (Get_Value (Get_Physical_Literal (Unit)));

               Str_Bnd.Dim_1.Length := Lit_End;
               if Is_Real then
                  Res := Create_I64_Value
                    (Ghdl_I64
                       (Grt.Values.Ghdl_Value_F64 (Str'Unrestricted_Access)
                          * Ghdl_F64 (Mult)));
               else
                  Res := Create_I64_Value
                    (Grt.Values.Ghdl_Value_I64 (Str'Unrestricted_Access)
                       * Mult);
               end if;
            end;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            declare
               Enums : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Get_Base_Type (Expr_Type));
               Lit_Start : Ghdl_Index_Type;
               Lit_End : Ghdl_Index_Type;
               Enum : Iir;
               Lit_Id : Name_Id;
               Enum_Id : Name_Id;
            begin
               --  Remove leading and trailing blanks
               for I in Str_Str'Range loop
                  if not Grt.Strings.Is_Whitespace (Str_Str (I)) then
                     Lit_Start := I;
                     exit;
                  end if;
               end loop;
               for I in reverse Lit_Start .. Str_Str'Last loop
                  if not Grt.Strings.Is_Whitespace (Str_Str (I)) then
                     Lit_End := I;
                     exit;
                  end if;
               end loop;

               if Str_Str (Lit_Start) = '''
                 and then Str_Str (Lit_End) = '''
                 and then Lit_End = Lit_Start + 2
               then
                  --  Enumeration literal.
                  Lit_Id := Get_Identifier (Str_Str (Lit_Start + 1));

                  for I in Natural loop
                     Enum := Get_Nth_Element (Enums, I);
                     exit when Enum = Null_Iir;
                     exit when Get_Identifier (Enum) = Lit_Id;
                  end loop;
               else
                  --  Literal identifier.
                  --  Convert to lower case.
                  for I in Lit_Start .. Lit_End loop
                     Str_Str (I) := Grt.Strings.To_Lower (Str_Str (I));
                  end loop;

                  for I in Natural loop
                     Enum := Get_Nth_Element (Enums, I);
                     exit when Enum = Null_Iir;
                     Enum_Id := Get_Identifier (Enum);
                     exit when (Get_Name_Length (Enum_Id) =
                                  Natural (Lit_End - Lit_Start + 1))
                       and then (Image (Enum_Id) =
                                   String (Str_Str (Lit_Start .. Lit_End)));
                  end loop;
               end if;

               if Enum = Null_Iir then
                  Error_Msg_Exec
                    ("incorrect enumeration literal for 'value", Expr);
               end if;

               return Create_Enum_Value
                 (Natural (Get_Enum_Pos (Enum)), Expr_Type);
            end;
         when others =>
            Error_Kind ("value_attribute", Expr_Type);
      end case;
      return Res;
   end Execute_Value_Attribute;

   --  For 'Last_Event and 'Last_Active: convert the absolute last time to
   --  a relative delay.
   function To_Relative_Time (T : Ghdl_I64) return Iir_Value_Literal_Acc
   is
      A : Ghdl_I64;
   begin
      if T = -Ghdl_I64'Last then
         A := Ghdl_I64'Last;
      else
         A := Ghdl_I64 (Grt.Types.Current_Time) - T;
      end if;
      return Create_I64_Value (A);
   end To_Relative_Time;

   -- Evaluate an expression.
   function Execute_Expression (Block: Block_Instance_Acc; Expr: Iir)
                               return Iir_Value_Literal_Acc
   is
      Res: Iir_Value_Literal_Acc;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Object_Alias_Declaration =>
            Res := Execute_Name (Block, Expr);
            return Res;

         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Psl_Endpoint_Declaration =>
            return Execute_Name (Block, Expr);

         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Attribute_Name =>
            return Execute_Expression (Block, Get_Named_Entity (Expr));

         when Iir_Kind_Aggregate =>
            return Execute_Aggregate (Block, Expr, Block, Get_Type (Expr));
         when Iir_Kind_Simple_Aggregate =>
            return Execute_Simple_Aggregate (Block, Expr);

         when Iir_Kinds_Dyadic_Operator
           | Iir_Kinds_Monadic_Operator =>
            declare
               Imp : constant Iir := Get_Implementation (Expr);
            begin
               if Get_Implicit_Definition (Imp) in Iir_Predefined_Explicit then
                  return Execute_Function_Call (Block, Expr, Imp);
               else
                  if Get_Kind (Expr) in Iir_Kinds_Dyadic_Operator then
                     Res := Execute_Implicit_Function
                       (Block, Expr, Get_Left (Expr), Get_Right (Expr),
                        Get_Type (Expr));
                  else
                     Res := Execute_Implicit_Function
                       (Block, Expr, Get_Operand (Expr), Null_Iir,
                        Get_Type (Expr));
                  end if;
                  return Res;
               end if;
            end;

         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Iir := Get_Implementation (Expr);
               Assoc : Iir;
               Args : Iir_Array (0 .. 1);
            begin
               if Get_Implicit_Definition (Imp) in Iir_Predefined_Explicit then
                  return Execute_Function_Call (Block, Expr, Imp);
               else
                  Assoc := Get_Parameter_Association_Chain (Expr);
                  if Assoc /= Null_Iir then
                     Args (0) := Get_Actual (Assoc);
                     Assoc := Get_Chain (Assoc);
                  else
                     Args (0) := Null_Iir;
                  end if;
                  if Assoc /= Null_Iir  then
                     Args (1) := Get_Actual (Assoc);
                  else
                     Args (1) := Null_Iir;
                  end if;
                  return Execute_Implicit_Function
                    (Block, Expr, Args (0), Args (1), Get_Type (Expr));
               end if;
            end;

         when Iir_Kind_Integer_Literal =>
            declare
               Lit_Type : constant Iir := Get_Base_Type (Get_Type (Expr));
               Lit : constant Int64 := Get_Value (Expr);
            begin
               case Get_Info (Lit_Type).Kind is
                  when Kind_I64_Type =>
                     return Create_I64_Value (Ghdl_I64 (Lit));
                  when others =>
                     raise Internal_Error;
               end case;
            end;

         when Iir_Kind_Floating_Point_Literal =>
            return Create_F64_Value (Ghdl_F64 (Get_Fp_Value (Expr)));

         when Iir_Kind_Enumeration_Literal =>
            return Create_Enum_Value (Natural (Get_Enum_Pos (Expr)),
                                      Get_Type (Expr));

         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Unit_Declaration =>
            return Create_I64_Value
              (Ghdl_I64 (Vhdl.Evaluation.Get_Physical_Value (Expr)));

         when Iir_Kind_String_Literal8 =>
            return Execute_String_Literal (Expr, Block);

         when Iir_Kind_Null_Literal =>
            return Null_Lit;

         when Iir_Kind_Overflow_Literal =>
            Error_Msg_Constraint (Expr);
            return null;

         when Iir_Kind_Parenthesis_Expression =>
            return Execute_Expression (Block, Get_Expression (Expr));

         when Iir_Kind_Type_Conversion =>
            return Execute_Type_Conversion
              (Block, Execute_Expression (Block, Get_Expression (Expr)),
               Get_Type (Expr), Expr);

         when Iir_Kind_Qualified_Expression =>
            Res := Execute_Expression_With_Type
              (Block, Get_Expression (Expr), Get_Type (Get_Type_Mark (Expr)));
            return Res;

         when Iir_Kind_Allocator_By_Expression =>
            Res := Execute_Expression (Block, Get_Expression (Expr));
            Res := Unshare_Heap (Res);
            return Create_Access_Value (Res);

         when Iir_Kind_Allocator_By_Subtype =>
            Res := Create_Value_For_Type
              (Block,
               Get_Type_Of_Subtype_Indication (Get_Subtype_Indication (Expr)),
               Init_Value_Default);
            Res := Unshare_Heap (Res);
            return Create_Access_Value (Res);

         when Iir_Kind_Left_Type_Attribute =>
            Res := Execute_Bounds (Block, Get_Prefix (Expr));
            return Execute_Left_Limit (Res);

         when Iir_Kind_Right_Type_Attribute =>
            Res := Execute_Bounds (Block, Get_Prefix (Expr));
            return Execute_Right_Limit (Res);

         when Iir_Kind_High_Type_Attribute =>
            Res := Execute_Bounds (Block, Get_Prefix (Expr));
            return Execute_High_Limit (Res);

         when Iir_Kind_Low_Type_Attribute =>
            Res := Execute_Bounds (Block, Get_Prefix (Expr));
            return Execute_Low_Limit (Res);

         when Iir_Kind_High_Array_Attribute =>
            Res := Execute_Indexes (Block, Expr);
            return Execute_High_Limit (Res);

         when Iir_Kind_Low_Array_Attribute =>
            Res := Execute_Indexes (Block, Expr);
            return Execute_Low_Limit (Res);

         when Iir_Kind_Left_Array_Attribute =>
            Res := Execute_Indexes (Block, Expr);
            return Execute_Left_Limit (Res);

         when Iir_Kind_Right_Array_Attribute =>
            Res := Execute_Indexes (Block, Expr);
            return Execute_Right_Limit (Res);

         when Iir_Kind_Length_Array_Attribute =>
            Res := Execute_Indexes (Block, Expr);
            return Execute_Length (Res);

         when Iir_Kind_Ascending_Array_Attribute =>
            Res := Execute_Indexes (Block, Expr);
            return Boolean_To_Lit (Res.Dir = Dir_To);

         when Iir_Kind_Event_Attribute =>
            Res := Execute_Name (Block, Get_Prefix (Expr), True);
            return Boolean_To_Lit (Execute_Event_Attribute (Res));

         when Iir_Kind_Active_Attribute =>
            Res := Execute_Name (Block, Get_Prefix (Expr), True);
            return Boolean_To_Lit (Execute_Active_Attribute (Res));

         when Iir_Kind_Driving_Attribute =>
            Res := Execute_Name (Block, Get_Prefix (Expr), True);
            return Boolean_To_Lit (Execute_Driving_Attribute (Res));

         when Iir_Kind_Last_Value_Attribute =>
            Res := Execute_Name (Block, Get_Prefix (Expr), True);
            return Execute_Last_Value_Attribute (Res);

         when Iir_Kind_Driving_Value_Attribute =>
            Res := Execute_Name (Block, Get_Prefix (Expr), True);
            return Execute_Driving_Value_Attribute (Res);

         when Iir_Kind_Last_Event_Attribute =>
            Res := Execute_Name (Block, Get_Prefix (Expr), True);
            return To_Relative_Time (Execute_Last_Event_Attribute (Res));

         when Iir_Kind_Last_Active_Attribute =>
            Res := Execute_Name (Block, Get_Prefix (Expr), True);
            return To_Relative_Time (Execute_Last_Active_Attribute (Res));

         when Iir_Kind_Val_Attribute =>
            declare
               Prefix_Type: constant Iir := Get_Type (Get_Prefix (Expr));
               Base_Type : constant Iir := Get_Base_Type (Prefix_Type);
               Kind : constant Kind_Discrete_Types :=
                 Get_Info (Base_Type).Kind;
            begin
               Res := Execute_Expression (Block, Get_Parameter (Expr));
               case Kind is
                  when Kind_I64_Type =>
                     null;
                  when Kind_E8_Type
                    | Kind_Log_Type =>
                     Res := Create_E8_Value (Ghdl_E8 (Res.I64));
                  when Kind_E32_Type =>
                     Res := Create_E32_Value (Ghdl_E32 (Res.I64));
                  when Kind_Bit_Type =>
                     Res := Create_B1_Value (Ghdl_B1'Val (Res.I64));
               end case;
               Check_Constraints (Block, Res, Prefix_Type, Expr);
               return Res;
            end;

         when Iir_Kind_Pos_Attribute =>
            declare
               N_Res: Iir_Value_Literal_Acc;
               Prefix_Type: constant Iir := Get_Type (Get_Prefix (Expr));
               Base_Type : constant Iir := Get_Base_Type (Prefix_Type);
               Mode : constant Kind_Discrete_Types :=
                 Get_Info (Base_Type).Kind;
            begin
               Res := Execute_Expression (Block, Get_Parameter (Expr));
               case Mode is
                  when Kind_I64_Type =>
                     null;
                  when Kind_Bit_Type =>
                     N_Res := Create_I64_Value (Ghdl_B1'Pos (Res.B1));
                     Res := N_Res;
                  when Kind_E8_Type
                    | Kind_Log_Type =>
                     N_Res := Create_I64_Value (Ghdl_I64 (Res.E8));
                     Res := N_Res;
                  when Kind_E32_Type =>
                     N_Res := Create_I64_Value (Ghdl_I64 (Res.E32));
                     Res := N_Res;
               end case;
               Check_Constraints (Block, Res, Get_Type (Expr), Expr);
               return Res;
            end;

         when Iir_Kind_Succ_Attribute =>
            Res := Execute_Expression (Block, Get_Parameter (Expr));
            Res := Execute_Inc (Res, Expr);
            Check_Constraints (Block, Res, Get_Type (Expr), Expr);
            return Res;

         when Iir_Kind_Pred_Attribute =>
            Res := Execute_Expression (Block, Get_Parameter (Expr));
            Res := Execute_Dec (Res, Expr);
            Check_Constraints (Block, Res, Get_Type (Expr), Expr);
            return Res;

         when Iir_Kind_Leftof_Attribute =>
            declare
               Bound : Iir_Value_Literal_Acc;
            begin
               Res := Execute_Expression (Block, Get_Parameter (Expr));
               Bound := Execute_Bounds
                 (Block, Get_Type (Get_Prefix (Expr)));
               case Bound.Dir is
                  when Dir_To =>
                     Res := Execute_Dec (Res, Expr);
                  when Dir_Downto =>
                     Res := Execute_Inc (Res, Expr);
               end case;
               Check_Constraints (Block, Res, Get_Type (Expr), Expr);
               return Res;
            end;

         when Iir_Kind_Rightof_Attribute =>
            declare
               Bound : Iir_Value_Literal_Acc;
            begin
               Res := Execute_Expression (Block, Get_Parameter (Expr));
               Bound := Execute_Bounds
                 (Block, Get_Type (Get_Prefix (Expr)));
               case Bound.Dir is
                  when Dir_Downto =>
                     Res := Execute_Dec (Res, Expr);
                  when Dir_To =>
                     Res := Execute_Inc (Res, Expr);
               end case;
               Check_Constraints (Block, Res, Get_Type (Expr), Expr);
               return Res;
            end;

         when Iir_Kind_Image_Attribute =>
            return Execute_Image_Attribute (Block, Expr);

         when Iir_Kind_Value_Attribute =>
            Res := Execute_Expression (Block, Get_Parameter (Expr));
            return Execute_Value_Attribute (Block, Res, Expr);

         when Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute =>
            return Execute_Path_Instance_Name_Attribute (Block, Expr);

         when others =>
            Error_Kind ("execute_expression", Expr);
      end case;
   end Execute_Expression;

   procedure Execute_Dyadic_Association (Out_Block: Block_Instance_Acc;
                                         In_Block: Block_Instance_Acc;
                                         Expr : Iir;
                                         Inter_Chain: Iir)
   is
      Inter: Iir;
      Val: Iir_Value_Literal_Acc;
   begin
      Inter := Inter_Chain;
      for I in 0 .. 1 loop
         if I = 0 then
            Val := Execute_Expression (Out_Block, Get_Left (Expr));
         else
            Val := Execute_Expression (Out_Block, Get_Right (Expr));
         end if;
         Implicit_Array_Conversion (In_Block, Val, Get_Type (Inter), Expr);
         Check_Constraints (In_Block, Val, Get_Type (Inter), Expr);

         Elaboration.Create_Object (In_Block, Inter);
         In_Block.Objects (Get_Info (Inter).Slot) :=
           Unshare (Val, Instance_Pool);
         Inter := Get_Chain (Inter);
      end loop;
   end Execute_Dyadic_Association;

   procedure Execute_Monadic_Association (Out_Block: Block_Instance_Acc;
                                          In_Block: Block_Instance_Acc;
                                          Expr : Iir;
                                          Inter: Iir)
   is
      Val: Iir_Value_Literal_Acc;
   begin
      Val := Execute_Expression (Out_Block, Get_Operand (Expr));
      Implicit_Array_Conversion (In_Block, Val, Get_Type (Inter), Expr);
      Check_Constraints (In_Block, Val, Get_Type (Inter), Expr);

      Elaboration.Create_Object (In_Block, Inter);
      In_Block.Objects (Get_Info (Inter).Slot) :=
        Unshare (Val, Instance_Pool);
   end Execute_Monadic_Association;

   --  Like Get_Protected_Type_Body, but also works for instances, where
   --  instantiated nodes have no bodies.
   --  FIXME: maybe fix the issue directly in Sem_Inst ?
   function Get_Protected_Type_Body_Origin (Spec : Iir) return Iir
   is
      Res : constant Iir := Get_Protected_Type_Body (Spec);
      Orig : Iir;
   begin
      if Res /= Null_Iir then
         return Res;
      else
         Orig := Vhdl.Sem_Inst.Get_Origin (Spec);
         return Get_Protected_Type_Body_Origin (Orig);
      end if;
   end Get_Protected_Type_Body_Origin;

   --  Create a block instance for subprogram IMP.
   function Create_Subprogram_Instance (Instance : Block_Instance_Acc;
                                        Prot_Obj : Block_Instance_Acc;
                                        Imp : Iir)
                                       return Block_Instance_Acc
   is
      Parent : Iir;
      Bod : Iir;

      Up_Block: Block_Instance_Acc;
      Up_Info : Sim_Info_Acc;

      Label : Iir;
   begin
      case Get_Kind (Imp) is
         when Iir_Kinds_Subprogram_Declaration =>
            Bod := Vhdl.Sem_Inst.Get_Subprogram_Body_Origin (Imp);
            Parent := Get_Parent (Imp);
            Label := Get_Subprogram_Specification (Bod);
         when Iir_Kind_Protected_Type_Declaration =>
            --  The parent of the protected type body must have the same scope
            --  as the parent of the protected type declaration.
            Bod := Get_Protected_Type_Body_Origin (Imp);
            Parent := Get_Parent (Get_Type_Declarator (Imp));
            Label := Imp;
         when others =>
            Error_Kind ("create_subprogram_instance", Imp);
      end case;

      if Prot_Obj /= null then
         --  This is a call to a method (from the outside to a subprogram of
         --  a protected type). Put the protected object as upblock.
         Up_Block := Prot_Obj;
      else
         --  This is a normal subprogram call.
         Up_Info := Get_Info_For_Scope (Parent);
         Up_Block := Get_Instance_By_Scope (Instance, Up_Info);
      end if;

      --  Extract the info from the body, as it is complete (has slot for
      --  internal declarations).  Usually, body and spec share the same info,
      --  but there are exceptions: there can be multiple spec for the same
      --  body for shared generic packages.
      declare
         Func_Info : constant Sim_Info_Acc := Get_Info (Bod);

         subtype Block_Type is Block_Instance_Type (Func_Info.Nbr_Objects);
         function To_Block_Instance_Acc is new
           Ada.Unchecked_Conversion (System.Address, Block_Instance_Acc);
         function Alloc_Block_Instance is new
           Alloc_On_Pool_Addr (Block_Type);

         Res : Block_Instance_Acc;
      begin
         Res := To_Block_Instance_Acc
           (Alloc_Block_Instance
              (Instance_Pool,
               Block_Instance_Type'(Max_Objs => Func_Info.Nbr_Objects,
                                    Id => No_Block_Instance_Id,
                                    Block_Scope => Get_Info (Label),
                                    Uninst_Scope => null,
                                    Up_Block => Up_Block,
                                    Label => Imp,
                                    Bod => Bod,
                                    Stmt => Null_Iir,
                                    Parent => Instance,
                                    Children => null,
                                    Brother => null,
                                    Marker => Empty_Marker,
                                    Objects => (others => null),
                                    Elab_Objects => 0,
                                    In_Wait_Flag => False,
                                    Actuals_Ref => null,
                                    Result => null)));
         return Res;
      end;
   end Create_Subprogram_Instance;

   function Get_Protected_Object_Instance
     (Block : Block_Instance_Acc; Call : Iir) return Block_Instance_Acc
   is
      Meth_Obj : constant Iir := Get_Method_Object (Call);
      Obj : Iir_Value_Literal_Acc;
   begin
      if Meth_Obj = Null_Iir then
         return null;
      else
         Obj := Execute_Name (Block, Meth_Obj, True);
         return Protected_Table.Table (Obj.Prot);
      end if;
   end Get_Protected_Object_Instance;

   -- Destroy a dynamic block_instance.
   procedure Execute_Subprogram_Call_Final (Instance : Block_Instance_Acc) is
   begin
      Finalize_Declarative_Part
        (Instance, Get_Declaration_Chain (Instance.Bod));
   end Execute_Subprogram_Call_Final;

   function Execute_Function_Body (Instance : Block_Instance_Acc)
                                  return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      Current_Process.Instance := Instance;

      Elaborate_Declarative_Part
        (Instance, Get_Declaration_Chain (Instance.Bod));

      -- execute statements
      Instance.Stmt := Get_Sequential_Statement_Chain (Instance.Bod);
      Execute_Sequential_Statements (Current_Process);
      pragma Assert (Current_Process.Instance = Instance);

      if Instance.Result = null then
         Error_Msg_Exec
           ("function scope exited without a return statement",
            Instance.Label);
      end if;

      -- Free variables, slots...
      -- Need to copy the return value, because it can contains values from
      -- arguments.
      Res := Instance.Result;

      Current_Process.Instance := Instance.Parent;
      Execute_Subprogram_Call_Final (Instance);

      return Res;
   end Execute_Function_Body;

   function Execute_Assoc_Function_Conversion (Block : Block_Instance_Acc;
                                               Func : Iir;
                                               Prot_Block : Block_Instance_Acc;
                                               Val : Iir_Value_Literal_Acc)
                                              return Iir_Value_Literal_Acc
   is
      Inter : Iir;
      Instance : Block_Instance_Acc;
      Res : Iir_Value_Literal_Acc;
      Marker : Mark_Type;
   begin
      Mark (Marker, Instance_Pool.all);

      -- Create an instance for this function.
      Instance := Create_Subprogram_Instance (Block, Prot_Block, Func);

      Inter := Get_Interface_Declaration_Chain (Func);
      Elaboration.Create_Object (Instance, Inter);
      --  FIXME: implicit conversion
      Instance.Objects (Get_Info (Inter).Slot) := Val;

      Res := Execute_Function_Body (Instance);
      Res := Unshare (Res, Expr_Pool'Access);
      Release (Marker, Instance_Pool.all);
      return Res;
   end Execute_Assoc_Function_Conversion;

   function Execute_Assoc_Conversion
     (Block : Block_Instance_Acc; Conv : Iir; Val : Iir_Value_Literal_Acc)
     return Iir_Value_Literal_Acc
   is
      Ent : Iir;
      Prot_Block : Block_Instance_Acc;
   begin
      case Get_Kind (Conv) is
         when Iir_Kind_Function_Call =>
            --  FIXME: shouldn't CONV always be a denoting_name ?
            Prot_Block := Get_Protected_Object_Instance (Block, Conv);
            return Execute_Assoc_Function_Conversion
              (Block, Get_Implementation (Conv), Prot_Block, Val);
         when Iir_Kind_Type_Conversion =>
            --  FIXME: shouldn't CONV always be a denoting_name ?
            return Execute_Type_Conversion (Block, Val, Get_Type (Conv), Conv);
         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Function_Declaration =>
            Ent := Strip_Denoting_Name (Conv);
            if Get_Kind (Ent) = Iir_Kind_Function_Declaration then
               return Execute_Assoc_Function_Conversion
                 (Block, Ent, null, Val);
            elsif Get_Kind (Ent) in Iir_Kinds_Type_Declaration then
               return Execute_Type_Conversion
                 (Block, Val, Get_Type (Ent), Ent);
            else
               Error_Kind ("execute_assoc_conversion(1)", Ent);
            end if;
         when others =>
            Error_Kind ("execute_assoc_conversion(2)", Conv);
      end case;
   end Execute_Assoc_Conversion;

   procedure Associate_By_Reference (Block : Block_Instance_Acc;
                                     Formal : Iir;
                                     Formal_Base : Iir_Value_Literal_Acc;
                                     Actual : Iir_Value_Literal_Acc)
   is
      Prefix : constant Iir := Strip_Denoting_Name (Get_Prefix (Formal));
      Is_Sig : Boolean;
      Pfx : Iir_Value_Literal_Acc;
      Pos : Iir_Index32;
   begin
      if Get_Kind (Prefix) = Iir_Kind_Slice_Name then
         --  That case is not handled correctly.
         raise Program_Error;
      end if;
      Execute_Name_With_Base (Block, Prefix, Formal_Base, Pfx, Is_Sig);

      case Get_Kind (Formal) is
         when Iir_Kind_Indexed_Name =>
            Execute_Indexed_Name (Block, Formal, Pfx, Pos);
            Store (Pfx.Val_Array.V (Pos + 1), Actual);
         when Iir_Kind_Slice_Name =>
            declare
               Low, High : Iir_Index32;
               Srange : Iir_Value_Literal_Acc;
            begin
               Srange := Execute_Bounds (Block, Get_Suffix (Formal));
               Execute_Slice_Name (Pfx, Srange, Low, High, Formal);
               for I in 1 .. High - Low + 1 loop
                  Store (Pfx.Val_Array.V (Low + I), Actual.Val_Array.V (I));
               end loop;
            end;
         when Iir_Kind_Selected_Element =>
            Pos := Get_Element_Position (Get_Named_Entity (Formal));
            Store (Pfx.Val_Record.V (Pos + 1), Actual);
         when others =>
            Error_Kind ("associate_by_reference", Formal);
      end case;
   end Associate_By_Reference;

   --  Establish correspondance for association list ASSOC_LIST from block
   --  instance OUT_BLOCK for subprogram of block SUBPRG_BLOCK.
   procedure Execute_Association (Out_Block : Block_Instance_Acc;
                                  Subprg_Block : Block_Instance_Acc;
                                  Inter_Chain : Iir;
                                  Assoc_Chain : Iir)
   is
      Nbr_Assoc : constant Natural := Get_Chain_Length (Assoc_Chain);
      Assoc: Iir;
      Assoc_Inter : Iir;
      Actual : Iir;
      Inter: Iir;
      Formal : Iir;
      Conv : Iir;
      Val: Iir_Value_Literal_Acc;
      Assoc_Idx : Iir_Index32;
      Last_Individual : Iir_Value_Literal_Acc;
      Mode : Iir_Mode;
      Marker : Mark_Type;
   begin
      Subprg_Block.Actuals_Ref := null;
      Mark (Marker, Expr_Pool);

      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      Assoc_Idx := 1;
      while Assoc /= Null_Iir loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         Formal := Get_Association_Formal (Assoc, Inter);

         --  Extract the actual value.
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               --  Not allowed in individual association.
               pragma Assert (Formal = Inter);
               pragma Assert (Get_Whole_Association_Flag (Assoc));
               Actual := Get_Default_Value (Inter);
            when Iir_Kind_Association_Element_By_Expression =>
               Actual := Get_Actual (Assoc);
            when Iir_Kind_Association_Element_By_Individual =>
               --  Directly create the whole value on the instance pool, as its
               --  life is longer than the statement.
               if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
                  Last_Individual := Create_Value_For_Type
                    (Out_Block, Get_Actual_Type (Assoc), Init_Value_Signal);
               else
                  Last_Individual := Create_Value_For_Type
                    (Out_Block, Get_Actual_Type (Assoc), Init_Value_Any);
               end if;
               Last_Individual :=
                 Unshare (Last_Individual, Instance_Pool);
               Elaboration.Create_Object (Subprg_Block, Inter);
               Subprg_Block.Objects (Get_Info (Inter).Slot) := Last_Individual;
               goto Continue;
            when others =>
               Error_Kind ("execute_association(1)", Assoc);
         end case;

         --  Compute actual value.
         case Get_Kind (Inter) is
            when Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               Val := Execute_Expression (Out_Block, Actual);
               Implicit_Array_Conversion
                 (Out_Block, Val, Get_Type (Formal), Assoc);
               Check_Constraints (Out_Block, Val, Get_Type (Formal), Assoc);
            when Iir_Kind_Interface_Signal_Declaration =>
               Val := Execute_Name (Out_Block, Actual, True);
               Implicit_Array_Conversion
                 (Out_Block, Val, Get_Type (Formal), Assoc);
            when Iir_Kind_Interface_Variable_Declaration =>
               Mode := Get_Mode (Inter);
               if Mode = Iir_In_Mode then
                  --  FIXME: Ref ?
                  Val := Execute_Expression (Out_Block, Actual);
               else
                  Val := Execute_Name (Out_Block, Actual, False);
               end if;

               --  FIXME: by value for scalars ?

               --  Keep ref for back-copy
               if Mode /= Iir_In_Mode then
                  if Subprg_Block.Actuals_Ref = null then
                     declare
                        subtype Actuals_Ref_Type is
                          Value_Array (Iir_Index32 (Nbr_Assoc));
                        function To_Value_Array_Acc is new
                          Ada.Unchecked_Conversion (System.Address,
                                                    Value_Array_Acc);
                        function Alloc_Actuals_Ref is new
                          Alloc_On_Pool_Addr (Actuals_Ref_Type);

                     begin
                        Subprg_Block.Actuals_Ref := To_Value_Array_Acc
                          (Alloc_Actuals_Ref
                             (Instance_Pool,
                              Actuals_Ref_Type'(Len => Iir_Index32 (Nbr_Assoc),
                                                V => (others => null))));
                     end;
                  end if;
                  Subprg_Block.Actuals_Ref.V (Assoc_Idx) :=
                    Unshare_Bounds (Val, Instance_Pool);
               end if;

               if Mode = Iir_Out_Mode then
                  if Get_Formal_Conversion (Assoc) /= Null_Iir then
                     --  For an OUT variable using an out conversion, don't
                     --  associate with the actual, create a temporary value.
                     Val := Create_Value_For_Type
                       (Out_Block, Get_Type (Formal), Init_Value_Default);
                  elsif Get_Kind (Get_Type (Formal)) in
                    Iir_Kinds_Scalar_Type_And_Subtype_Definition
                  then
                     --  These are passed by value.  Must be reset.
                     Val := Create_Value_For_Type
                       (Out_Block, Get_Type (Formal), Init_Value_Default);
                  end if;
               else
                  if Get_Kind (Assoc) =
                    Iir_Kind_Association_Element_By_Expression
                  then
                     Conv := Get_Actual_Conversion (Assoc);
                     if Conv /= Null_Iir then
                        Val := Execute_Assoc_Conversion
                          (Out_Block, Conv, Val);
                     end if;
                  end if;

                  --  FIXME: check constraints ?
               end if;

               Implicit_Array_Conversion
                 (Out_Block, Val, Get_Type (Formal), Assoc);

            when others =>
               Error_Kind ("execute_association(2)", Inter);
         end case;

         if Get_Whole_Association_Flag (Assoc) then
            case Get_Kind (Inter) is
               when Iir_Kind_Interface_Constant_Declaration
                 | Iir_Kind_Interface_Variable_Declaration
                 | Iir_Kind_Interface_File_Declaration =>
                  --  FIXME: Arguments are passed by copy.
                  Elaboration.Create_Object (Subprg_Block, Inter);
                  Subprg_Block.Objects (Get_Info (Inter).Slot) :=
                    Unshare (Val, Instance_Pool);
               when Iir_Kind_Interface_Signal_Declaration =>
                  Elaboration.Create_Signal (Subprg_Block, Inter);
                  Subprg_Block.Objects (Get_Info (Inter).Slot) :=
                    Unshare_Bounds (Val, Instance_Pool);
               when others =>
                  Error_Kind ("execute_association", Inter);
            end case;
         else
            Associate_By_Reference
              (Subprg_Block, Formal, Last_Individual, Val);
         end if;

         << Continue >> null;
         Next_Association_Interface (Assoc, Assoc_Inter);
         Assoc_Idx := Assoc_Idx + 1;
      end loop;

      Release (Marker, Expr_Pool);
   end Execute_Association;

   procedure Execute_Back_Association (Instance : Block_Instance_Acc)
   is
      Call : constant Iir := Get_Procedure_Call (Instance.Parent.Stmt);
      Imp : constant Iir := Get_Implementation (Call);
      Assoc : Iir;
      Assoc_Inter : Iir;
      Inter : Iir;
      Formal : Iir;
      Assoc_Idx : Iir_Index32;
   begin
      Assoc := Get_Parameter_Association_Chain (Call);
      Assoc_Inter := Get_Interface_Declaration_Chain (Imp);
      Assoc_Idx := 1;
      while Assoc /= Null_Iir loop
         if Get_Kind (Assoc) /= Iir_Kind_Association_Element_By_Individual then
            Inter := Get_Association_Interface (Assoc, Assoc_Inter);
            Formal := Get_Association_Formal (Assoc, Inter);

            case Get_Kind (Inter) is
               when Iir_Kind_Interface_Variable_Declaration =>
                  if Get_Mode (Inter) /= Iir_In_Mode
                    and then Get_Kind (Get_Type (Inter)) /=
                    Iir_Kind_File_Type_Definition
                  then
                     --  For out/inout variable interface, the value must
                     --  be copied (FIXME: unless when passed by reference ?).
                     declare
                        Targ : constant Iir_Value_Literal_Acc :=
                          Instance.Actuals_Ref.V (Assoc_Idx);
                        Base : constant Iir_Value_Literal_Acc :=
                          Instance.Objects (Get_Info (Inter).Slot);
                        Val : Iir_Value_Literal_Acc;
                        Conv : Iir;
                        Is_Sig : Boolean;
                        Expr_Mark : Mark_Type;
                     begin
                        Mark (Expr_Mark, Expr_Pool);

                        --  Extract for individual association.
                        Execute_Name_With_Base
                          (Instance, Formal, Base, Val, Is_Sig);
                        Conv := Get_Formal_Conversion (Assoc);
                        if Conv /= Null_Iir then
                           Val := Execute_Assoc_Conversion
                             (Instance, Conv, Val);
                           --  FIXME: free val ?
                        end if;
                        Store (Targ, Val);

                        Release (Expr_Mark, Expr_Pool);
                     end;
                  end if;
               when Iir_Kind_Interface_File_Declaration =>
                  null;
               when Iir_Kind_Interface_Signal_Declaration
                 | Iir_Kind_Interface_Constant_Declaration =>
                  null;
               when others =>
                  Error_Kind ("execute_back_association", Inter);
            end case;
         end if;
         Next_Association_Interface (Assoc, Assoc_Inter);
         Assoc_Idx := Assoc_Idx + 1;
      end loop;
   end Execute_Back_Association;

   function Execute_Foreign_Function_Call
     (Block: Block_Instance_Acc; Expr : Iir; Imp : Iir)
      return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      case Get_Identifier (Imp) is
         when Std_Names.Name_Get_Resolution_Limit =>
            Res := Create_I64_Value (1);
         when Std_Names.Name_Textio_Read_Real =>
            Res := Create_F64_Value
              (File_Operation.Textio_Read_Real (Block.Objects (1)));
         when others =>
            Error_Msg_Exec ("unsupported foreign function call", Expr);
      end case;
      return Res;
   end Execute_Foreign_Function_Call;

   -- BLOCK is the block instance in which the function call appears.
   function Execute_Function_Call
     (Block: Block_Instance_Acc; Expr: Iir; Imp : Iir)
      return Iir_Value_Literal_Acc
   is
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);
      Subprg_Block: Block_Instance_Acc;
      Prot_Block : Block_Instance_Acc;
      Assoc_Chain: Iir;
      Res : Iir_Value_Literal_Acc;
   begin
      Mark (Block.Marker, Instance_Pool.all);

      case Get_Kind (Expr) is
         when Iir_Kind_Function_Call =>
            Prot_Block := Get_Protected_Object_Instance (Block, Expr);
            Subprg_Block :=
              Create_Subprogram_Instance (Block, Prot_Block, Imp);
            Assoc_Chain := Get_Parameter_Association_Chain (Expr);
            Execute_Association
              (Block, Subprg_Block, Inter_Chain, Assoc_Chain);
            --  No out/inout interface for functions.
            pragma Assert (Subprg_Block.Actuals_Ref = null);
         when Iir_Kinds_Dyadic_Operator =>
            Subprg_Block := Create_Subprogram_Instance (Block, null, Imp);
            Execute_Dyadic_Association
              (Block, Subprg_Block, Expr, Inter_Chain);
         when Iir_Kinds_Monadic_Operator =>
            Subprg_Block := Create_Subprogram_Instance (Block, null, Imp);
            Execute_Monadic_Association
              (Block, Subprg_Block, Expr, Inter_Chain);
         when others =>
            Error_Kind ("execute_subprogram_call_init", Expr);
      end case;

      if Get_Foreign_Flag (Imp) then
         Res := Execute_Foreign_Function_Call (Subprg_Block, Expr, Imp);
      else
         Res := Execute_Function_Body (Subprg_Block);
      end if;

      --  Unfortunately, we don't know where the result has been allocated,
      --  so copy it before releasing the instance pool.
      Res := Unshare (Res, Expr_Pool'Access);

      Release (Block.Marker, Instance_Pool.all);

      return Res;
   end Execute_Function_Call;

   --  Slide an array VALUE using bounds from REF_VALUE.  Do not modify
   --  VALUE if not an array.
   procedure Implicit_Array_Conversion (Value : in out Iir_Value_Literal_Acc;
                                        Ref_Value : Iir_Value_Literal_Acc;
                                        Expr : Iir)
   is
      Res : Iir_Value_Literal_Acc;
   begin
      if Value.Kind /= Iir_Value_Array then
         return;
      end if;
      Res := Create_Array_Value (Value.Bounds.Nbr_Dims);
      Res.Val_Array := Value.Val_Array;
      for I in Value.Bounds.D'Range loop
         if Value.Bounds.D (I).Length /= Ref_Value.Bounds.D (I).Length then
            Error_Msg_Constraint (Expr);
            return;
         end if;
         Res.Bounds.D (I) := Ref_Value.Bounds.D (I);
      end loop;
      Value := Res;
   end Implicit_Array_Conversion;

   procedure Implicit_Array_Conversion (Instance : Block_Instance_Acc;
                                        Value : in out Iir_Value_Literal_Acc;
                                        Ref_Type : Iir;
                                        Expr : Iir)
   is
      Ref_Value : Iir_Value_Literal_Acc;
   begin
      --  Do array conversion only if REF_TYPE is a constrained array type
      --  definition.
      if Value.Kind /= Iir_Value_Array then
         return;
      end if;
      if Get_Constraint_State (Ref_Type) /= Fully_Constrained then
         return;
      end if;
      Ref_Value := Create_Array_Bounds_From_Type (Instance, Ref_Type, True);
      for I in Value.Bounds.D'Range loop
         if Value.Bounds.D (I).Length /= Ref_Value.Bounds.D (I).Length then
            Error_Msg_Constraint (Expr);
            return;
         end if;
      end loop;
      Ref_Value.Val_Array.V := Value.Val_Array.V;
      Value := Ref_Value;
   end Implicit_Array_Conversion;

   procedure Check_Range_Constraints (Instance : Block_Instance_Acc;
                                      Rng : Iir_Value_Literal_Acc;
                                      Rng_Type : Iir;
                                      Loc : Iir) is
   begin
      if not Is_Null_Range (Rng) then
         Check_Constraints (Instance, Rng.Left, Get_Type (Rng_Type), Loc);
         Check_Constraints (Instance, Rng.Right, Get_Type (Rng_Type), Loc);
      end if;
   end Check_Range_Constraints;

   procedure Check_Array_Constraints (Instance: Block_Instance_Acc;
                                      Value: Iir_Value_Literal_Acc;
                                      Def: Iir;
                                      Expr: Iir)
   is
      Index_List : Iir_Flist;
      Element_Subtype : Iir;
      New_Bounds : Iir_Value_Literal_Acc;
   begin
      --  Nothing to check for unconstrained arrays.
      if not Get_Index_Constraint_Flag (Def) then
         return;
      end if;

      Index_List := Get_Index_Subtype_List (Def);
      for I in Value.Bounds.D'Range loop
         New_Bounds := Execute_Bounds
           (Instance, Get_Nth_Element (Index_List, Natural (I - 1)));
         if not Is_Equal (Value.Bounds.D (I), New_Bounds) then
            Error_Msg_Constraint (Expr);
            return;
         end if;
      end loop;

      if Boolean'(False) then
         Index_List := Get_Index_List (Def);
         Element_Subtype := Get_Element_Subtype (Def);
         for I in Value.Val_Array.V'Range loop
            Check_Constraints
              (Instance, Value.Val_Array.V (I), Element_Subtype, Expr);
         end loop;
      end if;
   end Check_Array_Constraints;

   --  Check DEST and SRC are array compatible.
   procedure Check_Array_Match (Instance: Block_Instance_Acc;
                                Dest: Iir_Value_Literal_Acc;
                                Src : Iir_Value_Literal_Acc;
                                Expr: Iir)
   is
      pragma Unreferenced (Instance);
   begin
      for I in Dest.Bounds.D'Range loop
         if Dest.Bounds.D (I).Length /= Src.Bounds.D (I).Length then
            Error_Msg_Constraint (Expr);
            exit;
         end if;
      end loop;
   end Check_Array_Match;
   pragma Unreferenced (Check_Array_Match);

   procedure Check_Constraints (Instance: Block_Instance_Acc;
                                Value: Iir_Value_Literal_Acc;
                                Def: Iir;
                                Expr: Iir)
   is
      High, Low: Iir_Value_Literal_Acc;
      Bound : Iir_Value_Literal_Acc;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            Bound := Execute_Bounds (Instance, Def);
            if Bound.Dir = Dir_To then
               High := Bound.Right;
               Low := Bound.Left;
            else
               High := Bound.Left;
               Low := Bound.Right;
            end if;
            case Iir_Value_Scalars (Value.Kind) is
               when Iir_Value_I64 =>
                  if Value.I64 in Low.I64 .. High.I64 then
                     return;
                  end if;
               when Iir_Value_E8 =>
                  if Value.E8 in Low.E8 .. High.E8 then
                     return;
                  end if;
               when Iir_Value_E32 =>
                  if Value.E32 in Low.E32 .. High.E32 then
                     return;
                  end if;
               when Iir_Value_F64 =>
                  if Value.F64 in Low.F64 .. High.F64 then
                     return;
                  end if;
               when Iir_Value_B1 =>
                  if Value.B1 in Low.B1 .. High.B1 then
                     return;
                  end if;
            end case;
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition =>
            Check_Array_Constraints (Instance, Value, Def, Expr);
            return;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Def));
               El : Iir_Element_Declaration;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Check_Constraints
                    (Instance,
                     Value.Val_Record.V (Get_Element_Position (El) + 1),
                     Get_Type (El),
                     Expr);
               end loop;
            end;
            return;
         when Iir_Kind_Integer_Type_Definition =>
            return;
         when Iir_Kind_Floating_Type_Definition =>
            return;
         when Iir_Kind_Physical_Type_Definition =>
            return;
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            return;
         when Iir_Kind_File_Type_Definition =>
            return;
         when others =>
            Error_Kind ("check_constraints", Def);
      end case;
      Error_Msg_Constraint (Expr);
   end Check_Constraints;

   function Execute_Resolution_Function
     (Block: Block_Instance_Acc; Imp : Iir; Arr : Iir_Value_Literal_Acc)
      return Iir_Value_Literal_Acc
   is
      Inter : Iir;
      Instance : Block_Instance_Acc;
   begin
      -- Create a frame for this function.
      Instance := Create_Subprogram_Instance (Block, null, Imp);

      Inter := Get_Interface_Declaration_Chain (Imp);
      Elaboration.Create_Object (Instance, Inter);
      Instance.Objects (Get_Info (Inter).Slot) := Arr;

      return Execute_Function_Body (Instance);
   end Execute_Resolution_Function;

   procedure Execute_Signal_Assignment (Instance: Block_Instance_Acc;
                                        Stmt: Iir_Signal_Assignment_Statement;
                                        Wf : Iir)
   is
      Nbr_We : constant Natural := Get_Chain_Length (Wf);

      Transactions : Transaction_Type (Nbr_We);

      We: Iir_Waveform_Element;
      Res: Iir_Value_Literal_Acc;
      Rdest: Iir_Value_Literal_Acc;
      Targ_Type : Iir;
      Marker : Mark_Type;
   begin
      Mark (Marker, Expr_Pool);

      Rdest := Execute_Name (Instance, Get_Target (Stmt), True);
      Targ_Type := Get_Type (Get_Target (Stmt));

      --  Disconnection statement.
      if Wf = Null_Iir then
         Disconnect_Signal (Rdest);
         Release (Marker, Expr_Pool);
         return;
      elsif Get_Kind (Wf) = Iir_Kind_Unaffected_Waveform then
         return;
      end if;

      Transactions.Stmt := Stmt;

      -- LRM93 8.4.1
      -- Evaluation of a waveform consists of the evaluation of each waveform
      -- elements in the waveform.
      We := Wf;
      for I in Transactions.Els'Range loop
         declare
            Trans : Transaction_El_Type renames Transactions.Els (I);
         begin
            if Get_Time (We) /= Null_Iir then
               Res := Execute_Expression (Instance, Get_Time (We));
               -- LRM93 8.4.1
               -- It is an error if the time expression in a waveform element
               -- evaluates to a negative value.
               if Res.I64 < 0 then
                  Error_Msg_Exec ("time value is negative", Get_Time (We));
               end if;
               Trans.After := Std_Time (Res.I64);
            else
               -- LRM93 8.4.1
               -- If the after clause of a waveform element is not present,
               -- then an implicit "after 0 ns" is assumed.
               Trans.After := 0;
            end if;

            -- LRM93 8.4.1
            -- It is an error if the sequence of new transactions is not in
            -- ascending order with respect to time.
            if I > 1
              and then Trans.After <= Transactions.Els (I - 1).After
            then
               Error_Msg_Exec
                 ("sequence not in ascending order with respect to time", We);
            end if;

            if Get_Kind (Get_We_Value (We)) = Iir_Kind_Null_Literal then
               -- null transaction.
               Trans.Value := null;
            else
               -- LRM93 8.4.1
               -- For the first form of waveform element, the value component
               -- of the transaction is determined by the value expression in
               -- the waveform element.
               Trans.Value := Execute_Expression_With_Type
                 (Instance, Get_We_Value (We), Targ_Type);
            end if;
         end;
         We := Get_Chain (We);
      end loop;
      pragma Assert (We = Null_Iir);

      case Get_Delay_Mechanism (Stmt) is
         when Iir_Transport_Delay =>
            Transactions.Reject := 0;
         when Iir_Inertial_Delay =>
            -- LRM93 8.4
            -- or, in the case that a pulse rejection limit is specified,
            -- a pulse whose duration is shorter than that limit will not
            -- be transmitted.
            -- Every inertially delayed signal assignment has a pulse
            -- rejection limit.
            if Get_Reject_Time_Expression (Stmt) /= Null_Iir then
               -- LRM93 8.4
               -- If the delay mechanism specifies inertial delay, and if the
               -- reserved word reject followed by a time expression is
               -- present, then the time expression specifies the pulse
               -- rejection limit.
               Res := Execute_Expression
                 (Instance, Get_Reject_Time_Expression (Stmt));
               -- LRM93 8.4
               -- It is an error if the pulse rejection limit for any
               -- inertially delayed signal assignement statement is either
               -- negative ...
               if Res.I64 < 0 then
                  Error_Msg_Exec ("reject time negative", Stmt);
               end if;
               -- LRM93 8.4
               -- ... or greather than the time expression associated with
               -- the first waveform element.
               Transactions.Reject := Std_Time (Res.I64);
               if Transactions.Reject > Transactions.Els (1).After then
                  Error_Msg_Exec
                    ("reject time greather than time expression", Stmt);
               end if;
            else
               -- LRM93 8.4
               -- In all other cases, the pulse rejection limit is the time
               -- expression associated ith the first waveform element.
               Transactions.Reject := Transactions.Els (1).After;
            end if;
      end case;

      --  FIXME: slice Transactions to remove transactions after end of time.
      Assign_Value_To_Signal (Instance, Rdest, Transactions);

      Release (Marker, Expr_Pool);
   end Execute_Signal_Assignment;

   -- Display a message when an assertion has failed.
   -- REPORT is the value (string) to display, or null to use default message.
   -- SEVERITY is the severity or null to use default (error).
   -- STMT is used to display location.
   procedure Execute_Failed_Assertion (Msg : String;
                                       Report : String;
                                       Severity : Natural;
                                       Stmt: Iir) is
   begin
      -- LRM93 8.2
      -- The error message consists of at least:

      -- 4: name of the design unit containing the assertion.
      Put (Disp_Location (Stmt));

      Put (":@");
      Grt.Astdio.Vhdl.Put_Time (Grt.Stdio.stdout, Current_Time);

      -- 1: an indication that this message is from an assertion.
      Put (":(");
      Put (Msg);
      Put (' ');

      -- 2: the value of the severity level.
      case Severity is
         when 0 =>
            Put ("note");
         when 1 =>
            Put ("warning");
         when 2 =>
            Put ("error");
         when 3 =>
            Put ("failure");
         when others =>
            Error_Internal (Null_Iir, "execute_failed_assertion");
      end case;
      Put ("): ");

      -- 3: the value of the message string.
      Put_Line (Report);

      -- Stop execution if the severity is too high.
      if Severity >= Grt.Options.Severity_Level then
         Debug (Reason_Assert);
         Grt.Errors.Fatal_Error;
      end if;
   end Execute_Failed_Assertion;

   procedure Execute_Failed_Assertion (Instance: Block_Instance_Acc;
                                       Label : String;
                                       Stmt : Iir;
                                       Default_Msg : String;
                                       Default_Severity : Natural)
   is
      Expr: Iir;
      Report, Severity_Lit: Iir_Value_Literal_Acc;
      Severity : Natural;
      Marker : Mark_Type;
   begin
      Mark (Marker, Expr_Pool);
      Expr := Get_Report_Expression (Stmt);
      if Expr /= Null_Iir then
         Report := Execute_Expression (Instance, Expr);
      else
         Report := null;
      end if;
      Expr := Get_Severity_Expression (Stmt);
      if Expr /= Null_Iir then
         Severity_Lit := Execute_Expression (Instance, Expr);
         Severity := Natural'Val (Severity_Lit.E8);
      else
         Severity := Default_Severity;
      end if;
      if Report /= null then
         declare
            Msg : String (1 .. Natural (Report.Val_Array.Len));
         begin
            for I in Report.Val_Array.V'Range loop
               Msg (Positive (I)) :=
                 Character'Val (Report.Val_Array.V (I).E8);
            end loop;
            Execute_Failed_Assertion (Label, Msg, Severity, Stmt);
         end;
      else
         Execute_Failed_Assertion (Label, Default_Msg, Severity, Stmt);
      end if;
      Release (Marker, Expr_Pool);
   end Execute_Failed_Assertion;

   function Is_In_Choice (Instance : Block_Instance_Acc;
                          Choice : Iir;
                          Expr : Iir_Value_Literal_Acc)
                         return Boolean
   is
      Res : Boolean;
   begin
      case Get_Kind (Choice) is
         when Iir_Kind_Choice_By_Others =>
            return True;
         when Iir_Kind_Choice_By_Expression =>
            declare
               Expr1: Iir_Value_Literal_Acc;
            begin
               Expr1 := Execute_Expression
                 (Instance, Get_Choice_Expression (Choice));
               Res := Is_Equal (Expr, Expr1);
               return Res;
            end;
         when Iir_Kind_Choice_By_Range =>
            declare
               A_Range : Iir_Value_Literal_Acc;
            begin
               A_Range := Execute_Bounds
                 (Instance, Get_Choice_Range (Choice));
               Res := Is_In_Range (Expr, A_Range);
            end;
            return Res;
         when others =>
            Error_Kind ("is_in_choice", Choice);
      end case;
   end Is_In_Choice;

   function Execute_Choice (Instance : Block_Instance_Acc;
                            Expr : Iir;
                            First_Assoc : Iir) return Iir
   is
      Value: Iir_Value_Literal_Acc;
      Assoc: Iir;
      Assoc_Res : Iir;
      Marker : Mark_Type;
   begin
      Mark (Marker, Expr_Pool);
      Assoc := First_Assoc;

      Value := Execute_Expression (Instance, Expr);
      if Get_Type_Staticness (Get_Type (Expr)) /= Locally
        and then Get_Kind (Assoc) = Iir_Kind_Choice_By_Expression
      then
         --  Choice is not locally constrained, check length.
         declare
            Choice_Type : constant Iir :=
              Get_Type (Get_Choice_Expression (Assoc));
            Choice_Len : Int64;
         begin
            Choice_Len := Vhdl.Evaluation.Eval_Discrete_Type_Length
              (Get_String_Type_Bound_Type (Choice_Type));
            if Choice_Len /= Int64 (Value.Bounds.D (1).Length) then
               Error_Msg_Constraint (Expr);
            end if;
         end;
      end if;

      while Assoc /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Assoc_Res := Assoc;
         end if;

         if Is_In_Choice (Instance, Assoc, Value) then
            Release (Marker, Expr_Pool);
            return Assoc_Res;
         end if;

         Assoc := Get_Chain (Assoc);
      end loop;
      --  FIXME: infinite loop???
      Error_Msg_Exec ("no choice for expression", Expr);
      raise Internal_Error;
   end Execute_Choice;

   --  Return TRUE iff VAL is in the range defined by BOUNDS.
   function Is_In_Range (Val : Iir_Value_Literal_Acc;
                         Bounds : Iir_Value_Literal_Acc)
     return Boolean
   is
      Max, Min : Iir_Value_Literal_Acc;
   begin
      case Bounds.Dir is
         when Dir_To =>
            Min := Bounds.Left;
            Max := Bounds.Right;
         when Dir_Downto =>
            Min := Bounds.Right;
            Max := Bounds.Left;
      end case;

      case Iir_Value_Discrete (Val.Kind) is
         when Iir_Value_E8 =>
            return Val.E8 >= Min.E8 and Val.E8 <= Max.E8;
         when Iir_Value_E32 =>
            return Val.E32 >= Min.E32 and Val.E32 <= Max.E32;
         when Iir_Value_B1 =>
            return Val.B1 >= Min.B1 and Val.B1 <= Max.B1;
         when Iir_Value_I64 =>
            return Val.I64 >= Min.I64 and Val.I64 <= Max.I64;
      end case;
   end Is_In_Range;

   --  Increment or decrement VAL according to BOUNDS.DIR.
   --  FIXME: use increment ?
   procedure Update_Loop_Index (Val : Iir_Value_Literal_Acc;
                                Bounds : Iir_Value_Literal_Acc)
   is
   begin
      case Iir_Value_Discrete (Val.Kind) is
         when Iir_Value_E8 =>
            case Bounds.Dir is
               when Dir_To =>
                  Val.E8 := Val.E8 + 1;
               when Dir_Downto =>
                  Val.E8 := Val.E8 - 1;
            end case;
         when Iir_Value_E32 =>
            case Bounds.Dir is
               when Dir_To =>
                  Val.E32 := Val.E32 + 1;
               when Dir_Downto =>
                  Val.E32 := Val.E32 - 1;
            end case;
         when Iir_Value_B1 =>
            case Bounds.Dir is
               when Dir_To =>
                  Val.B1 := True;
               when Dir_Downto =>
                  Val.B1 := False;
            end case;
         when Iir_Value_I64 =>
            case Bounds.Dir is
               when Dir_To =>
                  Val.I64 := Val.I64 + 1;
               when Dir_Downto =>
                  Val.I64 := Val.I64 - 1;
            end case;
      end case;
   end Update_Loop_Index;

   procedure Finalize_For_Loop_Statement (Instance : Block_Instance_Acc;
                                          Stmt : Iir)
   is
   begin
      Destroy_Iterator_Declaration
        (Instance, Get_Parameter_Specification (Stmt));
   end Finalize_For_Loop_Statement;

   procedure Finalize_Loop_Statement (Instance : Block_Instance_Acc;
                                      Stmt : Iir)
   is
   begin
      if Get_Kind (Stmt) = Iir_Kind_For_Loop_Statement then
         Finalize_For_Loop_Statement (Instance, Stmt);
      end if;
   end Finalize_Loop_Statement;

   procedure Execute_For_Loop_Statement (Proc : Process_State_Acc)
   is
      Instance : constant Block_Instance_Acc := Proc.Instance;
      Stmt : constant Iir_For_Loop_Statement := Instance.Stmt;
      Iterator : constant Iir := Get_Parameter_Specification (Stmt);
      Bounds : Iir_Value_Literal_Acc;
      Index : Iir_Value_Literal_Acc;
      Stmt_Chain : Iir;
      Is_Nul : Boolean;
      Marker : Mark_Type;
   begin
      --  Elaborate the iterator (and its type).
      Elaborate_Declaration (Instance, Iterator);

      -- Extract bounds.
      Mark (Marker, Expr_Pool);
      Bounds := Execute_Bounds (Instance, Get_Type (Iterator));
      Index := Instance.Objects (Get_Info (Iterator).Slot);
      Store (Index, Bounds.Left);
      Is_Nul := Is_Null_Range (Bounds);
      Release (Marker, Expr_Pool);

      if Is_Nul then
         -- Loop is complete.
         Finalize_For_Loop_Statement (Instance, Stmt);
         Update_Next_Statement (Proc);
      else
         Stmt_Chain := Get_Sequential_Statement_Chain (Stmt);
         if Stmt_Chain = Null_Iir then
            --  Nothing to do for an empty loop.
            Finalize_For_Loop_Statement (Instance, Stmt);
            Update_Next_Statement (Proc);
         else
            Instance.Stmt := Stmt_Chain;
         end if;
      end if;
   end Execute_For_Loop_Statement;

   --  This function is called when there is no more statements to execute
   --  in the statement list of a for_loop.  Returns FALSE in case of end of
   --  loop.
   function Finish_For_Loop_Statement (Instance : Block_Instance_Acc)
                                      return Boolean
   is
      Iterator : constant Iir := Get_Parameter_Specification (Instance.Stmt);
      Bounds : Iir_Value_Literal_Acc;
      Index : Iir_Value_Literal_Acc;
      Marker : Mark_Type;
   begin
      --  FIXME: avoid allocation.
      Mark (Marker, Expr_Pool);
      Bounds := Execute_Bounds (Instance, Get_Type (Iterator));
      Index := Instance.Objects (Get_Info (Iterator).Slot);

      if Is_Equal (Index, Bounds.Right) then
         -- Loop is complete.
         Release (Marker, Expr_Pool);
         Finalize_For_Loop_Statement (Instance, Instance.Stmt);
         return False;
      else
         -- Update the loop index.
         Update_Loop_Index (Index, Bounds);

         Release (Marker, Expr_Pool);

         -- start the loop again.
         Instance.Stmt := Get_Sequential_Statement_Chain (Instance.Stmt);
         return True;
      end if;
   end Finish_For_Loop_Statement;

   --  Evaluate boolean condition COND.  If COND is Null_Iir, returns true.
   function Execute_Condition (Instance : Block_Instance_Acc;
                               Cond : Iir) return Boolean
   is
      V : Iir_Value_Literal_Acc;
      Res : Boolean;
      Marker : Mark_Type;
   begin
      if Cond = Null_Iir then
         return True;
      end if;

      Mark (Marker, Expr_Pool);
      V := Execute_Expression (Instance, Cond);
      Res := V.B1 = True;
      Release (Marker, Expr_Pool);
      return Res;
   end Execute_Condition;

   --  Start a while loop statement, or return FALSE if the loop is not
   --  executed.
   procedure Execute_While_Loop_Statement (Proc : Process_State_Acc)
   is
      Instance: constant Block_Instance_Acc := Proc.Instance;
      Stmt : constant Iir := Instance.Stmt;
      Cond : Boolean;
   begin
      Cond := Execute_Condition (Instance, Get_Condition (Stmt));
      if Cond then
         Init_Sequential_Statements (Proc, Stmt);
      else
         Update_Next_Statement (Proc);
      end if;
   end Execute_While_Loop_Statement;

   --  This function is called when there is no more statements to execute
   --  in the statement list of a while loop.  Returns FALSE iff loop is
   --  completed.
   function Finish_While_Loop_Statement (Instance : Block_Instance_Acc)
                                        return Boolean
   is
      Cond : Boolean;
   begin
      Cond := Execute_Condition (Instance, Get_Condition (Instance.Stmt));

      if Cond then
         -- start the loop again.
         Instance.Stmt := Get_Sequential_Statement_Chain (Instance.Stmt);
         return True;
      else
         -- Loop is complete.
         return False;
      end if;
   end Finish_While_Loop_Statement;

   --  Return TRUE if the loop must be executed again
   function Finish_Loop_Statement (Instance : Block_Instance_Acc; Stmt : Iir)
                                  return Boolean is
   begin
      Instance.Stmt := Stmt;
      case Get_Kind (Stmt) is
         when Iir_Kind_While_Loop_Statement =>
            return Finish_While_Loop_Statement (Instance);
         when Iir_Kind_For_Loop_Statement =>
            return Finish_For_Loop_Statement (Instance);
         when others =>
            Error_Kind ("finish_loop_statement", Stmt);
      end case;
   end Finish_Loop_Statement;

   --  Return FALSE if the next statement should be executed (possibly
   --  updated).
   procedure Execute_Exit_Next_Statement (Proc : Process_State_Acc;
                                          Is_Exit : Boolean)
   is
      Instance : constant Block_Instance_Acc := Proc.Instance;
      Stmt : constant Iir := Instance.Stmt;
      Label : constant Iir := Get_Named_Entity (Get_Loop_Label (Stmt));
      Cond : Boolean;
      Parent : Iir;
   begin
      Cond := Execute_Condition (Instance, Get_Condition (Stmt));
      if not Cond then
         Update_Next_Statement (Proc);
         return;
      end if;

      Parent := Stmt;
      loop
         Parent := Get_Parent (Parent);
         case Get_Kind (Parent) is
            when Iir_Kind_For_Loop_Statement
              | Iir_Kind_While_Loop_Statement =>
               if Label = Null_Iir or else Label = Parent then
                  --  Target is this statement.
                  if Is_Exit then
                     Finalize_Loop_Statement (Instance, Parent);
                     Instance.Stmt := Parent;
                     Update_Next_Statement (Proc);
                  elsif not Finish_Loop_Statement (Instance, Parent) then
                     Update_Next_Statement (Proc);
                  else
                     Init_Sequential_Statements (Proc, Parent);
                  end if;
                  return;
               else
                  Finalize_Loop_Statement (Instance, Parent);
               end if;
            when others =>
               null;
         end case;
      end loop;
   end Execute_Exit_Next_Statement;

   procedure Execute_Case_Statement (Proc : Process_State_Acc)
   is
      Instance : constant Block_Instance_Acc := Proc.Instance;
      Stmt : constant Iir := Instance.Stmt;
      Assoc: Iir;
      Stmt_Chain : Iir;
   begin
      Assoc := Execute_Choice (Instance, Get_Expression (Stmt),
                               Get_Case_Statement_Alternative_Chain (Stmt));
      Stmt_Chain := Get_Associated_Chain (Assoc);
      if Stmt_Chain = Null_Iir then
         Update_Next_Statement (Proc);
      else
         Instance.Stmt := Stmt_Chain;
      end if;
   end Execute_Case_Statement;

   procedure Execute_Call_Statement (Proc : Process_State_Acc)
   is
      Instance : constant Block_Instance_Acc := Proc.Instance;
      Stmt : constant Iir := Instance.Stmt;
      Call : constant Iir := Get_Procedure_Call (Stmt);
      Imp  : constant Iir := Get_Implementation (Call);
      Subprg_Instance : Block_Instance_Acc;
      Prot_Block : Block_Instance_Acc;
      Assoc_Chain: Iir;
      Inter_Chain : Iir;
   begin
      if Get_Implicit_Definition (Imp) in Iir_Predefined_Implicit then
         Execute_Implicit_Procedure (Instance, Call);
         Update_Next_Statement (Proc);
      elsif Get_Foreign_Flag (Imp) then
         Execute_Foreign_Procedure (Instance, Call);
         Update_Next_Statement (Proc);
      else
         Mark (Instance.Marker, Instance_Pool.all);
         Prot_Block := Get_Protected_Object_Instance (Instance, Call);
         Subprg_Instance :=
           Create_Subprogram_Instance (Instance, Prot_Block, Imp);
         Assoc_Chain := Get_Parameter_Association_Chain (Call);
         Inter_Chain := Get_Interface_Declaration_Chain (Imp);
         Execute_Association
           (Instance, Subprg_Instance, Inter_Chain, Assoc_Chain);

         Current_Process.Instance := Subprg_Instance;
         Elaborate_Declarative_Part
           (Subprg_Instance, Get_Declaration_Chain (Subprg_Instance.Bod));

         Init_Sequential_Statements (Proc, Subprg_Instance.Bod);
      end if;
   end Execute_Call_Statement;

   procedure Finish_Procedure_Frame (Proc : Process_State_Acc)
   is
      Old_Instance : constant Block_Instance_Acc := Proc.Instance;
   begin
      Execute_Back_Association (Old_Instance);
      Proc.Instance := Old_Instance.Parent;
      Execute_Subprogram_Call_Final (Old_Instance);
      Release (Proc.Instance.Marker, Instance_Pool.all);
   end Finish_Procedure_Frame;

   procedure Execute_If_Statement (Proc : Process_State_Acc; Stmt : Iir)
   is
      Clause: Iir;
      Cond: Boolean;
   begin
      Clause := Stmt;
      loop
         Cond := Execute_Condition (Proc.Instance, Get_Condition (Clause));
         if Cond then
            Init_Sequential_Statements (Proc, Clause);
            return;
         end if;
         Clause := Get_Else_Clause (Clause);
         exit when Clause = Null_Iir;
      end loop;
      Update_Next_Statement (Proc);
   end Execute_If_Statement;

   procedure Execute_Variable_Assignment (Proc : Process_State_Acc; Stmt : Iir)
   is
      Instance : constant Block_Instance_Acc := Proc.Instance;
      Target : constant Iir := Get_Target (Stmt);
      Target_Type : constant Iir := Get_Type (Target);
      Expr : constant Iir := Get_Expression (Stmt);
      Expr_Type : constant Iir := Get_Type (Expr);
      Target_Val: Iir_Value_Literal_Acc;
      Res : Iir_Value_Literal_Acc;
      Marker : Mark_Type;
   begin
      Mark (Marker, Expr_Pool);
      Target_Val := Execute_Expression (Instance, Target);

      --  If the type of the target is not static and the value is
      --  an aggregate, then the aggregate may be contrained by the
      --  target.
      if Get_Kind (Expr) = Iir_Kind_Aggregate
        and then Get_Kind (Expr_Type) in Iir_Kinds_Array_Type_Definition
      then
         Res := Copy_Array_Bound (Target_Val);
         Fill_Array_Aggregate (Instance, Expr, Res);
      else
         Res := Execute_Expression (Instance, Expr);
      end if;
      if Get_Kind (Target_Type) in Iir_Kinds_Array_Type_Definition then
         --  Note: target_type may be dynamic (slice case), so
         --  check_constraints is not called.
         Implicit_Array_Conversion (Res, Target_Val, Stmt);
      else
         Check_Constraints (Instance, Res, Target_Type, Stmt);
      end if;

      --  Note: we need to unshare before copying to avoid
      --  overwrites (in assignments like: v (1 to 4) := v (3 to 6)).
      --  FIXME: improve that handling (detect overlaps before).
      Store (Target_Val, Unshare (Res, Expr_Pool'Access));

      Release (Marker, Expr_Pool);
   end Execute_Variable_Assignment;

   function Execute_Return_Statement (Proc : Process_State_Acc)
                                     return Boolean
   is
      Res : Iir_Value_Literal_Acc;
      Instance : constant Block_Instance_Acc := Proc.Instance;
      Stmt : constant Iir := Instance.Stmt;
      Expr : constant Iir := Get_Expression (Stmt);
   begin
      if Expr /= Null_Iir then
         Res := Execute_Expression (Instance, Expr);
         Implicit_Array_Conversion (Instance, Res, Get_Type (Stmt), Stmt);
         Check_Constraints (Instance, Res, Get_Type (Stmt), Stmt);
         Instance.Result := Res;
      end if;

      case Get_Kind (Instance.Label) is
         when Iir_Kind_Procedure_Declaration =>
            Finish_Procedure_Frame (Proc);
            Update_Next_Statement (Proc);
            return False;
         when Iir_Kind_Function_Declaration =>
            return True;
         when others =>
            raise Internal_Error;
      end case;
   end Execute_Return_Statement;

   procedure Finish_Sequential_Statements
     (Proc : Process_State_Acc; Complex_Stmt : Iir)
   is
      Instance : Block_Instance_Acc := Proc.Instance;
      Stmt : Iir;
   begin
      Stmt := Complex_Stmt;
      loop
         Instance.Stmt := Stmt;
         case Get_Kind (Stmt) is
            when Iir_Kind_For_Loop_Statement =>
               if Finish_For_Loop_Statement (Instance) then
                  return;
               end if;
            when Iir_Kind_While_Loop_Statement =>
               if Finish_While_Loop_Statement (Instance) then
                  return;
               end if;
            when Iir_Kind_Case_Statement
              | Iir_Kind_If_Statement =>
               null;
            when Iir_Kind_Sensitized_Process_Statement =>
               Instance.Stmt := Null_Iir;
               return;
            when Iir_Kind_Process_Statement =>
               --  Start again.
               Instance.Stmt := Get_Sequential_Statement_Chain (Stmt);
               return;
            when Iir_Kind_Procedure_Body =>
               Finish_Procedure_Frame (Proc);
               Instance := Proc.Instance;
            when Iir_Kind_Function_Body =>
               Error_Msg_Exec ("missing return statement in function", Stmt);
            when others =>
               Error_Kind ("execute_next_statement", Stmt);
         end case;
         Stmt := Get_Chain (Instance.Stmt);
         if Stmt /= Null_Iir then
            Instance.Stmt := Stmt;
            return;
         end if;
         Stmt := Get_Parent (Instance.Stmt);
      end loop;
   end Finish_Sequential_Statements;

   procedure Init_Sequential_Statements
     (Proc : Process_State_Acc; Complex_Stmt : Iir)
   is
      Stmt : Iir;
   begin
      Stmt := Get_Sequential_Statement_Chain (Complex_Stmt);
      if Stmt /= Null_Iir then
         Proc.Instance.Stmt := Stmt;
      else
         Finish_Sequential_Statements (Proc, Complex_Stmt);
      end if;
   end Init_Sequential_Statements;

   procedure Update_Next_Statement (Proc : Process_State_Acc)
   is
      Instance : constant Block_Instance_Acc := Proc.Instance;
      Stmt : Iir;
   begin
      Stmt := Get_Chain (Instance.Stmt);
      if Stmt /= Null_Iir then
         Instance.Stmt := Stmt;
         return;
      end if;
      Finish_Sequential_Statements (Proc, Get_Parent (Instance.Stmt));
   end Update_Next_Statement;

   procedure Execute_Sequential_Statements (Proc : Process_State_Acc)
   is
      Instance : Block_Instance_Acc;
      Stmt: Iir;
   begin
      loop
         Instance := Proc.Instance;
         Stmt := Instance.Stmt;

         --  End of process or subprogram.
         exit when Stmt = Null_Iir;

         if Trace_Statements then
            declare
               Name : Name_Id;
               Line : Natural;
               Col : Natural;
            begin
               Files_Map.Location_To_Position
                 (Get_Location (Stmt), Name, Line, Col);
               Put_Line ("Execute statement at "
                           & Name_Table.Image (Name)
                           & Natural'Image (Line));
            end;
         end if;

         if Flag_Need_Debug then
            Debug (Reason_Break);
         end if;

         -- execute statement STMT.
         case Get_Kind (Stmt) is
            when Iir_Kind_Null_Statement =>
               Update_Next_Statement (Proc);

            when Iir_Kind_If_Statement =>
               Execute_If_Statement (Proc, Stmt);

            when Iir_Kind_Simple_Signal_Assignment_Statement =>
               Execute_Signal_Assignment
                 (Instance, Stmt, Get_Waveform_Chain (Stmt));
               Update_Next_Statement (Proc);

            when Iir_Kind_Selected_Waveform_Assignment_Statement =>
               declare
                  Assoc : Iir;
               begin
                  Assoc := Execute_Choice (Instance, Get_Expression (Stmt),
                                           Get_Selected_Waveform_Chain (Stmt));
                  Execute_Signal_Assignment
                    (Instance, Stmt, Get_Associated_Chain (Assoc));
                  Update_Next_Statement (Proc);
               end;
            when Iir_Kind_Assertion_Statement =>
               declare
                  Res : Boolean;
               begin
                  Res := Execute_Condition
                    (Instance, Get_Assertion_Condition (Stmt));
                  if not Res then
                     Execute_Failed_Assertion (Instance, "assertion", Stmt,
                                               "Assertion violation.", 2);
                  end if;
               end;
               Update_Next_Statement (Proc);

            when Iir_Kind_Report_Statement =>
               Execute_Failed_Assertion (Instance, "report", Stmt,
                                         "Assertion violation.", 0);
               Update_Next_Statement (Proc);

            when Iir_Kind_Variable_Assignment_Statement =>
               Execute_Variable_Assignment (Proc, Stmt);
               Update_Next_Statement (Proc);

            when Iir_Kind_Return_Statement =>
               if Execute_Return_Statement (Proc) then
                  return;
               end if;

            when Iir_Kind_For_Loop_Statement =>
               Execute_For_Loop_Statement (Proc);

            when Iir_Kind_While_Loop_Statement =>
               Execute_While_Loop_Statement (Proc);

            when Iir_Kind_Case_Statement =>
               Execute_Case_Statement (Proc);

            when Iir_Kind_Wait_Statement =>
               if Execute_Wait_Statement (Instance, Stmt) then
                  return;
               end if;
               Update_Next_Statement (Proc);

            when Iir_Kind_Procedure_Call_Statement =>
               Execute_Call_Statement (Proc);

            when Iir_Kind_Exit_Statement =>
               Execute_Exit_Next_Statement (Proc, True);
            when Iir_Kind_Next_Statement =>
               Execute_Exit_Next_Statement (Proc, False);

            when others =>
               Error_Kind ("execute_sequential_statements", Stmt);
         end case;
      end loop;
   end Execute_Sequential_Statements;
end Simul.Execution;
