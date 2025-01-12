--  Verilog semantic analyzer (expressions)
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
with Std_Names;
with Verilog.Types; use Verilog.Types;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Sem_Utils; use Verilog.Sem_Utils;
with Verilog.Sem_Types; use Verilog.Sem_Types;
with Verilog.Sem_Stmts; use Verilog.Sem_Stmts;
with Verilog.Sem_Names; use Verilog.Sem_Names;
with Verilog.Sem_Scopes; use Verilog.Sem_Scopes;
with Verilog.Sem_Eval; use Verilog.Sem_Eval;

package body Verilog.Sem_Expr is
   --  Used for concatenation element of size 0.
   --  Bounds are meaningless.
   Null_Packed_Array_Type : Node := Null_Node;

   --  Insert an N_Conversion node (using CONV) around EXPR.
   function Insert_Conversion (Conv : Conv_Ops; Expr : Node; Rtype : Node)
                              return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Conversion);
      Location_Copy (Res, Expr);
      Set_Conversion_Op (Res, Conv);
      Set_Expression (Res, Expr);
      Set_Expr_Type (Res, Rtype);
      return Res;
   end Insert_Conversion;

   function Implicit_Vec_Vec_Conversion_Op (Etype : Node;
                                            Rtype : Node;
                                            Zext_Op : Conv_Ops;
                                            Sext_Op : Conv_Ops;
                                            Trunc_Op : Conv_Ops;
                                            Nop_Op : Conv_Ops) return Conv_Ops
   is
      Esize : constant Width_Type := Get_Type_Width (Etype);
      Rsize : constant Width_Type := Get_Type_Width (Rtype);
   begin
      --  IEEE 1800-2012 10.7
      --  The following are the steps for evaluating an
      --  assignment:
      --  - [...]
      --  - When the right-hand side evaluate to fewer bits than
      --    the left-hand side, the right-hand side value is
      --    padded to the size of the left-hand side.  If the
      --    right-hand side is unsigned, it is padded according
      --    to the rules specified in 11.6.1.  If the right-hand
      --    side is signed, it is sign extended.
      --  - If the left-hand side is smaller than the right-hand
      --    side, truncation shall occur, as described in the
      --    following paragraphs.
      if Esize < Rsize then
         if not Get_Signed_Flag (Etype) then
            return Zext_Op;
         else
            return Sext_Op;
         end if;
      elsif Esize > Rsize then
         return Trunc_Op;
      else
         if Get_Kind (Etype) /= Get_Kind (Rtype)
           or else Get_Signed_Flag (Etype) /= Get_Signed_Flag (Rtype)
         then
            --  Type conversion or sign conversion.
            return Nop_Op;
         else
            --  Same width, same sign.
            return Convop_None;
         end if;
      end if;
   end Implicit_Vec_Vec_Conversion_Op;

   function Implicit_Conversion_Op
     (Rtype_Orig : Node; Etype : Node) return Conv_Ops
   is
      Rtype : Node;
   begin
      if Etype = Rtype_Orig then
         --  Nothing to convert.
         return Convop_None;
      end if;

      Rtype := Rtype_Orig;

      case Get_Kind (Etype) is
         when N_Logic_Type =>
            --  From logic to ...
            case Get_Kind (Rtype) is
               when N_Logic_Type =>
                  --  ... logic (different sign).
                  raise Program_Error;
               when N_Bit_Type =>
                  --  ... bit
                  return Convop_Log_Bit;
               when N_Log_Packed_Array_Cst =>
                  --  ... logic vector
                  if Get_Signed_Flag (Rtype) then
                     return Convop_Log_Slv;
                  else
                     return Convop_Log_Ulv;
                  end if;
               when N_Bit_Packed_Array_Cst =>
                  --  ... logic vector
                  if Get_Signed_Flag (Rtype) then
                     return Convop_Log_Sbv;
                  else
                     return Convop_Log_Ubv;
                  end if;
               when N_Real_Type =>
                  --  ... real
                  return Convop_Log_Real;
               when N_Shortreal_Type =>
                  --  ... shortreal
                  return Convop_Log_Shortreal;
               when others =>
                  Error_Kind ("implicit_conversion (logic)", Rtype);
            end case;
         when N_Bit_Type =>
            --  From bit to ...
            case Get_Kind (Rtype) is
               when N_Logic_Type =>
                  return Convop_Bit_Log;
               when N_Log_Packed_Array_Cst =>
                  --  ... logic vector
                  if Get_Signed_Flag (Rtype) then
                     return Convop_Bit_Slv;
                  else
                     return Convop_Bit_Ulv;
                  end if;
               when N_Bit_Packed_Array_Cst =>
                  --  ... bit vector
                  if Get_Signed_Flag (Rtype) then
                     return Convop_Bit_Sbv;
                  else
                     return Convop_Bit_Ubv;
                  end if;
               when others =>
                  Error_Kind ("implicit_conversion (bit)", Rtype);
            end case;
         when N_Log_Packed_Array_Cst =>
            --  From logic vector to ...
            loop
               case Get_Kind (Rtype) is
                  when N_Logic_Type =>
                     --  ..  logic
                     return Convop_Lv_Log;
                  when N_Bit_Type =>
                     --  ..  bit
                     return Convop_Lv_Bit;
                  when N_Log_Packed_Array_Cst =>
                     --  ..  logic vector
                     return Implicit_Vec_Vec_Conversion_Op
                       (Etype, Rtype,
                        Convop_Lv_Zext, Convop_Lv_Sext,
                        Convop_Lv_Trunc, Convop_Lv_Nop);
                  when N_Bit_Packed_Array_Cst =>
                     --  ..  bit vector
                     return Implicit_Vec_Vec_Conversion_Op
                       (Etype, Rtype,
                        Convop_Lv_Bv_Zext, Convop_Lv_Bv_Sext,
                        Convop_Lv_Bv_Trunc, Convop_Lv_Bv);
                  when N_Real_Type
                    | N_Shortreal_Type =>
                     --  ..  float
                     return Convop_Lv_Float;
                  when N_Packed_Struct_Type =>
                     Rtype := Get_Packed_Base_Type (Rtype);
                     --  Try again.
                  when others =>
                     Error_Kind ("implicit_conversion (logic vector)", Rtype);
               end case;
            end loop;
         when N_Bit_Packed_Array_Cst =>
            --  From bit vector to ...
            loop
               case Get_Kind (Rtype) is
                  when N_Bit_Type =>
                     --  ..  bit
                     return Convop_Bv_Bit;
                  when N_Logic_Type =>
                     --  ..  logic
                     return Convop_Bv_Log;
                  when N_Bit_Packed_Array_Cst =>
                     --  ..  bit vector
                     return Implicit_Vec_Vec_Conversion_Op
                       (Etype, Rtype,
                        Convop_Bv_Zext, Convop_Bv_Sext,
                        Convop_Bv_Trunc, Convop_Bv_Nop);
                  when N_Log_Packed_Array_Cst =>
                     --  ..  logic vector
                     return Implicit_Vec_Vec_Conversion_Op
                       (Etype, Rtype,
                        Convop_Bv_Lv_Zext, Convop_Bv_Lv_Sext,
                        Convop_Bv_Lv_Trunc, Convop_Bv_Lv);
                  when N_Real_Type
                    | N_Shortreal_Type =>
                     --  ..  float
                     return Convop_Bv_Float;
                  when N_Packed_Struct_Type =>
                     Rtype := Get_Packed_Base_Type (Rtype);
                     --  Try again.
                  when others =>
                     Error_Kind ("implicit_conversion (bit vector)", Rtype);
               end case;
            end loop;
         when N_Real_Type =>
            --  From real to ...
            case Get_Kind (Rtype) is
               when N_Shortreal_Type =>
                  return Convop_Fp64_Fp32;
               when N_Log_Packed_Array_Cst =>
                  if Get_Signed_Flag (Rtype) then
                     return Convop_Fp64_Slv;
                  else
                     return Convop_Fp64_Ulv;
                  end if;
               when N_Bit_Packed_Array_Cst =>
                  if Get_Signed_Flag (Rtype) then
                     return Convop_Fp64_Sbv;
                  else
                     return Convop_Fp64_Ubv;
                  end if;
               when others =>
                  Error_Kind ("implicit_conversion (real)", Rtype);
            end case;
         when N_Shortreal_Type =>
            --  From shortreal to ...
            case Get_Kind (Rtype) is
               when N_Real_Type =>
                  return Convop_Fp32_Fp64;
               when others =>
                  Error_Kind ("implicit_conversion (real)", Rtype);
            end case;
         when N_Enum_Type =>
            --  1800-2017 6.19.3 Type checking
            --  Enumerated variables are auto-cast into integral values.
            return Implicit_Conversion_Op (Rtype, Get_Enum_Base_Type (Etype));
         when N_Packed_Struct_Type =>
            return Implicit_Conversion_Op
              (Rtype, Get_Packed_Base_Type (Etype));
         when others =>
            Error_Kind ("implicit_conversion", Etype);
      end case;
   end Implicit_Conversion_Op;

   function Implicit_Conversion (Expr : Node; Rtype : Node) return Node
   is
      Op : Conv_Ops;
   begin
      Op := Implicit_Conversion_Op (Rtype, Get_Expr_Type (Expr));
      if Op = Convop_None then
         return Expr;
      else
         return Insert_Conversion (Op, Expr, Rtype);
      end if;
   end Implicit_Conversion;

   function Is_Null (N : Node) return Boolean is
   begin
      return Get_Kind (N) = N_Null;
   end Is_Null;

   procedure Sem_Null (Expr : Node; Etype : Node) is
   begin
      if Etype = Null_Node then
         Set_Expr_Type (Expr, Null_Type);
      else
         case Get_Kind (Etype) is
            when N_Event_Type
              | Nkinds_Class
              | N_Typedef_Class =>
               Set_Expr_Type (Expr, Etype);
            when others =>
               Error_Msg_Sem (+Expr, "'null' not allowed for the type");
         end case;
      end if;
   end Sem_Null;

   function Sem_Binary_Expression_Type (Lt : Node; Rt : Node) return Node
   is
      Res_Type : Node;
      Res_Length : Width_Type;
      Res_Signed : Boolean;
   begin
      --  Handle error cases.
      if Lt = Null_Node or Rt = Null_Node then
         if Lt /= Null_Node then
            return Lt;
         end if;
         if Rt /= Null_Node then
            return Rt;
         end if;
         return Null_Node;
      end if;

      --  Simple case.
      if Lt = Rt then
         return Lt;
      end if;

      if Lt = Unsigned_Logic_Type and Rt = Unsigned_Logic_Type then
         Res_Type := Unsigned_Logic_Type;
      elsif Is_Integral_Type (Lt) and Is_Integral_Type (Rt) then
         --  IEEE 1364-2005 4.4.1 Rules for expression bit lengths
         --  IEEE 1800-2017 11.6.1 Rules for expression bit lengths
         --  bit length: max (L(i), L(j))

         --  IEEE 1364-2005 4.5.1 Rules for expression types
         --  IEEE 1800-2017 11.8.1 Rules for expression types
         --  if any operand is unsigned, the result is unsigned, regardless of
         --  the operator; if all operands are signed, the result will be
         --  signed.
         Res_Length := Width_Type'Max (Get_Type_Width (Lt),
                                       Get_Type_Width (Rt));
         Res_Signed := Get_Signed_Flag (Lt) and Get_Signed_Flag (Rt);

         --  FIXME: base type, non-packed array types.
         Res_Type := Get_Packed_Array_Type
           (Int32 (Res_Length - 1), 0, Unsigned_Logic_Type, Res_Signed);
      elsif Lt = Real_Type or Rt = Real_Type then
         Res_Type := Real_Type;
      elsif Lt = Shortreal_Type or Rt = Shortreal_Type then
         Res_Type := Shortreal_Type;
      elsif Lt = Error_Type or Rt = Error_Type then
         --  Propagate error.
         Res_Type := Error_Type;
      else
         --  FIXME: todo
         raise Internal_Error;
      end if;

      return Res_Type;
   end Sem_Binary_Expression_Type;

   procedure Change_Binop_To_Signed (Expr : Node)
   is
      Op : Binary_Ops;
   begin
      Op := Get_Binary_Op (Expr);
      case Op is
         when Binop_Udiv =>
            Op := Binop_Sdiv;
         when Binop_Umul =>
            Op := Binop_Smul;
         when Binop_Umod =>
            Op := Binop_Smod;
         when Binop_Ugt =>
            Op := Binop_Sgt;
         when Binop_Uge =>
            Op := Binop_Sge;
         when Binop_Ult =>
            Op := Binop_Slt;
         when Binop_Ule =>
            Op := Binop_Sle;
         when others =>
            null;
      end case;
      Set_Binary_Op (Expr, Op);
   end Change_Binop_To_Signed;

   function Sem_Binary_Arith (Expr : Node; Etype : Node) return Node
   is
      L, R : Node;
      Res_Type : Node;
   begin
      L := Get_Left (Expr);
      L := Sem_Sub_Expression (L, Etype);
      Set_Left (Expr, L);

      R := Get_Right (Expr);
      R := Sem_Sub_Expression (R, Etype);
      Set_Right (Expr, R);

      Res_Type := Sem_Binary_Expression_Type (Get_Expr_Type (L),
                                              Get_Expr_Type (R));
      Set_Expr_Type (Expr, Res_Type);
      Set_Is_Constant (Expr, Get_Is_Constant (L) and Get_Is_Constant (R));

      if Res_Type /= Null_Node and then Get_Signed_Flag (Res_Type) then
         Change_Binop_To_Signed (Expr);
      end if;

      return Expr;
   end Sem_Binary_Arith;

   procedure Implicit_String_Convert (Expr : Node) is
   begin
      if Get_Kind (Expr) = N_String_Literal then
         Set_Expr_Type (Expr, String_Type);
      else
         Error_Msg_Sem (+Expr, "operator with a non-string expression");
      end if;
   end Implicit_String_Convert;

   function Sem_Relational_Operator (Expr : Node) return Node
   is
      L, R : Node;
      Lt, Rt, Op_Type : Node;
      Length : Width_Type;
      Signed : Boolean;
   begin
      --  IEEE1364-2005 5.4.1 Rules for expression bit lengths
      --  Operands are sized to max (L(i),L(j)).

      L := Get_Left (Expr);
      L := Sem_Sub_Expression (L, Null_Node);
      if L = Null_Node then
         --  Error.
         return Expr;
      end if;
      Lt := Get_Expr_Type (L);

      R := Get_Right (Expr);
      R := Sem_Sub_Expression (R, Null_Node);
      if R = Null_Node then
         --  Error.
         return Expr;
      end if;

      Rt := Get_Expr_Type (R);

      if Lt = Null_Node or Rt = Null_Node then
         --  Error.
         null;
      elsif Lt = String_Type or Rt = String_Type then
         --  1800-2017 6.16 String data type
         --  SystemVerilog provides a set of operators that can be used to
         --  manipulate combinations of string variables and string literals.
         if Lt = String_Type and Rt = String_Type then
            --  String/string operation.
            null;
         elsif Lt = String_Type then
            Implicit_String_Convert (R);
         else
            pragma Assert (Rt = String_Type);
            Implicit_String_Convert (L);
         end if;
      elsif Lt = Real_Type or Rt = Real_Type then
         L := Implicit_Conversion (L, Real_Type);
         R := Implicit_Conversion (R, Real_Type);
      elsif Lt = Shortreal_Type or Rt = Shortreal_Type then
         L := Implicit_Conversion (L, Shortreal_Type);
         R := Implicit_Conversion (R, Shortreal_Type);
      elsif Get_Kind (Lt) = N_Logic_Type and then Get_Kind (Rt) = N_Logic_Type
      then
         if Get_Signed_Flag (Lt) and Get_Signed_Flag (Rt) then
            Op_Type := Signed_Logic_Type;
            Change_Binop_To_Signed (Expr);
         else
            Op_Type := Unsigned_Logic_Type;
         end if;

         --  Apply it.
         L := Sem_Propagate_Length (L, Op_Type);
         R := Sem_Propagate_Length (R, Op_Type);
      elsif Is_Integral_Type (Lt) and then Is_Integral_Type (Rt) then
         --  Compute the max length.
         Length := Width_Type'Max (Get_Type_Width (Lt), Get_Type_Width (Rt));
         Signed := Get_Signed_Flag (Lt) and Get_Signed_Flag (Rt);

         if Signed then
            Change_Binop_To_Signed (Expr);
         end if;

         Op_Type := Get_Packed_Array_Type
           (Int32 (Length - 1), 0, Unsigned_Logic_Type, Signed);

         --  Apply it.
         L := Sem_Propagate_Length (L, Op_Type);
         R := Sem_Propagate_Length (R, Op_Type);
      elsif Lt = Rt then
         case Get_Kind (Lt) is
            when N_Class
              | N_Instantiated_Class =>
               null;
            when N_Real_Type
              | N_Shortreal_Type =>
               null;
            when N_Queue_Cst =>
               --  1800-2017 7.10.1 Queue operators
               --  Queues shall support the same operations that can be
               --  performed on unpacked arrays.
               null;
            when N_Null_Type =>
               --  Also allow 'null == null'.
               null;
            when others =>
               Error_Kind ("sem_relational_operator(1)", Lt);
         end case;
      elsif Is_Class_Or_Null_Type (Lt) and then Is_Class_Or_Null_Type (Rt) then
         if Is_Null (L) then
            --  Allow comparison of null with a class.
            Set_Expr_Type (L, Rt);
         elsif Is_Null (R) then
            --  Allow comparison of a class with null.
            Set_Expr_Type (R, Lt);
         else
            if not (Is_Subclass_Of (Lt, Rt) or Is_Subclass_Of (Rt, Lt)) then
               Error_Msg_Sem (+Expr, "comparison between unrelated classes");
            end if;
         end if;
      else
         Error_Kind ("sem_relational_operator", Lt);
      end if;

      Set_Left (Expr, L);
      Set_Right (Expr, R);

      --  Bit length of the result is 1 bit.
      Set_Expr_Type (Expr, Unsigned_Logic_Type);

      return Expr;
   end Sem_Relational_Operator;

   function Sem_Unary_Logic (Expr : Node; Etype : Node) return Node
   is
      Op : constant Unary_Ops := Get_Unary_Op (Expr);
      Operand : Node;
   begin
      case Op is
         when Unary_Red_Ops
           | Unop_Logic_Neg =>
            --  Bit length of operand is self-determined.
            Operand := Sem_Sub_Expression (Get_Expression (Expr), Null_Node);
            Set_Expression (Expr, Operand);
            Set_Expr_Type (Expr, Unsigned_Logic_Type);
         when Unop_Plus
           | Unop_Minus
           | Unop_Bit_Neg =>
            --  Bit length: length of operand.
            Operand := Sem_Sub_Expression (Get_Expression (Expr), Etype);
            Set_Expression (Expr, Operand);
            Set_Expr_Type (Expr, Get_Expr_Type (Operand));
      end case;
      Set_Is_Constant (Expr, Get_Is_Constant (Operand));
      return Expr;
   end Sem_Unary_Logic;

   function Sem_Shift_Operator (Expr : Node; Etype : Node) return Node
   is
      Left, Right : Node;
      Ltype, Rtype : Node;
   begin
      --  IEEE 1364-2005 4.1.2 Shift operators
      --  [...] The right operand is always treated as an unsigned number and
      --  has no effect on the signedness of the result.  The result signedness
      --  is determined by the left-handle operand and the remainder of the
      --  expression, as outlined in 4.5.1

      --  IEEE 1364-2005 4.4 Expression bit lengths
      --  right is self determined, result length is length of left.
      Left := Get_Left (Expr);
      Left := Sem_Sub_Expression (Left, Etype);
      Ltype := Get_Expr_Type (Left);
      if not Is_Integral_Type (Ltype) then
         Error_Msg_Sem (+Left, "operand must be of integral type");
      end if;
      Set_Left (Expr, Left);

      Right := Get_Right (Expr);
      Right := Sem_Expression (Right, Null_Node);
      Rtype := Get_Expr_Type (Right);
      if Rtype /= Null_Node and then not Is_Integral_Type (Rtype) then
         Error_Msg_Sem (+Right, "operand must be of integral type");
      end if;
      Set_Right (Expr, Right);

      Set_Expr_Type (Expr, Ltype);
      return Expr;
   end Sem_Shift_Operator;

   function Sem_Binary_Logical (Expr : Node) return Node
   is
      Left, Right : Node;
      Ltype : Node;
      Rtype : Node;
   begin
      --  IEEE 1364-2005 5.4.1 Expression bit lengths
      --  All operands are self-determined, result is 1 bit.
      --  right is self determined, result length is length of left.
      Left := Get_Left (Expr);
      Left := Sem_Expression (Left, Null_Node);
      Ltype := Get_Expr_Type (Left);
      if Ltype /= Null_Node and then not Is_Integral_Type (Ltype) then
         Error_Msg_Sem (+Left, "logical operand must be of integral type");
      end if;
      Set_Left (Expr, Left);

      Right := Get_Right (Expr);
      Right := Sem_Expression (Right, Null_Node);
      Rtype := Get_Expr_Type (Right);
      if Rtype /= Null_Node and then not Is_Integral_Type (Rtype) then
         Error_Msg_Sem (+Right, "logical operand must be of integral type");
      end if;
      Set_Right (Expr, Right);

      Set_Expr_Type (Expr, Unsigned_Logic_Type);
      return Expr;
   end Sem_Binary_Logical;

   procedure Sem_Replication (N : Node; Count : out Int32)
   is
      Rep : Node;
   begin
      Rep := Get_Replication (N);
      if Rep = Null_Node then
         --  No repetition.
         Count := 1;
         return;
      end if;

      Rep := Sem_Sub_Expression (Rep, Null_Node);
      Set_Replication (N, Rep);
      if not Is_Integral_Type (Get_Expr_Type (Rep)) then
         Error_Msg_Sem (+Rep, "replication count must be of integral type");
         Count := 0;
         return;
      end if;

      --  FIXME: according to 6.16 for string type, the multiplier is not
      --  required to be a constant expression.
      Count := Sem_Constant_Integer_Expression (Rep);
      if Count <= 0 then
         Error_Msg_Sem (+Rep, "repetition count must be positive");
         Count := 0;
      end if;
   end Sem_Replication;

   --  1800-2017 11.4.12 Concatenation operators
   --  1800-2017 10.10 Unpacked array concatenation
   --
   --  ALLOW_0 is set only within concatenations to support replications
   --  with a constant of 0.
   function Sem_Concatenation (Expr : Node; Etype : Node; Allow_0 : Boolean)
                              return Node
   is
      --  Type of concatenation.
      type Concat_Type is
        (
         Concat_Unknown,
         Concat_Integral,
         Concat_String,
         Concat_Array
        );
      Kind : Concat_Type;
      El : Node;
      Count : Int32;
      Len : Int32;
      N : Node;
      El_Type : Node;
      Res : Node;
      Has_Error : Boolean;
      Rep : Node;
   begin
      --  Note: the length of all operands is self-determined.

      --  GHDL: the main difficulty is to determine the type of the
      --  concatenation: it can be a vector or a string.

      --  By default, the type of the elements is self-determined.
      El_Type := Null_Node;

      if Etype = Null_Node then
         Kind := Concat_Unknown;
      elsif Etype = String_Type then
         Kind := Concat_String;
         --  Unless the result is string, in that case only concatenation of
         --  strings is supported.
         El_Type := String_Type;
      elsif Is_Integral_Type (Etype) then
         Kind := Concat_Integral;
      elsif Is_Unpacked_Array_Type (Etype) then
         Kind := Concat_Array;
         El_Type := Get_Type_Element_Type (Etype);
      else
         Kind := Concat_Unknown;
      end if;

      Has_Error := False;
      El := Get_Expressions (Expr);
      while El /= Null_Node loop
         --  Self-determined
         --  FIXME: ref
         N := Get_Expression (El);
         if Get_Kind (N) = N_Concatenation then
            N := Sem_Concatenation (N, El_Type, True);
         else
            N := Sem_Sub_Expression (N, El_Type);
         end if;
         --  FIXME: where ?
         if N /= Null_Node then
            Set_Expression (El, N);
            if Kind = Concat_Unknown then
               if Get_Expr_Type (N) = String_Type then
                  --  Can only be a string type in presence of a string type.
                  Kind := Concat_String;
               elsif Get_Kind (N) = N_String_Literal then
                  --  A string literal can be a string or a vector.
                  null;
               else
                  --  Must be a vector.
                  Kind := Concat_Integral;
               end if;
            end if;
         else
            Has_Error := True;
         end if;

         El := Get_Chain (El);
      end loop;

      Len := 0;
      El := Get_Expressions (Expr);
      while El /= Null_Node loop
         N := Get_Expression (El);
         El_Type := Get_Expr_Type (N);

         case Kind is
            when Concat_Integral =>
               if not Is_Integral_Type (El_Type) then
                  Error_Msg_Sem
                    (+El, "expression within concatenation must be of "
                       & "integral type");
                  Has_Error := True;
               else
                  N := Sem_Propagate_Length (N, El_Type);
                  Set_Expression (El, N);
                  pragma Assert (Get_Expr_Type (N) = El_Type);
                  Len := Len + Int32 (Get_Type_Width (El_Type));
               end if;
            when Concat_String =>
               if Get_Kind (N) = N_String_Literal then
                  Set_Expr_Type (N, String_Type);
               elsif El_Type /= Null_Node and then El_Type /= String_Type then
                  Error_Msg_Sem
                    (+El, "expression within concatenation must be a string");
                  Has_Error := True;
               end if;
            when Concat_Array =>
               null;
            when Concat_Unknown =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;

      Rep := Get_Replication (Expr);
      if Rep = Null_Node then
         --  A standard concatenation.
         Count := 1;
         if not Has_Error and then Len = 0 and then Kind = Concat_Integral then
            --  This is expected when all operands are replication with a
            --  constant of 0.
            Error_Msg_Sem
              (+Expr, "at least one operand must have a positive size");
         end if;
      else
         --  A concatenation with repetition.
         Rep := Sem_Sub_Expression (Rep, Null_Node);
         Set_Replication (Expr, Rep);
         if not Is_Integral_Type (Get_Expr_Type (Rep)) then
            Error_Msg_Sem (+Rep, "replication count must be of integral type");
            Has_Error := True;
         elsif Get_Is_Constant (Rep) then
            Count := Sem_Constant_Integer_Expression (Rep);
            if Count < 0 or else (Count = 0 and not Allow_0) then
               Error_Msg_Sem (+Rep, "repetition count must be positive");
               Count := 1;
            end if;
         else
            --  Repetition is not constant; allowed only for strings.
            case Kind is
               when Concat_Integral =>
                  Error_Msg_Sem (+Rep, "repetition count must be constant");
               when Concat_String =>
                  --  Do not create a Repetition_Cst node.
                  Rep := Null_Node;
               when Concat_Array =>
                  --  1800-2017 10.10.1 Unpacked array concatenation [...]
                  --  By contrast, unpacked array concatentations forbid
                  --  replication [...]
                  Error_Msg_Sem (+Rep, "replication forbidden for "
                                   & "unpacked array concatenation");
               when Concat_Unknown =>
                  null;
            end case;
         end if;
      end if;

      if Has_Error then
         null;
      else
         case Kind is
            when Concat_String =>
               Set_Expr_Type (Expr, String_Type);
            when Concat_Integral =>
               if Count = 0 then
                  if Null_Packed_Array_Type = Null_Node then
                     Null_Packed_Array_Type :=
                       Create_Node (N_Log_Packed_Array_Cst);
                     Set_Msb_Cst (Null_Packed_Array_Type, 0);
                     Set_Lsb_Cst (Null_Packed_Array_Type, 0);
                     Set_Type_Element_Type (Null_Packed_Array_Type,
                                            Unsigned_Logic_Type);
                     Set_Signed_Flag (Null_Packed_Array_Type, False);
                     Set_Type_Width (Null_Packed_Array_Type, 0);
                     Set_Stride_Width (Null_Packed_Array_Type, 1);
                  end if;
                  Set_Expr_Type (Expr, Null_Packed_Array_Type);
               else
                  --  FIXME: handle overflow ?
                  Len := Len * Count;
                  Set_Expr_Type
                    (Expr, Get_Packed_Array_Type (Len - 1, 0,
                                                  Unsigned_Logic_Type, False));
               end if;
            when Concat_Array =>
               Set_Expr_Type (Expr, Etype);
            when Concat_Unknown =>
               null;
         end case;
      end if;

      if Rep /= Null_Node then
         Res := Create_Node (N_Replication_Cst);
         Set_Location (Res, Get_Location (Expr));
         Set_Expressions (Res, Get_Expressions (Expr));
         Set_Replication_Cst (Res, Count);
         Set_Expr_Type (Res, Get_Expr_Type (Expr));

         Free_Node (Expr);
         return Res;
      else
         return Expr;
      end if;
   end Sem_Concatenation;

   --  1800-2017 11.4.11 Conditional operator
   function Sem_Conditional_Operator (Expr : Node; Etype : Node) return Node
   is
      Cond : Node;
      C_Type : Node;
      T_Expr : Node;
      F_Expr : Node;
      T_Type : Node;
      F_Type : Node;
   begin
      --  1364-2005 5.5.1 Rules for expression types
      --  - Comparison results (1, 0) are unsigned, regardless of the operands
      Cond := Sem_Sub_Expression (Get_Condition (Expr), Unsigned_Logic_Type);
      Set_Condition (Expr, Cond);
      C_Type := Get_Expr_Type (Cond);
      if C_Type /= Null_Node and then not Is_Integral_Type (C_Type) then
         Error_Msg_Sem (+Cond, "condition must be of integral type");
      end if;

      T_Expr := Sem_Sub_Expression (Get_Cond_True (Expr), Etype);
      Set_Cond_True (Expr, T_Expr);

      F_Expr := Sem_Sub_Expression (Get_Cond_False (Expr), Etype);
      Set_Cond_False (Expr, F_Expr);

      --  1800-2017 11.4.11 Conditional operator
      --  The conditional operator can be used with nonintegral types (see
      --  6.11.1) and aggregate expressions (see 11.2.2) using the following
      --  rules:
      T_Type := Get_Expr_Type (T_Expr);
      F_Type := Get_Expr_Type (F_Expr);
      if T_Type = Null_Node or F_Type = Null_Node then
         null;
      elsif (Is_Integral_Type (T_Type) or else Is_Float_Type (T_Type))
        and then (Is_Integral_Type (F_Type) or else Is_Float_Type (F_Type))
      then
         --  1800-2017 11.4.11 Conditional operator
         --  - If both the first expression and second expression are of
         --    integral types, the operation proceeds as defined.
         --
         --  - If both expressions are real, then the resulting type is real.
         --    if one expression is real and the other expression is
         --    shortreal or integral, the other expression is cast to real,
         --    and the resulting type is real. If one expression is shortreal
         --    and the other expression is integral, the integral expression is
         --    cast to shortreal, and the resulting type is shortreal.
         Set_Expr_Type (Expr, Sem_Binary_Expression_Type (T_Type, F_Type));
      elsif (T_Type = String_Type
               or else Get_Kind (T_Expr) = N_String_Literal)
        and then (F_Type = String_Type
                    or else Get_Kind (F_Expr) = N_String_Literal)
      then
         --  1800-2017 11.4.11 Conditional operator
         --  - For all other cases, the type of the first expression and
         --    second expression shall be equivalent (see 6.22.2).
         Set_Expr_Type (Expr, String_Type);
         Set_Expr_Type (F_Expr, String_Type);
         Set_Expr_Type (T_Expr, String_Type);
      elsif Is_Class_Type (T_Type) and then Is_Null (F_Expr) then
         --    b) Else, if either first expression or second expression is the
         --       literal null, the resulting tye is the type of the non-null
         --       expression
         Set_Expr_Type (Expr, T_Type);
      elsif Is_Null (T_Expr) and then Is_Class_Type (F_Type) then
         --    Dual.
         Set_Expr_Type (Expr, F_Type);
      elsif Is_Class_Type (T_Type) and then Is_Class_Type (F_Type) then
         --    c) Else, if the first expression is assignment compatible with
         --       the second expression, the resulting type is the type of
         --       the second expression.
         --    d) Else, if the second expression is assignment compatible with
         --       the first expression, the resulting type is the type of the
         --       first expression.
         if Is_Subclass_Of (T_Type, F_Type) then
            Set_Expr_Type (Expr, F_Type);
         elsif Is_Subclass_Of (F_Type, T_Type) then
            Set_Expr_Type (Expr, T_Type);
         else
            Error_Msg_Sem
              (+Expr, "incompatible classes in conditional operator");
            Set_Expr_Type (Expr, Error_Type);
         end if;
      else
         Error_Msg_Sem
           (+Expr, "conditional operator applied on non integral operands");
         Set_Expr_Type (Expr, Error_Type);
      end if;

      return Expr;
   end Sem_Conditional_Operator;

   --  The wildcard types are used on type methods to designate a type from
   --  the type (eg: element type for an array type method).
   function Sem_Wildcard_Type (Atype : Node; Obj_Type : Node) return Node is
   begin
      if Get_Kind (Atype) /= N_Wildcard_Type then
         return Atype;
      end if;

      case Atype is
         when Wildcard_Type_Type =>
            return Obj_Type;
         when Wildcard_Element_Type =>
            return Get_Type_Element_Type (Obj_Type);
         when Wildcard_Index_Type =>
            return Get_Type_Index_Type (Obj_Type);
         when Wildcard_Element_Queue_Type =>
            return Get_Queue_Type (Get_Type_Element_Type (Obj_Type), - 1);
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Wildcard_Type;

   function Sem_Access_Function_Call (Call : Node) return Node
   is
      Func_Name : constant Node := Get_Subroutine (Call);
      Func : constant Node := Get_Declaration (Func_Name);
      pragma Assert (Get_Kind (Func) = N_Nature_Access);
      Res : Node;
      Args : Node;
      Next_Args : Node;
      Arg1, Arg2 : Node;
   begin
      Res := Create_Node (N_Access_Call);
      Set_Location (Res, Get_Location (Call));
      Set_Access (Res, Func_Name);
      Set_Is_Constant (Res, False);
      Args := Get_Arguments (Call);
      Free_Node (Call);

      Set_Expr_Type (Res, Get_Parent (Func));

      if Args = Null_Node then
         Error_Msg_Sem
           (+Res, "access function call needs a branch or two nets");
         return Res;
      end if;
      if Get_Port (Args) /= Null_Node then
         Error_Msg_Sem
           (+Args, "port not allowed in access function call");
      end if;
      Arg1 := Get_Expression (Args);
      Set_Arg1 (Res, Arg1);

      Next_Args := Get_Chain (Args);
      Free_Node (Args);
      Args := Next_Args;

      if Args /= Null_Node then
         if Get_Port (Args) /= Null_Node then
            Error_Msg_Sem
              (+Args, "port not allowed in access function call");
         end if;
         Arg2 := Get_Expression (Args);
         Set_Arg2 (Res, Arg2);

         Next_Args := Get_Chain (Args);
         Free_Node (Args);

         if Next_Args /= Null_Node then
            Error_Msg_Sem
              (+Next_Args,
               "one branch or two nets allowed for access function call");
         end if;
      end if;

      return Res;
   end Sem_Access_Function_Call;

   procedure Sem_Generic_Builtin_Method_Call (Call : Node; Method : Node)
   is
      Obj : constant Node := Get_Object (Call);
      Obj_Type : constant Node := Get_Expr_Type (Obj);
      Expr : Node;
      Port : Node;
      Arg : Node;
      Port_Type : Node;
   begin
      if Get_Kind (Method) = N_Function then
         Set_Expr_Type
           (Call, Sem_Wildcard_Type (Get_Type_Data_Type (Method), Obj_Type));
      end if;

      Arg := Get_Arguments (Call);
      Port := Get_Tf_Ports_Chain (Method);

      if Port = Null_Node then
         if Arg /= Null_Node then
            Error_Msg_Sem (+Call, "%i method has no arguments", +Method);
         end if;
         return;
      end if;

      while Arg /= Null_Node loop
         if Port = Null_Node then
            Error_Msg_Sem (+Arg, "extra argument for %i", +Method);
            return;
         end if;

         if Get_Port (Arg) /= Null_Node then
            --  Named arguments are not (yet) supported.
            raise Program_Error;
         end if;

         Port_Type := Get_Type_Data_Type (Port);
         Port_Type := Sem_Wildcard_Type (Port_Type, Obj_Type);

         Expr := Sem_Expression (Get_Expression (Arg), Port_Type);
         Set_Expression (Arg, Expr);

         Arg := Get_Chain (Arg);
         Port := Get_Chain (Port);
      end loop;

      if Port /= Null_Node then
         Error_Msg_Sem (+Call, "missing argument for %i", +Method);
         return;
      end if;
   end Sem_Generic_Builtin_Method_Call;

   procedure Sem_Array_Method_Call (Call : Node)
   is
      Method_Name : constant Node := Get_Subroutine (Call);
      Method : Node;
   begin
      Method := Get_Declaration (Method_Name);

      case Method is
         when Array_Min_Method
           | Array_Max_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Array_Method_Call;

   procedure Sem_Queue_Method_Call (Call : Node)
   is
      Method_Name : constant Node := Get_Subroutine (Call);
      Queue_Name : constant Node := Get_Object (Call);
      Queue_Type : constant Node := Get_Expr_Type (Queue_Name);
      Method : Node;
      Arg : Node;
      Arg_Expr : Node;
   begin
      Method := Get_Declaration (Method_Name);
      Arg := Get_Arguments (Call);

      case Method is
         when Queue_Push_Back_Method
           | Queue_Push_Front_Method =>
            Set_Expr_Type (Call, Void_Type);
            if Arg = Null_Node then
               Error_Msg_Sem
                 (+Call, "missing 'item' argument for %i", +Method);
               return;
            end if;

            --  FIXME: check named arguments.
            Arg_Expr := Sem_Expression
              (Get_Expression (Arg), Get_Type_Element_Type (Queue_Type));
            if Arg_Expr /= Null_Node then
               Set_Expression (Arg, Arg_Expr);
            end if;

            Arg := Get_Chain (Arg);
            if Arg /= Null_Node then
               Error_Msg_Sem (+Arg, "extra argument for %i", +Method);
               return;
            end if;
         when Queue_Pop_Front_Method
           | Queue_Pop_Back_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when Queue_Size_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when Queue_Delete_Method =>
            if Arg /= Null_Node then
               Method := Queue_Delete_Index_Method;
               Set_Declaration (Method_Name, Method);
            end if;
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when Queue_Insert_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Queue_Method_Call;

   procedure Sem_Dynamic_Method_Call (Call : Node)
   is
      Method_Name : constant Node := Get_Subroutine (Call);
      Method : Node;
   begin
      Method := Get_Declaration (Method_Name);

      case Method is
         when Dynamic_Size_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when Dynamic_Delete_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Dynamic_Method_Call;

   procedure Sem_Enum_Method_Call (Call : Node)
   is
      Method : constant Node := Get_Declaration (Get_Subroutine (Call));
   begin
      case Method is
         when Enum_First_Method
            | Enum_Last_Method
            | Enum_Next_Method
            | Enum_Prev_Method
            | Enum_Name_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Enum_Method_Call;

   procedure Sem_String_Method_Call (Call : Node)
   is
      Method : constant Node := Get_Declaration (Get_Subroutine (Call));
   begin
      case Method is
         when String_Len_Method
           | String_Toupper_Method
           | String_Tolower_Method
           | String_Substr_Method
           | String_Atoi_Method
           | String_Atohex_Method
           | String_Atooct_Method
           | String_Atobin_Method
           | String_Itoa_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when others =>
            raise Internal_Error;
      end case;
   end Sem_String_Method_Call;

   procedure Sem_Associative_Method_Call (Call : Node)
   is
      Method_Name : constant Node := Get_Subroutine (Call);
      Arg : constant Node := Get_Arguments (Call);
      Method : Node;
   begin
      Method := Get_Declaration (Method_Name);

      case Method is
         when Associative_Exists_Method
           | Associative_Num_Method
           | Associative_Size_Method =>
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when Associative_First_Method
           | Associative_Last_Method
           | Associative_Next_Method
           | Associative_Prev_Method =>
            --  1800-2017 7.9.4 First()
            --  Associative arrays that specify a wildcard index type shall not
            --  be allowed.
            declare
               Obj : constant Node := Get_Object (Call);
               Obj_Type : constant Node := Get_Expr_Type (Obj);
            begin
               if Get_Type_Index_Type (Obj_Type) = Null_Node then
                  Error_Msg_Sem
                    (+Call, "cannot use method %i for associative array with "
                       & "wildcard index type", +Method);
               else
                  Sem_Generic_Builtin_Method_Call (Call, Method);
               end if;
            end;
         when Associative_Delete_Method =>
            if Arg /= Null_Node then
               Method := Associative_Delete_Index_Method;
               Set_Declaration (Method_Name, Method);
            end if;
            Sem_Generic_Builtin_Method_Call (Call, Method);
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Associative_Method_Call;

   --  Return the head of ports declaration for subroutine DECL.
   function Get_Tf_Ports (Decl : Node) return Node is
   begin
      if Get_Ansi_Port_Flag (Decl) then
         return Get_Tf_Ports_Chain (Decl);
      else
         return Get_Tf_Item_Declaration_Chain (Decl);
      end if;
   end Get_Tf_Ports;

   function Count_Tf_Ports (Ports : Node) return Nat32
   is
      Port : Node;
      Res : Nat32;
   begin
      Res := 0;

      Port := Ports;
      while Port /= Null_Node loop
         if Get_Kind (Port) in Nkinds_Tf_Port then
            Res := Res + 1;
         end if;
         Port := Get_Chain (Port);
      end loop;

      return Res;
   end Count_Tf_Ports;

   procedure Sem_Subroutine_Call_Arguments (Call : Node; Subrtn : Node)
   is
      Ports : constant Node := Get_Tf_Ports (Subrtn);
      Nbr_Ports : constant Nat32 := Count_Tf_Ports (Ports);
      Ports_Arr : Node_Array (1 .. Nbr_Ports);
      Assoc_Arr : Node_Array (1 .. Nbr_Ports);
      Port : Node;
      Arg : Node;
      Expr : Node;
      Idx : Nat32;
      Has_Named : Boolean;
      Port_Idx : Nat32;
   begin
      --  Fill the array of ports.
      Idx := Ports_Arr'First;
      Port := Ports;
      while Port /= Null_Node loop
         if Get_Kind (Port) in Nkinds_Tf_Port then
            Ports_Arr (Idx) := Port;
            Idx := Idx + 1;
         end if;
         Port := Get_Chain (Port);
      end loop;
      pragma Assert (Idx = Nbr_Ports + 1);

      Assoc_Arr := (others => Null_Node);

      Has_Named := False;
      Arg := Get_Arguments (Call);
      Port := Ports;
      Port_Idx := 0;
      while Arg /= Null_Node loop
         declare
            Port_Name : constant Node := Get_Port (Arg);
            Port_Id : Name_Id;
         begin
            if Port_Name = Null_Node then
               --  1800-2017 13.5.4 Argument binding by name
               --  If both positional and named arguments are specified in a
               --  single subroutine call, then all the positional arguments
               --  shall come before the named arguments.
               if Has_Named then
                  Error_Msg_Sem (+Arg, "positional argument must come before "
                                   & "named arguments");
               end if;

               --  Search for the next port (in case of non-ANSI declaration).
               while Port /= Null_Node
                 and then Get_Kind (Port) not in Nkinds_Tf_Port
               loop
                  Port := Get_Chain (Port);
               end loop;

               Port_Idx := Port_Idx + 1;
            else
               Has_Named := True;

               Port_Id := Get_Identifier (Port_Name);
               Port := Null_Node;
               for I in Ports_Arr'Range loop
                  if Get_Identifier (Ports_Arr (I)) = Port_Id then
                     Port := Ports_Arr (I);
                     Port_Idx := I;
                     exit;
                  end if;
               end loop;
            end if;
         end;

         if Port = Null_Node then
            Error_Msg_Sem (+Arg, "extra argument in subroutine call");
            exit;
         end if;

         Assoc_Arr (Port_Idx) := Arg;

         Expr := Get_Expression (Arg);
         if Expr = Null_Node then
            if Get_Default_Value (Port) = Null_Node then
               Error_Msg_Sem (+Arg, "no default value for argument %i", +Port);
            end if;
         else
            if Get_Kind (Port) /= N_Tf_Input then
               --  1800-2017 13.5 Subroutine calls and argument passing
               --  If the argument in the subroutine is declared as an
               --  OUTPUT or an INOUT, then the corresponding expression in
               --  the subroutine call shall be restricted to an expression
               --  that is valid of the left-hand side of a procedural
               --  assignment.
               Expr := Sem_Lvalue (Expr, Allow_Var => True);
               --  TODO: check type;
            else
               Expr := Sem_Expression (Expr, Get_Type_Data_Type (Port));
            end if;
            if Expr /= Null_Node then
               Set_Expression (Arg, Expr);
            end if;
         end if;

         Set_Port (Arg, Port);

         Arg := Get_Chain (Arg);
         Port := Get_Chain (Port);
      end loop;

      --  1800-2017 13.5.3 Default argument values
      --  If an unspecified argument is used for an argument that does not
      --  have a default, ac compiler error shall be issued.
      for I in Assoc_Arr'Range loop
         if Assoc_Arr (I) = Null_Node
           and then Get_Default_Value (Ports_Arr (I)) = Null_Node
         then
            Error_Msg_Sem
              (+Call, "no value for argument %i (without default value)",
               +Ports_Arr (I));
         end if;
      end loop;

      --  Create arguments for missing association, reorder arguments.
      for I in Assoc_Arr'Range loop
         if Assoc_Arr (I) = Null_Node then
            Arg := Create_Node (N_Argument);
            Set_Location (Arg, Get_Location (Call));
            Set_Port (Arg, Ports_Arr (I));
            Assoc_Arr (I) := Arg;
         end if;
         if I < Nbr_Ports then
            Set_Chain (Assoc_Arr (I), Assoc_Arr (I + 1));
         else
            Set_Chain (Assoc_Arr (I), Null_Node);
         end if;
      end loop;

      if Nbr_Ports = 0 then
         Set_Arguments (Call, Null_Node);
      else
         Set_Arguments (Call, Assoc_Arr (Assoc_Arr'First));
      end if;
   end Sem_Subroutine_Call_Arguments;

   function Sem_Subroutine_Call_Suffix (Call : Node) return Node
   is
      Name : constant Node := Get_Subroutine (Call);
      Decl : constant Node := Get_Declaration (Name);
      Obj : Node;
   begin
      if Decl = Null_Node then
         --  Error.
         return Decl;
      end if;

      if Decl <= Last_Builtin_Method then
         --  Type methods.
         pragma Assert (Get_Kind (Name) = N_Dotted_Name);
         Obj := Get_Name (Name);
         Set_Object (Call, Obj);

         case Decl is
            when First_String_Method .. Last_String_Method =>
               Sem_String_Method_Call (Call);
            when First_Enum_Method .. Last_Enum_Method =>
               Sem_Enum_Method_Call (Call);
            when First_Array_Method .. Last_Array_Method =>
               Sem_Array_Method_Call (Call);
            when First_Queue_Method .. Last_Queue_Method =>
               Sem_Queue_Method_Call (Call);
            when First_Associative_Method .. Last_Associative_Method =>
               Sem_Associative_Method_Call (Call);
            when First_Dynamic_Method .. Last_Dynamic_Method =>
               Sem_Dynamic_Method_Call (Call);
            when Event_Triggered_Method =>
               Sem_Generic_Builtin_Method_Call (Call, Decl);
            when Rand_Mode_Func_Method
              | Rand_Mode_Task_Method =>
               Sem_Generic_Builtin_Method_Call (Call, Decl);
            when others =>
               raise Internal_Error;
         end case;
      else
         case Get_Kind (Decl) is
            when N_Function
              | N_Extern_Function =>
               --  pragma Assert (Get_Fully_Analyzed_Flag (Decl));
               Set_Expr_Type (Call, Get_Type_Data_Type (Decl));
            when N_Import_DPI_Function =>
               Set_Expr_Type (Call, Get_Type_Data_Type (Decl));
            when N_Task
              | N_Extern_Task =>
               pragma Assert (Get_Fully_Analyzed_Flag (Decl));
               null;
            when N_Nature_Access =>
               --  An access identifier, use the nature as a type.
               return Sem_Access_Function_Call (Call);
            when others =>
               Error_Kind ("sem_subroutine_call", Decl);
         end case;

         --  Normal subroutine call.
         Sem_Subroutine_Call_Arguments (Call, Decl);
      end if;
      return Call;
   end Sem_Subroutine_Call_Suffix;

   procedure Sem_Subroutine_Call_Name (Call : Node)
   is
      Name : Node;
   begin
      Name := Sem_Tf_Name (Get_Subroutine (Call));
      Set_Subroutine (Call, Name);
   end Sem_Subroutine_Call_Name;

   procedure Sem_New_Call (Expr : Node; Etype : Node)
   is
      Constructor : Node;
      Args : Node;
   begin
      if Etype = Null_Node then
         --  FIXME: ref?
         Error_Msg_Sem
           (+Expr, "new() not allowed in self-determined context");
         return;
      end if;
      case Get_Kind (Etype) is
         when N_Class
           | N_Instantiated_Class
           | N_Typedef_Class =>
            null;
         when others =>
            Error_Msg_Sem (+Expr, "new() only allowed for a class type");
            return;
      end case;

      Set_Expr_Type (Expr, Etype);

      --  FIXME: typedef class ?
      Constructor := Get_Class_Constructor (Etype);

      Args := Get_Arguments (Expr);
      if Constructor = Null_Node then
         if Args /= Null_Node then
            Error_Msg_Sem
              (+Expr,
               "cannot call default constructor of class %i with arguments",
               +Etype);
         end if;
      else
         Sem_Subroutine_Call_Arguments (Expr, Constructor);
      end if;
   end Sem_New_Call;

   procedure Sem_Array_Method_Call_With (Call : Node)
   is
      Name : Node;
      Expr : Node;
      Expr_Type : Node;
      With_Expr : Node;
      Iterator : Node;
      Method : Node;
      Index_Type : Node;
   begin
      Name := Get_Subroutine (Call);

      if Get_Kind (Name) /= N_Dotted_Name then
         Error_Msg_Sem (+Name, "array method call must use method notation");
         return;
      end if;

      Expr := Get_Name (Name);
      Expr := Sem_Expression (Expr, Null_Node);
      Set_Expression (Call, Expr);

      --  1800-2017 7.12.1 Array locator methods
      --  Array locator methods operate on any unpacked array, including
      --  queues, but theur return type is a queue.
      --  FIXME: definition of unpacked array.
      Expr_Type := Get_Expr_Type (Expr);
      case Get_Kind (Expr_Type) is
         when N_Array_Cst
           | N_Queue_Cst
           | N_Dynamic_Array_Cst
           | N_Associative_Array_Cst =>
            null;
         when others =>
            Error_Msg_Sem
              (+Name, "expression of array method call must be an array");
            return;
      end case;

      case Get_Identifier (Name) is
         when Std_Names.Name_Find =>
            --  1800-2017 7.12.1 Array locator methods
            --  find() returns all the elements satisfying the given
            --  expression.
            Method := Array_Find_Method;
         when Std_Names.Name_Find_Index =>
            Method := Array_Find_Index_Method;
         when Std_Names.Name_Find_First_Index =>
            Method := Array_Find_First_Index_Method;
         when Std_Names.Name_Unique =>
            Method := Array_Unique_Method;
         when Std_Names.Name_Sort =>
            Method := Array_Sort_Method;
         when others =>
            Error_Msg_Sem
              (+Name, "unknown array manipulation method %i", +Name);
            return;
      end case;

      Free_Node (Name);
      Set_Subroutine (Call, Method);

      case Method is
         when Array_Find_Method
           | Array_Unique_Method =>
            --  1800-2017 7.12.1 Array locator methods
            --  Array locator methods operate on any unpacked array, including
            --  queues, but their return type is a queue.
            --  - find() returns all the elements satisfying the given
            --    expression.
            Set_Expr_Type
              (Call, Get_Queue_Type (Get_Type_Element_Type (Expr_Type), - 1));
         when Array_Sort_Method =>
            Set_Expr_Type (Call, Void_Type);
         when Array_Find_Index_Method
           | Array_Find_First_Index_Method =>
            --  1800-2017 7.12.1 Array locator methods
            --  Index locator methods return a queue of INT for all arrays
            --  except asociative arrays, which return a queue of the same
            --  type as the associative index type.  Associative arrays that
            --  specify a wildcard index type shall not be allowed.
            if Get_Kind (Expr_Type) = N_Associative_Array then
               Index_Type := Get_Index_Data_Type (Expr_Type);
               if Index_Type = Null_Node then
                  Error_Msg_Sem
                    (+Expr,
                     "associative arrays with wildcard index not allowed");
                  Index_Type := Signed_Int_Type;
               end if;
            else
               Index_Type := Signed_Int_Type;
            end if;

            Set_Expr_Type (Call, Get_Queue_Type (Index_Type, -1));
         when others =>
            raise Internal_Error;
      end case;

      With_Expr := Get_With_Expression (Call);
      Iterator := Get_Iterator_Argument (Call);

      if With_Expr /= Null_Node then
         --  1800-2017 7.12.1 Array locator methods
         --  The iterator_argument optionally specifies the name of the
         --  variable used by the WITH expression to designate the element of
         --  the array at each iteration.  If it is not specified, the name
         --  item is used by default.
         if Iterator = Null_Node then
            Iterator := Create_Node (N_Iterator_Argument);
            Location_Copy (Iterator, Call);
            Set_Identifier (Iterator, Std_Names.Name_Item);
            Set_Iterator_Argument (Call, Iterator);
         end if;
         Set_Expr_Type (Iterator, Get_Type_Element_Type (Expr_Type));

         --  1800-2017 7.12.1 Array locator methods
         --  The scope for the iterator_argument is the WITH expression.
         Open_Name_Space;
         Add_Decl (Iterator);

         With_Expr := Sem_Expression (With_Expr, Null_Node);
         Close_Name_Space;

         Set_With_Expression (Call, With_Expr);
      else
         --  1800-2017 7.12.1 Array locator methods
         --  Specifying an iterator_argument without also specifying a WITH
         --  clause shall be illegal.
         if Iterator /= Null_Node then
            Error_Msg_Sem
              (+Iterator, "cannot specify an iterator without a WITH clause");
         end if;
      end if;
   end Sem_Array_Method_Call_With;

   procedure Sem_Dynamic_Array_New (Expr : Node; Etype : Node)
   is
      Size : Node;
      Init : Node;
   begin
      if Etype = Null_Node then
         Error_Msg_Sem (+Expr, "new[] cannot be used in this context");
         return;
      end if;
      if Get_Kind (Etype) /= N_Dynamic_Array_Cst then
         Error_Msg_Sem (+Expr, "new[] allowed only for dynamic array");
         return;
      end if;

      Set_Expr_Type (Expr, Etype);

      --  1800-2017 7.5.1 new[]
      --  The type of this operand is longint.
      Size := Get_Size_Expression (Expr);
      Size := Sem_Sub_Expression (Size, Signed_Longint_Type);
      Set_Size_Expression (Expr, Size);

      Init := Get_Init_Expression (Expr);
      if Init /= Null_Node then
         Init := Sem_Sub_Expression (Init, Etype);
         Set_Init_Expression (Expr, Init);
      end if;
   end Sem_Dynamic_Array_New;

   procedure Sem_Struct_Pattern_Positional
     (First_El : Node; First_Mb : Node; Lit : Node)
   is
      El : Node;
      Mb : Node;
      Expr : Node;
   begin
      El := First_El;
      Mb := First_Mb;
      loop
         if Get_Pattern_Key (El) /= Null_Node then
            Error_Msg_Sem (+El, "cannot mix by-key and positional patterns");
         end if;

         --  Idx := Get_Member_Index (Mb);
         --  Members (Idx) := El;

         Expr := Get_Expression (El);
         Expr := Sem_Expression (Expr, Get_Type_Data_Type (Mb));
         Set_Expression (El, Expr);

         Mb := Get_Chain (Mb);
         El := Get_Chain (El);
         exit when El = Null_Node;
         if Mb = Null_Node then
            Error_Msg_Sem (+El, "too many elements for structure");
            exit;
         end if;
      end loop;
      if Mb /= Null_Node then
         Error_Msg_Sem (+Lit, "not enough elements for structure");
      end if;
   end Sem_Struct_Pattern_Positional;

   procedure Sem_Struct_Pattern_Keys (First_El : Node;
                                      First_Mb : Node;
                                      Lit : Node;
                                      Members : in out Node_Array)
   is
      El : Node;
      Last_El : Node;
      Default : Node;
      Key : Node;
      Mb : Node;
      Mb_Type : Node;
      Idx : Int32;
      Expr : Node;
   begin
      Members := (others => Null_Node);
      Default := Null_Node;

      Last_El := First_El;
      El := First_El;
      while El /= Null_Node loop
         Key := Get_Pattern_Key (El);
         if Key = Null_Node then
            Error_Msg_Sem (+El, "cannot mix by-key and positional patterns");
         elsif Get_Kind (Key) = N_Default then
            if Default = Null_Node then
               Default := El;
            else
               Error_Msg_Sem
                 (+El, "duplicate default key, previous at %l", +Default);
            end if;

            --  1800-2017 10.9.2 Structure assignment patterns
            --  [...] matches the self-determined type of the value
            Expr := Get_Expression (El);
            Expr := Sem_Expression (Expr, Null_Node);
            Set_Expression (El, Expr);
         elsif Get_Kind (Key) /= N_Name then
            --  TODO: simple_type
            Error_Msg_Sem (+El, "pattern key must be a name (TODO)");
         else
            Mb := Find_Member_By_Id (Get_Identifier (Key), First_Mb);
            if Mb = Null_Node then
               Error_Msg_Sem (+El, "no %i member in struct %n",
                              (+Key, +Get_Expr_Type (Lit)));
            else
               Set_Declaration (Key, Mb);
               Idx := Get_Member_Index (Mb);
               if Members (Idx) /= Null_Node then
                  Error_Msg_Sem (+El, "duplicate member key, previous at %l",
                                 +Members (Idx));
               else
                  Members (Idx) := El;
               end if;

               Expr := Get_Expression (El);
               Expr := Sem_Expression (Expr, Get_Type_Data_Type (Mb));
               Set_Expression (El, Expr);
            end if;
         end if;

         Last_El := El;
         El := Get_Chain (El);
      end loop;

      --  Check for completness.
      Mb := First_Mb;
      for I in Members'Range loop
         pragma Assert (Get_Member_Index (Mb) = I);
         if Members (I) = Null_Node then
            if Default = Null_Node then
               Error_Msg_Sem (+Lit, "no expression for member %i", +Mb);
            else
               Mb_Type := Get_Type_Data_Type (Mb);
               El := Create_Node (N_Aggregate_Element);
               Location_Copy (El, Default);
               Set_Expr_Type (El, Mb_Type);
               Set_Pattern_Key (El, Mb);
               Expr := Get_Expression (Default);
               Set_Expression (El, Implicit_Conversion (Expr, Mb_Type));

               Set_Chain (Last_El, El);
               Last_El := El;
            end if;
         end if;
         Mb := Get_Chain (Mb);
      end loop;
   end Sem_Struct_Pattern_Keys;

   -- IEEE 1800-2017 10.9.2 Structure assignment patterns
   function Sem_Struct_Pattern (Lit : Node; Etype : Node) return Node
   is
      Members : Node_Array (1 .. Get_Nbr_Members (Etype));
      Mb : Node;
      El : Node;
      --  Idx : Int32;
   begin
      pragma Assert (Get_Expr_Type (Lit) = Null_Node);

      Set_Expr_Type (Lit, Etype);

      El := Get_Elements (Lit);
      Mb := Get_Members (Etype);
      if El /= Null_Node and then Get_Pattern_Key (El) /= Null_Node then
         --  With keys
         Sem_Struct_Pattern_Keys (El, Mb, Lit, Members);
      else
         --  Positional notation
         Sem_Struct_Pattern_Positional (El, Mb, Lit);
      end if;

      return Lit;
   end Sem_Struct_Pattern;

   -- IEEE 1800-2017 10.9 Assignment patterns
   function Sem_Array_Pattern (Lit : Node; Etype : Node; Length : Int32)
                              return Node
   is
      El_Type : constant Node := Get_Type_Element_Type (Etype);
      Count : Int32;
      Num : Int32;
      Default : Node;
      El : Node;
      Expr : Node;
      Res : Node;
      Key : Node;
   begin
      pragma Assert (Get_Expr_Type (Lit) = Null_Node);

      Set_Expr_Type (Lit, Etype);

      Sem_Replication (Lit, Count);

      El := Get_Elements (Lit);
      Default := Null_Node;
      Num := 0;
      while El /= Null_Node loop
         Num := Num + 1;
         Key := Get_Pattern_Key (El);
         if Key = Null_Node then
            null;
         elsif Get_Kind (Key) = N_Default then
            --  TODO: can 'default' appear many times ?
            Default := El;
         else
            --  Not yet handled.
            raise Program_Error;
         end if;

         Expr := Get_Expression (El);
         Expr := Sem_Expression (Expr, El_Type);
         Set_Expression (El, Expr);

         El := Get_Chain (El);
      end loop;
      if Count /= 0 then
         Num := Num * Count;
         if Length > 0 then
            if Num > Length then
               Error_Msg_Sem (+Lit, "too many elements for array");
            elsif Num < Length and then Default = Null_Node then
               Error_Msg_Sem (+Lit, "not enough elements for array");
            end if;
         end if;
      end if;

      if Count /= 1 then
         Res := Create_Node (N_Aggregate_Literal_Cst);
         Set_Location (Res, Get_Location (Lit));
         Set_Elements (Res, Get_Elements (Lit));
         Set_Replication_Cst (Res, Count);
         Set_Expr_Type (Res, Get_Expr_Type (Lit));
         Free_Node (Lit);

         return Res;
      else
         return Lit;
      end if;
   end Sem_Array_Pattern;

   function Sem_Aggregate_Literal (Expr : Node; Etype : Node) return Node is
   begin
      if Etype = Null_Node then
         --  FIXME: error or self-determined
         --  FIXME: ref ?
         Error_Msg_Sem
           (+Expr, "aggregate not allowed in self-determined context");
         return Expr;
      end if;

      case Get_Kind (Etype) is
         when N_Struct_Type
           | N_Packed_Struct_Type =>
            return Sem_Struct_Pattern (Expr, Etype);
         when N_Array_Cst
            | N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            declare
               Length : constant Int32 := Compute_Length (Get_Msb_Cst (Etype),
                                                          Get_Lsb_Cst (Etype));
            begin
               return Sem_Array_Pattern (Expr, Etype, Length);
            end;
         when N_Queue_Cst =>
            --  No length.
            return Sem_Array_Pattern (Expr, Etype, -1);
         when others =>
            Error_Kind ("sem_aggregate_literal", Etype);
      end case;
   end Sem_Aggregate_Literal;

   procedure Sem_Assign_Operator (Expr : Node)
   is
      Name : Node;
      Ltype : Node;
      Val : Node;
   begin
      Name := Sem_Lvalue (Get_Lvalue (Expr), Allow_Var => True);
      Set_Lvalue (Expr, Name);
      Ltype := Get_Expr_Type (Name);
      if Ltype = Null_Node then
         return;
      end if;
      Set_Expr_Type (Expr, Ltype);

      --  TODO: handle width correctly.
      Val := Sem_Expression (Get_Expression (Expr), Ltype);
      Set_Expression (Expr, Val);
   end Sem_Assign_Operator;

   procedure Sem_Type_Cast (Expr : Node)
   is
      Ctype : constant Node := Get_Cast_Data_Type (Expr);
      Arg : Node;
   begin
      if Get_Type_Owner (Expr) then
         Sem_Data_Type (Ctype);
      end if;
      Set_Expr_Type (Expr, Get_Expr_Type (Ctype));

      Arg := Sem_Expression (Get_Expression (Expr), Null_Node);
      Set_Expression (Expr, Arg);

      --  FIXME: compatibility
   end Sem_Type_Cast;

   procedure Sem_Size_Cast (Expr : Node)
   is
      Sz : Node;
      Arg : Node;
      Base_Type : Node;
      Res_Type : Node;
      El : Node;
      Isize : Int32;
   begin
      Sz := Get_Size_Expression (Expr);
      Sz := Sem_Sub_Expression (Sz, Null_Node);
      Set_Size_Expression (Expr, Sz);

      --  1800-2017 6.24.1 Cast operator
      --  If the casting type is a constant expression with a positive integral
      --  value, the expression is parenthesis shall be padded or truncated
      --  to the size specified.  It shall be an error if the size specified
      --  is zero or negative.
      if not Is_Integral_Type (Get_Expr_Type (Sz)) then
         Error_Msg_Sem (+Sz, "size expression must be an integral value");
         Isize := 1;
      else
         Isize := Sem_Constant_Integer_Expression (Sz);
         if Isize <= 0 then
            Error_Msg_Sem (+Sz, "size must be positive");
            Isize := 1;
         end if;
      end if;

      --  1800-2017 6.24.1 Cast operator
      --  The expression inside the cast shall be an integral value when
      --  changing the size or signing.
      Arg := Sem_Expression (Get_Expression (Expr), Null_Node);
      Set_Expression (Expr, Arg);
      Base_Type := Get_Base_Integral_Type (Get_Expr_Type (Arg));
      if Base_Type = Null_Node then
         Error_Msg_Sem (+Sz, "expression must be an integral value");
         Res_Type := Unsigned_Logic_Type;
      else
         case Get_Kind (Base_Type) is
            when N_Logic_Type
              | N_Log_Packed_Array_Cst =>
               El := Unsigned_Logic_Type;
            when N_Bit_Type
              | N_Bit_Packed_Array_Cst =>
               El := Unsigned_Bit_Type;
            when others =>
               raise Internal_Error;
         end case;
         Res_Type := Get_Packed_Array_Type
           (Isize - 1, 0, El, Get_Signed_Flag (Base_Type));
      end if;
      Set_Expr_Type (Expr, Res_Type);
      Set_Conversion_Op (Expr, Implicit_Conversion_Op (Res_Type, Base_Type));
   end Sem_Size_Cast;

   function Sem_Bits_Common (Expr : Node) return Node is
   begin
      --  1800-2017 20.6.2 Expression size system function
      --  The return type is integer.

      Set_Expr_Type (Expr, Signed_Integer_Type);
      Set_Type_Owner (Expr, False);

      --  1800-2017 20.6.2 Expression size system function
      --  The $bits function can be used as an elaboration time constant
      --  when used on fixed-size data types [...]
      --  TODO: the restriction.
      Set_Is_Constant (Expr, True);

      return Expr;
   end Sem_Bits_Common;

   function Sem_Bits_Type (Expr : Node) return Node is
   begin
      if Get_Type_Owner (Expr) then
         Sem_Data_Type (Get_Type_Argument (Expr));
      end if;

      return Sem_Bits_Common (Expr);
   end Sem_Bits_Type;

   function Sem_Bits_Expr (Expr : Node) return Node
   is
      Arg : Node;
      Res : Node;
   begin
      Arg := Get_Expression (Expr);
      if Is_Type_Name (Arg) then
         Res := Create_Node (N_Bits_Type);
         Location_Copy (Res, Expr);
         Set_Type_Argument (Res, Arg);
         Set_Type_Owner (Res, True);
         Free_Node (Expr);

         return Sem_Bits_Type (Res);
      end if;

      Arg := Sem_Expression (Arg, Null_Node);
      Set_Expression (Expr, Arg);

      return Sem_Bits_Common (Expr);
   end Sem_Bits_Expr;

   --  1800-2017 11.4.13 Set membership operator
   function Sem_Membership (Memb : Node) return Node
   is
      Expr : Node;
      Lsb, Msb : Node;
      Etype : Node;
      El : Node;
      Cst : Boolean;
   begin
      --  The operator returns either 1'b1 or 1'b0, which is a logic vector
      --  of width 1.
      --  TODO: adjust.
      Set_Expr_Type (Memb, Unsigned_Logic_Type);

      --  Self determined ?
      Expr := Get_Expression (Memb);
      Expr := Sem_Expression (Expr, Null_Node);
      Set_Expression (Memb, Expr);
      Etype := Get_Expr_Type (Expr);
      Cst := Get_Is_Constant (Expr);

      El := Get_Expressions (Memb);
      while El /= Null_Node loop
         pragma Assert (Get_Kind (El) = N_Element);
         Expr := Get_Expression (El);
         if Get_Kind (Expr) = N_Value_Range then
            Lsb := Get_Lsb (Expr);
            Lsb := Sem_Expression (Lsb, Etype);
            Set_Lsb (Expr, Lsb);
            if Cst and then not Get_Is_Constant (Lsb) then
               Cst := False;
            end if;

            Msb := Get_Msb (Expr);
            Msb := Sem_Expression (Msb, Etype);
            Set_Lsb (Expr, Msb);
            if Cst and then not Get_Is_Constant (Msb) then
               Cst := False;
            end if;
         else
            --  TODO: handle unpacked array.
            Expr := Sem_Expression (Expr, Etype);
            Set_Expression (El, Expr);
            if Cst and then not Get_Is_Constant (Expr) then
               Cst := False;
            end if;
         end if;
         El := Get_Chain (El);
      end loop;
      Set_Is_Constant (Memb, Cst);
      return Memb;
   end Sem_Membership;

   function Build_Error_Expr (Orig : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Error_Expr);
      Set_Location (Res, Get_Location (Orig));
      Set_Error_Origin (Res, Orig);
      Set_Expr_Type (Res, Null_Node);
      return Res;
   end Build_Error_Expr;

   function Sem_Name_To_Expression (Expr : Node) return Node
   is
      Base : Node;
   begin
      Base := Expr;
      loop
         case Get_Kind (Base) is
            when N_Name
              | N_Hierarchical
              | N_Interface_Item
              | N_Scoped_Name
              | N_Class_Qualified_Name =>
               declare
                  Decl : constant Node := Get_Declaration (Base);
               begin
                  if Decl = Null_Node then
                     return Expr;
                  end if;
                  case Get_Kind (Decl) is
                     when N_Var
                       | N_Return_Var
                       | N_This
                       | N_Foreach_Variable
                       | N_Iterator_Argument
                       | N_Parameter
                       | N_Localparam
                       | Nkinds_Nets
                       | Nkinds_Tf_Port
                       | N_Enum_Name
                       | N_Genvar =>
                        Sem_Name_Declaration (Base);
                        return Expr;
                     when others =>
                        exit;
                  end case;
               end;
            when N_This_Var
              | N_This
              | N_Super
              | N_This_Name =>
               return Expr;
            when N_Modport_Item =>
               declare
                  Decl : constant Node := Get_Declaration (Base);
               begin
                  if Decl = Null_Node then
                     return Expr;
                  end if;
                  case Get_Kind (Decl) is
                     when N_Modport_Input
                        | N_Modport_Output =>
                        Sem_Name_Declaration (Base);
                        return Expr;
                     when others =>
                        exit;
                  end case;
               end;
            when N_Part_Select_Cst
              | N_Slice_Name_Cst
              | N_Plus_Part_Select_Cst
              | N_Minus_Part_Select_Cst
              | N_Bit_Select
              | N_Part_Select
              | N_Indexed_Name
              | N_Member_Name
              | N_String_Index
              | N_Associative_Index =>
               Base := Get_Name (Base);
            when N_Property_Name =>
               Base := Get_Name (Base);
               if Base = Null_Node then
                  return Expr;
               end if;
            when N_Dotted_Name
              | N_Method_Name =>
               declare
                  Decl : constant Node := Get_Declaration (Base);
                  Res : Node;
               begin
                  if Decl /= Null_Node
                    and then Get_Kind (Decl) = N_Function
                  then
                     --  Convert to a call.
                     Res := Create_Node (N_Call);
                     Location_Copy (Res, Base);
                     Set_Subroutine (Res, Base);

                     return Sem_Subroutine_Call_Suffix (Res);
                  end if;
               end;
               Base := Get_Name (Base);
            when others =>
               exit;
         end case;
      end loop;

      --  FIXME: improve error message.  %i cannot be used for names
      --  without identifiers.
      Error_Msg_Sem (+Expr, "name cannot be used in expression");
      return Build_Error_Expr (Expr);
   end Sem_Name_To_Expression;

   function Sem_Sub_Expression (Expr : Node; Etype : Node) return Node
   is
      Ntype : Node;
   begin
      case Get_Kind (Expr) is
         when N_Number
           | N_Bignum =>
            --  At this time, numbers don't have a type.
            --  FIXME: why is it not true ?
            Ntype := Get_Expr_Type (Expr);
            -- pragma Assert (Ntype = Null_Node);
            if Get_Number_Size (Expr) = 0 then
               --  LRM
               --  Bit length is same as integer.
               Ntype := Signed_Integer_Type;
            else
               Ntype := Get_Packed_Array_Type
                 (Int32 (Get_Number_Size (Expr)) - 1, 0,
                  Unsigned_Logic_Type, Get_Signed_Flag (Expr));
            end if;
            Set_Expr_Type (Expr, Ntype);
            Set_Is_Constant (Expr, True);

            return Expr;

         when N_Unbased_Literal =>
            --  1800-2017 5.7.1 Integer literal constants
            --  In a self-determined context, an unsized single-bit value
            --  shall have a width of 1 bit, and the value shall be treated
            --  as unsigned.
            if Etype = Null_Node or else Is_Float_Type (Etype) then
               Ntype := Unsigned_Logic_Type;
            elsif not Is_Integral_Type (Etype) then
               Error_Msg_Sem (+Expr, "bad context for unbased literal");
               Ntype := Unsigned_Logic_Type;
            else
               Ntype := Etype;
            end if;
            Set_Expr_Type (Expr, Ntype);
            Set_Is_Constant (Expr, True);
            return Expr;

         when N_Time_Literal =>
            --  1800-2017 5.8 Time literals
            --  The time literal is interpreted as a REALTIME value scaled to
            --  the current time unit and rounded to the current time
            --  precision.
            --  FIXME: scale, round.
            Set_Expr_Type (Expr, Realtime_Type);
            Set_Is_Constant (Expr, True);
            if Etype /= Null_Node then
               return Implicit_Conversion (Expr, Etype);
            else
               return Expr;
            end if;

         when N_Real_Number =>
            --  1800-2017 5.7.2 Real literal constants
            --  The default type for fixed-point format (e.g., 1.2), and
            --  exponent format (e.g., 2.0e10) shall be real.
            Set_Expr_Type (Expr, Real_Type);
            Set_Is_Constant (Expr, True);
            if Etype /= Null_Node then
               return Implicit_Conversion (Expr, Etype);
            else
               return Expr;
            end if;

         when N_String_Literal =>
            --  IEEE 1364-2005 4.2.3.2 String value padding and potential [...]
            --  [...] the values stored shall be padded on the left with zeros.
            --
            --  GHDL: we suppose that strings are unsigned integers.
            --  FIXME: better ref ?
            if Etype = String_Type then
               --  FIXME: ref ?
               Set_Expr_Type (Expr, Etype);
            else
               Ntype := Get_Packed_Array_Type
                 (Int32 (Get_String_Size (Expr) * 8) - 1, 0,
                  Unsigned_Logic_Type, False);
               Set_Expr_Type (Expr, Ntype);
            end if;
            Set_Is_Constant (Expr, True);

            return Expr;
         when N_Aggregate_Literal =>
            return Sem_Aggregate_Literal (Expr, Etype);
         when N_Concatenation =>
            return Sem_Concatenation (Expr, Etype, False);
         when N_Call =>
            --  FIXME: Etype ?
            Sem_Subroutine_Call_Name (Expr);
            return Sem_Subroutine_Call_Suffix (Expr);
         when N_New_Call =>
            Sem_New_Call (Expr, Etype);
            return Expr;
         when N_Array_Method_Call =>
            --  FIXME: Etype ?
            Sem_Array_Method_Call_With (Expr);
            return Expr;
         when N_Dynamic_Array_New =>
            Sem_Dynamic_Array_New (Expr, Etype);
            return Expr;
         when N_Binary_Op
           | N_Short_Circuit_Op =>
            case Get_Binary_Op (Expr) is
               when Binary_Shift_Ops =>
                  return Sem_Shift_Operator (Expr, Etype);
               when Binary_Arith_Ops
                 | Binary_Bitwise_Ops =>
                  return Sem_Binary_Arith (Expr, Etype);
               when Binary_Logical_Ops =>
                  return Sem_Binary_Logical (Expr);
               when Binary_Relational_Ops
                 | Binary_Equality_Ops =>
                  return Sem_Relational_Operator (Expr);
               when others =>
                  Error_Kind ("sem_sub_expression(binary)-"
                              & Binary_Ops'Image (Get_Binary_Op (Expr)), Expr);
            end case;
         when N_Unary_Op =>
            return Sem_Unary_Logic (Expr, Etype);
         when N_Cond_Op =>
            return Sem_Conditional_Operator (Expr, Etype);
         when N_Bit_Select
           | N_Part_Select
           | N_Plus_Part_Select
           | N_Minus_Part_Select
           | N_Dotted_Name
           | N_Name
           | N_This_Name
           | N_Property_Name
           | N_Hierarchical
           | N_Interface_Item
           | N_Modport_Item
           | N_This
           | N_Scoped_Name =>
            return Sem_Name_To_Expression (Sem_Name (Expr));
         when N_System_Call =>
            Sem_System_Function_Call (Expr, Etype);
            return Expr;
         when Nkinds_Inc_Dec =>
            declare
               Name : Node;
            begin
               Name := Sem_Lvalue (Get_Lvalue (Expr), Allow_Var => True);
               Set_Lvalue (Expr, Name);
               Set_Expr_Type (Expr, Get_Expr_Type (Name));
               return Expr;
            end;
         when N_Assign_Operator =>
            Sem_Assign_Operator (Expr);
            return Expr;
         when N_Parenthesis_Expr =>
            declare
               Expr1 : Node;
            begin
               Expr1 := Sem_Sub_Expression (Get_Expression (Expr), Etype);
               Set_Expression (Expr, Expr1);
               Set_Expr_Type (Expr, Get_Expr_Type (Expr1));
               Set_Is_Constant (Expr, Get_Is_Constant (Expr1));
               return Expr;
            end;
         when N_Null =>
            Sem_Null (Expr, Etype);
            return Expr;
         when N_Type_Cast =>
            Sem_Type_Cast (Expr);
            return Expr;
         when N_Size_Cast =>
            Sem_Size_Cast (Expr);
            return Expr;
         when N_Bits_Expr =>
            return Sem_Bits_Expr (Expr);
         when N_Bits_Type =>
            return Sem_Bits_Type (Expr);

         when N_Membership =>
            return Sem_Membership (Expr);

         when N_Left_Streaming_Type =>
            Sem_Streaming_Concatenation (Expr);
            return Expr;

         when N_Error_Expr =>
            return Expr;

         when others =>
            Error_Kind ("sem_sub_expression", Expr);
      end case;
   end Sem_Sub_Expression;

   --  1800-2017 11.6.1 Rules for expression bit lengths
   --
   --  Propagate the length from ETYPE to EXPR, insert casts.
   function Sem_Propagate_Length (Expr : Node; Etype : Node) return Node
   is
      Expr_Type : Node;
      Res : Node;
      Res_Type : Node;
   begin
      if Expr = Null_Node then
         --  Don't crash on error.
         return Null_Node;
      end if;

      Expr_Type := Get_Expr_Type (Expr);
      if Expr_Type = Null_Node then
         --  Error.
         return Expr;
      end if;

      if Expr_Type = Error_Type then
         --  If it is an error, just pretend it has the correct type.
         if Etype /= Null_Node then
            if Get_Kind (Expr) = N_Error_Expr then
               Res := Expr;
            else
               Res := Build_Error_Expr (Expr);
            end if;
            Set_Expr_Type (Res, Etype);
            return Res;
         end if;
         return Expr;
      end if;

      --  Length propagation applies only to integral types.
      if not Is_Integral_Type (Expr_Type) then
         return Expr;
      end if;

      if Etype = Null_Node then
         --  Fully self determined type.  Just continue the propagation to sub
         --  expressions.
         Res_Type := Expr_Type;
      else
         if Get_Type_Width (Etype) > Get_Type_Width (Expr_Type) then
            --  Will expand the result, but keep the same sign.
            if Get_Signed_Flag (Etype) = Get_Signed_Flag (Expr_Type) then
               Res_Type := Etype;
            else
               declare
                  El_Type : Node;
                  Base_Type : Node;
               begin
                  Base_Type := Expr_Type;
                  loop
                     case Get_Kind (Base_Type) is
                        when Nkinds_Vector_Types =>
                           El_Type := Get_Type_Element_Type (Base_Type);
                           exit;
                        when N_Logic_Type
                          | N_Bit_Type =>
                           El_Type := Base_Type;
                           exit;
                        when N_Enum_Type =>
                           Base_Type := Get_Enum_Base_Type (Base_Type);
                           --  Again.
                        when others =>
                           --  TODO: packed unions, packed struct ?
                           raise Internal_Error;
                     end case;
                  end loop;
                  Res_Type := Get_Packed_Array_Type
                    (Int32 (Get_Type_Width (Etype) - 1), 0,
                     El_Type, Get_Signed_Flag (Expr_Type));
               end;
            end if;
         else
            --  Keep the type of the expression.
            --  FIXME: sign ?
            Res_Type := Expr_Type;
         end if;
      end if;

      case Get_Kind (Expr) is
         when N_Number
            | N_Bignum
            | N_Computed_Number
            | N_String_Literal
            | N_Aggregate_Literal =>
            --  FIXME: directly expand number/string ?
            Res := Implicit_Conversion (Expr, Res_Type);
         when N_Unbased_Literal
            | N_Error_Expr =>
            Set_Expr_Type (Expr, Res_Type);
            Res := Expr;
         when N_Name
           | N_This_Name
           | N_Scoped_Name
           | N_Class_Qualified_Name
           | N_Member_Name
           | N_Property_Name
           | N_Method_Name
           | N_Hierarchical
           | N_Interface_Item
           | N_Modport_Item
           | N_Bit_Select
           | N_Part_Select_Cst
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst
           | N_Indexed_Name
           | N_String_Index
           | N_Associative_Index
           | N_Concatenation
           | N_Replication_Cst
           | N_System_Call
           | N_Call
           | N_Conversion
           | N_Type_Cast
           | N_Size_Cast
           | N_Bits_Type
           | N_Bits_Expr
           | Nkinds_Inc_Dec
           | N_Membership
           | N_Assign_Operator =>
            Res := Implicit_Conversion (Expr, Res_Type);
         when N_Cond_Op =>
            Set_Cond_True
              (Expr, Sem_Propagate_Length (Get_Cond_True (Expr), Res_Type));
            Set_Cond_False
              (Expr, Sem_Propagate_Length (Get_Cond_False (Expr), Res_Type));
            Set_Expr_Type (Expr, Res_Type);
            Res := Expr;
         when N_Binary_Op
           | N_Short_Circuit_Op =>
            declare
               Left, Right : Node;
            begin
               case Get_Binary_Op (Expr) is
                  when Binary_Shift_Ops =>
                     --  Right operand is self-determined, nothing to do.
                     Left := Sem_Propagate_Length (Get_Left (Expr), Res_Type);
                     Set_Left (Expr, Left);
                     Set_Expr_Type (Expr, Res_Type);
                     Res := Expr;
                  when Binary_Arith_Ops
                    | Binary_Bitwise_Ops =>
                     Left := Sem_Propagate_Length
                       (Get_Left (Expr), Res_Type);
                     Set_Left (Expr, Left);
                     Right := Sem_Propagate_Length
                       (Get_Right (Expr), Res_Type);
                     Set_Right (Expr, Right);
                     Set_Expr_Type (Expr, Res_Type);
                     Res := Expr;
                  when Binary_Equality_Ops
                    | Binary_Relational_Ops
                    | Binary_Logical_Ops =>
                     --  Operands are self-determined.
                     Res := Implicit_Conversion (Expr, Res_Type);
                  when others =>
                     Error_Kind ("sem_propagate_length (binary:"
                                   & Binary_Ops'Image (Get_Binary_Op (Expr))
                                   & ")", Expr);
               end case;
            end;
         when N_Unary_Op =>
            declare
               Operand : Node;
            begin
               case Get_Unary_Op (Expr) is
                  when Unop_Bit_Neg
                    | Unop_Plus
                    | Unop_Minus =>
                     Operand := Sem_Propagate_Length
                       (Get_Expression (Expr), Res_Type);
                     Set_Expression (Expr, Operand);
                     Set_Expr_Type (Expr, Res_Type);
                     Res := Expr;
                  when Unop_Logic_Neg =>
                     --  Self-determined.
                     Res := Expr;
                  when Unary_Red_Ops =>
                     --  Self-determined.
                     Res := Implicit_Conversion (Expr, Res_Type);
               end case;
            end;
         when N_Parenthesis_Expr =>
            Res := Sem_Propagate_Length (Get_Expression (Expr), Res_Type);
            Set_Expression (Expr, Res);
            Set_Expr_Type (Expr, Res_Type);
            Res := Expr;
         when others =>
            Error_Kind ("sem_propagate_length", Expr);
      end case;

      if Etype /= Null_Node and then Res_Type /= Etype then
         --  FIXME: can only truncate.
         Res := Implicit_Conversion (Res, Etype);
      end if;

      return Res;
   end Sem_Propagate_Length;

   function Sem_Expression (Expr : Node; Etype : Node) return Node
   is
      Res : Node;
   begin
      --  ETYPE is used only to determine EXPR type (SV only).  The length
      --  of the result is the length as propagated from operands.
      Res := Sem_Sub_Expression (Expr, Etype);

      --  Propagate bit lengths from result to operands.
      if Etype = Null_Node then
         Res := Sem_Propagate_Length (Res, Null_Node);
      elsif Get_Kind (Etype) = N_Enum_Type then
         Res := Sem_Propagate_Length (Res, Get_Enum_Base_Type (Etype));
      elsif Is_Integral_Type (Etype) then
         Res := Sem_Propagate_Length (Res, Etype);
      end if;

      return Res;
   end Sem_Expression;

   function Sem_Event_Expression (Expr : Node) return Node is
   begin
      case Get_Kind (Expr) is
         when N_Posedge
           | N_Negedge =>
            declare
               E : Node;
            begin
               E :=  Sem_Expression (Get_Expression (Expr), Null_Node);

               --  IEEE 1364-2005 4.8.1 Operators and real numbers
               --  Real number constants and real variables are also prohibited
               --  in the following cases:
               --   - Edge descriptors (posedge, negedge) applied to real
               --     variables.
               if E /= Null_Node
                 and then not Is_Integral_Type (Get_Expr_Type (E))
               then
                  Error_Msg_Sem
                    (+E, "edge expression must be an integral type");
               end if;
               Set_Expression (Expr, E);
               return Expr;
            end;
         when N_Or =>
            declare
               E : Node;
            begin
               E := Sem_Event_Expression (Get_Left (Expr));
               Set_Left (Expr, E);
               E := Sem_Event_Expression (Get_Right (Expr));
               Set_Right (Expr, E);
               return Expr;
            end;
         when N_Name
            | N_Dotted_Name
            | N_This_Name =>
            return Sem_Name (Expr);
         when N_Implicit_Event =>
            return Expr;
         when N_Parenthesis_Expr =>
            return Sem_Event_Expression (Get_Expression (Expr));
         when others =>
            Error_Kind ("sem_event_expression", Expr);
      end case;
   end Sem_Event_Expression;

   procedure Sem_Cond_Expression (Stmt : Node)
   is
      Cond : Node;
   begin
      Cond := Get_Condition (Stmt);
      Cond := Sem_Expression (Cond, Null_Node);
      Set_Condition (Stmt, Cond);
   end Sem_Cond_Expression;

   --  1800-2017 11.4.14 Streaming operators (pack/unpack)
   procedure Sem_Streaming_Concatenation (Expr : Node)
   is
      El : Node;
      El_Expr : Node;
   begin
      El := Get_Expressions (Expr);
      while El /= Null_Node loop
         El_Expr := Get_Expression (El);

         --  TODO: El_Expr can be a streaming concatenation.
         El_Expr := Sem_Expression (El_Expr, Null_Node);

         Set_Expression (El, El_Expr);

         --  TODO: check El_Expr is a bit-stream type (cf 6.24.3).

         El := Get_Chain (El);
      end loop;

      case Nkinds_Streaming (Get_Kind (Expr)) is
         when N_Left_Streaming_Expr
           | N_Right_Streaming_Expr =>
            declare
               Size : Node;
            begin
               Size := Get_Expression (Expr);
               if Size /= Null_Node then
                  Size := Sem_Expression (Size, Null_Node);
                  Set_Expression (Expr, Size);
                  --  FIXME: ref ?
                  if not Is_Integral_Type (Get_Expr_Type (Size)) then
                     Error_Msg_Sem
                       (+Size, "size expression must be an integral type");
                  end if;
               end if;
            end;
         when N_Left_Streaming_Type
           | N_Right_Streaming_Type =>
            declare
               Size_Type : Node;
            begin
               Size_Type := Get_Slice_Size_Type (Expr);
               if Size_Type /= Null_Node then
                  if Get_Type_Owner (Expr) then
                     Sem_Data_Type (Size_Type);
                  end if;
                  --  FIXME: ref ?
                  if not Is_Integral_Type (Get_Expr_Type (Size_Type)) then
                     Error_Msg_Sem
                       (+Size_Type, "size type must be an integral type");
                  end if;
               end if;
            end;
      end case;
   end Sem_Streaming_Concatenation;

   --  Return the SIGNED version of ATYPE.
   function Convert_Sign (Atype : Node; Signed : Boolean) return Node
   is
      El_Type : Node;
   begin
      case Get_Kind (Atype) is
         when Nkinds_Vector_Types =>
            El_Type := Get_Type_Element_Type (Atype);
         when N_Logic_Type =>
            if Signed then
               return Signed_Logic_Type;
            else
               return Unsigned_Logic_Type;
            end if;
         when N_Bit_Type =>
            if Signed then
               return Signed_Bit_Type;
            else
               return Unsigned_Bit_Type;
            end if;
         when others =>
            --  TODO: packed unions, packed struct ?
            raise Internal_Error;
      end case;
      return Get_Packed_Array_Type
        (Int32 (Get_Type_Width (Atype) - 1), 0, El_Type, Signed);
   end Convert_Sign;

   procedure Sem_Sign_System_Function_Call (Call : Node; Is_Signed : Boolean)
   is
      Arg : constant Node := Get_Arguments (Call);
      Expr : Node;
      Expr_Type : Node;
   begin
      if Arg = Null_Node or else Get_Chain (Arg) /= Null_Node then
         Error_Msg_Sem (+Call, "call to %i must have one argument", +Call);
         return;
      end if;

      Expr := Get_Expression (Arg);
      Expr_Type := Get_Expr_Type (Expr);
      if Expr_Type = Null_Node then
         return;
      end if;
      if not Is_Integral_Type (Expr_Type) then
         Error_Msg_Sem (+Arg, "%i allows only integral expression", +Call);
         return;
      end if;
      if Get_Signed_Flag (Expr_Type) = Is_Signed then
         Warning_Msg_Sem (+Arg, "useless sign conversion");
         Set_Expr_Type (Call, Expr_Type);
         return;
      end if;
      Set_Expr_Type (Call, Convert_Sign (Expr_Type, Is_Signed));
   end Sem_Sign_System_Function_Call;

   --  1800-2017 6.24.2 $cast dynamic casting
   procedure Sem_Cast_System_Function_Call (Call : Node)
   is
      Dest_Arg : constant Node := Get_Arguments (Call);
      Src_Arg : Node;
      Expr : Node;
   begin
      Set_Expr_Type (Call, Unsigned_Bit_Type);

      if Dest_Arg /= Null_Node then
         Src_Arg := Get_Chain (Dest_Arg);
      else
         Src_Arg := Null_Node;
      end if;
      if Src_Arg = Null_Node or else Get_Chain (Src_Arg) /= Null_Node then
         Error_Msg_Sem (+Call, "call to $cast must have two arguments");
         return;
      end if;

      --  1800-2017 6.24.2 $cast dynamic casting
      --  The dest_var is the variabke to which the assignment is made.
      --  GHDL: assume only variable is allowed.
      Expr := Get_Expression (Dest_Arg);
      loop
         case Get_Kind (Expr) is
            when N_Name
              | N_Property_Name
              | N_This_Name =>
               Expr := Get_Declaration (Expr);
            when N_Var
              | N_Return_Var
              | N_Tf_Output
              | N_Tf_Inout =>
               exit;
            when N_Indexed_Name
              | N_Associative_Index =>
               Expr := Get_Name (Expr);
            when others =>
               Error_Msg_Sem
                 (+Dest_Arg, "first $cast argument must be a variable");
               exit;
         end case;
      end loop;
   end Sem_Cast_System_Function_Call;

   --  1800-2017 20.6.1 Type name function
   procedure Sem_Typename_System_Function_Call (Call : Node)
   is
      Arg : constant Node := Get_Arguments (Call);
   begin
      Set_Expr_Type (Call, String_Type);

      if Arg = Null_Node or else Get_Chain (Arg) /= Null_Node then
         Error_Msg_Sem (+Call, "call to $typename must have one argument");
         return;
      end if;

      --  TODO...
   end Sem_Typename_System_Function_Call;

   --  1800-2017 20.7 Array query functions
   --  array_query_function ::=
   --      array_dimension_function ( array_expr [, dimension_expr ] )
   --    | array_dimension_function ( data_type [, dimension_expr ] )
   --    | ...
   --
   --  array_dimension_function ::=
   --      $left
   --    | $right
   --    | $low
   --    | $high
   --    | $increment
   --    | $size
   procedure Sem_Array_Dimension_System_Function_Call (Call : Node)
   is
      Arr_Arg : constant Node := Get_Arguments (Call);
      Dim_Arg : Node;
      Arr, Dim : Node;
      Arr_Type : Node;
      Is_Type : Boolean;
   begin
      --  1800-2017 20.7 Array query functions
      --  The return type is INTEGER
      Set_Expr_Type (Call, Signed_Integer_Type);

      --  Extract and check number of arguments.
      if Arr_Arg /= Null_Node then
         Arr := Get_Expression (Arr_Arg);
         Dim_Arg := Get_Chain (Arr_Arg);
         if Dim_Arg /= Null_Node then
            Dim := Get_Expression (Dim_Arg);
         else
            Dim := Null_Node;
         end if;
      else
         Arr := Null_Node;
         Dim := Null_Node;
         Dim_Arg := Null_Node;
      end if;

      if Arr = Null_Node then
         Error_Msg_Sem
           (+Call, "array dimension function requires at least one argument");
         return;
      end if;
      if Dim_Arg /= Null_Node and then Get_Chain (Dim_Arg) /= Null_Node then
         Error_Msg_Sem
           (+Call, "too many arguments to array dimension function");
         return;
      end if;

      Is_Type := Is_Type_Name (Arr);
      if Is_Type then
         Sem_Data_Type (Arr);
      else
         Arr := Sem_Expression (Arr, Null_Node);
         Set_Expression (Arr_Arg, Arr);
      end if;

      --  1800-2017 20.7 Array query functions
      --  SystemVerilog provides system functions to return information about
      --  a particular dimension of an array or integral data type or of data
      --  objects of such a data type.
      Arr_Type := Get_Expr_Type (Arr);
      case Get_Kind (Arr_Type) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst =>
            null;
         when N_Dynamic_Array_Cst
           | N_Associative_Array_Cst
           | N_Queue_Cst =>
            --  1800-2017 20.7 Array query functions
            --  It is an error to use these functions directly on a dynamically
            --  sized type identifier.
            if Is_Type then
               Error_Msg_Sem
                 (+Arr, "cannot use array query on a dynamic array type");
            end if;
         when N_Error_Type =>
            null;
         when others =>
            Error_Msg_Sem
              (+Arr, "array dimension function called on a non-array arg");
      end case;

      if Dim /= Null_Node then
         Dim := Sem_Expression (Dim, Signed_Integer_Type);
         Set_Expression (Dim_Arg, Dim);
      end if;
   end Sem_Array_Dimension_System_Function_Call;

end Verilog.Sem_Expr;
