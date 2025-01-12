--  Verilog semantic analyzer (expressions folder)
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

with Verilog.Types; use Verilog.Types;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Sem_Expr; use Verilog.Sem_Expr;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Storages;
with Verilog.Allocates;
with Verilog.Executions;
--  with Verilog.Vpi;

package body Verilog.Sem_Eval is
   --  1800-2017 11.2.1 Constant expressions

   --  Evaluate an expression.  Return it if simple enough (number or string
   --  literal), or return a computed number.
   function Sem_Eval_Constant_Expression (Expr : Node) return Node
   is
      Orig_Res_Type : constant Node := Get_Expr_Type (Expr);
      Res : Node;
   begin
      Res := Expr;

      --  FIXME: should be generic.
      loop
         case Get_Kind (Res) is
            when N_Number
               | N_Bignum
               | N_Computed_Number
               | N_Unbased_Literal
               | N_Real_Number
               | N_String_Literal
               | N_Error_Expr =>
               --  Cannot do better: it's already a constant.
               return Res;
            when N_Binary_Op
               | N_Unary_Op
               | N_Short_Circuit_Op
               | N_Cond_Op
               | N_Concatenation
               | N_Aggregate_Literal
               | N_Conversion
               | N_Bit_Select
               | N_Member_Name
               | N_Bits_Type =>
               exit;
            when N_Parameter
              | N_Localparam =>
               exit;
            when N_Name
               | N_Scoped_Name =>
               Res := Get_Declaration (Res);
            when N_Parenthesis_Expr
               | N_Enum_Name =>
               Res := Get_Expression (Res);
            when N_System_Call =>
               exit;
            when others =>
               Error_Kind ("sem_eval_constant_expression", Res);
         end case;
      end loop;

      --  Unpacked type.
      case Get_Kind (Orig_Res_Type) is
         when N_Array_Cst =>
            return Res;
         when N_Error_Type =>
            return Build_Error_Expr (Expr);
         when others =>
            null;
      end case;

      declare
         use Verilog.Storages;
         use Verilog.Allocates;
         use Verilog.Executions;
         Ssize : constant Storage_Index := Get_Storage_Size (Orig_Res_Type);
         pragma Assert (Ssize /= 0);
         Val : Storage_Type (0 .. Ssize - 1);
         R : Node;
         Res_Type : Node;
      begin
         Execute_Expression (null, Val'Address, Res);

         Res_Type := Orig_Res_Type;
         loop
            case Get_Kind (Res_Type) is
               when N_Log_Packed_Array_Cst =>
                  declare
                     V : constant Logvec_Ptr := To_Logvec_Ptr (Val'Address);
                  begin
                     if Get_Type_Width (Res_Type) <= 64 then
                        R := Create_Node (N_Computed_Number);
                        Location_Copy (R, Res);
                        Set_Expr_Type (R, Res_Type);
                        Set_Number_Lo_Val (R, V (0).Val);
                        Set_Number_Lo_Zx (R, V (0).Zx);
                        if Get_Type_Width (Res_Type) > 32 then
                           Set_Number_Hi_Val (R, V (1).Val);
                           Set_Number_Hi_Zx (R, V (1).Zx);
                        end if;
                        Set_Expr_Origin (R, Expr);
                        return R;
                     else
                        raise Program_Error;
                     end if;
                  end;
               when N_Bit_Packed_Array_Cst =>
                  declare
                     V : constant Bitvec_Ptr := To_Bitvec_Ptr (Val'Address);
                  begin
                     if Get_Type_Width (Res_Type) <= 64 then
                        R := Create_Node (N_Computed_Number);
                        Location_Copy (R, Res);
                        Set_Expr_Type (R, Res_Type);
                        Set_Number_Lo_Val (R, V (0));
                        Set_Number_Lo_Zx (R, 0);
                        if Get_Type_Width (Res_Type) > 32 then
                           Set_Number_Hi_Val (R, V (1));
                           Set_Number_Hi_Zx (R, 0);
                        end if;
                        Set_Expr_Origin (R, Expr);
                        return R;
                     else
                        raise Program_Error;
                     end if;
                  end;
               when N_Logic_Type =>
                  declare
                     V : constant Logic_Type := To_Logic_Ptr (Val'Address).all;
                     L : constant Logic_32 := To_Logic_32 (V);
                  begin
                     R := Create_Node (N_Computed_Number);
                     Location_Copy (R, Res);
                     Set_Expr_Type (R, Res_Type);
                     Set_Number_Lo_Val (R, L.Val);
                     Set_Number_Lo_Zx (R, L.Zx);
                     Set_Expr_Origin (R, Expr);
                     return R;
                  end;
               when N_Bit_Type =>
                  declare
                     V : constant Bit_Type := To_Bit_Ptr (Val'Address).all;
                  begin
                     R := Create_Node (N_Computed_Number);
                     Location_Copy (R, Res);
                     Set_Expr_Type (R, Res_Type);
                     Set_Number_Lo_Val (R, Bit_Type'Pos (V));
                     Set_Number_Lo_Zx (R, 0);
                     Set_Expr_Origin (R, Expr);
                     return R;
                  end;
               when N_Packed_Struct_Type =>
                  --  Try again.
                  Res_Type := Get_Packed_Base_Type (Res_Type);
               when N_Enum_Type =>
                  --  Try again.
                  Res_Type := Get_Enum_Base_Type (Res_Type);
               when others =>
                  Error_Kind ("sem_constant_expression(type)", Res_Type);
            end case;
         end loop;
      end;
   end Sem_Eval_Constant_Expression;

   function Sem_Constant_Expression (Expr : Node; Atype : Node) return Node
   is
      Res : Node;
   begin
      Res := Sem_Expression (Expr, Atype);

      return Sem_Eval_Constant_Expression (Res);
   end Sem_Constant_Expression;

   function Number_To_Int32 (Expr : Node) return Int32
   is
      Etype : constant Node := Get_Expr_Type (Expr);
   begin
      --  IEEE 1364-2005 4 Expressions
      --  The operands of constant expression consist of constant
      --  numbers, [...]

      --  Note: a number can never be negative.

      --  Constant numbers cannot have x/z.
      --  FIXME: ref ?
      if Get_Number_Hi_Zx (Expr) /= 0 or Get_Number_Lo_Zx (Expr) /= 0
      then
         Error_Msg_Sem (+Expr, "constant number contains x/z bits");
         return 0;
      end if;
      if Get_Type_Width (Etype) > 32
        and then (Get_Number_Hi_Val (Expr) /= 0
                    or (Get_Number_Lo_Val (Expr) and 16#8000_000#) /= 0)
      then
         Error_Msg_Sem (+Expr, "constant number is too big");
         return Int32'Last;
      end if;
      return To_Int32 (Get_Number_Lo_Val (Expr));
   end Number_To_Int32;

   function Sem_Constant_Integer_Expression (Expr : Node) return Int32 is
   begin
      case Get_Kind (Expr) is
         when N_Number
           | N_Computed_Number =>
            return Number_To_Int32 (Expr);
         when N_Genvar =>
            return Get_Generate_Index (Expr);
         when N_Name
            | N_Scoped_Name =>
            declare
               Decl : constant Node := Get_Declaration (Expr);
            begin
               if Decl = Null_Node then
                  return 0;
               else
                  return Sem_Constant_Integer_Expression (Decl);
               end if;
            end;
         when N_Enum_Name
            | N_Localparam =>
            return Sem_Constant_Integer_Expression (Get_Expression (Expr));
         when N_Parameter =>
            return Sem_Constant_Integer_Expression
              (Get_Parameter_Expression (Expr));
         when N_Short_Circuit_Op
           | N_Binary_Op
           | N_Unary_Op
           | N_Conversion
           | N_Parenthesis_Expr
           | N_Cond_Op
           | N_System_Call
           | N_Bits_Type =>
            declare
               Res : Node;
               Val : Int32;
            begin
               --  TODO: simply execute ?
               --  Use Execute_Expression_Int32 ?
               Res := Sem_Eval_Constant_Expression (Expr);
               Val := Number_To_Int32 (Res);
               if Res /= Expr then
                  Free_Node (Res);
               end if;
               return Val;
            end;
         when N_Error_Expr =>
            return 0;
         when others =>
            Error_Kind ("sem_constant_integer_expression", Expr);
      end case;
   end Sem_Constant_Integer_Expression;

end Verilog.Sem_Eval;
