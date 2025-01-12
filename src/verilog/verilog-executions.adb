--  Verilog expressions interpreter
--  Copyright (C) 2023 Tristan Gingold
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

with System; use System;
with Tables;
with Str_Table;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Parse;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Sem_Utils;
with Verilog.Sem_Types;
with Verilog.Sv_Strings; use Verilog.Sv_Strings;
with Verilog.Sv_Classes; use Verilog.Sv_Classes;
with Verilog.Sv_Arrays; use Verilog.Sv_Arrays;
with Verilog.Sv_Queues; use Verilog.Sv_Queues;
with Verilog.Sv_Maps; use Verilog.Sv_Maps;
with Verilog.Vpi;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Simulation; use Verilog.Simulation;

package body Verilog.Executions is
   type Lit_Kind is (Lit_String);
   type Lit_Type (Kind : Lit_Kind := Lit_String) is record
      case Kind is
         when Lit_String =>
            Str : Sv_String;
      end case;
   end record;

   package Lits is new Tables
     (Table_Component_Type => Lit_Type,
      Table_Index_Type => Lit_Id,
      Table_Low_Bound => No_Lit_Id + 1,
      Table_Initial => 16);

   function Boolean_To_Logic (B : Boolean) return Logic_Type is
   begin
      if B then
         return V_1;
      else
         return V_0;
      end if;
   end Boolean_To_Logic;

   procedure Execute_Conversion
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Arg : constant Node := Get_Expression (Expr);
      Arg_Type : constant Node := Get_Expr_Type (Arg);
      Ssize : constant Storage_Index := Get_Storage_Size (Arg_Type);
      Res : Storage_Type (0 .. Ssize - 1);
   begin
      Execute_Expression (Frame, Res'Address, Arg);

      Compute_Conversion (Dest, Expr, Res'Address);
   end Execute_Conversion;

   procedure Execute_Binary_Lv_Expression (Dest : Data_Ptr;
                                           Expr : Node;
                                           Ldata : Logvec_Ptr;
                                           Left_Width : Width_Type;
                                           Rdata : Logvec_Ptr;
                                           Right_Width : Width_Type)
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
   begin
      case Get_Binary_Op (Expr) is
         when Binop_Bit_And =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_And (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Bit_Or =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Or (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Bit_Xor =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Xor (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Bit_Nxor
            | Binop_Bit_Xnor =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Xnor (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Add =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Add (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Sub =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Sub (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Umul =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Umul (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Smul =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Smul (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Sdiv =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Sdiv (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Udiv =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Udiv (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Smod =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Smod (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Umod =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Umod (To_Logvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Right_Lshift =>
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Shr (To_Logvec_Ptr (Dest),
                         Ldata, Left_Width, Rdata, Right_Width);
         when Binop_Right_Ashift =>
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            if Get_Signed_Flag (Expr_Type) then
               Compute_Asr (To_Logvec_Ptr (Dest),
                            Ldata, Left_Width, Rdata, Right_Width);
            else
               Compute_Shr (To_Logvec_Ptr (Dest),
                            Ldata, Left_Width, Rdata, Right_Width);
            end if;
         when Binop_Left_Lshift
           | Binop_Left_Ashift =>
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Shl (To_Logvec_Ptr (Dest),
                         Ldata, Left_Width, Rdata, Right_Width);
         when Binop_Case_Eq =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Case (Ldata, Rdata, Left_Width, True);
         when Binop_Case_Ne =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Case (Ldata, Rdata, Left_Width, False);
         when Binop_Log_Ne =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Log_Eq (Ldata, Rdata, Left_Width, False);
         when Binop_Log_Eq =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Log_Eq (Ldata, Rdata, Left_Width, True);
         when Binop_Ule =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Ule (Ldata, Rdata, Left_Width);
         when Binop_Sle =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Sle (Ldata, Rdata, Left_Width);
         when Binop_Uge =>
            --  A >= B  <=>  B <= A
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Ule (Rdata, Ldata, Left_Width);
         when Binop_Sge =>
            --  A >= B  <=>  B <= A
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Sle (Rdata, Ldata, Left_Width);
         when Binop_Ult =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Ult (Ldata, Rdata, Left_Width);
         when Binop_Slt =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Slt (Ldata, Rdata, Left_Width);
         when Binop_Ugt =>
            --  A > B  <=>  B < A
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Ult (Rdata, Ldata, Left_Width);
         when Binop_Sgt =>
            --  A > B  <=>  B < A
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Compute_Slt (Rdata, Ldata, Left_Width);
         when others =>
            Error_Kind ("execute_binary_lv_expression:"
                          & Binary_Ops'Image (Get_Binary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Binary_Lv_Expression;

   procedure Execute_Binary_Log_Expression (Dest : Logic_Ptr;
                                            Expr : Node;
                                            Left : Logic_Type;
                                            Right : Logic_Type)
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
   begin
      --  FIXME: short circuit ?
      case Get_Binary_Op (Expr) is
         when Binop_Case_Ne =>
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            Dest.all := Boolean_To_Logic (Left /= Right);
         when Binop_Log_Ne =>
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            if Left >= V_X or Right >= V_X then
               Dest.all := V_X;
            else
               Dest.all := Boolean_To_Logic (Left /= Right);
            end if;
         when Binop_Bit_And =>
            Dest.all := Logic_And (Left, Right);
         when Binop_Bit_Or =>
            Dest.all := Logic_Or (Left, Right);
         when Binop_Bit_Xor =>
            Dest.all := Logic_Xor (Left, Right);
         when others =>
            Error_Kind ("execute_binary_log_expression:"
                          & Binary_Ops'Image (Get_Binary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Binary_Log_Expression;

   procedure Execute_Binary_Bv_Expression (Dest : Data_Ptr;
                                           Expr : Node;
                                           Ldata : Bitvec_Ptr;
                                           Left_Width : Width_Type;
                                           Rdata : Bitvec_Ptr;
                                           Right_Width : Width_Type)
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
   begin
      case Get_Binary_Op (Expr) is
         when Binop_Left_Lshift
           | Binop_Left_Ashift =>
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Shl (To_Bitvec_Ptr (Dest),
                         Ldata, Left_Width, Rdata, Right_Width);
         when Binop_Bit_Or =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Or (To_Bitvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Add =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Add (To_Bitvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Sub =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Sub (To_Bitvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Udiv =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Udiv (To_Bitvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when Binop_Smod =>
            pragma Assert (Left_Width = Right_Width);
            pragma Assert (Left_Width = Get_Type_Width (Expr_Type));
            Compute_Smod (To_Bitvec_Ptr (Dest), Ldata, Rdata, Left_Width);
         when others =>
            Error_Kind ("execute_binary_bv_expression:"
                          & Binary_Ops'Image (Get_Binary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Binary_Bv_Expression;

   procedure Execute_Binary_String_Expression (Dest : Data_Ptr;
                                               Expr : Node;
                                               Left : Sv_String;
                                               Right : Sv_String)
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
   begin
      --  FIXME: short circuit ?
      case Get_Binary_Op (Expr) is
         when Binop_Log_Ne
           | Binop_Case_Ne =>
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Boolean_To_Logic (not Is_Eq (Left, Right));
         when Binop_Log_Eq =>
            pragma Assert (Expr_Type = Unsigned_Logic_Type);
            To_Logic_Ptr (Dest).all :=
              Boolean_To_Logic (Is_Eq (Left, Right));
         when others =>
            Error_Kind ("execute_binary_string_expression:"
                          & Binary_Ops'Image (Get_Binary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Binary_String_Expression;

   procedure Execute_Binary_Fp64_Expression (Dest : Data_Ptr;
                                             Expr : Node;
                                             Left : Fp64;
                                             Right : Fp64) is
   begin
      case Get_Binary_Op (Expr) is
         when Binop_Sdiv =>
            To_Fp64_Ptr (Dest).all := Left / Right;
         when Binop_Log_Ne
           | Binop_Case_Ne =>
            --  FIXME: handle NaN ?
            To_Logic_Ptr (Dest).all := Boolean_To_Logic (Left /= Right);
         when others =>
            Error_Kind ("execute_binary_fp64_expression:"
                          & Binary_Ops'Image (Get_Binary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Binary_Fp64_Expression;

   procedure Execute_Binary_Fp32_Expression (Dest : Data_Ptr;
                                             Expr : Node;
                                             Left : Fp32;
                                             Right : Fp32) is
   begin
      case Get_Binary_Op (Expr) is
         when Binop_Smul =>
            To_Fp32_Ptr (Dest).all := Left * Right;
         when Binop_Sub =>
            To_Fp32_Ptr (Dest).all := Left - Right;
         when Binop_Add =>
            To_Fp32_Ptr (Dest).all := Left + Right;
         when others =>
            Error_Kind ("execute_binary_fp32_expression:"
                          & Binary_Ops'Image (Get_Binary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Binary_Fp32_Expression;

   procedure Execute_Binary_Class_Expression (Dest : Data_Ptr;
                                              Expr : Node;
                                              Left : Sv_Class_Handle;
                                              Right : Sv_Class_Handle) is
   begin
      case Get_Binary_Op (Expr) is
         when Binop_Log_Ne
           | Binop_Case_Ne =>
            To_Logic_Ptr (Dest).all := Boolean_To_Logic (Left /= Right);
         when Binop_Log_Eq
           | Binop_Case_Eq =>
            To_Logic_Ptr (Dest).all := Boolean_To_Logic (Left = Right);
         when others =>
            Error_Kind ("execute_binary_class_expression:"
                          & Binary_Ops'Image (Get_Binary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Binary_Class_Expression;

   procedure Execute_Unary_Lv_Expression
     (Dest : Data_Ptr; Expr : Node; Op : Logvec_Ptr; Width : Width_Type) is
   begin
      case Get_Unary_Op (Expr) is
         when Unop_Bit_Neg =>
            pragma Assert (Width = Get_Type_Width (Get_Expr_Type (Expr)));
            Compute_Not (To_Logvec_Ptr (Dest), Op, Width);
         when Unop_Plus =>
            Assign (To_Logvec_Ptr (Dest), Op, Width);
         when Unop_Logic_Neg =>
            To_Logic_Ptr (Dest).all := Compute_Log_Neg (Op, Width);
         when Unop_Minus =>
            pragma Assert (Width = Get_Type_Width (Get_Expr_Type (Expr)));
            Compute_Neg (To_Logvec_Ptr (Dest), Op, Width);
         when Unop_Red_Or =>
            To_Logic_Ptr (Dest).all := Compute_Log_Red_Or (Op, Width);
         when Unop_Red_Nor =>
            To_Logic_Ptr (Dest).all := Compute_Log_Red_Nor (Op, Width);
         when Unop_Red_And =>
            To_Logic_Ptr (Dest).all := Compute_Log_Red_And (Op, Width);
         when Unop_Red_Nand =>
            To_Logic_Ptr (Dest).all := Compute_Log_Red_Nand (Op, Width);
         when Unop_Red_Xor =>
            To_Logic_Ptr (Dest).all := Compute_Log_Red_Xor (Op, Width);
         when Unop_Red_Nxor
            | Unop_Red_Xnor =>
            To_Logic_Ptr (Dest).all := Compute_Log_Red_Xnor (Op, Width);
      end case;
   end Execute_Unary_Lv_Expression;

   procedure Execute_Unary_Bv_Expression
     (Dest : Data_Ptr; Expr : Node; Op : Bitvec_Ptr; Width : Width_Type) is
   begin
      case Get_Unary_Op (Expr) is
         when Unop_Minus =>
            pragma Assert (Width = Get_Type_Width (Get_Expr_Type (Expr)));
            Compute_Neg (To_Bitvec_Ptr (Dest), Op, Width);
         when others =>
            Error_Kind ("execute_unary_bv_expression:"
                          & Unary_Ops'Image (Get_Unary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Unary_Bv_Expression;

   procedure Execute_Unary_Log_Expression
     (Dest : Data_Ptr; Expr : Node; Op : Logic_Type)
   is
      R : Logic_Type;
   begin
      case Get_Unary_Op (Expr) is
         when Unop_Bit_Neg
           | Unop_Logic_Neg =>
            case Op is
               when V_0 => R := V_1;
               when V_1 => R := V_0;
               when V_X => R := V_X;
               when V_Z => R := V_X;
            end case;
            To_Logic_Ptr (Dest).all := R;
         when others =>
            Error_Kind ("execute_unary_log_expression:"
                          & Unary_Ops'Image (Get_Unary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Unary_Log_Expression;

   procedure Execute_Unary_Bit_Expression
     (Dest : Data_Ptr; Expr : Node; Op : Bit_Type)
   is
      R : Bit_Type;
   begin
      case Get_Unary_Op (Expr) is
         when Unop_Bit_Neg
           | Unop_Logic_Neg =>
            case Op is
               when B_0 => R := B_1;
               when B_1 => R := B_0;
            end case;
            To_Bit_Ptr (Dest).all := R;
         when others =>
            Error_Kind ("execute_unary_bit_expression:"
                          & Unary_Ops'Image (Get_Unary_Op (Expr)),
                        Expr);
      end case;
   end Execute_Unary_Bit_Expression;

   procedure Execute_Unary_Fp64_Expression
     (Dest : Data_Ptr; Expr : Node; Op : Fp64) is
   begin
      case Get_Unary_Op (Expr) is
         when Unop_Minus =>
            To_Fp64_Ptr (Dest).all := -Op;
         when others =>
            Error_Kind ("execute_unary_fp64_expression:"
                          & Unary_Ops'Image (Get_Unary_Op (Expr)), Expr);
      end case;
   end Execute_Unary_Fp64_Expression;

   function Execute_Bit_Select_Log_Expression
     (Lv : Logvec_Ptr; Width : Width_Type; Offset : Bit_Offset)
     return Logic_Type is
   begin
      if Width = 0 then
         return V_X;
      else
         return Compute_Bit_Select (Lv, Offset);
      end if;
   end Execute_Bit_Select_Log_Expression;

   function Execute_Bit_Select_Bit_Expression
     (Bv : Bitvec_Ptr; Width : Width_Type; Offset : Bit_Offset)
     return Bit_Type is
   begin
      if Width = 0 then
         return B_0;
      else
         return Compute_Bit_Select (Bv, Offset);
      end if;
   end Execute_Bit_Select_Bit_Expression;

   procedure Execute_Builtin_Method_Call
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node; Subprg : Node)
   is
      Obj_Expr : constant Node := Get_Object (Expr);
   begin
      case Subprg is
         when Enum_Name_Method =>
            declare
               Obj_Type : constant Node := Get_Expr_Type (Obj_Expr);
               Base_Type : constant Node := Get_Enum_Base_Type (Obj_Type);
               Ssize : constant Storage_Index := Get_Storage_Size (Obj_Type);
               Obj : Storage_Type (0 .. Ssize - 1);
               Enum : Storage_Type (0 .. Ssize - 1);
               Name : Node;
               Eq : Boolean;
            begin
               Execute_Expression (Frame, Obj'Address, Obj_Expr);
               Name := Get_Enum_Names (Obj_Type);
               while Name /= Null_Node loop
                  pragma Assert (Get_Kind (Name) = N_Enum_Name);
                  Execute_Expression
                    (null, Enum'Address, Get_Expression (Name));
                  case Get_Kind (Get_Enum_Base_Type (Obj_Type)) is
                     when N_Log_Packed_Array_Cst =>
                        Eq := Is_Eq (To_Logvec_Ptr (Obj'Address),
                                     To_Logvec_Ptr (Enum'Address),
                                     Get_Type_Width (Base_Type));
                     when N_Bit_Packed_Array_Cst =>
                        Eq := Is_Eq (To_Bitvec_Ptr (Obj'Address),
                                     To_Bitvec_Ptr (Enum'Address),
                                     Get_Type_Width (Base_Type));
                     when others =>
                        raise Internal_Error;
                  end case;
                  if Eq then
                     To_Sv_String_Ptr (Dest).all :=
                       New_Sv_String (Get_Identifier (Name));
                     return;
                  end if;
                  Name := Get_Chain (Name);
               end loop;
               To_Sv_String_Ptr (Dest).all := Empty_Sv_String;
               return;
            end;
         when String_Toupper_Method =>
            declare
               Sv_Str : Sv_String;
               Res : Sv_String;
               Len : Natural;
               C : Character;
            begin
               Execute_Expression (Frame, Sv_Str'Address, Obj_Expr);
               Len := Get_Length (Sv_Str);
               if Len = 0 then
                  To_Sv_String_Ptr (Dest).all := Sv_Str;
                  return;
               end if;

               Res := New_Sv_String (Len);
               for I in 1 .. Len loop
                  C := Get_String_El (Sv_Str, I);
                  --  TODO: which charset ?
                  if C in 'a' .. 'z' then
                     C := Character'Val (Character'Pos (C) - 32);
                  end if;
                  Set_String_El (Res, I, C);
               end loop;

               Unref (Sv_Str);

               To_Sv_String_Ptr (Dest).all := Res;
               return;
            end;
         when Dynamic_Size_Method =>
            declare
               Sv_Dyn : Sv_Dyn_Array_Ptr;
               Sz : Int32;
            begin
               Execute_Expression (Frame, Sv_Dyn'Address, Obj_Expr);
               if Sv_Dyn = null then
                  Sz := 0;
               else
                  Sz := Sv_Dyn.Size;
               end if;
               To_Bitvec_Ptr (Dest).all (0) := Uns32 (Sz);
            end;
         when Dynamic_Delete_Method =>
            declare
               Obj : Data_Ptr;
               Update : Update_Acc;
            begin
               Execute_Name_Nonvec (Frame, Obj_Expr, False, Obj, Update);
               pragma Assert (Update = null);
               Delete (To_Sv_Dyn_Array_Ptr_Ptr (Obj).all);
            end;
         when Queue_Size_Method =>
            declare
               Sv_Q : Sv_Queue;
            begin
               Execute_Expression (Frame, Sv_Q'Address, Obj_Expr);
               To_Bitvec_Ptr (Dest).all (0) := Queue_Size (Sv_Q);
            end;
         when Queue_Push_Back_Method =>
            declare
               Obj_Type : constant Node := Get_Expr_Type (Obj_Expr);
               El_Type : constant Node := Get_Type_Element_Type (Obj_Type);
               Args : constant Node := Get_Arguments (Expr);
               Ssize : constant Storage_Index := Get_Storage_Size (El_Type);
               Val : Storage_Type (0 .. Ssize - 1);
               Arg : Node;
               Dest : Data_Ptr;
               Sv_Q : Sv_Queue;
            begin
               Execute_Expression (Frame, Sv_Q'Address, Obj_Expr);
               Dest := Queue_Push_Back (Sv_Q);
               Arg := Get_Expression (Args);
               Execute_Expression (Frame, Val'Address, Arg);
               Execute_Simple_Copy (Dest, Val'Address, El_Type);
            end;
         when Associative_Delete_Method =>
            null;
         when Associative_Exists_Method =>
            --  Result is an int.
            To_Bitvec_Ptr (Dest)(0) := 0;
            return;
         when others =>
            --  Builtin method not implemented.
            raise Internal_Error;
      end case;
   end Execute_Builtin_Method_Call;

   procedure Execute_Declarations (Frame : Frame_Ptr; Decls : Node)
   is
      Decl : Node;
   begin
      Decl := Decls;
      while Decl /= Null_Node loop
         case Get_Kind (Decl) is
            when N_Var =>
               if Get_Is_Automatic (Decl) then
                  Init_Var (Frame, Decl);
               end if;
            when Nkinds_Tf_Port
              | N_Typedef =>
               --  Already handled.
               null;
            when others =>
               Error_Kind ("execute_declarations", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Execute_Declarations;

   procedure Finalize_Data (Dest : Data_Ptr; Atype : Node) is
   begin
      case Get_Kind (Atype) is
         when N_String_Type =>
            Unref (To_Sv_String_Ptr (Dest).all);
         when Nkinds_Class
           | N_Queue_Cst =>
            --  TODO.
            null;
         when N_Bit_Packed_Array_Cst
           | N_Log_Packed_Array_Cst
           | N_Bit_Type
           | N_Logic_Type =>
            null;
         when others =>
            Error_Kind ("finalize_data", Atype);
      end case;
   end Finalize_Data;

   procedure Finalize_Variable (Frame : Frame_Ptr; Decl : Node)
   is
      Decl_Type : constant Node := Get_Type_Data_Type (Decl);
      Data : Data_Ptr;
   begin
      case Get_Kind (Decl_Type) is
         when N_String_Type
           | Nkinds_Class
           | N_Queue_Cst =>
            Data := Get_Var_Data (Frame, Decl);
            Finalize_Data (Data, Decl_Type);
         when N_Bit_Packed_Array_Cst
           | N_Log_Packed_Array_Cst
           | N_Bit_Type
           | N_Logic_Type =>
            null;
         when others =>
            Error_Kind ("finalize_variable", Decl_Type);
      end case;
   end Finalize_Variable;

   procedure Finalize_Declarations (Frame : Frame_Ptr; Decls : Node)
   is
      Decl : Node;
   begin
      Decl := Decls;
      while Decl /= Null_Node loop
         case Get_Kind (Decl) is
            when N_Var =>
               if Get_Is_Automatic (Decl) then
                  Finalize_Variable (Frame, Decl);
               end if;
            when Nkinds_Tf_Port
              | N_Typedef =>
               --  Already handled.
               null;
            when others =>
               Error_Kind ("finalize_declarations", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Finalize_Declarations;

   --  Copy SRC to DEST.  DEST must be standalone (not a part of a vector).
   procedure Execute_Simple_Copy
     (Dest : Data_Ptr; Src : Data_Ptr; Etype : Node) is
   begin
      case Nkinds_Types (Get_Kind (Etype)) is
         when N_Logic_Type =>
            To_Logic_Ptr (Dest).all := To_Logic_Ptr (Src).all;
         when N_Bit_Type =>
            To_Bit_Ptr (Dest).all := To_Bit_Ptr (Src).all;
         when N_Shortreal_Type =>
            To_Fp32_Ptr (Dest).all := To_Fp32_Ptr (Src).all;
         when N_Real_Type =>
            To_Fp64_Ptr (Dest).all := To_Fp64_Ptr (Src).all;
         when N_Log_Packed_Array_Cst =>
            declare
               Width : constant Width_Type := Get_Type_Width (Etype);
            begin
               Assign (To_Logvec_Ptr (Dest), To_Logvec_Ptr (Src), Width);
            end;
         when N_Bit_Packed_Array_Cst =>
            declare
               Width : constant Width_Type := Get_Type_Width (Etype);
            begin
               Assign (To_Bitvec_Ptr (Dest), To_Bitvec_Ptr (Src), Width);
            end;
         when N_Array_Cst
           | N_Struct_Type =>
            declare
               Ssize : constant Storage_Index := Get_Storage_Size (Etype);
            begin
               To_Storage (Dest) (0 .. Ssize - 1) :=
                 To_Storage (Src) (0 .. Ssize - 1);
            end;
         when N_Class
           | N_Instantiated_Class =>
            To_Sv_Class_Ptr (Dest).all := To_Sv_Class_Ptr (Src).all;
         when N_Enum_Type =>
            Execute_Simple_Copy (Dest, Src, Get_Enum_Base_Type (Etype));
         when N_Packed_Struct_Type =>
            Execute_Simple_Copy (Dest, Src, Get_Packed_Base_Type (Etype));
         when N_String_Type =>
            To_Sv_String_Ptr (Dest).all := To_Sv_String_Ptr (Src).all;
         when N_Dynamic_Array_Cst =>
            To_Sv_Dyn_Array_Ptr_Ptr (Dest).all :=
              To_Sv_Dyn_Array_Ptr_Ptr (Src).all;
         when N_Queue_Cst =>
            To_Sv_Queue_Ptr (Dest).all := To_Sv_Queue_Ptr (Src).all;
         when others =>
            Error_Kind ("execute_simple_copy", Etype);
      end case;
   end Execute_Simple_Copy;

   --  INNER_FRAME is the frame of the subroutine (where arguments will be
   --   stored).
   --  OUTER_FRAME is the frame of the caller (used to evaluate expressions)
   --  HANDLE is the frame of the object, in case of default expressions.
   procedure Execute_Subroutine_Arguments (Inner_Frame : Frame_Ptr;
                                           Outer_Frame : Frame_Ptr;
                                           Handle : Sv_Class_Handle;
                                           Args : Node)
   is
      Arg : Node;
   begin
      --  Evaluate arguments.
      Arg := Args;
      while Arg /= Null_Node loop
         declare
            Port : constant Node := Get_Port (Arg);
            Port_Value : Data_Ptr;
            Expr : Node;
            Expr_Frame : Frame_Ptr;
         begin
            Port_Value := Get_Var_Data (Inner_Frame, Port);
            Expr := Get_Expression (Arg);
            if Expr = Null_Node then
               Expr := Get_Default_Value (Port);
               --  1800-2017 13.5.3 Default argument values
               --  The default_expression is evaluated in the scope containing
               --  the subroutine declaration each time a call using the
               --  default is made.
               if Handle = null then
                  Expr_Frame := Global_Frame;
               else
                  Expr_Frame := To_Frame_Ptr (Handle);
               end if;
               Execute_Expression (Expr_Frame, Port_Value, Expr);
            else
               --  Evaluate argument and assign it to the parameter.
               Execute_Expression (Outer_Frame, Port_Value, Expr);
            end if;
         end;
         Arg := Get_Chain (Arg);
      end loop;
   end Execute_Subroutine_Arguments;

   procedure Allocate_Subroutine_Frame (Subprg : Node;
                                        Link : out Frame_Link_Type)
   is
      Inner_Frame : Frame_Ptr;
   begin
      Inner_Frame := Allocate_Frame (Subprg);

      --  Previous value.
      To_Frame_Link_Ptr (Inner_Frame).all := (Subprg, Null_Node, null);

      Link := (Origin => Subprg,
               Pc => Get_Statements_Chain (Subprg),
               Frame => Inner_Frame);
   end Allocate_Subroutine_Frame;

   procedure Prepare_Call (Outer_Frame : Frame_Ptr;
                           Call : Node;
                           Subprg : Node;
                           Handle : Sv_Class_Handle;
                           Link : out Frame_Link_Type)
   is
      Var_This : constant Node := Get_This_Variable (Subprg);
      Ret_Var : Node;
      Inner_Frame : Frame_Ptr;
   begin
      Allocate_Subroutine_Frame (Subprg, Link);
      Inner_Frame := Link.Frame;
      if Get_Kind (Subprg) = N_Function
        and then Get_Lifetime (Subprg) = Life_Automatic
      then
         Ret_Var := Get_Return_Variable (Subprg);
         --  No return variable for void functions.
         if Ret_Var /= Null_Node then
            Clear_Var (Inner_Frame, Ret_Var, Get_Expr_Type (Ret_Var));
         end if;
      end if;

      if Var_This /= Null_Node then
         --  Set 'this' parameter.
         declare
            Value : Data_Ptr;
         begin
            Value := Get_Var_Data (Inner_Frame, Var_This);
            To_Sv_Class_Ptr (Value).all := Handle;
         end;
      else
         pragma Assert (Handle = null);
         null;
      end if;

      --  Evaluate arguments.
      Execute_Subroutine_Arguments
        (Inner_Frame, Outer_Frame, Handle, Get_Arguments (Call));

      Execute_Declarations
        (Inner_Frame, Get_Tf_Item_Declaration_Chain (Subprg));
   end Prepare_Call;

   procedure Execute_Function_Call (Outer_Frame : Frame_Ptr;
                                    Dest : Data_Ptr;
                                    Handle : Sv_Class_Handle;
                                    Expr : Node;
                                    Subprg : Node)
   is
      Func_Type : constant Node := Get_Type_Data_Type (Subprg);
      Inner_Frame : Frame_Ptr;
      Link : Frame_Link_Type;
      Src : Data_Ptr;
   begin
      Prepare_Call (Outer_Frame, Expr, Subprg, Handle, Link);
      Inner_Frame := Link.Frame;

      Execute_Statements (Link, null);

      --  Result.
      if Func_Type /= Void_Type then
         Src := Get_Var_Data (Inner_Frame, Get_Return_Variable (Subprg));
         Execute_Simple_Copy (Dest, Src, Func_Type);
      end if;

      Finalize_Declarations
        (Inner_Frame, Get_Tf_Item_Declaration_Chain (Subprg));

      Deallocate_Frame (Inner_Frame);
   end Execute_Function_Call;

   procedure Init_Class_Object (Frame : Frame_Ptr;
                                Cls : Node;
                                Obj : Frame_Ptr;
                                Call_Constructor : Boolean;
                                Args : Node)
   is
      Parent : constant Node := Get_Type_Base_Class_Type (Cls);
      Constructor : constant Node := Get_Class_Constructor (Cls);
      Call_Sub_Constructor : Boolean;
   begin
      --  Base class
      --  TODO: only if no call to super-constructor!
      if Parent /= Null_Node then
         pragma Assert (Get_Kind (Parent) in Nkinds_Class);
         --  Call the constructor in the parent class if either there is no
         --  constructor in this class, or if the constructor doesn't call
         --  super.new().
         Call_Sub_Constructor := Constructor = Null_Node
           or else (not Sem_Utils.Is_Call_To_Super_New
                      (Get_Statements_Chain (Constructor)));
         Init_Class_Object
           (Null_Frame, Parent, Obj, Call_Sub_Constructor, Null_Node);
      end if;
      --  Properties
      Init_Class_Scope (Cls, Obj);
      if Call_Constructor and then Constructor /= Null_Node then
         declare
            Var_This : constant Node := Get_This_Variable (Constructor);
            Handle : constant Sv_Class_Handle := To_Sv_Class_Handle (Obj);
            Link : Frame_Link_Type;
            Constr_Frame : Frame_Ptr;
            Value : Data_Ptr;
         begin
            Allocate_Subroutine_Frame (Constructor, Link);
            Constr_Frame := Link.Frame;

            --  Set 'this' parameter.
            Value := Get_Var_Data (Constr_Frame, Var_This);
            To_Sv_Class_Ptr (Value).all := Handle;

            if Args /= Null_Node then
               Execute_Subroutine_Arguments
                 (Constr_Frame, Frame, Handle, Args);
            else
               --  No arguments (in implicit call), try default ones.
               declare
                  Port : Node;
                  Port_Value : Data_Ptr;
                  Expr : Node;
               begin
                  Port := Get_Tf_Ports_Chain (Constructor);
                  while Port /= Null_Node loop
                     Port_Value := Get_Var_Data (Constr_Frame, Port);
                     Expr := Get_Default_Value (Port);
                     pragma Assert (Expr /= Null_Node);
                     Execute_Expression (Constr_Frame, Port_Value, Expr);
                     Port := Get_Chain (Port);
                  end loop;
               end;
            end if;
            Execute_Declarations
              (Constr_Frame, Get_Tf_Item_Declaration_Chain (Constructor));

            Execute_Statements (Link, null);

            Deallocate_Frame (Constr_Frame);
         end;
      end if;
   end Init_Class_Object;

   function Allocate_Class_Object (Frame : Frame_Ptr; Cls : Node; Args : Node)
                                  return Sv_Class_Handle
   is
      Obj : Frame_Ptr;
   begin
      Obj := Allocate_Frame (Cls);
      To_Sv_Class_Handle (Obj).all :=
        (Orig => Cls, Refcnt => 1, Vtable => Null_Address);
      Init_Class_Object (Frame, Cls, Obj, True, Args);
      return To_Sv_Class_Handle (Obj);
   end Allocate_Class_Object;

   function Is_Vector_Name (Name : Node; Ntype : Node) return Boolean is
   begin
      case Nkinds_Types (Get_Kind (Ntype)) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Packed_Struct_Type
           | N_Packed_Union_Type =>
            return True;
         when N_Enum_Type =>
            --  The base type is always a vector type.
            return True;
         when N_Bit_Type
           | N_Logic_Type =>
            case Get_Kind (Name) is
               when N_Name
                 | N_Hierarchical
                 | N_Interface_Item
                 | N_Var
                 | N_Return_Var
                 | Nkinds_Nets
                 | Nkinds_Tf_Port
                 | N_Parameter
                 | N_Localparam
                 | N_This_Name
                 | N_Property_Name
                 | N_Associative_Index =>
                  --  Object declared as a bit/logic.
                  return False;
               when N_Indexed_Name =>
                  --  An unpacked array of bit/logic.
                  pragma Assert
                    (Get_Kind (Get_Expr_Type (Get_Name (Name))) = N_Array_Cst);
                  return False;
               when N_Bit_Select
                 | N_Member_Name =>
                  --  Within an object.
                  return True;
               when others =>
                  --  To be completed.
                  Error_Kind ("is_vector_name(bit/logic)", Name);
            end case;
         when N_Array_Cst
           | N_String_Type
           | N_Struct_Type
           | N_Union_Type
           | N_Real_Type
           | N_Shortreal_Type
           | N_Queue_Cst
           | N_Dynamic_Array_Cst
           | N_Associative_Array_Cst
           | N_Chandle_Type
           | N_Event_Type
           | N_Nature
           | N_Class
           | N_Instantiated_Class
           | N_Class_Instance
           | N_Virtual_Interface
           | N_Void_Type =>
            return False;
         when N_Array
           | N_Packed_Array
           | N_Queue
           | N_Dynamic_Array
           | N_Associative_Array
           | N_Error_Type
           | N_Null_Type =>
            raise Internal_Error;
      end case;
   end Is_Vector_Name;

   procedure Execute_Vector_Name_To_Expression (Dest : Data_Ptr;
                                                Name : Node;
                                                Name_Type : Node;
                                                Name_Data : Data_Ptr;
                                                Name_Off : Bit_Offset;
                                                Name_Doff : Bit_Offset;
                                                Name_Width : Width_Type) is
   begin
      case Nkinds_Types (Get_Kind (Name_Type)) is
         when N_Logic_Type =>
            if Name_Width = 0 then
               To_Logic_Ptr (Dest).all := V_X;
            else
               pragma Assert (Name_Doff = 0);
               pragma Assert (Name_Width = 1);
               To_Logic_Ptr (Dest).all := Execute_Bit_Select_Log_Expression
                 (To_Logvec_Ptr (Name_Data), Name_Width, Name_Off);
               end if;
         when N_Bit_Type =>
            if Name_Width = 0 then
               To_Bit_Ptr (Dest).all := B_0;
            else
               pragma Assert (Name_Width = 1);
               pragma Assert (Name_Doff = 0);
               To_Bit_Ptr (Dest).all := Execute_Bit_Select_Bit_Expression
                    (To_Bitvec_Ptr (Name_Data), Name_Width, Name_Off);
            end if;
         when N_Log_Packed_Array_Cst =>
            if Name_Width = 0 then
               Set_X (To_Logvec_Ptr (Dest), Name_Width);
            elsif Name_Off = 0 and then Get_Type_Width (Name_Type) = Name_Width
            then
               pragma Assert (Name_Doff = 0);
               Assign (To_Logvec_Ptr (Dest),
                       To_Logvec_Ptr (Name_Data), Name_Width);
            else
               Compute_Part_Extract
                 (To_Logvec_Ptr (Dest), Name_Doff, Get_Type_Width (Name_Type),
                  To_Logvec_Ptr (Name_Data), Name_Off, Name_Width);
            end if;
         when N_Bit_Packed_Array_Cst =>
            if Name_Width = 0 then
               raise Internal_Error;
            elsif Name_Off = 0 and then Get_Type_Width (Name_Type) = Name_Width
            then
               pragma Assert (Name_Doff = 0);
               Assign (To_Bitvec_Ptr (Dest),
                       To_Bitvec_Ptr (Name_Data), Name_Width);
            else
               Compute_Bit_Part_Extract
                 (To_Bitvec_Ptr (Dest), Name_Doff, Get_Type_Width (Name_Type),
                  To_Bitvec_Ptr (Name_Data), Name_Off, Name_Width);
            end if;
         when N_Enum_Type =>
            Execute_Vector_Name_To_Expression
              (Dest, Name, Get_Enum_Base_Type (Name_Type),
               Name_Data, Name_Off, Name_Doff, Name_Width);
         when N_Packed_Struct_Type =>
            Execute_Vector_Name_To_Expression
              (Dest, Name, Get_Packed_Base_Type (Name_Type),
               Name_Data, Name_Off, Name_Doff, Name_Width);
         when others =>
            Error_Kind ("execute_vector_name_to_expression", Name_Type);
      end case;
   end Execute_Vector_Name_To_Expression;

   procedure Execute_Nonvec_Name_To_Expression (Dest : Data_Ptr;
                                                Name : Node;
                                                Name_Type : Node;
                                                Name_Data : Data_Ptr) is
   begin
      case Nkinds_Types (Get_Kind (Name_Type)) is
         when N_Logic_Type =>
            if Name_Data = No_Data_Ptr then
               To_Logic_Ptr (Dest).all := V_X;
            else
               To_Logic_Ptr (Dest).all := To_Logic_Ptr (Name_Data).all;
            end if;
         when N_Bit_Type =>
            To_Bit_Ptr (Dest).all := To_Bit_Ptr (Name_Data).all;
         when N_Shortreal_Type
           | N_Real_Type
           | N_Class
           | N_Instantiated_Class
           | N_Array_Cst
           | N_Struct_Type
           | N_Dynamic_Array_Cst
           | N_Queue_Cst =>
            Execute_Simple_Copy (Dest, Name_Data, Name_Type);
         when N_String_Type =>
            declare
               Str : Sv_String;
            begin
               Str := To_Sv_String_Ptr (Name_Data).all;
               Ref (Str);
               To_Sv_String_Ptr (Dest).all := Str;
            end;
         when N_Enum_Type =>
            Execute_Nonvec_Name_To_Expression
              (Dest, Name, Get_Enum_Base_Type (Name_Type), Name_Data);
         when N_Packed_Struct_Type =>
            Execute_Nonvec_Name_To_Expression
              (Dest, Name, Get_Packed_Base_Type (Name_Type), Name_Data);
         when others =>
            Error_Kind ("execute_nonvec_name_to_expression", Name_Type);
      end case;
   end Execute_Nonvec_Name_To_Expression;

   procedure Execute_Name_To_Expression
     (Frame : Frame_Ptr; Dest : Data_Ptr; Name : Node; Name_Type : Node)
   is
      Data : Data_Ptr;
      Offset : Bit_Offset;
      Doffset : Bit_Offset;
      Width : Width_Type;
      Update : Update_Acc;
   begin
      if Is_Vector_Name (Name, Name_Type) then
         Execute_Name_Vector
           (Frame, Name, False, Data, Offset, Doffset, Width, Update);
         Execute_Vector_Name_To_Expression
           (Dest, Name, Name_Type, Data, Offset, Doffset, Width);
      else
         Execute_Name_Nonvec (Frame, Name, False, Data, Update);
         Execute_Nonvec_Name_To_Expression
           (Dest, Name, Name_Type, Data);
      end if;
   end Execute_Name_To_Expression;

   function Execute_Condition (Val : Data_Ptr; Expr : Node)
                              return Tri_State_Type
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
   begin
      case Get_Kind (Expr_Type) is
         when N_Logic_Type =>
            declare
               V : constant Logic_Type := To_Logic_Ptr (Val).all;
               pragma Assert (V'Valid);
            begin
               case V is
                  when V_0 => return False;
                  when V_1 => return True;
                  when V_X | V_Z => return Unknown;
               end case;
            end;
         when N_Bit_Type =>
            declare
               V : constant Bit_Type := To_Bit_Ptr (Val).all;
               pragma Assert (V'Valid);
            begin
               case V is
                  when B_0 => return False;
                  when B_1 => return True;
               end case;
            end;
         when N_Log_Packed_Array_Cst =>
            return Compute_Predicate
              (To_Logvec_Ptr (Val), Get_Type_Width (Expr_Type));
         when N_Bit_Packed_Array_Cst =>
            return Compute_Predicate
              (To_Bitvec_Ptr (Val), Get_Type_Width (Expr_Type));
         when others =>
            Error_Kind ("execute_condition", Expr_Type);
      end case;
   end Execute_Condition;

   function Execute_Condition
     (Frame : Frame_Ptr; Expr : Node) return Tri_State_Type
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Res : Storage_Type (0 .. Ssize - 1);
   begin
      Execute_Expression (Frame, Res'Address, Expr);
      return Execute_Condition (Res'Address, Expr);
   end Execute_Condition;

   procedure Execute_Conditional_Operator
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Res : Tri_State_Type;
   begin
      --  1800-2017 11.4.11 Conditional operator
      --  If cond_predicate is true, the operator returns the value of the
      --  first expression without evaluating the second expression; if false,
      --  it returns the value of the second expression without evaluating the
      --  first expression.
      Res := Execute_Condition (Frame, Get_Condition (Expr));

      case Res is
         when True =>
            Execute_Expression (Frame, Dest, Get_Cond_True (Expr));
            return;
         when False =>
            Execute_Expression (Frame, Dest, Get_Cond_False (Expr));
            return;
         when Unknown =>
            null;
      end case;

      --  1800-2017 11.4.11 Conditional operator
      --  If cond_predicate evaluates to an ambiguous value (x or z), then
      --  both the first expression and the second expression shall be
      --  evaluated.
      declare
         Texpr : constant Node := Get_Cond_True (Expr);
         Fexpr : constant Node := Get_Cond_False (Expr);
         Expr_Type : constant Node := Get_Expr_Type (Expr);
         Esize : constant Storage_Index := Get_Storage_Size (Expr_Type);
         pragma Assert (Get_Storage_Size (Get_Expr_Type (Texpr)) = Esize);
         pragma Assert (Get_Storage_Size (Get_Expr_Type (Fexpr)) = Esize);
         Tres : Storage_Type (0 .. Esize - 1);
         Fres : Storage_Type (0 .. Esize - 1);
      begin
         Execute_Expression (Frame, Tres'Address, Texpr);
         Execute_Expression (Frame, Fres'Address, Fexpr);

         case Get_Kind (Expr_Type) is
            when N_Log_Packed_Array_Cst =>
               Compute_Conditional_Mixed_Lv (To_Logvec_Ptr (Dest),
                                             To_Logvec_Ptr (Tres'Address),
                                             To_Logvec_Ptr (Fres'Address),
                                             Get_Type_Width (Expr_Type));
            when N_Logic_Type =>
               Compute_Conditional_Mixed_Log (To_Logic_Ptr (Dest),
                                              To_Logic_Ptr (Tres'Address).all,
                                              To_Logic_Ptr (Fres'Address).all);
            when others =>
               Error_Kind ("execute_conditional_operator", Expr_Type);
         end case;
      end;
   end Execute_Conditional_Operator;

   procedure Execute_Set_Logical
     (Dest : Data_Ptr; Expr : Node; Val : Tri_State_Type) is
   begin
      case Get_Kind (Get_Expr_Type (Expr)) is
         when N_Logic_Type =>
            declare
               Res : Logic_Type;
            begin
               case Val is
                  when True =>
                     Res := V_1;
                  when False =>
                     Res := V_0;
                  when Unknown =>
                     Res := V_X;
               end case;
               To_Logic_Ptr (Dest).all := Res;
            end;
         when others =>
            Error_Kind ("execute_set_logical", Get_Expr_Type (Expr));
      end case;
   end Execute_Set_Logical;

   procedure Execute_String_Literal (Dest : Data_Ptr; Expr : Node)
   is
      Etype : constant Node := Get_Expr_Type (Expr);
   begin
      case Get_Kind (Etype) is
         when N_String_Type =>
            declare
               Id : Lit_Id;
               Res : Sv_String;
            begin
               Id := Get_Lit_Id (Expr);
               if Id = No_Lit_Id then
                  --  create one.
                  Res := New_Sv_String (Get_String_Id (Expr),
                                        Natural (Get_String_Size (Expr)));
                  Lits.Append (Lit_Type'(Kind => Lit_String, Str => Res));
                  Set_Lit_Id (Expr, Lits.Last);
               else
                  Res := Lits.Table (Id).Str;
               end if;
               Ref (Res);
               To_Sv_String_Ptr (Dest).all := Res;
            end;
         when N_Log_Packed_Array_Cst =>
            declare
               Str_Id : constant String8_Id := Get_String_Id (Expr);
               Str_Sz : constant Nat32 := Nat32 (Get_String_Size (Expr));
               Width : constant Width_Type := Get_Type_Width (Etype);
               R : constant Logvec_Ptr := To_Logvec_Ptr (Dest);
               L : constant Nat32 := Nat32'Min (Str_Sz,
                                                Nat32 (Width + 7) / 8);
               Didx : Digit_Index;
               C : Nat8;
            begin
               Set_0 (R, Width);
               for I in 0 .. L - 1 loop
                  C := Str_Table.Element_String8 (Str_Id, Str_Sz - I);
                  Didx := Digit_Index (I / 4);
                  R (Didx).Val :=
                    R (Didx).Val or Shift_Left (Uns32 (C),
                                                Natural ((I mod 4) * 8));
               end loop;
            end;
         when others =>
            --  TODO.
            raise Internal_Error;
      end case;
   end Execute_String_Literal;

   procedure Execute_Real_Number (Dest : Data_Ptr; Expr : Node) is
   begin
      To_Fp64_Ptr (Dest).all := Get_Real_Number (Expr);
   end Execute_Real_Number;

   procedure Execute_Increment (Dest : Data_Ptr; Src : Data_Ptr; Expr : Node)
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Wd : constant Width_Type := Get_Type_Width (Expr_Type);
   begin
      case Get_Kind (Expr_Type) is
         when N_Log_Packed_Array_Cst =>
            Compute_Inc (To_Logvec_Ptr (Dest), To_Logvec_Ptr (Src), Wd);
         when N_Bit_Packed_Array_Cst =>
            Compute_Inc (To_Bitvec_Ptr (Dest), To_Bitvec_Ptr (Src), Wd);
         when others =>
            Error_Kind ("execute_increment", Expr_Type);
      end case;
   end Execute_Increment;

   procedure Execute_Pre_Increment
     (Frame : Frame_Ptr; Data : Data_Ptr; Expr : Node)
   is
      Lval : constant Node := Get_Lvalue (Expr);
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Res : Storage_Type (0 .. Ssize - 1);
   begin
      Execute_Expression (Frame, Res'Address, Lval);
      Execute_Increment (Data, Res'Address, Expr);
      --  FIXME: do not evaluate Lval twice!
      Blocking_Assign_Lvalue (Frame, Lval, Data, Expr_Type);
   end Execute_Pre_Increment;

   procedure Execute_Post_Increment
     (Frame : Frame_Ptr; Data : Data_Ptr; Expr : Node)
   is
      Lval : constant Node := Get_Lvalue (Expr);
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Res : Storage_Type (0 .. Ssize - 1);
   begin
      Execute_Expression (Frame, Data, Lval);
      Execute_Increment (Res'Address, Data, Expr);
      --  FIXME: do not evaluate Lval twice!
      Blocking_Assign_Lvalue (Frame, Lval, Res'Address, Expr_Type);
   end Execute_Post_Increment;

   procedure Execute_Assign_Operator (Frame : Frame_Ptr; Stmt : Node)
   is
      Name : constant Node := Get_Lvalue (Stmt);
      Name_Type : constant Node := Get_Expr_Type (Name);
      Name_Ssize : constant Storage_Index := Get_Storage_Size (Name_Type);
      Expr : constant Node := Get_Expression (Stmt);
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Expr_Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Res : Storage_Type (0 .. Name_Ssize - 1);
      Val : Storage_Type (0 .. Expr_Ssize - 1);
      Upd : Update_Acc;
      Lv_Data : Data_Ptr;
      Lv_Noff : Bit_Offset;
      Lv_Doff : Bit_Offset;
      Lv_Wd : Width_Type;
   begin
      if Is_Vector_Name (Name, Name_Type) then
         Execute_Name_Vector
           (Frame, Name, False, Lv_Data, Lv_Noff, Lv_Doff, Lv_Wd, Upd);
         pragma Assert (Lv_Noff = 0);
         pragma Assert (Lv_Doff = 0);
         Execute_Vector_Name_To_Expression
           (Res'Address, Name, Expr_Type, Lv_Data, Lv_Noff, Lv_Doff, Lv_Wd);
         Execute_Expression (Frame, Val'Address, Get_Expression (Stmt));
         case Get_Kind (Expr_Type) is
            when N_Log_Packed_Array_Cst =>
               Execute_Binary_Lv_Expression
                 (Res'Address, Stmt,
                  To_Logvec_Ptr (Res'Address), Get_Type_Width (Name_Type),
                  To_Logvec_Ptr (Val'Address), Get_Type_Width (Expr_Type));
            when N_Bit_Packed_Array_Cst =>
               Execute_Binary_Bv_Expression
                 (Res'Address, Stmt,
                  To_Bitvec_Ptr (Res'Address), Get_Type_Width (Name_Type),
                  To_Bitvec_Ptr (Val'Address), Get_Type_Width (Expr_Type));
            when others =>
               Error_Kind ("execute_assign_operator(vector)", Expr_Type);
         end case;
         Assign_Vector
           (Lv_Data, Lv_Doff, Lv_Wd, Name_Type, Upd, Res'Address, Lv_Noff);
      else
         --  TODO.
         raise Internal_Error;
      end if;
      null;
   end Execute_Assign_Operator;

   function Create_Dynamic_Array (Arr_Type : Node; Size : Int32)
                                 return Sv_Dyn_Array_Ptr
   is
      Stride : constant Storage_Index :=
        Storage_Index (Get_Stride_Size (Arr_Type));
      Res : Sv_Dyn_Array_Ptr;
   begin
      if Size = 0 then
         Res := null;
      else
         Res := new Sv_Dyn_Array_Type (Stride * Storage_Index (Size));
         Res.Size := Size;
      end if;
      return Res;
   end Create_Dynamic_Array;

   procedure Execute_Dynamic_Array_New
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Etype : constant Node := Get_Expr_Type (Expr);
      El_Type : constant Node := Get_Type_Element_Type (Etype);
      Init_Expr : constant Node := Get_Init_Expression (Expr);
      Ptr : constant Sv_Dyn_Array_Ptr_Ptr := To_Sv_Dyn_Array_Ptr_Ptr (Dest);
      Stride : constant Storage_Index :=
        Storage_Index (Get_Stride_Size (Etype));
      Res : Sv_Dyn_Array_Ptr;
      Size : Int32;
      Is_Undef : Boolean;
      Istart : Storage_Index;
   begin
      Execute_Expression_Int32 (Frame, Get_Size_Expression (Expr),
                                Size, Is_Undef);
      if Is_Undef or else Size < 0 then
         --  1800-2017 7.5.1 new[]
         --  It shall be an error if the value of this operand is negative.
         --  TODO
         raise Constraint_Error;
      end if;

      Res := Create_Dynamic_Array (Etype, Size);

      if Init_Expr /= Null_Node then
         declare
            Init_Type : constant Node := Get_Expr_Type (Init_Expr);
            Isize : constant Storage_Index := Get_Storage_Size (Init_Type);
            Init : Storage_Type (0 .. Isize - 1);
            Ibase : Data_Ptr;
            Ilen : Int32;
         begin
            Execute_Expression (Frame, Init'Address, Init_Expr);

            case Get_Kind (Init_Type) is
               when N_Array_Cst =>
                  Ilen := Sem_Utils.Compute_Length (Init_Type);
                  Ibase := Init'Address;
               when N_Dynamic_Array_Cst =>
                  declare
                     Darr : constant Sv_Dyn_Array_Ptr :=
                       To_Sv_Dyn_Array_Ptr_Ptr (Init'Address).all;
                  begin
                     if Darr = null then
                        Ilen := 0;
                        Ibase := Null_Address;
                     else
                        Ilen := Darr.Size;
                        Ibase := Darr.Base(1)'Address;
                     end if;
                  end;
               when others =>
                  Error_Kind ("execute_dynamic_array_new", Init_Type);
            end case;
            if Ilen > Size then
               Ilen := Size;
            end if;
            if Ilen > 0 then
               Res.Base (1 .. Storage_Index (Ilen) * Stride) :=
                 To_Storage (Ibase).all (0 ..
                                         Storage_Index (Ilen) * Stride - 1);
            end if;
            Istart := Storage_Index (Ilen + 1);
         end;
      else
         Istart := 0;
      end if;

      if Istart < Storage_Index (Size) then
         --  Initialize.
         Init (Res.Base (1 + Istart * Stride)'Address, El_Type);
         for I in Istart + 1 .. Storage_Index (Size - 1) loop
            Res.Base (I * Stride + 1 .. (I + 1) * Stride) :=
              Res.Base (1 + Istart * Stride .. (Istart + 1) * Stride);
         end loop;
      end if;

      Ptr.all := Res;
   end Execute_Dynamic_Array_New;

   procedure Execute_Cast_From_Real
     (Dest : Data_Ptr; Dest_Type : Node; Arg : Fp64) is
   begin
      case Get_Kind (Dest_Type) is
         when N_Shortreal_Type =>
            To_Fp32_Ptr (Dest).all := Fp32 (Arg);
         when others =>
            Error_Kind ("execute_case_from_real", Dest_Type);
      end case;
   end Execute_Cast_From_Real;

   procedure Execute_Cast
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Arg : constant Node := Get_Expression (Expr);
      Arg_Type : constant Node := Get_Expr_Type (Arg);
   begin
      case Get_Kind (Arg_Type) is
         when N_Real_Type =>
            declare
               Res : Fp64;
            begin
               Execute_Expression (Frame, Res'Address, Arg);
               Execute_Cast_From_Real (Dest, Get_Expr_Type (Expr), Res);
            end;
         when others =>
            Error_Kind ("execute_cast", Arg_Type);
      end case;
   end Execute_Cast;

   procedure Execute_Unary_Expression
     (Expr : Node; Op : Data_Ptr; Res : Data_Ptr)
   is
      Operand : constant Node := Get_Expression (Expr);
      Op_Type : constant Node := Get_Expr_Type (Operand);
   begin
      case Get_Kind (Op_Type) is
         when N_Log_Packed_Array_Cst =>
            Execute_Unary_Lv_Expression
              (Res, Expr, To_Logvec_Ptr (Op), Get_Type_Width (Op_Type));
         when N_Bit_Packed_Array_Cst =>
            Execute_Unary_Bv_Expression
              (Res, Expr, To_Bitvec_Ptr (Op), Get_Type_Width (Op_Type));
         when N_Logic_Type =>
            Execute_Unary_Log_Expression
              (Res, Expr, To_Logic_Ptr (Op).all);
         when N_Bit_Type =>
            Execute_Unary_Bit_Expression
              (Res, Expr, To_Bit_Ptr (Op).all);
         when N_Real_Type =>
            Execute_Unary_Fp64_Expression
              (Res, Expr, To_Fp64_Ptr (Op).all);
         when others =>
            Error_Kind ("execute_unary_expression (unary)", Op_Type);
      end case;
   end Execute_Unary_Expression;

   procedure Execute_Binary_Expression
     (Expr : Node; Left : Data_Ptr; Right : Data_Ptr; Dest : Data_Ptr)
   is
      Left_Op : constant Node := Get_Left (Expr);
      Right_Op : constant Node := Get_Right (Expr);
      Right_Type : constant Node := Get_Expr_Type (Right_Op);
      Left_Type : Node;
   begin
      Left_Type := Get_Expr_Type (Left_Op);
      loop
         case Get_Kind (Left_Type) is
            when N_Log_Packed_Array_Cst =>
               Execute_Binary_Lv_Expression
                 (Dest, Expr,
                  To_Logvec_Ptr (Left), Get_Type_Width (Left_Type),
                  To_Logvec_Ptr (Right), Get_Type_Width (Right_Type));
               return;
            when N_Bit_Packed_Array_Cst =>
               Execute_Binary_Bv_Expression
                 (Dest, Expr,
                  To_Bitvec_Ptr (Left), Get_Type_Width (Left_Type),
                  To_Bitvec_Ptr (Right), Get_Type_Width (Right_Type));
               return;
            when N_Logic_Type =>
               pragma Assert (Left_Type = Unsigned_Logic_Type);
               pragma Assert (Right_Type = Unsigned_Logic_Type);
               Execute_Binary_Log_Expression
                 (To_Logic_Ptr (Dest), Expr,
                  To_Logic_Ptr (Left).all,
                  To_Logic_Ptr (Right).all);
               return;
            when N_String_Type =>
               pragma Assert (Left_Type = String_Type);
               pragma Assert (Right_Type = String_Type);
               declare
                  L_Str : constant Sv_String := To_Sv_String_Ptr (Left).all;
                  R_Str : constant Sv_String := To_Sv_String_Ptr (Right).all;
               begin
                  Execute_Binary_String_Expression
                    (Dest, Expr, L_Str, R_Str);
                  Unref (L_Str);
                  Unref (R_Str);
                  return;
               end;
            when N_Real_Type =>
               Execute_Binary_Fp64_Expression
                 (Dest, Expr, To_Fp64_Ptr (Left).all, To_Fp64_Ptr (Right).all);
               return;
            when N_Shortreal_Type =>
               Execute_Binary_Fp32_Expression
                 (Dest, Expr, To_Fp32_Ptr (Left).all, To_Fp32_Ptr (Right).all);
               return;
            when Nkinds_Class =>
               Execute_Binary_Class_Expression
                 (Dest, Expr,
                  To_Sv_Class_Ptr (Left).all, To_Sv_Class_Ptr (Right).all);
               return;
            when N_Enum_Type =>
               Left_Type := Get_Enum_Base_Type (Left_Type);
               --  Try again.
            when N_Packed_Struct_Type =>
               Left_Type := Get_Packed_Base_Type (Left_Type);
               --  Try again.
            when others =>
               Error_Kind ("execute_binary_expression (binary)", Left_Type);
         end case;
      end loop;
   end Execute_Binary_Expression;

   procedure Execute_Logvec_Concatenation
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      pragma Assert (Get_Replication (Expr) = Null_Node);
      Off : Width_Type;
      Els : Node;
   begin
      pragma Assert (Get_Kind (Get_Expr_Type (Expr)) = N_Log_Packed_Array_Cst);
      Off := Get_Type_Width (Get_Expr_Type (Expr));
      Els := Get_Expressions (Expr);
      while Els /= Null_Node loop
         declare
            E : constant Node := Get_Expression (Els);
            Orig_Etype : constant Node := Get_Expr_Type (E);
            Ewidth : constant Width_Type := Get_Type_Width (Orig_Etype);
            Esize : constant Storage_Index := Get_Storage_Size (Orig_Etype);
            Etype : Node;
         begin
            --  11.4.12.1 Replication operator
            --  A replication with a zero replication constant is considered
            --  to have a size of zero and is ignored.
            if Ewidth > 0 then
               declare
                  Eval : Storage_Type (0 .. Esize - 1);
                  Change : Boolean;
               begin
                  Execute_Expression (Frame, Eval'Address, E);
                  pragma Assert (Off >= Ewidth);
                  Off := Off - Ewidth;
                  Etype := Orig_Etype;
                  loop
                     case Get_Kind (Etype) is
                        when N_Log_Packed_Array_Cst =>
                           Compute_Part_Insert
                             (To_Logvec_Ptr (Dest), Bit_Offset (Off),
                              To_Logvec_Ptr (Eval'Address), 0, Ewidth, Change);
                           exit;
                        when N_Logic_Type =>
                           Compute_Log_Insert
                             (To_Logvec_Ptr (Dest),Bit_Offset (Off),
                              To_Logic_Ptr (Eval'Address).all, Change);
                           exit;
                        when N_Packed_Struct_Type =>
                           Etype := Get_Packed_Base_Type (Etype);
                        when others =>
                           Error_Kind ("execute_concatenation", Etype);
                     end case;
                  end loop;
               end;
            end if;
         end;
         Els := Get_Chain (Els);
      end loop;
      pragma Assert (Off = 0);
   end Execute_Logvec_Concatenation;

   procedure Execute_String_Concatenation
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      pragma Assert (Get_Replication (Expr) = Null_Node);
      Els : constant Node := Get_Expressions (Expr);
      Nels : constant Natural := Nutils.Get_Chain_Length (Els);
      Arr : Sv_String_Arr (1 .. Nels);

      Len : Natural;
      Off : Natural;
      El : Node;
      E : Node;
      Res : Sv_String;
   begin
      Len := 0;
      El := Els;
      for I in Arr'Range loop
         E := Get_Expression (El);
         pragma Assert (Get_Kind (Get_Expr_Type (E)) = N_String_Type);
         Execute_Expression (Frame, Arr (I)'Address, E);
         Len := Len + Get_Length (Arr (I));

         El := Get_Chain (El);
      end loop;
      pragma Assert (El = Null_Node);

      --  Allocate the string and fill it.
      Res := New_Sv_String (Len);
      Off := 0;
      for I in Arr'Range loop
         for K in 1 .. Get_Length (Arr (I)) loop
            Off := Off + 1;
            Set_String_El (Res, Off, Get_String_El (Arr (I), K));
         end loop;
         --         Unref (Arr (I));
      end loop;
      pragma Assert (Off = Len);

      To_Sv_String_Ptr (Dest).all := Res;
   end Execute_String_Concatenation;

   procedure Execute_Queue_Concatenation
     (Frame : Frame_Ptr; Dest : Data_Ptr; Concat : Node)
   is
      pragma Assert (Get_Replication (Concat) = Null_Node);
      Res_Type : constant Node := Get_Expr_Type (Concat);
      El_Type : constant Node := Get_Type_Element_Type (Res_Type);
      Els : constant Node := Get_Expressions (Concat);
      Nels : constant Natural := Nutils.Get_Chain_Length (Els);
      --Arr : Any_Handle_Arr (1 .. Nels);
      subtype Arr is Natural range 1 .. Nels;

      Len : Uns32;
      El : Node;
      Expr, Expr_Type : Node;
      Ptr : Data_Ptr;
      Res : Sv_Queue;
   begin
      --  1. Count number of elements in the result.
      --     Evaluate queue (and array) expressions, but not the other ones
      --     (they count for 1).
      Len := 0;
      El := Els;
      for I in Arr'Range loop
         Expr := Get_Expression (El);
         Expr_Type := Get_Expr_Type (Expr);
         if Expr_Type = El_Type then
            Len := Len + 1;
         else
            --  Unpacked array: add length
            --  dynamic array, queues: eval element, save, add length
            raise Internal_Error;
         end if;

         El := Get_Chain (El);
      end loop;
      pragma Assert (El = Null_Node);

      --  2. Allocate a queue and reserve the size (unless unlimited ?)
      Res := Queue_New (Get_Storage_Size (El_Type), Unlimited, Len);

      --  3. Fill the queue.
      El := Els;
      for I in Arr'Range loop
         Expr := Get_Expression (El);
         Expr_Type := Get_Expr_Type (Expr);
         if Expr_Type = El_Type then
            Ptr := Queue_Push_Back (Res);
            Execute_Expression (Frame, Ptr, Expr);
         else
            raise Internal_Error;
         end if;
      end loop;

      To_Sv_Queue_Ptr (Dest).all := Res;
   end Execute_Queue_Concatenation;

   procedure Execute_Unpacked_Struct_Aggregate_Literal
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Etype : constant Node := Get_Expr_Type (Expr);
      El : Node;
      Off : Storage_Index;
      Member : Node;
      Key : Node;
   begin
      Member := Get_Members (Etype);
      El := Get_Elements (Expr);
      while El /= Null_Node loop
         Key := Get_Pattern_Key (El);
         if Key /= Null_Node and then Get_Kind (Key) = N_Default then
            --  Discard default.  Must have been expanded.
            null;
         else
            if Key /= Null_Node then
               --  By name.
               if Get_Kind (Key) = N_Name then
                  Member := Get_Declaration (Key);
               else
                  pragma Assert (Get_Kind (Key) = N_Member);
                  Member := Key;
               end if;
            end if;
            Off := Get_Unpacked_Member_Offset (Member);
            Execute_Expression (Frame, Dest + Off, Get_Expression (El));
            Member := Get_Chain (Member);
         end if;
         El := Get_Chain (El);
      end loop;
   end Execute_Unpacked_Struct_Aggregate_Literal;

   procedure Execute_Packed_Aggregate_Element (Frame : Frame_Ptr;
                                               Dest : Data_Ptr;
                                               Doff : Uns32;
                                               Dtype : Node;
                                               Expr : Node)
   is
      Orig_Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Orig_Expr_Type);
      Tmp : Storage_Type (0 .. Ssize - 1);
      Expr_Type : Node;
      Change : Boolean;
   begin
      Execute_Expression (Frame, Tmp'Address, Expr);
      case Get_Kind (Dtype) is
         when N_Log_Packed_Array_Cst =>
            Expr_Type := Orig_Expr_Type;
            loop
               case Get_Kind (Expr_Type) is
                  when N_Log_Packed_Array_Cst =>
                     Compute_Part_Insert
                       (To_Logvec_Ptr (Dest), Bit_Offset (Doff),
                        To_Logvec_Ptr (Tmp'Address), 0,
                        Get_Type_Width (Expr_Type), Change);
                     return;
                  when N_Bit_Packed_Array_Cst =>
                     Compute_Log_Bit_Part_Insert
                       (To_Logvec_Ptr (Dest), Bit_Offset (Doff),
                        To_Bitvec_Ptr (Tmp'Address),
                        Get_Type_Width (Expr_Type));
                     return;
                  when N_Logic_Type =>
                     Compute_Log_Insert
                       (To_Logvec_Ptr (Dest), Bit_Offset (Doff),
                        To_Logic_Ptr (Tmp'Address).all, Change);
                     return;
                  when N_Enum_Type =>
                     Expr_Type := Get_Enum_Base_Type (Expr_Type);
                  when others =>
                     Error_Kind
                       ("execute_packed_aggregate_element(log)", Expr_Type);
               end case;
            end loop;
         when others =>
            Error_Kind ("execute_packed_aggregate_element", Dtype);
      end case;
   end Execute_Packed_Aggregate_Element;

   procedure Execute_Packed_Struct_Aggregate_Literal
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Etype : constant Node := Get_Expr_Type (Expr);
      Base_Type : constant Node := Get_Packed_Base_Type (Etype);
      Key : Node;
      El : Node;
      Off : Uns32;
      Member : Node;
   begin
      El := Get_Elements (Expr);
      if Get_Pattern_Key (El) /= Null_Node then
         --  By name
         while El /= Null_Node loop
            Key := Get_Pattern_Key (El);
            if Get_Kind (Key) = N_Default then
               --  Skip, must have been expanded.
               null;
            else
               if Get_Kind (Key) = N_Name then
                  Member := Get_Declaration (Key);
               else
                  pragma Assert (Get_Kind (Key) = N_Packed_Member);
                  Member := Key;
               end if;
               Off := Get_Packed_Member_Offset (Member);
               Execute_Packed_Aggregate_Element
                 (Frame, Dest, Off, Base_Type, Get_Expression (El));
            end if;
            El := Get_Chain (El);
         end loop;
      else
         --  By position
         Member := Get_Members (Etype);
         while Member /= Null_Node loop
            Off := Get_Packed_Member_Offset (Member);
            Execute_Packed_Aggregate_Element
              (Frame, Dest, Off, Base_Type, Get_Expression (El));
            El := Get_Chain (El);
            Member := Get_Chain (Member);
         end loop;
         pragma Assert (El = Null_Node);
      end if;
   end Execute_Packed_Struct_Aggregate_Literal;

   procedure Execute_Array_Aggregate_Literal
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Etype : constant Node := Get_Expr_Type (Expr);
      El : Node;
      Off : Storage_Index;
      Stride : constant Tsize_Type := Get_Stride_Size (Etype);
      Count : Int32;
   begin
      if Get_Kind (Expr) = N_Aggregate_Literal_Cst then
         Count := Get_Replication_Cst (Expr);
      else
         Count := 1;
      end if;
      Off := 0;
      for I in 1 .. Count loop
         El := Get_Elements (Expr);
         while El /= Null_Node loop
            pragma Assert (Get_Pattern_Key (El) = Null_Node);
            Execute_Expression
              (Frame, Dest + Off, Get_Expression (El));
            El := Get_Chain (El);
            Off := Off + Storage_Index (Stride);
         end loop;
      end loop;
   end Execute_Array_Aggregate_Literal;

   procedure Execute_Sysfunc_Size
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Arr_Arg : constant Node := Get_Arguments (Expr);
      Arr : constant Node := Get_Expression (Arr_Arg);
      Arr_Type : constant Node := Get_Expr_Type (Arr);
      Dim : Node;
      Res : Uns32;
   begin
      Dim := Get_Chain (Arr_Arg);
      if Dim /= Null_Node then
         --  TODO
         raise Internal_Error;
      end if;

      if Sem_Types.Is_Type_Name (Arr) then
         --  TODO
         raise Internal_Error;
      else
         --  Execute prefix.
         --  Note: could be a packed type.
         declare
            Ssize : constant Storage_Index := Get_Storage_Size (Arr_Type);
            A : Storage_Type (0 .. Ssize - 1);
         begin
            Execute_Expression (Frame, A'Address, Arr);
            case Get_Kind (Arr_Type) is
               when N_Array_Cst =>
                  Res := Uns32 (Sem_Utils.Compute_Length (Arr_Type));
               when others =>
                  Error_Kind ("execute_sysfunc_size", Arr_Type);
            end case;
         end;
      end if;
      To_Logvec_Ptr (Dest)(0) := (Val => Res, Zx => 0);
   end Execute_Sysfunc_Size;

   procedure Execute_System_Function_Call
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node)
   is
      Id : constant Sys_Tf_Id := Get_Sys_Tf_Id (Expr);
   begin
      case Id is
         when Sys_Tf_Signed_Id
           | Sys_Tf_Unsigned_Id =>
            --  Just execute expression, the sign is just for compile
            --  time.
            Execute_Expression
              (Frame, Dest, Get_Expression (Get_Arguments (Expr)));
         when Sys_Tf_Size_Id =>
            Execute_Sysfunc_Size (Frame, Dest, Expr);
         when User_Sys_Tf_Id =>
            Vpi.Call_Sysfunc_Calltf (Frame, Id, Expr, Dest);
         when others =>
            --  TODO.
            raise Internal_Error;
      end case;
   end Execute_System_Function_Call;

   procedure Execute_Expression
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node) is
   begin
      case Get_Kind (Expr) is
         when N_Parenthesis_Expr =>
            Execute_Expression (Frame, Dest, Get_Expression (Expr));
         when N_Conversion
            | N_Size_Cast =>
            Execute_Conversion (Frame, Dest, Expr);
         when N_Number
           | N_Computed_Number =>
            declare
               Etype : Node;
            begin
               Etype := Get_Expr_Type (Expr);
               loop
                  case Get_Kind (Etype) is
                     when N_Log_Packed_Array_Cst =>
                        Compute_Number (To_Logvec_Ptr (Dest), Expr);
                        exit;
                     when N_Bit_Packed_Array_Cst =>
                        Compute_Number (To_Bitvec_Ptr (Dest), Expr);
                        exit;
                     when N_Bit_Type =>
                        To_Bit_Ptr (Dest).all :=
                          Bit_Type'Val (Get_Number_Lo_Val (Expr));
                        exit;
                     when N_Logic_Type =>
                        To_Logic_Ptr (Dest).all :=
                          Logic_Type'Val (Get_Number_Lo_Val (Expr));
                        exit;
                     when N_Enum_Type =>
                        Etype := Get_Enum_Base_Type (Etype);
                     when others =>
                        Error_Kind
                          ("execute_expression(number)", Get_Expr_Type (Expr));
                  end case;
               end loop;
            end;
         when N_Bignum =>
            declare
               Etype : Node;
            begin
               Etype := Get_Expr_Type (Expr);
               loop
                  case Get_Kind (Etype) is
                     when N_Log_Packed_Array_Cst =>
                        Compute_Bignum (To_Logvec_Ptr (Dest), Expr);
                        exit;
--                     when N_Bit_Packed_Array_Cst =>
--                        Compute_Bignum (To_Bitvec_Ptr (Dest), Expr);
--                        exit;
                     when N_Enum_Type =>
                        Etype := Get_Enum_Base_Type (Etype);
                     when others =>
                        Error_Kind
                          ("execute_expression(bignum)", Get_Expr_Type (Expr));
                  end case;
               end loop;
            end;
         when N_Real_Number =>
            Execute_Real_Number (Dest, Expr);
         when N_Unbased_Literal =>
            case Get_Kind (Get_Expr_Type (Expr)) is
               when N_Log_Packed_Array_Cst =>
                  Compute_Unbased_Literal (To_Logvec_Ptr (Dest), Expr);
               when N_Bit_Packed_Array_Cst =>
                  Compute_Unbased_Literal (To_Bitvec_Ptr (Dest), Expr);
               when N_Logic_Type =>
                  Compute_Unbased_Literal (To_Logic_Ptr (Dest), Expr);
               when others =>
                  Error_Kind
                    ("execute_expression(unbased_lit)", Get_Expr_Type (Expr));
            end case;
         when N_Time_Literal =>
            declare
               Val : Fp64;
               Div : Fp64;
               R : Fp64;
               Unit : Int32;
               Prec : Int32;
            begin
               --  1800-2017 5.8 Time literals
               --  The time literal is interpreted as a realtime value
               --  scaled to the current time unit and rounded to the current
               --  time precision.
               Val := Get_Real_Number (Expr);
               Unit := Get_Time_Unit (Expr);
               Prec := Get_Time_Precision (Get_Timescale (Expr));
               if Unit < Prec then
                  --  Round to precision.
                  Div := Fp64 (10**Natural (Prec - Unit));
                  R := Val / Div;
                  if Val - (R * Div) >= (Div / 2.0) then
                     R := R + 1.0;
                  end if;
                  Val := R;
               else
                  --  Scale to precision.
                  while Unit > Prec loop
                     Val := Val * 10.0;
                     Unit := Unit - 1;
                  end loop;
               end if;
               --  Scale to simulation time unit.
               while Prec > Parse.Simulation_Time_Unit loop
                  Val := Val * 10.0;
                  Prec := Prec - 1;
               end loop;

               To_Fp64_Ptr (Dest).all := Val;
            end;
         when N_Null =>
            declare
               Etype : constant Node := Get_Expr_Type (Expr);
            begin
               case Get_Kind (Etype) is
                  when Nkinds_Class =>
                     To_Sv_Class_Ptr (Dest).all := null;
                  when others =>
                     Error_Kind ("execute_expression(null)", Etype);
               end case;
            end;
         when N_Name
           | N_Scoped_Name
           | N_Hierarchical
           | N_Class_Qualified_Name =>
            Execute_Expression (Frame, Dest, Get_Declaration (Expr));
         when N_Enum_Name =>
            Execute_Expression (Frame, Dest, Get_Expression (Expr));
         when Nkinds_Net_Port =>
            Execute_Expression (Frame, Dest, Get_Redeclaration (Expr));
         when N_Var
           | Nkinds_Nets
           | Nkinds_Tf_Port =>
            Execute_Name_To_Expression
              (Frame, Dest, Expr, Get_Type_Data_Type (Expr));
         when N_Parameter
           | N_Localparam =>
            Execute_Name_To_Expression
              (Frame, Dest, Expr, Get_Param_Type (Expr));
         when N_Return_Var
           | N_This_Var =>
            Execute_Name_To_Expression
              (Frame, Dest, Expr, Get_Expr_Type (Expr));
         when N_Indexed_Name
           | N_Bit_Select
           | N_Part_Select_Cst
           | N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst
           | N_Member_Name
           | N_This_Name
           | N_This
           | N_Super
           | N_Property_Name
           | N_Associative_Index
           | N_Interface_Item =>
            Execute_Name_To_Expression
              (Frame, Dest, Expr, Get_Expr_Type (Expr));
         when N_String_Index =>
            declare
               Pfx : constant Node := Get_Name (Expr);
               Index : Int32;
               Undef : Boolean;
               Str : Sv_String;
               C : Uns32;
            begin
               --  Prefix is a string.
               Execute_Name_To_Expression
                 (Frame, Str'Address, Pfx, Get_Expr_Type (Pfx));
               Execute_Expression_Int32
                 (Frame, Get_Expression (Expr), Index, Undef);
               --  If the index is out of range, returns 0.
               if Undef
                 or else Index < 0
                 or else Index >= Int32 (Get_Length (Str))
               then
                  C := 0;
               else
                  C := Character'Pos
                    (Get_String_El (Str, Positive (Index + 1)));
               end if;
               To_Bitvec_Ptr (Dest) (0) := C;
            end;
         when N_Post_Increment =>
            Execute_Post_Increment (Frame, Dest, Expr);
         when N_Pre_Increment =>
            Execute_Pre_Increment (Frame, Dest, Expr);
         when N_Cond_Op =>
            Execute_Conditional_Operator (Frame, Dest, Expr);
         when N_Short_Circuit_Op =>
            declare
               Op : constant Binary_Short_Circuit_Ops := Get_Binary_Op (Expr);
               L, R : Tri_State_Type;
            begin
               --  1800-2017 11.4.7 Logical operators
               --  The && and || operators shall use short circuit evaluation
               --  as follows:
               --  - The first operand expression shall always be evaluated.
               L := Execute_Condition (Frame, Get_Left (Expr));

               case Op is
                  when Binop_Logic_And =>
                     --  1800-2017 11.4.7 Logical operators
                     --  - For &&, if the first operand value is logically
                     --    false, then the second operand shall not be
                     --    evaluated.
                     if L = False then
                        Execute_Set_Logical (Dest, Expr, False);
                        return;
                     end if;
                  when Binop_Logic_Or =>
                     if L = True then
                        --  1800-2017 11.4.7 Logical operators
                        --  - For ||, if the first operand value is logically
                        --    true, the the second operand shall not be
                        --    evaluated.
                        Execute_Set_Logical (Dest, Expr, True);
                        return;
                     end if;
               end case;

               R := Execute_Condition (Frame, Get_Right (Expr));
               case Op is
                  when Binop_Logic_And =>
                     if R = False then
                        R := False;
                     elsif L = True and R = True then
                        R := True;
                     else
                        R := Unknown;
                     end if;
                  when Binop_Logic_Or =>
                     if R = True then
                        R := True;
                     elsif L = False and R = False then
                        R := False;
                     else
                        R := Unknown;
                     end if;
               end case;
               Execute_Set_Logical (Dest, Expr, R);
               end;
         when N_Binary_Op =>
            declare
               Left : constant Node := Get_Left (Expr);
               Left_Type : constant Node := Get_Expr_Type (Left);
               Right : constant Node := Get_Right (Expr);
               Right_Type : constant Node := Get_Expr_Type (Right);
               Lsize : constant Storage_Index := Get_Storage_Size (Left_Type);
               Rsize : constant Storage_Index := Get_Storage_Size (Right_Type);
               Ldata : Storage_Type (0 .. Lsize - 1);
               Rdata : Storage_Type (0 .. Rsize - 1);
            begin
               Execute_Expression (Frame, Ldata'Address, Left);
               Execute_Expression (Frame, Rdata'Address, Right);

               Execute_Binary_Expression
                 (Expr, Ldata'Address, Rdata'Address, Dest);
            end;
         when N_Unary_Op =>
            declare
               Operand : constant Node := Get_Expression (Expr);
               Op_Type : constant Node := Get_Expr_Type (Operand);
               Ssize : constant Storage_Index := Get_Storage_Size (Op_Type);
               Op_Data : Storage_Type (0 .. Ssize - 1);
            begin
               Execute_Expression (Frame, Op_Data'Address, Operand);
               Execute_Unary_Expression (Expr, Op_Data'Address, Dest);
            end;
         when N_Concatenation =>
            case Get_Kind (Get_Expr_Type (Expr)) is
               when N_Log_Packed_Array_Cst =>
                  Execute_Logvec_Concatenation (Frame, Dest, Expr);
               when N_String_Type =>
                  Execute_String_Concatenation (Frame, Dest, Expr);
               when N_Queue_Cst =>
                  Execute_Queue_Concatenation (Frame, Dest, Expr);
               when others =>
                  Error_Kind
                    ("execute_expression(concat)", Get_Expr_Type (Expr));
            end case;
         when N_Replication_Cst =>
            declare
               Width : constant Width_Type :=
                 Get_Type_Width (Get_Expr_Type (Expr));
               Count : constant Int32 := Get_Replication_Cst (Expr);
               Stride : constant Width_Type := Width / Width_Type (Count);
               Off : Width_Type;
               Els : Node;
            begin
               pragma Assert (Get_Kind (Get_Expr_Type (Expr))
                                = N_Log_Packed_Array_Cst);
               Off := Width;
               Els := Get_Expressions (Expr);
               while Els /= Null_Node loop
                  declare
                     E : constant Node := Get_Expression (Els);
                     Etype : constant Node := Get_Expr_Type (E);
                     Ewidth : constant Width_Type := Get_Type_Width (Etype);
                     Esize : constant Storage_Index :=
                       Get_Storage_Size (Etype);
                     Eval : Storage_Type (0 .. Esize - 1);
                     Change : Boolean;
                  begin
                     Execute_Expression (Frame, Eval'Address, E);
                     pragma Assert (Off >= Ewidth);
                     Off := Off - Ewidth;
                     case Get_Kind (Etype) is
                        when N_Log_Packed_Array_Cst =>
                           for I in 0 .. Width_Type (Count - 1) loop
                              Compute_Part_Insert
                                (To_Logvec_Ptr (Dest),
                                 Bit_Offset (Off - Stride * I),
                                 To_Logvec_Ptr (Eval'Address), 0, Ewidth,
                                 Change);
                           end loop;
                        when N_Logic_Type =>
                           for I in 0 .. Width_Type (Count - 1) loop
                              Compute_Log_Insert
                                (To_Logvec_Ptr (Dest),
                                 Bit_Offset (Off - Stride * I),
                                 To_Logic_Ptr (Eval'Address).all, Change);
                           end loop;
                        when others =>
                           Error_Kind
                             ("execute_expression(repetition)", Etype);
                     end case;
                  end;
                  Els := Get_Chain (Els);
               end loop;
               pragma Assert (Off = Width - Stride);
            end;
         when N_Aggregate_Literal
           | N_Aggregate_Literal_Cst =>
            declare
               Etype : constant Node := Get_Expr_Type (Expr);
            begin
               Allocate_Type (Etype);
               case Get_Kind (Etype) is
                  when N_Struct_Type =>
                     Execute_Unpacked_Struct_Aggregate_Literal
                       (Frame, Dest, Expr);
                  when N_Packed_Struct_Type =>
                     Execute_Packed_Struct_Aggregate_Literal
                       (Frame, Dest, Expr);
                  when N_Array_Cst =>
                     Execute_Array_Aggregate_Literal (Frame, Dest, Expr);
                  when others =>
                     Error_Kind ("execute_expression(aggr_lit)", Etype);
               end case;
            end;
         when N_String_Literal =>
            Execute_String_Literal (Dest, Expr);
         when N_Type_Cast =>
            Execute_Cast (Frame, Dest, Expr);
         when N_Bits_Type =>
            declare
               Etype : constant Node :=
                 Get_Expr_Type (Get_Type_Argument (Expr));
            begin
               --  pragma Assert (Get_Size_Flag (Etype));
               To_Logvec_Ptr (Dest)(0) := (Uns32 (Get_Type_Width (Etype)), 0);
            end;
         when N_System_Call =>
            Execute_System_Function_Call (Frame, Dest, Expr);
         when N_Call =>
            declare
               Rtn : constant Node := Get_Subroutine (Expr);
               Subprg : constant Node := Get_Declaration (Rtn);
               Handle : Sv_Class_Handle;
            begin
               if Get_Object (Expr) /= Null_Node then
                  if Subprg <= Last_Builtin_Method then
                     Execute_Builtin_Method_Call (Frame, Dest, Expr, Subprg);
                  else
                     raise Internal_Error;
                  end if;
               else
                  case Get_Kind (Rtn) is
                     when N_This_Name =>
                        Execute_Expression
                          (Frame, Handle'Address, Get_This_Declaration (Rtn));
                     when N_Method_Name =>
                        Execute_Expression
                          (Frame, Handle'Address, Get_Name (Rtn));
                     when others =>
                        Handle := null;
                  end case;
                  Execute_Function_Call (Frame, Dest, Handle, Expr, Subprg);
               end if;
            end;
         when N_New_Call =>
            To_Sv_Class_Ptr (Dest).all := Allocate_Class_Object
              (Frame, Get_Expr_Type (Expr), Get_Arguments (Expr));
         when N_Dynamic_Array_New =>
            Execute_Dynamic_Array_New (Frame, Dest, Expr);
         when N_Genvar =>
            declare
               Val : constant Int32 := Get_Generate_Index (Expr);
               Wd : Width_Type;
               Etype : Node;
            begin
               --  Only during elaboration
               pragma Assert (Frame = null);
               Etype := Get_Expr_Type (Expr);
               Wd := Get_Type_Width (Etype);
               case Get_Kind (Etype) is
                  when N_Log_Packed_Array_Cst =>
                     Compute_Int32 (To_Logvec_Ptr (Dest), Wd, Val);
--                  when N_Bit_Packed_Array_Cst =>
--                     Compute_Int32
--                       (To_Bitvec_Ptr (Dest), Get_Generate_Index (Expr));
                  when others =>
                     raise Internal_Error;
               end case;
            end;
         when N_Error_Expr =>
            Allocates.Init (Dest, Get_Expr_Type (Expr));
         when others =>
            Error_Kind ("execute_expression", Expr);
      end case;
   end Execute_Expression;

   procedure Execute_Expression_Int32 (Frame : Frame_Ptr;
                                       Expr : Node;
                                       Val : out Int32;
                                       Is_Undef : out Boolean)
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Res : Storage_Type (0 .. Ssize - 1);
   begin
      Execute_Expression (Frame, Res'Address, Expr);
      case Get_Kind (Expr_Type) is
         when N_Log_Packed_Array_Cst =>
            declare
               Lv : constant Logvec_Ptr := To_Logvec_Ptr (Res'Address);
               Width : constant Width_Type := Get_Type_Width (Expr_Type);
            begin
               if Has_Unknowns (Lv, Width) then
                  Val := 0;
                  Is_Undef := True;
               elsif not In_Uns32 (Lv, Width) then
                  raise Constraint_Error;
               else
                  Val := Int32 (To_Uns32 (Lv, Width));
                  Is_Undef := False;
               end if;
            end;
         when N_Bit_Packed_Array_Cst =>
            declare
               Bv : constant Bitvec_Ptr := To_Bitvec_Ptr (Res'Address);
               Width : constant Width_Type := Get_Type_Width (Expr_Type);
            begin
               if not In_Uns32 (Bv, Width) then
                  raise Constraint_Error;
               else
                  Val := Int32 (To_Uns32 (Bv, Width));
                  Is_Undef := False;
               end if;
            end;
         when N_Real_Type =>
            --  1800-2017 5.7.2 Real literal constants
            --  Real numbers shall be converted to integers by rounding the
            --  real number to the nearest integer, rather than by
            --  truncating it. [...] The ties shall be rounded away from
            --  zero.
            Val := Int32 (To_Fp64_Ptr (Res'Address).all);
            Is_Undef := False;
         when N_Logic_Type =>
            declare
               V : constant Logic_Type := To_Logic_Ptr (Res'Address).all;
            begin
               case V is
                  when V_0 =>
                     Val := 0;
                     Is_Undef := False;
                  when V_1 =>
                     Val := 1;
                     Is_Undef := False;
                  when V_X | V_Z =>
                     Val := 0;
                     Is_Undef := True;
               end case;
            end;
         when others =>
            Error_Kind ("execute_expression_int32", Expr_Type);
      end case;
   end Execute_Expression_Int32;

   function Create_Update (T : Node) return Update_Acc is
   begin
      case Get_Kind (T) is
         when N_Logic_Type =>
            return new Update_Type (Val_Logic);
         when N_Real_Type
           | N_Shortreal_Type =>
            return new Update_Type (Val_Real);
         when N_Log_Packed_Array_Cst =>
            return new Update_Type (Val_Vector);
         when N_Array_Cst =>
            return new Update_Type (Val_Array);
         when others =>
            raise Internal_Error;
      end case;
   end Create_Update;

   function Compute_Width (Atype : Node) return Width_Type is
   begin
      case Nkinds_Types (Get_Kind (Atype)) is
         when N_Log_Packed_Array_Cst
            | N_Bit_Packed_Array_Cst =>
            return Get_Type_Width (Atype);
         when N_Logic_Type
            | N_Bit_Type =>
            return 1;
         when N_Enum_Type
            | N_Packed_Struct_Type =>
            return Get_Type_Width (Atype);
         when others =>
            Error_Kind ("compute_width", Atype);
      end case;
   end Compute_Width;

   procedure Execute_Packed_Index
     (Frame : Frame_Ptr; Name : Node; Undef : out Boolean; Offset : out Uns32)
   is
      Pfx : constant Node := Get_Name (Name);
      Pfx_Type : constant Node := Get_Expr_Type (Pfx);
      Msb : constant Int32 := Get_Msb_Cst (Pfx_Type);
      Lsb : constant Int32 := Get_Lsb_Cst (Pfx_Type);
      Index : Int32;
   begin
      Execute_Expression_Int32 (Frame, Get_Expression (Name), Index, Undef);
      if not Undef then
         if Msb >= Lsb then
            if Index > Msb or Index < Lsb then
               --  Out of range
               Undef := True;
               Offset := 0;
            else
               Undef := False;
               Offset := Uns32 (Index - Lsb);
            end if;
         else
            if Index < Msb or Index > Lsb then
               --  Out of range
               Undef := True;
               Offset := 0;
            else
               Undef := False;
               Offset := Uns32 (Lsb - Index);
            end if;
         end if;
      else
         Offset := not 0;
      end if;
   end Execute_Packed_Index;

   procedure Execute_Name_Nonvec_Index (Frame : Frame_Ptr;
                                        Name : Node;
                                        Force : Boolean;
                                        Value : out Data_Ptr;
                                        Update : out Update_Acc)
   is
      Pfx : constant Node := Get_Name (Name);
      Pfx_Type : constant Node := Get_Expr_Type (Pfx);
      Index : Int32;
      Index_Offset : Uns32;
      Undef : Boolean;
      Pfx_Update : Update_Acc;
   begin
      Execute_Name_Nonvec (Frame, Pfx, Force, Value, Pfx_Update);

      Execute_Expression_Int32 (Frame, Get_Expression (Name), Index, Undef);

      if not Undef then
         case Get_Kind (Pfx_Type) is
            when N_Array_Cst =>
               declare
                  Msb : constant Int32 := Get_Msb_Cst (Pfx_Type);
                  Lsb : constant Int32 := Get_Lsb_Cst (Pfx_Type);
               begin
                  if Msb >= Lsb then
                     if Index > Msb or Index < Lsb then
                        --  Out of range
                        Undef := True;
                     else
                        Undef := False;
                        Index_Offset := Uns32 (Msb - Index);
                     end if;
                  else
                     if Index < Msb or Index > Lsb then
                        --  Out of range
                        Undef := True;
                     else
                        Undef := False;
                        Index_Offset := Uns32 (Index - Msb);
                     end if;
                  end if;
               end;
            when N_Dynamic_Array_Cst =>
               declare
                  Ptr : constant Sv_Dyn_Array_Ptr :=
                    To_Sv_Dyn_Array_Ptr_Ptr (Value).all;
               begin
                  if Ptr = null
                    or else Index < 0
                    or else Index >= Ptr.Size
                  then
                     Undef := True;
                  else
                     Value := Ptr.Base (1)'Address;
                     Index_Offset := Uns32 (Index);
                  end if;
               end;
            when N_Queue_Cst =>
               declare
                  Ptr : constant Sv_Queue := To_Sv_Queue_Ptr (Value).all;
               begin
                  Value := Queue_Index (Ptr, Index);
                  Update := null;
                  return;
               end;
            when others =>
               Error_Kind ("execute_name_nonvec_index", Pfx_Type);
         end case;
      end if;

      if Undef then
         Value := No_Data_Ptr;
         Update := null;
         return;
      end if;

      Value := Value
        + (Storage_Index (Index_Offset)
             * Storage_Index (Get_Stride_Size (Pfx_Type)));

      if Pfx_Update = null then
         pragma Assert (not Force);
         Update := null;
      else
         if Pfx_Update.Arr = null then
            if Force then
               --  TODO.
               raise Program_Error;
            else
               Update := Pfx_Update;
            end if;
         else
            Update :=
              Pfx_Update.Arr.Arr (Bit_Offset (Index_Offset));
            if Update = null and Force then
               raise Program_Error;
            end if;
         end if;
      end if;
   end Execute_Name_Nonvec_Index;

   function Execute_Sub_Frame (Frame : Frame_Ptr; Name : Node) return Frame_Ptr
   is
      N : Node;
   begin
      N := Name;
      pragma Unreferenced (Name);
      loop
         case Get_Kind (N) is
            when N_Name =>
               N := Get_Declaration (N);
            when N_Interface_Instance =>
               --  FIXME: what about interface within interface ?  Allowed ?
               return Get_Sub_Frame (Frame, N);
            when N_Interface_Port =>
               return Get_Sub_Frame (Frame, N);
            when others =>
               Error_Kind ("execute_sub_frame", N);
         end case;
      end loop;
   end Execute_Sub_Frame;

   procedure Execute_Property_Name (Frame : Frame_Ptr;
                                    Handle : Node;
                                    Prop : Node;
                                    Value : out Data_Ptr;
                                    Update : out Update_Acc)
   is
      Pfx_Update : Update_Acc;
      Obj_Frame : Frame_Ptr;
   begin
      --  Get the object (the 'this').
      if Handle = Null_Node then
         Obj_Frame := Frame;
      else
         Execute_Name_Nonvec (Frame, Handle, False, Value, Pfx_Update);
         pragma Assert (Pfx_Update = null);
         Obj_Frame := To_Frame_Ptr (To_Sv_Class_Ptr (Value).all);
         if Obj_Frame = null and then not Get_Static_Flag (Prop) then
            --  TODO: error message: null object deferenced.
            raise Internal_Error;
         end if;
      end if;

      --  Reference the property
      Value := Get_Var_Data (Obj_Frame, Prop);
      Update := null;
   end Execute_Property_Name;

   procedure Execute_Name_Nonvec_Internal (Frame : Frame_Ptr;
                                           Name : Node;
                                           Force : Boolean;
                                           Value : out Data_Ptr;
                                           Update : out Update_Acc)
   is
      N : Node;
   begin
      N := Name;
      pragma Unreferenced (Name);
      loop
         case Get_Kind (N) is
            when N_Name
               | N_Hierarchical
               | N_This
               | N_Super
               | N_Scoped_Name =>
               N := Get_Declaration (N);
            when N_Interface_Item =>
               Execute_Name_Nonvec_Internal
                 (Execute_Sub_Frame (Frame, Get_Name (N)),
                  Get_Declaration (N), Force, Value, Update);
               return;
            when Nkinds_Net_Port =>
               N := Get_Redeclaration (N);
            when N_Parameter
              | N_Localparam =>
               pragma Assert (not Force);
               Value := Get_Parameter_Data (N);
               Update := null;

               return;
            when N_Var
              | Nkinds_Nets
              | Nkinds_Tf_Port =>
               declare
                  N_Type : constant Node := Get_Data_Type (N);
               begin
                  Value := Get_Var_Data (Frame, N);
                  if Get_Is_Automatic (N) then
                     Update := null;
                  else
                     Update := Get_Var_Update (N);
                  end if;
                  if Force and Update = null then
                     Set_Var_Update (N, Create_Update (N_Type));
                  end if;
                  return;
               end;
            when N_Return_Var
              | N_This_Var =>
               Value := Get_Var_Data (Frame, N);
               Update := null;
               return;
            when N_Member_Name =>
               declare
                  Pfx : constant Node := Get_Name (N);
                  Memb : constant Node := Get_Declaration (N);
                  Pfx_Update : Update_Acc;
               begin
                  --  FIXME: set update!
                  Execute_Name_Nonvec (Frame, Pfx, Force, Value, Pfx_Update);
                  Value := Value + Get_Unpacked_Member_Offset (Memb);
                  return;
               end;
            when N_Indexed_Name =>
               Execute_Name_Nonvec_Index (Frame, N, Force, Value, Update);
               return;
            when N_Associative_Index =>
               --  TODO
               raise Internal_Error;
            when N_This_Name =>
               Execute_Property_Name
                 (Frame, Get_This_Declaration (N), Get_Declaration (N),
                  Value, Update);
               return;
            when N_Property_Name =>
               Execute_Property_Name
                 (Frame, Get_Name (N), Get_Declaration (N), Value, Update);
               return;
            when others =>
               Error_Kind ("execute_name_nonvec", N);
         end case;
      end loop;
   end Execute_Name_Nonvec_Internal;

   procedure Execute_HILO_Part_Select_Offset (Pfx_Msb, Pfx_Lsb : Int32;
                                              Msb, Lsb : Int32;
                                              Poffset : out Bit_Offset;
                                              Doffset : out Bit_Offset;
                                              Width : out Width_Type) is
   begin
      pragma Assert (Pfx_Msb >= Pfx_Lsb);
      pragma Assert (Msb >= Lsb);
      if Lsb >= Pfx_Lsb then
         --  Low bound is OK (at least above the prefix).
         Poffset := Bit_Offset (Lsb - Pfx_Lsb);
         Doffset := 0;
         if Msb <= Pfx_Msb then
            --  Msb bound is OK too.
            Width := Width_Type (Msb - Lsb + 1);
         else
            if Lsb <= Pfx_Msb then
               Width := Width_Type (Pfx_Msb - Lsb + 1);
            else
               Width := 0;
            end if;
         end if;
      else
         --  Low bound is not OK.
         Poffset := 0;
         Doffset := Bit_Offset (Pfx_Lsb - Lsb);
         if Msb < Pfx_Lsb then
            --  Completly on the right hand side
            Width := 0;
         elsif Msb <= Pfx_Msb then
            --  Msb is in the range.
            Width := Width_Type (Msb - Pfx_Lsb + 1);
         else
            Width := Width_Type (Pfx_Msb - Pfx_Lsb + 1);
         end if;
      end if;
   end Execute_HILO_Part_Select_Offset;

   procedure Execute_LOHI_Part_Select_Offset (Pfx_Msb, Pfx_Lsb : Int32;
                                              Msb, Lsb : Int32;
                                              Poffset : out Bit_Offset;
                                              Doffset : out Bit_Offset;
                                              Width : out Width_Type) is
   begin
      pragma Assert (Pfx_Msb <= Pfx_Lsb);
      pragma Assert (Msb <= Lsb);
      if Lsb <= Pfx_Lsb then
         --  LSB is OK.
         Poffset := Bit_Offset (Pfx_Lsb - Lsb);
         Doffset := 0;
         if Msb >= Pfx_Msb then
            --  MSB bound is OK too.
            Width := Width_Type (Lsb - Msb + 1);
         else
            if Lsb >= Pfx_Msb then
               Width := Width_Type (Lsb - Pfx_Msb + 1);
            else
               Width := 0;
            end if;
         end if;
      else
         --  LSB is not OK.
         Poffset := 0;
         Doffset := Bit_Offset (Lsb - Pfx_Lsb);
         if Msb > Pfx_Lsb then
            --  Completly on the right handle side
            Width := 0;
         elsif Msb >= Pfx_Msb then
            --  MSB is in the range.
            Width := Width_Type (Pfx_Lsb - Msb + 1);
         else
            Width := Width_Type (Pfx_Lsb - Pfx_Msb + 1);
         end if;
      end if;
   end Execute_LOHI_Part_Select_Offset;

   --  Execute part select vector of NAME (that is [MSB:LSB]).
   --  VALUE is the address of the vector.
   --  WIDTH is the number of bits that can be extract.  Could be from 0
   --  to the width of NAME (because of out of bounds).
   --  POFFSET is the offset of the first bit to be extracted from VALUE.
   --  DOFFSET is the number of bits to be ignored on the right hand side.
   procedure Execute_Part_Select_Vector (Frame : Frame_Ptr;
                                         Name : Node;
                                         Value : out Data_Ptr;
                                         Poffset : out Bit_Offset;
                                         Doffset : out Bit_Offset;
                                         Width : out Width_Type;
                                         Update : out Update_Acc)
   is
      Pfx : constant Node := Get_Name (Name);
      Pfx_Type : constant Node := Get_Expr_Type (Pfx);
      Pfx_Lsb : constant Int32 := Get_Lsb_Cst (Pfx_Type);
      Pfx_Msb : constant Int32 := Get_Msb_Cst (Pfx_Type);
      Lsb : constant Int32 := Get_Lsb_Cst (Name);
      Msb : constant Int32 := Get_Msb_Cst (Name);
      Pfx_Offset : Bit_Offset;
      Pfx_Doffset : Bit_Offset;
      Pfx_Width : Width_Type;
      Pfx_Update : Update_Acc;
   begin
      --  1800-2017 11.5.1 Vector bit-select and part-select
      --    addressing.
      Execute_Name_Vector (Frame, Pfx, False, Value,
                           Pfx_Offset, Pfx_Doffset, Pfx_Width, Pfx_Update);
      pragma Assert (Pfx_Doffset = 0);  --  TODO

      --  Get direction from the prefix.
      if Pfx_Msb >= Pfx_Lsb then
         --  HI:LO
         Execute_HILO_Part_Select_Offset (Pfx_Msb, Pfx_Lsb,
                                          Msb, Lsb,
                                          Poffset, Doffset, Width);
      else
         --  LO:HI
         Execute_LOHI_Part_Select_Offset (Pfx_Msb, Pfx_Lsb,
                                          Msb, Lsb,
                                          Poffset, Doffset, Width);
      end if;

      pragma Assert (Width <= Get_Type_Width (Get_Expr_Type (Name)));
      Poffset := Poffset + Pfx_Offset;

      if Pfx_Update = null then
         Update := null;
      else
         Update := Pfx_Update;
      end if;
   end Execute_Part_Select_Vector;

   --  Execute indexed part select vector of NAME (that is -: or +:).
   --  PLUS is the direction (so True for +:, False for -:).
   --  VALUE is the address of the vector.
   --  WIDTH is the number of bits that can be extract.  Could be from 0
   --  to the width of NAME (because of out of bounds).
   --  POFFSET is the offset of the first bit to be extracted from VALUE.
   --  DOFFSET is the number of bits to be ignored on the right hand side.
   procedure Execute_Indexed_Part_Select_Vector (Frame : Frame_Ptr;
                                                 Name : Node;
                                                 Plus : Boolean;
                                                 Value : out Data_Ptr;
                                                 Poffset : out Bit_Offset;
                                                 Doffset : out Bit_Offset;
                                                 Width : out Width_Type;
                                                 Update : out Update_Acc)
   is
      Pfx : constant Node := Get_Name (Name);
      Pfx_Type : constant Node := Get_Expr_Type (Pfx);
      Pfx_Lsb : constant Int32 := Get_Lsb_Cst (Pfx_Type);
      Pfx_Msb : constant Int32 := Get_Msb_Cst (Pfx_Type);
      Pwidth : constant Int32 := Get_Width_Cst (Name);
      Base : Int32;
      Msb, Lsb : Int32;
      Pfx_Offset : Bit_Offset;
      Pfx_Doffset : Bit_Offset;
      Pfx_Width : Width_Type;
      Pfx_Update : Update_Acc;
      Undef : Boolean;
   begin
      --  1800-2017 11.5.1 Vector bit-select and part-select
      --    addressing.
      Execute_Name_Vector (Frame, Pfx, False, Value,
                           Pfx_Offset, Pfx_Doffset, Pfx_Width, Pfx_Update);
      pragma Assert (Pfx_Doffset = 0);  --  TODO

      Execute_Expression_Int32 (Frame, Get_Base_Expr (Name), Base, Undef);
      if Undef then
         Value := Null_Address;
         Poffset := 0;
         Doffset := 0;
         Width := 0;
         Update := null;
         return;
      end if;

      --  Get direction from the prefix.
      if Pfx_Msb >= Pfx_Lsb then
         --  HI:LO
         if Plus then
            Msb := Base + Pwidth - 1;
            Lsb := Base;
         else
            Msb := Base;
            Lsb := Base - Pwidth + 1;
         end if;

         Execute_HILO_Part_Select_Offset (Pfx_Msb, Pfx_Lsb,
                                          Msb, Lsb,
                                          Poffset, Doffset, Width);
      else
         --  LO:HI
         if Plus then
            Msb := Base;
            Lsb := Base + Pwidth - 1;
         else
            Msb := Base - Pwidth + 1;
            Lsb := Base;
         end if;
         Execute_LOHI_Part_Select_Offset (Pfx_Msb, Pfx_Lsb,
                                          Msb, Lsb,
                                          Poffset, Doffset, Width);
      end if;

      Poffset := Poffset + Pfx_Offset;

      if Pfx_Update = null then
         Update := null;
      else
         Update := Pfx_Update;
      end if;
   end Execute_Indexed_Part_Select_Vector;

   function Execute_Associative_Index (Frame : Frame_Ptr; Name : Node)
                                      return Data_Ptr
   is
      Pfx : constant Node := Get_Name (Name);
      Idx_Expr : constant Node := Get_Expression (Name);
      Idx_Type : constant Node := Get_Expr_Type (Idx_Expr);
      Idx_Ssize : constant Storage_Index := Get_Storage_Size (Idx_Type);
      Idx : Storage_Type (0 .. Idx_Ssize - 1);
      Dest : Data_Ptr;
      Map : Sv_Map;
      Update : Update_Acc;
   begin
      Execute_Name_Nonvec (Frame, Pfx, False, Dest, Update);
      Map := To_Sv_Map_Ptr (Dest).all;
      Execute_Expression (Frame, Idx'Address, Idx_Expr);
      return Get_Map (Map, Idx'Address);
   end Execute_Associative_Index;

   procedure Execute_Name_Vector (Frame : Frame_Ptr;
                                  Name : Node;
                                  Force : Boolean;
                                  Value : out Data_Ptr;
                                  Noffset : out Bit_Offset;
                                  Doffset : out Bit_Offset;
                                  Width : out Width_Type;
                                  Update : out Update_Acc)
   is
      pragma Assert (not Force); --  ??
      N : Node;
   begin
      N := Name;
      pragma Unreferenced (Name);
      loop
         case Get_Kind (N) is
            when N_Name
              | N_Scoped_Name
              | N_Hierarchical
              | N_Class_Qualified_Name =>
               N := Get_Declaration (N);
            when N_Interface_Item =>
               Execute_Name_Vector (Execute_Sub_Frame (Frame, Get_Name (N)),
                                    Get_Declaration (N), Force,
                                    Value, Noffset, Doffset, Width, Update);
               return;
            when Nkinds_Net_Port =>
               N := Get_Redeclaration (N);
            when N_Parameter
              | N_Localparam =>
               pragma Assert (not Force);
               Value := Get_Parameter_Data (N);
               Noffset := 0;
               Doffset := 0;
               Width := Get_Type_Width (Get_Param_Type (N));
               Update := null;
               return;
            when N_Var
              | Nkinds_Nets
              | Nkinds_Tf_Port =>
               declare
                  N_Type : constant Node := Get_Type_Data_Type (N);
               begin
                  Value := Get_Var_Data (Frame, N);
                  Noffset := 0;
                  Doffset := 0;
                  Width := Compute_Width (N_Type);
                  if Get_Is_Automatic (N) then
                     pragma Assert (not Force);
                     Update := null;
                  else
                     Update := Get_Var_Update (N);
                     if Force and Update = null then
                        Set_Var_Update (N, Create_Update (N_Type));
                     end if;
                  end if;

                  return;
               end;
            when N_Return_Var
              | N_This_Var =>
               declare
                  N_Type : constant Node := Get_Expr_Type (N);
               begin
                  Value := Get_Var_Data (Frame, N);
                  Noffset := 0;
                  Doffset := 0;
                  Width := Compute_Width (N_Type);
                  pragma Assert (not Force);
                  Update := null;
                  return;
               end;
            when N_Member_Name =>
               declare
                  Pfx : constant Node := Get_Name (N);
                  Memb : constant Node := Get_Declaration (N);
                  Pfx_Update : Update_Acc;
                  Pfx_Offset : Bit_Offset;
                  Pfx_Doffset : Bit_Offset;
                  Pfx_Width : Width_Type;
               begin
                  if Is_Vector_Name (Pfx, Get_Expr_Type (Pfx)) then
                     --  FIXME: from packed struct
                     Execute_Name_Vector
                       (Frame, Pfx, Force, Value,
                        Pfx_Offset, Pfx_Doffset, Pfx_Width, Pfx_Update);
                     pragma Assert (Pfx_Doffset = 0);  --  TODO
                     Width := Get_Type_Width (Get_Type_Data_Type (Memb));
                     Noffset := Pfx_Offset
                       + Bit_Offset (Get_Packed_Member_Offset (Memb));
                     Doffset := 0;
                     return;
                  else
                     Execute_Name_Nonvec
                       (Frame, Pfx, Force, Value, Pfx_Update);
                     pragma Assert (Pfx_Update = null);
                     Value := Value + Get_Unpacked_Member_Offset (Memb);
                     Width := Compute_Width (Get_Expr_Type (N));
                     Noffset := 0;
                     Doffset := 0;
                     return;
                  end if;
               end;
            when N_Indexed_Name =>
               declare
                  Pfx : constant Node := Get_Name (N);
               begin
                  if Is_Vector_Name (Pfx, Get_Expr_Type (Pfx)) then
                     --  Vector to vector
                     declare
                        Ntype : constant Node := Get_Expr_Type (N);
                        Index_Offset : Uns32;
                        Pfx_Offset : Bit_Offset;
                        Pfx_Doffset : Bit_Offset;
                        Pfx_Width : Width_Type;
                        Undef : Boolean;
                        Pfx_Update : Update_Acc;
                     begin
                        Execute_Name_Vector
                          (Frame, Pfx, Force, Value,
                           Pfx_Offset, Pfx_Doffset, Pfx_Width, Pfx_Update);
                        pragma Assert (Pfx_Doffset = 0);  --  TODO
                        Doffset := 0;
                        Execute_Packed_Index (Frame, N, Undef, Index_Offset);
                        if Undef then
                           Value := No_Data_Ptr;
                           Update := null;
                           Noffset := 0;
                           Width := 0;
                           return;
                        end if;

                        Width := Get_Type_Width (Ntype);
                        Noffset := Pfx_Offset
                          + Bit_Offset (Index_Offset * Uns32 (Width));

                        if Pfx_Update = null then
                           pragma Assert (not Force);
                           Update := null;
                        else
                           if Pfx_Update.Arr = null then
                              if Force then
                                 --  TODO.
                                 raise Program_Error;
                              else
                                 Update := null;
                              end if;
                           else
                              Update := Pfx_Update.Arr.Arr (Noffset);
                              if Update = null and Force then
                                 raise Program_Error;
                              end if;
                           end if;
                        end if;
                     end;
                  else
                     Execute_Name_Nonvec_Internal
                       (Frame, N, Force, Value, Update);
                     Noffset := 0;
                     Doffset := 0;
                     if Value = Null_Address then
                        Width := 0;
                     else
                        Width := Get_Type_Width (Get_Expr_Type (N));
                     end if;
                  end if;
               end;
               return;
            when N_Bit_Select =>
               declare
                  Pfx : constant Node := Get_Name (N);
                  Index_Offset : Uns32;
                  Pfx_Offset : Bit_Offset;
                  Pfx_Doffset : Bit_Offset;
                  Pfx_Width : Width_Type;
                  Undef : Boolean;
                  Pfx_Update : Update_Acc;
               begin
                  Execute_Name_Vector
                    (Frame, Pfx, Force, Value,
                     Pfx_Offset, Pfx_Doffset, Pfx_Width, Pfx_Update);
                  pragma Assert (Pfx_Doffset = 0);
                  Execute_Packed_Index (Frame, N, Undef, Index_Offset);
                  if Undef then
                     Value := No_Data_Ptr;
                     Update := null;
                     Noffset := 0;
                     Width := 0;
                     return;
                  end if;

                  Noffset := Pfx_Offset + Bit_Offset (Index_Offset);
                  Width := 1;
                  Doffset := 0;

                  if Pfx_Update = null then
                     pragma Assert (not Force);
                     Update := null;
                  else
                     if Pfx_Update.Arr = null then
                        if Force then
                           --  TODO.
                           raise Program_Error;
                        else
                           Update := Pfx_Update;
                        end if;
                     else
                        Update := Pfx_Update.Arr.Arr (Noffset);
                        if Update = null and Force then
                           raise Program_Error;
                        end if;
                     end if;
                  end if;

                  return;
               end;
            when N_Part_Select_Cst =>
               Execute_Part_Select_Vector
                 (Frame, N, Value, Noffset, Doffset, Width, Update);
               return;
            when N_Plus_Part_Select_Cst =>
               Execute_Indexed_Part_Select_Vector
                 (Frame, N, True, Value, Noffset, Doffset, Width, Update);
               return;
            when N_Minus_Part_Select_Cst =>
               Execute_Indexed_Part_Select_Vector
                 (Frame, N, False, Value, Noffset, Doffset, Width, Update);
               return;
            when N_Property_Name =>
               declare
                  Prop : constant Node := Get_Declaration (N);
               begin
                  Execute_Property_Name
                    (Frame, Get_Name (N), Prop, Value, Update);
                  Width := Compute_Width (Get_Type_Data_Type (Prop));
                  Noffset := 0;
                  Doffset := 0;
                  return;
               end;
            when N_This_Name =>
               declare
                  Prop : constant Node := Get_Declaration (N);
               begin
                  Execute_Property_Name
                    (Frame, Get_This_Declaration (N), Prop, Value, Update);
                  Width := Compute_Width (Get_Type_Data_Type (Prop));
                  Noffset := 0;
                  Doffset := 0;
                  return;
               end;
            when N_Associative_Index =>
               Value := Execute_Associative_Index (Frame, N);
               Update := null;
               Width := Compute_Width (Get_Expr_Type (N));
               Noffset := 0;
               Doffset := 0;
               return;
            when others =>
               Error_Kind ("execute_name_vector", N);
         end case;
      end loop;
   end Execute_Name_Vector;

   procedure Execute_Name_Nonvec (Frame : Frame_Ptr;
                                  Name : Node;
                                  Force : Boolean;
                                  Value : out Data_Ptr;
                                  Update : out Update_Acc) is
   begin
      --  The result cannot be a packed type; execute_name_vector must be
      --  used instead.
      Execute_Name_Nonvec_Internal (Frame, Name, Force, Value, Update);
   end Execute_Name_Nonvec;
end Verilog.Executions;
