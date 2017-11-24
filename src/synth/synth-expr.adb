--  Expressions synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Std_Names;
with Ieee.Std_Logic_1164;
with Std_Package;
with Errorout; use Errorout;
with Simul.Execution;
with Grt.Types; use Grt.Types;

with Synth.Errors; use Synth.Errors;
with Synth.Context; use Synth.Context;
with Synth.Types; use Synth.Types;

with Netlists; use Netlists;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;

package body Synth.Expr is
   function Is_Const (Val : Value_Acc) return Boolean is
   begin
      return Val.Kind = Value_Lit;
   end Is_Const;

   function Get_Width (Val : Value_Acc) return Uns32 is
   begin
      case Val.Kind is
         when Value_Lit =>
            if Is_Bit_Type (Val.Lit_Type) then
               return 1;
            else
               raise Internal_Error;
            end if;
         when Value_Wire
           | Value_Net =>
            return Get_Width (Get_Net (Val));
         when others =>
            raise Internal_Error; --  TODO
      end case;
   end Get_Width;

   procedure To_Logic (Lit : Iir_Value_Literal_Acc;
                       Val : out Uns32;
                       Xz : out Uns32) is
   begin
      case Lit.Kind is
         when Iir_Value_B1 =>
            Xz := 0;
            Val := Ghdl_B1'Pos (Lit.B1);
         when Iir_Value_E8 =>
            --  Std_logic.
            case Lit.E8 is
               when Ieee.Std_Logic_1164.Std_Logic_0_Pos
                 |  Ieee.Std_Logic_1164.Std_Logic_L_Pos =>
                  Val := 0;
                  Xz := 0;
               when Ieee.Std_Logic_1164.Std_Logic_1_Pos
                 |  Ieee.Std_Logic_1164.Std_Logic_H_Pos =>
                  Val := 1;
                  Xz := 0;
               when Ieee.Std_Logic_1164.Std_Logic_U_Pos
                 |  Ieee.Std_Logic_1164.Std_Logic_X_Pos
                 |  Ieee.Std_Logic_1164.Std_Logic_D_Pos =>
                  Val := 0;
                  Xz := 1;
               when Ieee.Std_Logic_1164.Std_Logic_Z_Pos
                 |  Ieee.Std_Logic_1164.Std_Logic_W_Pos =>
                  Val := 1;
                  Xz := 1;
               when others =>
                  --  Only 9 values.
                  raise Internal_Error;
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end To_Logic;

   function Bit_Extract (Val : Value_Acc; Off : Uns32) return Value_Acc is
   begin
      case Val.Kind is
         when Value_Lit =>
            declare
               Lit : constant Iir_Value_Literal_Acc := Val.Lit;
            begin
               pragma Assert (Lit.Kind = Iir_Value_Array);
               pragma Assert (Lit.Bounds.Nbr_Dims = 1);
               pragma Assert (Lit.Bounds.D (1).Length >= Iir_Index32 (Off));
               return Create_Value_Lit
                 (Lit.Val_Array.V (Lit.Val_Array.Len - Iir_Index32 (Off)),
                  Get_Element_Subtype (Val.Lit_Type));
            end;
         when Value_Net
           | Value_Wire =>
            return Create_Value_Net
              (Build_Extract_Bit (Build_Context, Get_Net (Val), Off),
               No_Range);
         when others =>
            raise Internal_Error;
      end case;
   end Bit_Extract;

   function Synth_Uresize (Val : Value_Acc; W : Width) return Net
   is
      N : constant Net := Get_Net (Val);
      Wn : constant Width := Get_Width (N);
   begin
      if Wn > W then
         return Build_Trunc (Build_Context, Id_Utrunc, N, W);
      elsif Wn < W then
         return Build_Extend (Build_Context, Id_Uextend, N, W);
      else
         return N;
      end if;
   end Synth_Uresize;

   procedure Fill_Array_Aggregate
     (Syn_Inst : Synth_Instance_Acc;
      Aggr : Iir;
      Res : Value_Acc;
      Dim : Iir_Index32;
      Orig : Iir_Index32;
      Stride : Iir_Index32)
   is
      Bound : constant Iir_Value_Literal_Acc := Res.Bounds.D (Dim);
      Value : Iir;
      Assoc : Iir;
      Pos : Iir_Index32;

      procedure Set_Elem (Pos : Iir_Index32)
      is
         Val : Value_Acc;
      begin
         if Dim = Res.Bounds.Nbr_Dims then
            Val := Synth_Expression_With_Type
              (Syn_Inst, Value, Get_Element_Subtype (Get_Type (Aggr)));
            Res.Arr.V (Orig + Stride * Pos) := Val;
         else
            Error_Msg_Synth (+Assoc, "multi-dim aggregate not handled");
         end if;
      end Set_Elem;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := 0;
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  if Pos >= Bound.Length then
                     Error_Msg_Synth (+Assoc, "element out of array bound");
                  else
                     Set_Elem (Pos);
                  end if;
                  Pos := Pos + 1;
               when others =>
                  Error_Msg_Synth
                    (+Assoc, "unhandled association form");
            end case;
            Assoc := Get_Chain (Assoc);
            exit when Is_Null (Assoc);
            exit when not Get_Same_Alternative_Flag (Assoc);
         end loop;
      end loop;
   end Fill_Array_Aggregate;

   type Net_Array is array (Iir_Index32 range <>) of Net;
   type Net_Array_Acc is access Net_Array;
   procedure Free_Net_Array is new Ada.Unchecked_Deallocation
     (Net_Array, Net_Array_Acc);

   --  Convert the one-dimension VAL to a net.
   function Vectorize_Array (Val : Value_Acc) return Value_Acc
   is
      Arr : Net_Array_Acc;
      Len : Iir_Index32;
      Idx, New_Idx : Iir_Index32;
      Res : Value_Acc;
   begin
      Len := Val.Arr.Len;

      --  Dynamically allocate ARR to handle large arrays.
      Arr := new Net_Array (1 .. Len);
      for I in Arr'Range loop
         Arr (I) := Get_Net (Val.Arr.V (I));
      end loop;

      while Len > 1 loop
         Idx := 1;
         New_Idx := 0;
         while Idx <= Len loop
            --  Gather at most 4 nets.
            New_Idx := New_Idx + 1;

            if Idx = Len then
               Arr (New_Idx) := Arr (Idx);
               Idx := Idx + 1;
            elsif Idx + 1 = Len then
               Arr (New_Idx) := Build_Concat2
                 (Build_Context, Arr (Idx), Arr (Idx + 1));
               Idx := Idx + 2;
            elsif Idx + 2 = Len then
               Arr (New_Idx) := Build_Concat3
                 (Build_Context, Arr (Idx), Arr (Idx + 1), Arr (Idx + 2));
               Idx := Idx + 3;
            else
               Arr (New_Idx) := Build_Concat4
                 (Build_Context,
                  Arr (Idx), Arr (Idx + 1), Arr (Idx + 2), Arr (Idx + 3));
               Idx := Idx + 4;
            end if;
         end loop;
         Len := New_Idx;
      end loop;

      Res := Create_Value_Net (Arr (1), Bounds_To_Range (Val.Bounds.D (1)));

      Free_Net_Array (Arr);

      return Res;
   end Vectorize_Array;

   function Synth_Aggregate (Syn_Inst : Synth_Instance_Acc;
                             Aggr : Iir;
                             Aggr_Type : Iir) return Value_Acc is
   begin
      case Get_Kind (Aggr_Type) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            declare
               Bnd : Iir_Value_Literal_Acc;
               Res : Value_Acc;
            begin
               --  Create bounds.
               Bnd := Simul.Execution.Create_Array_Bounds_From_Type
                 (Syn_Inst.Sim, Aggr_Type, False);
               --  Allocate result
               Res := Create_Array_Value (Bnd.Bounds);
               Create_Array_Data (Res);
               Fill_Array_Aggregate
                 (Syn_Inst, Aggr, Res,
                  1, 1, Res.Arr.Len / Res.Bounds.D (1).Length);
               if Is_Vector_Type (Aggr_Type) then
                  --  Vectorize
                  Res := Vectorize_Array (Res);
               end if;
               return Res;
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            raise Internal_Error;
         when others =>
            Error_Kind ("synth_aggregate", Aggr_Type);
      end case;
   end Synth_Aggregate;

   function Synth_Bit_Eq_Const (Cst : Value_Acc; Expr : Value_Acc; Loc : Iir)
                               return Value_Acc
   is
      pragma Unreferenced (Loc);
      Val : Uns32;
      Xz : Uns32;
   begin
      To_Logic (Cst.Lit, Val, Xz);
      if Xz /= 0 then
         return Create_Value_Net
           (Build_Const_UL32 (Build_Context, 0, 1, 1), No_Range);
      elsif Val = 1 then
         return Expr;
      else
         pragma Assert (Val = 0);
         return Create_Value_Net
           (Build_Monadic (Build_Context, Id_Not, Get_Net (Expr)), No_Range);
      end if;
   end Synth_Bit_Eq_Const;

   function Extract_Range (Val : Value_Acc) return Value_Range_Acc is
   begin
      case Val.Kind is
         when Value_Net =>
            return Val.N_Range;
         when Value_Wire =>
            return Val.W_Range;
         when others =>
            raise Internal_Error;
      end case;
   end Extract_Range;

   --  Create the result range of an operator.  According to the ieee standard,
   --  the range is LEN-1 downto 0.
   function Create_Res_Range (Prev : Value_Acc; N : Net)
                             return Value_Range_Acc
   is
      Res : Value_Range_Acc;
      Wd : Width;
   begin
      case Prev.Kind is
         when Value_Net
           | Value_Wire =>
            Res := Extract_Range (Prev);
         when Value_Lit =>
            Res := No_Range;
         when others =>
            raise Internal_Error;
      end case;

      if Res /= No_Range
        and then Res.Dir = Iir_Downto
        and then Res.Right = 0
      then
         --  Normalized range
         return Res;
      end if;

      Wd := Get_Width (N);
      return Create_Range_Value ((Iir_Downto, Wd, Int32 (Wd - 1), 0));
   end Create_Res_Range;

   function Synth_Dyadic_Operation (Def : Iir_Predefined_Functions;
                                    Left : Value_Acc;
                                    Right : Value_Acc;
                                    Loc : Iir) return Value_Acc
   is
      function Synth_Bit_Dyadic (Id : Dyadic_Module_Id) return Value_Acc is
      begin
         return Create_Value_Net
           (Build_Dyadic (Build_Context, Id, Get_Net (Left), Get_Net (Right)),
           No_Range);
      end Synth_Bit_Dyadic;

      --  function Synth_Vec_Dyadic (Id : Dyadic_Module_Id) return Value_Acc
      --  is
      --     L : constant Net := Get_Net (Left);
      --  begin
      --     return Create_Value_Net
      --       (Build_Dyadic (Build_Context, Id, L, Get_Net (Right)),
      --        Create_Res_Range (Left, L));
      --  end Synth_Vec_Dyadic;
   begin
      case Def is
         when Iir_Predefined_Error =>
            return null;

         when Iir_Predefined_Bit_And
           | Iir_Predefined_Boolean_And
           | Iir_Predefined_Ieee_1164_Scalar_And =>
            return Synth_Bit_Dyadic (Id_And);
         when Iir_Predefined_Bit_Xor
           | Iir_Predefined_Ieee_1164_Scalar_Xor =>
            return Synth_Bit_Dyadic (Id_Xor);
         when Iir_Predefined_Bit_Or
           | Iir_Predefined_Ieee_1164_Scalar_Or =>
            return Synth_Bit_Dyadic (Id_Or);

         when Iir_Predefined_Bit_Nor
           | Iir_Predefined_Ieee_1164_Scalar_Nor =>
            return Synth_Bit_Dyadic (Id_Nor);
         when Iir_Predefined_Bit_Nand
           | Iir_Predefined_Ieee_1164_Scalar_Nand =>
            return Synth_Bit_Dyadic (Id_Nand);
         when Iir_Predefined_Bit_Xnor
           | Iir_Predefined_Ieee_1164_Scalar_Xnor =>
            return Synth_Bit_Dyadic (Id_Xnor);

         when Iir_Predefined_Enum_Equality =>
            if Get_Width (Left) = 1 then
               if Is_Const (Left) then
                  return Synth_Bit_Eq_Const (Left, Right, Loc);
               elsif Is_Const (Right) then
                  return Synth_Bit_Eq_Const (Right, Left, Loc);
               end if;
            end if;
            --  TODO
            Error_Msg_Synth (+Loc, "unsupported enum equality");
            raise Internal_Error;
         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat =>
            --  "+" (Unsigned, Natural)
            declare
               L : constant Net := Get_Net (Left);
            begin
               return Create_Value_Net
                 (Build_Dyadic (Build_Context, Id_Add,
                                L,
                                Synth_Uresize (Right, Get_Width (Left))),
                  Create_Res_Range (Left, L));
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat =>
            --  "=" (Unsigned, Natural)
            return Create_Value_Net
              (Build_Compare (Build_Context, Id_Eq,
                              Get_Net (Left),
                              Synth_Uresize (Right, Get_Width (Left))),
               No_Range);
         when others =>
            Error_Msg_Synth
              (+Loc,
               "unhandled dyadic: " & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Dyadic_Operation;

   function Synth_Monadic_Operation (Def : Iir_Predefined_Functions;
                                     Operand : Value_Acc;
                                     Loc : Iir) return Value_Acc
   is
      function Synth_Bit_Monadic (Id : Monadic_Module_Id) return Value_Acc is
      begin
         return Create_Value_Net
           (Build_Monadic (Build_Context, Id, Get_Net (Operand)),
            No_Range);
      end Synth_Bit_Monadic;
   begin
      case Def is
         when Iir_Predefined_Error =>
            return null;
         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Synth_Bit_Monadic (Id_Not);
         when others =>
            Error_Msg_Synth
              (+Loc,
               "unhandled monadic: " & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Monadic_Operation;

   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Iir)
                       return Value_Acc is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration =>
            return Get_Value (Syn_Inst, Name);
         when others =>
            Error_Kind ("synth_name", Name);
      end case;
   end Synth_Name;

   function In_Range (Rng : Value_Range_Acc; V : Int32) return Boolean is
   begin
      case Rng.Dir is
         when Iir_To =>
            return V >= Rng.Left and then V <= Rng.Right;
         when Iir_Downto =>
            return V <= Rng.Left and then V >= Rng.Right;
      end case;
   end In_Range;

   function Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc; Name : Iir)
                               return Value_Acc
   is
      Pfx : constant Value_Acc :=
        Synth_Expression (Syn_Inst, Get_Prefix (Name));
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      Idx_Val : constant Value_Acc :=
        Synth_Expression (Syn_Inst, Get_Nth_Element (Indexes, 0));
      Rng : Value_Range_Acc;
      Idx : Int32;
   begin
      if Get_Nbr_Elements (Indexes) /= 1 then
         Error_Msg_Synth (+Name, "multi-dim arrays not supported");
         return null;
      end if;

      if Idx_Val.Kind /= Value_Lit
        or else Idx_Val.Lit.Kind /= Iir_Value_I64
      then
         Error_Msg_Synth (+Name, "non constant integer index not supported");
         return null;
      end if;

      Rng := Extract_Range (Pfx);
      Idx := Int32 (Idx_Val.Lit.I64);
      if not In_Range (Rng, Idx) then
         Error_Msg_Synth (+Name, "index not within bounds");
         return null;
      end if;

      case Rng.Dir is
         when Iir_To =>
            return Bit_Extract (Pfx, Uns32 (Rng.Right - Idx));
         when Iir_Downto =>
            return Bit_Extract (Pfx, Uns32 (Idx - Rng.Right));
      end case;
   end Synth_Indexed_Name;

   --  Match: clk_signal_name'event
   --  and return clk_signal_name.
   function Extract_Event_Expr_Prefix (Expr : Iir) return Iir is
   begin
      if Get_Kind (Expr) = Iir_Kind_Event_Attribute then
         return Get_Prefix (Expr);
      else
         return Null_Iir;
      end if;
   end Extract_Event_Expr_Prefix;

   function Is_Same_Node (Left, Right : Iir) return Boolean is
   begin
      if Get_Kind (Left) /= Get_Kind (Right) then
         return False;
      end if;
      case Get_Kind (Left) is
         when Iir_Kind_Simple_Name =>
            return Get_Named_Entity (Left) = Get_Named_Entity (Right);
         when others =>
            Error_Kind ("is_same_node", Left);
      end case;
   end Is_Same_Node;

   --  Match: clk_signal_name = '1' | clk_signal_name = '0'
   function Extract_Clock_Level
     (Syn_Inst : Synth_Instance_Acc; Expr : Iir; Prefix : Iir) return Net
   is
      Clk : Net;
      Imp : Iir;
      Left, Right : Iir;
      Lit : Iir;
      Posedge : Boolean;
   begin
      Clk := Get_Net (Synth_Name (Syn_Inst, Prefix));
      if Get_Kind (Expr) /= Iir_Kind_Equality_Operator then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, True, Clk);
      end if;
      Imp := Get_Implementation (Expr);
      if Get_Implicit_Definition (Imp) /= Iir_Predefined_Enum_Equality then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, True, Clk);
      end if;
      Left := Get_Left (Expr);
      Right := Get_Right (Expr);
      if Get_Kind (Right) /= Iir_Kind_Character_Literal then
         Error_Msg_Synth
           (+Expr, "ill-formed clock-level, '0' or '1' expected");
         return Build_Edge (Build_Context, True, Clk);
      end if;
      Lit := Get_Named_Entity (Right);
      if Lit = Std_Package.Bit_0
        or else Lit = Ieee.Std_Logic_1164.Std_Ulogic_0
      then
         Posedge := False;
      elsif Lit = Std_Package.Bit_1
        or else Lit = Ieee.Std_Logic_1164.Std_Ulogic_1
      then
         Posedge := True;
      else
         Error_Msg_Synth
           (+Lit, "ill-formed clock-level, '0' or '1' expected");
         Posedge := True;
      end if;
      if not Is_Same_Node (Prefix, Left) then
         Error_Msg_Synth
           (+Left, "clock signal name doesn't match");
      end if;
      return Build_Edge (Build_Context, Posedge, Clk);
   end Extract_Clock_Level;

   function Synth_Clock_Edge (Syn_Inst : Synth_Instance_Acc; Expr : Iir)
                             return Value_Acc
   is
      pragma Assert (Get_Kind (Expr) = Iir_Kind_And_Operator);
      Left : constant Iir := Get_Left (Expr);
      Right : constant Iir := Get_Right (Expr);
      Prefix : Iir;
   begin
      --  Try with left.
      Prefix := Extract_Event_Expr_Prefix (Left);
      if Is_Valid (Prefix) then
         return Create_Value_Net
           (Extract_Clock_Level (Syn_Inst, Right, Prefix), No_Range);
      end if;

      --  Try with right.
      Prefix := Extract_Event_Expr_Prefix (Right);
      if Is_Valid (Prefix) then
         return Create_Value_Net
           (Extract_Clock_Level (Syn_Inst, Left, Prefix), No_Range);
      end if;

      return null;
   end Synth_Clock_Edge;

   function Synth_Type_Conversion (Syn_Inst : Synth_Instance_Acc; Conv : Iir)
                                  return Value_Acc
   is
      Expr : constant Iir := Get_Expression (Conv);
      Val : Value_Acc;
   begin
      Val := Synth_Expression (Syn_Inst, Expr);
      if Is_Vector_Type (Get_Type (Conv)) then
         return Val;
      else
         Error_Msg_Synth (+Conv, "unhandled type conversion");
         return Val;
      end if;
   end Synth_Type_Conversion;

   function Synth_Assoc_In (Syn_Inst : Synth_Instance_Acc;
                            Assoc : Iir) return Value_Acc is
   begin
      if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression then
         return Synth_Expression (Syn_Inst, Get_Actual (Assoc));
      else
         Error_Kind ("synth_assoc_in", Assoc);
      end if;
   end Synth_Assoc_In;

   procedure Error_Unknown_Operator (Imp : Iir; Loc : Iir) is
   begin
      if Get_Kind (Get_Parent (Imp)) = Iir_Kind_Package_Declaration
        and then (Get_Identifier
                    (Get_Library
                       (Get_Design_File (Get_Design_Unit (Get_Parent (Imp)))))
                    = Std_Names.Name_Ieee)
      then
         Error_Msg_Synth (+Loc, "unhandled predefined IEEE operator %i", +Imp);
         Error_Msg_Synth (+Imp, " declared here");
      else
         Error_Msg_Synth (+Loc, "user defined operator %i not handled", +Imp);
      end if;
   end Error_Unknown_Operator;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Iir; Expr_Type : Iir)
     return Value_Acc is
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Imp : constant Iir := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Left : Value_Acc;
               Right : Value_Acc;
            begin
               --  Match clock-edge
               if Def = Iir_Predefined_Boolean_And then
                  Left := Synth_Clock_Edge (Syn_Inst, Expr);
                  if Left /= null then
                     return Left;
                  end if;
               end if;

               Left := Synth_Expression (Syn_Inst, Get_Left (Expr));
               Right := Synth_Expression (Syn_Inst, Get_Right (Expr));
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Dyadic_Operation (Def, Left, Right, Expr);
               else
                  Error_Unknown_Operator (Imp, Expr);
                  return Left;
               end if;
            end;
         when Iir_Kinds_Monadic_Operator =>
            declare
               Imp : constant Iir := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Operand : Value_Acc;
            begin
               Operand := Synth_Expression (Syn_Inst, Get_Operand (Expr));
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Monadic_Operation (Def, Operand, Expr);
               else
                  Error_Unknown_Operator (Imp, Expr);
                  return Operand;
               end if;
            end;
         when Iir_Kind_Simple_Name =>
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Indexed_Name =>
            return Synth_Indexed_Name (Syn_Inst, Expr);
         when Iir_Kind_Character_Literal
           | Iir_Kind_Integer_Literal
           | Iir_Kind_String_Literal8 =>
            return Create_Value_Lit
              (Simul.Execution.Execute_Expression (Syn_Inst.Sim, Expr),
               Get_Base_Type (Get_Type (Expr)));
         when Iir_Kind_Type_Conversion =>
            return Synth_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr), Get_Type (Expr));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Iir := Get_Implementation (Expr);
               Clk : Net;
            begin
               if Imp = Ieee.Std_Logic_1164.Rising_Edge then
                  Clk := Get_Net
                    (Synth_Assoc_In
                       (Syn_Inst, Get_Parameter_Association_Chain (Expr)));
                  return Create_Value_Net
                    (Build_Edge (Build_Context, True, Clk), No_Range);
               end if;
               Error_Msg_Synth
                 (+Expr, "user function call to %i is not handled", +Imp);
            end;
         when Iir_Kind_Aggregate =>
            return Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
         when others =>
            Error_Kind ("synth_expression", Expr);
      end case;
      return null;
   end Synth_Expression_With_Type;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Iir)
                             return Value_Acc is
   begin
      return Synth_Expression_With_Type (Syn_Inst, Expr, Get_Type (Expr));
   end Synth_Expression;

end Synth.Expr;
