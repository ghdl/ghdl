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
with Types_Utils; use Types_Utils;
with Std_Names;
with Str_Table;
with Mutils; use Mutils;
with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;

with Areapools;
with Vhdl.Annotations; use Vhdl.Annotations;

with Synth.Errors; use Synth.Errors;
with Synth.Types; use Synth.Types;
with Synth.Stmts; use Synth.Stmts;
with Synth.Decls;
with Synth.Environment; use Synth.Environment;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;

package body Synth.Expr is
   function Is_Const (Val : Value_Acc) return Boolean is
   begin
      case Val.Kind is
         when Value_Discrete =>
            return True;
         when Value_Net
           | Value_Wire
           | Value_Mux2 =>
            return False;
         when others =>
            --  TODO.
            raise Internal_Error;
      end case;
   end Is_Const;

   function Is_Float (Val : Value_Acc) return Boolean is
   begin
      return Val.Kind = Value_Float;
   end Is_Float;

   function Get_Width (Val : Value_Acc) return Uns32 is
   begin
      case Val.Kind is
         when Value_Wire
           | Value_Net =>
            return Get_Width (Get_Net (Val));
         when others =>
            raise Internal_Error; --  TODO
      end case;
   end Get_Width;

   procedure Set_Location2 (N : Net; Loc : Node) is
   begin
      Set_Location (Get_Net_Parent (N), Get_Location (Loc));
   end Set_Location2;

   procedure Set_Location (N : Net; Loc : Node) is
   begin
      --  Short and compact code as it is inlined.
      if Flag_Locations then
         Set_Location2 (N, Loc);
      end if;
   end Set_Location;

   procedure From_Std_Logic (Enum : Int64; Val : out Uns32; Zx : out Uns32) is
   begin
      case Enum is
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_0_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_L_Pos =>
            Val := 0;
            Zx := 0;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_1_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_H_Pos =>
            Val := 1;
            Zx := 0;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_U_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_X_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_D_Pos =>
            Val := 1;
            Zx := 1;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_Z_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_W_Pos =>
            Val := 0;
            Zx := 1;
         when others =>
            --  Only 9 values.
            raise Internal_Error;
      end case;
   end From_Std_Logic;

   procedure From_Bit (Enum : Int64; Val : out Uns32) is
   begin
      if Enum = 0 then
         Val := 0;
      elsif Enum = 1 then
         Val := 1;
      else
         raise Internal_Error;
      end if;
   end From_Bit;

   procedure To_Logic
     (Enum : Int64; Etype : Type_Acc; Val : out Uns32; Zx  : out Uns32) is
   begin
      if Etype = Logic_Type then
         From_Std_Logic (Enum, Val, Zx);
      elsif Etype = Boolean_Type or Etype = Bit_Type then
         From_Bit (Enum, Val);
         Zx := 0;
      else
         raise Internal_Error;
      end if;
   end To_Logic;

   function Bit_Extract (Val : Value_Acc; Off : Uns32; Loc : Node)
                        return Value_Acc
   is
      N : Net;
   begin
      case Val.Kind is
         when Value_Array =>
            pragma Assert (Val.Typ.Vbound.Len >= Off);
            return Val.Arr.V (Iir_Index32 (Val.Typ.Vbound.Len - Off));
         when Value_Net
           | Value_Wire =>
            N := Build_Extract_Bit (Build_Context, Get_Net (Val), Off);
            Set_Location (N, Loc);
            return Create_Value_Net (N, Val.Typ.Vec_El);
         when others =>
            raise Internal_Error;
      end case;
   end Bit_Extract;

   function Synth_Uresize (N : Net; W : Width) return Net
   is
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

   function Synth_Uresize (Val : Value_Acc; Vtype : Node; W : Width)
                          return Net is
   begin
      if Is_Const (Val)
        and then (Get_Kind (Get_Base_Type (Vtype))
                    = Iir_Kind_Integer_Type_Definition)
      then
         return Build_Const_UB32
           (Build_Context, Uns32 (Val.Scal), W);
      end if;
      return Synth_Uresize (Get_Net (Val), W);
   end Synth_Uresize;

   --  Resize for a discrete value.
   function Synth_Resize (Val : Value_Acc; W : Width; Loc : Node) return Net
   is
      Wn : constant Width := Val.Typ.Drange.W;
      N : Net;
      Res : Net;
   begin
      if Is_Const (Val) then
         if Wn /= W then
            raise Internal_Error;
         end if;
      end if;

      N := Get_Net (Val);
      if Wn > W then
         Res := Build_Trunc (Build_Context, Id_Utrunc, N, W);
         Set_Location (Res, Loc);
         return Res;
      elsif Wn < W then
         if Val.Typ.Drange.Is_Signed then
            Res := Build_Extend (Build_Context, Id_Sextend, N, W);
         else
            Res := Build_Extend (Build_Context, Id_Uextend, N, W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      else
         return N;
      end if;
   end Synth_Resize;


   function Get_Index_Offset
     (Index : Value_Acc; Bounds : Bound_Type; Expr : Iir) return Uns32 is
   begin
      if Index.Kind = Value_Discrete then
         declare
            Left : constant Int64 := Int64 (Bounds.Left);
            Right : constant Int64 := Int64 (Bounds.Right);
         begin
            case Bounds.Dir is
               when Iir_To =>
                  if Index.Scal >= Left and then Index.Scal <= Right then
                     -- to
                     return Uns32 (Index.Scal - Left);
                  end if;
               when Iir_Downto =>
                  if Index.Scal <= Left and then Index.Scal >= Right then
                     -- downto
                     return Uns32 (Left - Index.Scal);
                  end if;
            end case;
         end;
      else
         raise Internal_Error;
      end if;
      Error_Msg_Synth (+Expr, "index out of bounds");
      return 0;
   end Get_Index_Offset;

   procedure Fill_Array_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Res : Value_Acc;
                                   Dim : Natural)
   is
      Bound : constant Bound_Type := Res.Typ.Abounds.D (1);
      Aggr_Type : constant Node := Get_Type (Aggr);
      El_Type : constant Node := Get_Element_Subtype (Aggr_Type);
      Nbr_Dims : constant Natural := Get_Nbr_Dimensions (Aggr_Type);
      Idx_Type : constant Node := Get_Index_Type (Aggr_Type, Dim);
      type Boolean_Array is array (Uns32 range <>) of Boolean;
      pragma Pack (Boolean_Array);
      Is_Set : Boolean_Array (0 .. Bound.Len - 1);
      Value : Node;
      Assoc : Node;
      Pos : Uns32;

      procedure Set_Elem (Pos : Uns32)
      is
         Val : Value_Acc;
      begin
         if Dim = Nbr_Dims - 1 then
            Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Type);
            Res.Arr.V (Iir_Index32 (Pos + 1)) := Val;
            pragma Assert (not Is_Set (Pos));
            Is_Set (Pos) := True;
         else
            Error_Msg_Synth (+Assoc, "multi-dim aggregate not handled");
         end if;
      end Set_Elem;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := 0;
      Is_Set := (others => False);
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  if Pos >= Bound.Len then
                     Error_Msg_Synth (+Assoc, "element out of array bound");
                  else
                     Set_Elem (Pos);
                  end if;
                  Pos := Pos + 1;
               when Iir_Kind_Choice_By_Others =>
                  while Pos < Bound.Len loop
                     if not Is_Set (Pos) then
                        Set_Elem (Pos);
                     end if;
                     Pos := Pos + 1;
                  end loop;
               when Iir_Kind_Choice_By_Expression =>
                  declare
                     Ch : constant Node := Get_Choice_Expression (Assoc);
                     Idx : Value_Acc;
                  begin
                     Idx := Synth_Expression_With_Type
                       (Syn_Inst, Ch, Get_Base_Type (Idx_Type));
                     if not Is_Const (Idx) then
                        Error_Msg_Synth (+Ch, "choice is not static");
                     else
                        Set_Elem (Get_Index_Offset (Idx, Bound, Ch));
                     end if;
                  end;
               when Iir_Kind_Choice_By_Range =>
                  declare
                     Ch : constant Node := Get_Choice_Range (Assoc);
                     Rng : Discrete_Range_Type;
                     Val : Value_Acc;
                  begin
                     Rng := Synth_Discrete_Range_Expression (Syn_Inst, Ch);
                     Val := Create_Value_Discrete
                       (Rng.Left,
                        Get_Value_Type (Syn_Inst, Get_Type (Ch)));
                     while In_Range (Rng, Val.Scal) loop
                        Set_Elem (Get_Index_Offset (Val, Bound, Ch));
                        Update_Index (Rng, Val.Scal);
                     end loop;
                  end;
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

   procedure Concat_Array (Arr : in out Net_Array)
   is
      Last : Int32;
      Idx, New_Idx : Int32;
   begin
      Last := Arr'Last;
      while Last > Arr'First loop
         Idx := Arr'First;
         New_Idx := Arr'First - 1;
         while Idx <= Last loop
            --  Gather at most 4 nets.
            New_Idx := New_Idx + 1;

            if Idx = Last then
               Arr (New_Idx) := Arr (Idx);
               Idx := Idx + 1;
            elsif Idx + 1 = Last then
               Arr (New_Idx) := Build_Concat2
                 (Build_Context, Arr (Idx), Arr (Idx + 1));
               Idx := Idx + 2;
            elsif Idx + 2 = Last then
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
         Last := New_Idx;
      end loop;
   end Concat_Array;

   function Concat_Array (Arr : Net_Array_Acc) return Net is
   begin
      Concat_Array (Arr.all);
      return Arr (Arr'First);
   end Concat_Array;

   --  Convert the one-dimension VAL to a net by concatenating.
   function Vectorize_Array (Val : Value_Acc; Etype : Node) return Value_Acc
   is
      Arr : Net_Array_Acc;
      Len : Int32;
      Idx : Iir_Index32;
      Res : Value_Acc;
   begin
      --  Dynamically allocate ARR to handle large arrays.
      Arr := new Net_Array (1 .. Int32 (Val.Arr.Len));

      --  Gather consecutive constant values.
      Idx := Val.Arr.Len;
      Len := 0;
      while Idx > 0 loop
         declare
            W_Zx, B_Zx : Uns32;
            W_Va, B_Va : Uns32;
            Off : Natural;
            E : Net;
         begin
            W_Zx := 0;
            W_Va := 0;
            Off := 0;
            while Idx > 0
              and then Off < 32
              and then Is_Const (Val.Arr.V (Idx))
              and then Is_Bit_Type (Etype)
            loop
               To_Logic (Val.Arr.V (Idx).Scal, Val.Typ.Arr_El, B_Va, B_Zx);
               W_Zx := W_Zx or Shift_Left (B_Zx, Off);
               W_Va := W_Va or Shift_Left (B_Va, Off);
               Off := Off + 1;
               Idx := Idx - 1;
            end loop;
            if Off = 0 then
               E := Get_Net (Val.Arr.V (Idx));
               Idx := Idx - 1;
            else
               if W_Zx = 0 then
                  E := Build_Const_UB32
                    (Build_Context, W_Va, Uns32 (Off));
               else
                  E := Build_Const_UL32
                    (Build_Context, W_Va, W_Zx, Uns32 (Off));
               end if;
            end if;
            Len := Len + 1;
            Arr (Len) := E;
         end;
      end loop;

      Concat_Array (Arr (1 .. Len));
      Res := Create_Value_Net (Arr (1), Val.Typ);

      Free_Net_Array (Arr);

      return Res;
   end Vectorize_Array;

   function Synth_Discrete_Range_Expression
     (L : Int64; R : Int64; Dir : Iir_Direction) return Discrete_Range_Type
   is
      V : Discrete_Range_Type;
      Lo, Hi : Int64;
   begin
      V.Dir := Dir;
      V.Left := L;
      V.Right := R;

      case V.Dir is
         when Iir_To =>
            Lo := V.Left;
            Hi := V.Right;
         when Iir_Downto =>
            Lo := V.Right;
            Hi := V.Left;
      end case;
      if Lo > Hi then
         --  Null range.
         V.Is_Signed := False;
         V.W := 0;
      elsif Lo >= 0 then
         --  Positive.
         V.Is_Signed := False;
         V.W := Width (Clog2 (Uns64 (Hi) + 1));
      elsif Lo = Int64'First then
         --  Handle possible overflow.
         V.Is_Signed := True;
         V.W := 64;
      elsif Hi < 0 then
         --  Negative only.
         V.Is_Signed := True;
         V.W := Width (Clog2 (Uns64 (-Lo))) + 1;
      else
         declare
            Wl : constant Width := Width (Clog2 (Uns64 (-Lo)));
            Wh : constant Width := Width (Clog2 (Uns64 (Hi)));
         begin
            V.Is_Signed := True;
            V.W := Width'Max (Wl, Wh) + 1;
         end;
      end if;
      return V;
   end Synth_Discrete_Range_Expression;

   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type
   is
      L, R : Value_Acc;
   begin
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));

      if not (Is_Const (L) and Is_Const (R)) then
         Error_Msg_Synth (+Rng, "limits of range are not constant");
         raise Internal_Error;
      end if;

      return Synth_Discrete_Range_Expression
        (L.Scal, R.Scal, Get_Direction (Rng));
   end Synth_Discrete_Range_Expression;

   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type
   is
      L, R : Value_Acc;
   begin
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));
      return ((Get_Direction (Rng), L.Fp, R.Fp));
   end Synth_Float_Range_Expression;

   function Synth_Discrete_Range (Syn_Inst : Synth_Instance_Acc; Bound : Node)
                                 return Discrete_Range_Type is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            return Synth_Discrete_Range_Expression (Syn_Inst, Bound);
         when Iir_Kind_Integer_Subtype_Definition =>
            if Get_Type_Declarator (Bound) /= Null_Node then
               --  This is a named subtype, so it has been evaluated.
               return Get_Value_Type (Syn_Inst, Bound).Drange;
            else
               return Synth_Discrete_Range
                 (Syn_Inst, Get_Range_Constraint (Bound));
            end if;
         when others =>
            Error_Kind ("synth_discrete_range", Bound);
      end case;
   end Synth_Discrete_Range;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Natural) return Bound_Type
   is
      Info : constant Sim_Info_Acc := Get_Info (Atype);
   begin
      if Info = null then
         pragma Assert (Get_Type_Declarator (Atype) = Null_Node);
         declare
            Index_Type : constant Node := Get_Index_Type (Atype, Dim);
         begin
            return Synth_Bounds_From_Range (Syn_Inst, Index_Type);
         end;
      else
         declare
            Bnds : constant Value_Acc := Get_Value (Syn_Inst, Atype);
         begin
            case Bnds.Typ.Kind is
               when Type_Vector =>
                  pragma Assert (Dim = 0);
                  return Bnds.Typ.Vbound;
               when Type_Array =>
                  return Bnds.Typ.Abounds.D (Iir_Index32 (Dim + 1));
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end if;
   end Synth_Array_Bounds;

   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Bound_Type
   is
      Rng : Discrete_Range_Type;
      Len : Int64;
   begin
      Rng := Synth_Discrete_Range (Syn_Inst, Atype);
      case Rng.Dir is
         when Iir_To =>
            Len := Rng.Right - Rng.Left + 1;
         when Iir_Downto =>
            Len := Rng.Left - Rng.Right + 1;
      end case;
      if Len < 0 then
         Len := 0;
      end if;
      return (Dir => Rng.Dir,
              Wlen => Width (Clog2 (Uns64 (Len))),
              Wbounds => Rng.W,
              Left => Int32 (Rng.Left), Right => Int32 (Rng.Right),
              Len => Uns32 (Len));
   end Synth_Bounds_From_Range;

   function Synth_Aggregate_Array (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Aggr_Type : Node) return Value_Acc
   is
      Ndims : constant Natural := Get_Nbr_Dimensions (Aggr_Type);
      El_Type : constant Node := Get_Element_Subtype (Aggr_Type);
      Bnds : Bound_Array_Acc;
      Res_Type : Type_Acc;
      Res : Value_Acc;
   begin
      --  Allocate the result.
      Bnds := Create_Bound_Array (Iir_Index32 (Ndims));
      for I in 1 .. Ndims loop
         Bnds.D (Iir_Index32 (I)) :=
           Synth_Array_Bounds (Syn_Inst, Aggr_Type, I - 1);
      end loop;
      Res_Type := Create_Array_Type
        (Bnds, Get_Value (Syn_Inst, El_Type).Typ);
      Res := Create_Value_Array (Res_Type);

      Fill_Array_Aggregate (Syn_Inst, Aggr, Res, 0);

      if Is_Vector_Type (Aggr_Type) then
         Res := Vectorize_Array (Res, Get_Element_Subtype (Aggr_Type));
      end if;

      return Res;
   end Synth_Aggregate_Array;

   --  Aggr_Type is the type from the context.
   function Synth_Aggregate (Syn_Inst : Synth_Instance_Acc;
                             Aggr : Node;
                             Aggr_Type : Node) return Value_Acc is
   begin
      case Get_Kind (Aggr_Type) is
         when Iir_Kind_Array_Type_Definition =>
            return Synth_Aggregate_Array (Syn_Inst, Aggr, Get_Type (Aggr));
         when Iir_Kind_Array_Subtype_Definition =>
            return Synth_Aggregate_Array (Syn_Inst, Aggr, Aggr_Type);
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            raise Internal_Error;
         when others =>
            Error_Kind ("synth_aggregate", Aggr_Type);
      end case;
   end Synth_Aggregate;

   function Synth_Bit_Eq_Const (Cst : Value_Acc; Expr : Value_Acc; Loc : Node)
                               return Value_Acc
   is
      Val : Uns32;
      Zx : Uns32;
      N : Net;
   begin
      To_Logic (Cst.Scal, Cst.Typ, Val, Zx);
      if Zx /= 0 then
         N := Build_Const_UL32 (Build_Context, 0, 1, 1);
         Set_Location (N, Loc);
         return Create_Value_Net (N, Boolean_Type);
      elsif Val = 1 then
         return Expr;
      else
         pragma Assert (Val = 0);
         N := Build_Monadic (Build_Context, Id_Not, Get_Net (Expr));
         Set_Location (N, Loc);
         return Create_Value_Net (N, Boolean_Type);
      end if;
   end Synth_Bit_Eq_Const;

   --  Create the result range of an operator.  According to the ieee standard,
   --  the range is LEN-1 downto 0.
   function Create_Res_Bound (Prev : Value_Acc; N : Net) return Type_Acc
   is
      Res : Type_Acc;
      Wd : Width;
   begin
      Res := Prev.Typ;

      if Res.Vbound.Dir = Iir_Downto
        and then Res.Vbound.Right = 0
      then
         --  Normalized range
         return Res;
      end if;

      Wd := Get_Width (N);
      return Create_Vec_Type_By_Length (Wd, Res.Vec_El);
   end Create_Res_Bound;

   function Create_Bounds_From_Length
     (Syn_Inst : Synth_Instance_Acc; Atype : Iir; Len : Iir_Index32)
     return Bound_Type
   is
      Res : Bound_Type;
      Index_Bounds : Discrete_Range_Type;
   begin
      Index_Bounds := Synth_Discrete_Range (Syn_Inst, Atype);

      Res := (Left => Int32 (Index_Bounds.Left),
              Right => 0,
              Dir => Index_Bounds.Dir,
              Wbounds => Index_Bounds.W,
              Wlen => Width (Clog2 (Uns64 (Len))),
              Len => Uns32 (Len));

      if Len = 0 then
         --  Special case.
         Res.Right := Res.Left;
         case Index_Bounds.Dir is
            when Iir_To =>
               Res.Left := Res.Right + 1;
            when Iir_Downto =>
               Res.Left := Res.Right - 1;
         end case;
      else
         case Index_Bounds.Dir is
            when Iir_To =>
               Res.Right := Res.Left + Int32 (Len - 1);
            when Iir_Downto =>
               Res.Right := Res.Left - Int32 (Len - 1);
         end case;
      end if;
      return Res;
   end Create_Bounds_From_Length;

   function Synth_Subtype_Conversion
     (Val : Value_Acc; Dtype : Type_Acc; Loc : Source.Syn_Src)
     return Value_Acc
   is
      Vtype : constant Type_Acc := Val.Typ;
   begin
      case Dtype.Kind is
         when Type_Bit =>
            pragma Assert (Vtype.Kind = Type_Bit);
            return Val;
         when Type_Discrete =>
            pragma Assert (Vtype.Kind = Type_Discrete);
            declare
               Vrng : Discrete_Range_Type renames Vtype.Drange;
               Drng : Discrete_Range_Type renames Dtype.Drange;
               N : Net;
            begin
               if Vrng.W > Drng.W then
                  --  Truncate.
                  --  TODO: check overflow.
                  case Val.Kind is
                     when Value_Net
                       | Value_Wire =>
                        N := Get_Net (Val);
                        N := Build_Trunc (Build_Context, Id_Utrunc, N, Drng.W);
                        Set_Location (N, Loc);
                        return Create_Value_Net (N, Dtype);
                     when others =>
                        raise Internal_Error;
                  end case;
               elsif Vrng.W < Drng.W then
                  --  Extend.
                  case Val.Kind is
                     when Value_Discrete =>
                        return Create_Value_Discrete (Val.Scal, Dtype);
                     when Value_Net
                       | Value_Wire =>
                        N := Get_Net (Val);
                        if Vrng.Is_Signed then
                           N := Build_Extend
                             (Build_Context, Id_Sextend, N, Drng.W);
                        else
                           N := Build_Extend
                             (Build_Context, Id_Uextend, N, Drng.W);
                        end if;
                        Set_Location (N, Loc);
                        return Create_Value_Net (N, Dtype);
                     when others =>
                        raise Internal_Error;
                  end case;
               else
                  --  TODO: check overflow if sign differ.
                  return Val;
               end if;
            end;
         when Type_Float =>
            pragma Assert (Vtype.Kind = Type_Float);
            --  TODO: check range
            return Val;
         when Type_Vector =>
            --  TODO: check width
            return Val;
         when Type_Array =>
            --  TODO: check bounds, handle elements
            return Val;
         when Type_Unbounded_Array =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or else Vtype.Kind = Type_Array);
            return Val;
         when Type_Record =>
            --  TODO: handle elements.
            return Val;
      end case;
   end Synth_Subtype_Conversion;

   --  Implicit conversion of literals.
   function Synth_Dyadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                    Imp : Node;
                                    Left_Expr : Node;
                                    Right_Expr : Node;
                                    Expr : Node) return Value_Acc
   is
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Inter_Chain : constant Node :=
        Get_Interface_Declaration_Chain (Imp);
      Expr_Type : constant Node := Get_Type (Expr);
      Left_Type : constant Node := Get_Type (Inter_Chain);
      Right_Type : constant Node := Get_Type (Get_Chain (Inter_Chain));
      Left : Value_Acc;
      Right : Value_Acc;

      function Synth_Bit_Dyadic (Id : Dyadic_Module_Id) return Value_Acc
      is
         N : Net;
      begin
         N := Build_Dyadic (Build_Context, Id,
                            Get_Net (Left), Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Left.Typ);
      end Synth_Bit_Dyadic;

      function Synth_Compare (Id : Compare_Module_Id) return Value_Acc
      is
         N : Net;
      begin
         N := Build_Compare (Build_Context, Id,
                             Get_Net (Left), Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Boolean_Type);
      end Synth_Compare;

      function Synth_Compare_Uns_Nat (Id : Compare_Module_Id)
                                     return Value_Acc
      is
         N : Net;
      begin
         N := Synth_Uresize (Right, Right_Type, Get_Width (Left));
         Set_Location (N, Expr);
         N := Build_Compare (Build_Context, Id, Get_Net (Left), N);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Boolean_Type);
      end Synth_Compare_Uns_Nat;

      function Synth_Vec_Dyadic (Id : Dyadic_Module_Id) return Value_Acc
      is
         L : constant Net := Get_Net (Left);
         N : Net;
      begin
         N := Build_Dyadic (Build_Context, Id, L, Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Create_Res_Bound (Left, L));
      end Synth_Vec_Dyadic;

      function Synth_Int_Dyadic (Id : Dyadic_Module_Id) return Value_Acc
      is
         Etype : constant Type_Acc := Get_Value_Type (Syn_Inst, Expr_Type);
         N : Net;
      begin
         N := Build_Dyadic
           (Build_Context, Id, Get_Net (Left), Get_Net (Right));
         Set_Location (N, Expr);
         return Create_Value_Net (N, Etype);
      end Synth_Int_Dyadic;

      function Synth_Dyadic_Uns (Id : Dyadic_Module_Id; Is_Res_Vec : Boolean)
                                return Value_Acc
      is
         L : constant Net := Get_Net (Left);
         R : constant Net := Get_Net (Right);
         W : constant Width := Width'Max (Get_Width (L), Get_Width (R));
         Rtype : Type_Acc;
         L1, R1 : Net;
         N : Net;
      begin
         if Is_Res_Vec then
            Rtype := Create_Vec_Type_By_Length (W, Left.Typ.Vec_El);
         else
            Rtype := Left.Typ;
         end if;
         L1 := Synth_Uresize (L, W);
         Set_Location (L1, Expr);
         R1 := Synth_Uresize (R, W);
         Set_Location (R1, Expr);
         N := Build_Dyadic (Build_Context, Id, L1, R1);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Rtype);
      end Synth_Dyadic_Uns;

      function Synth_Compare_Uns_Uns (Id : Compare_Module_Id)
                                     return Value_Acc
      is
         L : constant Net := Get_Net (Left);
         R : constant Net := Get_Net (Right);
         W : constant Width := Width'Max (Get_Width (L), Get_Width (R));
         L1, R1 : Net;
         N : Net;
      begin
         L1 := Synth_Uresize (L, W);
         Set_Location (L1, Expr);
         R1 := Synth_Uresize (R, W);
         Set_Location (R1, Expr);
         N := Build_Compare (Build_Context, Id, L1, R1);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Boolean_Type);
      end Synth_Compare_Uns_Uns;

      function Synth_Dyadic_Uns_Nat (Id : Dyadic_Module_Id) return Value_Acc
      is
         L : constant Net := Get_Net (Left);
         R1 : Net;
         N : Net;
      begin
         R1 := Synth_Uresize (Right, Right_Type, Get_Width (Left));
         Set_Location (R1, Expr);
         N := Build_Dyadic (Build_Context, Id, L, R1);
         Set_Location (N, Expr);
         return Create_Value_Net (N, Create_Res_Bound (Left, L));
      end Synth_Dyadic_Uns_Nat;
   begin
      Left := Synth_Expression_With_Type (Syn_Inst, Left_Expr, Left_Type);
      Right := Synth_Expression_With_Type (Syn_Inst, Right_Expr, Right_Type);

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
           | Iir_Predefined_Boolean_Or
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

         when Iir_Predefined_Ieee_1164_Vector_And
            | Iir_Predefined_Ieee_Numeric_Std_And_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_And);
         when Iir_Predefined_Ieee_1164_Vector_Or
            | Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Or);
         when Iir_Predefined_Ieee_1164_Vector_Nand
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Nand);
         when Iir_Predefined_Ieee_1164_Vector_Nor
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Nor);
         when Iir_Predefined_Ieee_1164_Vector_Xor
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Xor);
         when Iir_Predefined_Ieee_1164_Vector_Xnor
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Sgn =>
            return Synth_Vec_Dyadic (Id_Xnor);

         when Iir_Predefined_Enum_Equality =>
            if Is_Bit_Type (Left_Type) then
               pragma Assert (Is_Bit_Type (Right_Type));
               if Is_Const (Left) then
                  return Synth_Bit_Eq_Const (Left, Right, Expr);
               elsif Is_Const (Right) then
                  return Synth_Bit_Eq_Const (Right, Left, Expr);
               end if;
            end if;
            return Synth_Compare (Id_Eq);
         when Iir_Predefined_Enum_Inequality =>
            --  TODO: Optimize ?
            return Synth_Compare (Id_Ne);
         when Iir_Predefined_Enum_Less_Equal =>
            return Synth_Compare (Id_Ult);

         when Iir_Predefined_Array_Equality =>
            --  TODO: check size, handle non-vector.
            if Is_Vector_Type (Left_Type) then
               return Synth_Compare (Id_Eq);
            else
               raise Internal_Error;
            end if;
         when Iir_Predefined_Array_Inequality =>
            --  TODO: check size, handle non-vector.
            if Is_Vector_Type (Left_Type) then
               return Synth_Compare (Id_Ne);
            else
               raise Internal_Error;
            end if;
         when Iir_Predefined_Array_Greater =>
            --  TODO: check size, non-vector.
            --  TODO: that's certainly not the correct operator.
            if Is_Vector_Type (Left_Type) then
               return Synth_Compare (Id_Ugt);
            else
               raise Internal_Error;
            end if;

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat =>
            --  "+" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Id_Add);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Sl =>
            --  "+" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns (Id_Add, True);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat =>
            --  "-" (Unsigned, Natural)
            return Synth_Dyadic_Uns_Nat (Id_Sub);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns =>
            --  "-" (Unsigned, Unsigned)
            return Synth_Dyadic_Uns (Id_Sub, True);

         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat =>
            --  "=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Eq);
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Slv =>
            --  "=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Eq);

         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Slv =>
            --  "/=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ne);
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Nat =>
            --  "/=" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ne);

         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat =>
            --  "<" (Unsigned, Natural)
            if Is_Const (Right) and then Right.Scal = 0 then
               --  Always false.
               return Create_Value_Discrete (0, Boolean_Type);
            end if;
            return Synth_Compare_Uns_Nat (Id_Ult);
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Slv =>
            --  "<" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ult);

         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Slv =>
            --  "<=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ule);

         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat =>
            --  ">" (Unsigned, Natural)
            return Synth_Compare_Uns_Nat (Id_Ugt);
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Slv =>
            --  ">" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Ugt);

         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Slv =>
            --  ">=" (Unsigned, Unsigned) [resize]
            return Synth_Compare_Uns_Uns (Id_Uge);

         when Iir_Predefined_Array_Element_Concat =>
            declare
               L : constant Net := Get_Net (Left);
               Bnd : Bound_Type;
               N : Net;
            begin
               N := Build_Concat2 (Build_Context, L, Get_Net (Right));
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Width (L) + 1));

               return Create_Value_Net
                 (N, Create_Vector_Type (Bnd, Right.Typ));
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               R : constant Net := Get_Net (Right);
               Bnd : Bound_Type;
               N : Net;
            begin
               N := Build_Concat2 (Build_Context, Get_Net (Left), R);
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Width (R) + 1));

               return Create_Value_Net
                 (N, Create_Vector_Type (Bnd, Left.Typ));
            end;
         when Iir_Predefined_Element_Element_Concat =>
            declare
               N : Net;
               Bnd : Bound_Type;
            begin
               N := Build_Concat2
                 (Build_Context, Get_Net (Left), Get_Net (Right));
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), 2);
               return Create_Value_Net
                 (N, Create_Vector_Type (Bnd, Left.Typ));
            end;
         when Iir_Predefined_Array_Array_Concat =>
            declare
               L : constant Net := Get_Net (Left);
               R : constant Net := Get_Net (Right);
               Bnd : Bound_Type;
               N : Net;
            begin
               N := Build_Concat2 (Build_Context, L, R);
               Set_Location (N, Expr);
               Bnd := Create_Bounds_From_Length
                 (Syn_Inst,
                  Get_Index_Type (Get_Type (Expr), 0),
                  Iir_Index32 (Get_Width (L) + Get_Width (R)));

               return Create_Value_Net
                 (N, Create_Vector_Type (Bnd, Left.Typ.Vec_El));
            end;
         when Iir_Predefined_Integer_Plus =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Left.Scal + Right.Scal,
                  Get_Value_Type (Syn_Inst, Get_Type (Expr)));
            else
               return Synth_Int_Dyadic (Id_Add);
            end if;
         when Iir_Predefined_Integer_Minus =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Left.Scal - Right.Scal,
                  Get_Value_Type (Syn_Inst, Get_Type (Expr)));
            else
               return Synth_Int_Dyadic (Id_Sub);
            end if;
         when Iir_Predefined_Integer_Mul =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Left.Scal * Right.Scal,
                  Get_Value_Type (Syn_Inst, Get_Type (Expr)));
            else
               return Synth_Int_Dyadic (Id_Mul);
            end if;
         when Iir_Predefined_Integer_Div =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Left.Scal / Right.Scal,
                  Get_Value_Type (Syn_Inst, Get_Type (Expr)));
            else
               Error_Msg_Synth (+Expr, "non-constant division not supported");
               return null;
            end if;
         when Iir_Predefined_Integer_Mod =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Left.Scal mod Right.Scal,
                  Get_Value_Type (Syn_Inst, Get_Type (Expr)));
            else
               Error_Msg_Synth (+Expr, "non-constant mod not supported");
               return null;
            end if;
         when Iir_Predefined_Integer_Less_Equal =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Boolean'Pos (Left.Scal <= Right.Scal), Boolean_Type);
            else
               return Synth_Compare (Id_Sle);
            end if;
         when Iir_Predefined_Integer_Less =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Boolean'Pos (Left.Scal < Right.Scal), Boolean_Type);
            else
               return Synth_Compare (Id_Slt);
            end if;
         when Iir_Predefined_Integer_Greater_Equal =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Boolean'Pos (Left.Scal >= Right.Scal), Boolean_Type);
            else
               return Synth_Compare (Id_Sge);
            end if;
         when Iir_Predefined_Integer_Greater =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Boolean'Pos (Left.Scal > Right.Scal), Boolean_Type);
            else
               return Synth_Compare (Id_Sgt);
            end if;
         when Iir_Predefined_Integer_Equality =>
            if Is_Const (Left) and then Is_Const (Right) then
               return Create_Value_Discrete
                 (Boolean'Pos (Left.Scal = Right.Scal), Boolean_Type);
            else
               return Synth_Compare (Id_Eq);
            end if;

         when others =>
            Error_Msg_Synth (+Expr, "synth_dyadic_operation: unhandled "
                               & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Dyadic_Operation;

   function Synth_Monadic_Operation (Syn_Inst : Synth_Instance_Acc;
                                     Def : Iir_Predefined_Functions;
                                     Operand_Expr : Node;
                                     Loc : Node) return Value_Acc
   is
      Operand : Value_Acc;

      function Synth_Bit_Monadic (Id : Monadic_Module_Id) return Value_Acc
      is
         N : Net;
      begin
         N := Build_Monadic (Build_Context, Id, Get_Net (Operand));
         Set_Location (N, Loc);
         return Create_Value_Net (N, Operand.Typ);
      end Synth_Bit_Monadic;

      function Synth_Vec_Monadic (Id : Monadic_Module_Id) return Value_Acc
      is
         Op: constant Net := Get_Net (Operand);
         N : Net;
      begin
         N := Build_Monadic (Build_Context, Id, Op);
         Set_Location (N, Loc);
         return Create_Value_Net (N, Create_Res_Bound (Operand, Op));
      end Synth_Vec_Monadic;
   begin
      Operand := Synth_Expression (Syn_Inst, Operand_Expr);
      case Def is
         when Iir_Predefined_Error =>
            return null;
         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Synth_Bit_Monadic (Id_Not);
         when Iir_Predefined_Ieee_1164_Vector_Not
            | Iir_Predefined_Ieee_Numeric_Std_Not_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Not_Sgn =>
            return Synth_Vec_Monadic (Id_Not);
         when Iir_Predefined_Ieee_Numeric_Std_Neg_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn =>
            return Synth_Vec_Monadic (Id_Neg);
         when others =>
            Error_Msg_Synth
              (+Loc,
               "unhandled monadic: " & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Monadic_Operation;

   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Value_Acc is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Anonymous_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration =>
            return Get_Value (Syn_Inst, Name);
         when Iir_Kind_Enumeration_Literal =>
            return Create_Value_Discrete
              (Int64 (Get_Enum_Pos (Name)),
               Get_Value_Type (Syn_Inst, Get_Type (Name)));
         when others =>
            Error_Kind ("synth_name", Name);
      end case;
   end Synth_Name;

   function In_Bounds (Bnd : Bound_Type; V : Int32) return Boolean is
   begin
      case Bnd.Dir is
         when Iir_To =>
            return V >= Bnd.Left and then V <= Bnd.Right;
         when Iir_Downto =>
            return V <= Bnd.Left and then V >= Bnd.Right;
      end case;
   end In_Bounds;

   function Index_To_Offset (Bnd : Bound_Type; Idx : Int64; Loc : Node)
                            return Uns32 is
   begin
      if not In_Bounds (Bnd, Int32 (Idx)) then
         Error_Msg_Synth (+Loc, "index not within bounds");
         return 0;
      end if;

      --  The offset is from the LSB (bit 0).  Bit 0 is the rightmost one.
      case Bnd.Dir is
         when Iir_To =>
            return Uns32 (Bnd.Right - Int32 (Idx));
         when Iir_Downto =>
            return Uns32 (Int32 (Idx) - Bnd.Right);
      end case;
   end Index_To_Offset;

   function Dyn_Index_To_Offset
     (Bnd : Bound_Type; Idx_Val : Value_Acc; Loc : Node) return Net
   is
      Idx2 : Net;
      Off : Net;
      Right : Net;
   begin
      Idx2 := Synth_Resize (Idx_Val, Bnd.Wbounds, Loc);

      if Bnd.Right = 0 and then Bnd.Dir = Iir_Downto then
         --  Simple case without adjustments.
         return Idx2;
      end if;

      Right := Build_Const_UB32 (Build_Context, To_Uns32 (Bnd.Right),
                                 Bnd.Wbounds);
      Set_Location (Right, Loc);

      case Bnd.Dir is
         when Iir_To =>
            --  L <= I <= R    -->   off = R - I
            Off := Build_Dyadic (Build_Context, Id_Sub, Right, Idx2);
         when Iir_Downto =>
            --  L >= I >= R    -->   off = I - R
            Off := Build_Dyadic (Build_Context, Id_Sub, Idx2, Right);
      end case;
      Set_Location (Off, Loc);
      return Off;
   end Dyn_Index_To_Offset;

   procedure Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Type : Type_Acc;
                                 Voff : out Net;
                                 Mul : out Uns32;
                                 Off : out Uns32;
                                 W : out Width)
   is
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      Idx_Expr : constant Node := Get_Nth_Element (Indexes, 0);
      Idx_Val : Value_Acc;
   begin
      if Get_Nbr_Elements (Indexes) /= 1 then
         Error_Msg_Synth (+Name, "multi-dim arrays not yet supported");
         raise Internal_Error;
      end if;

      --  Use the base type as the subtype of the index is not synth-ed.
      Idx_Val := Synth_Expression_With_Type
        (Syn_Inst, Idx_Expr, Get_Base_Type (Get_Type (Idx_Expr)));

      if Pfx_Type.Kind = Type_Vector then
         W := 1;
         Mul := 0;
         if Idx_Val.Kind = Value_Discrete then
            Voff := No_Net;
            Off := Index_To_Offset (Pfx_Type.Vbound, Idx_Val.Scal, Name);
         else
            Voff := Dyn_Index_To_Offset (Pfx_Type.Vbound, Idx_Val, Name);
            Off := 0;
         end if;
      elsif Pfx_Type.Kind = Type_Array then
         Voff := Dyn_Index_To_Offset (Pfx_Type.Abounds.D (1), Idx_Val, Name);
         W := Get_Type_Width (Pfx_Type.Arr_El);
         Mul := W;
         Off := 0;
      else
         raise Internal_Error;
      end if;
   end Synth_Indexed_Name;

   function Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                               return Value_Acc
   is
      Pfx_Val : Value_Acc;
      Voff : Net;
      Mul : Uns32;
      Off : Uns32;
      W : Width;
      Res : Net;
   begin
      Pfx_Val := Synth_Expression (Syn_Inst, Get_Prefix (Name));

      Synth_Indexed_Name (Syn_Inst, Name, Pfx_Val.Typ, Voff, Mul, Off, W);

      if Voff = No_Net then
         pragma Assert (Mul = 0);
         if W = 1 and then Pfx_Val.Kind = Value_Array then
            return Bit_Extract (Pfx_Val, Off, Name);
         else
            Res := Build_Extract (Build_Context, Get_Net (Pfx_Val), Off, W);
            Set_Location (Res, Name);
            return Create_Value_Net (Res, Get_Array_Element (Pfx_Val.Typ));
         end if;
      else
         Res := Build_Dyn_Extract
           (Build_Context, Get_Net (Pfx_Val), Voff, Mul, Int32 (Off), W);
         Set_Location (Res, Name);
         return Create_Value_Net (Res, Get_Array_Element (Pfx_Val.Typ));
      end if;
   end Synth_Indexed_Name;

   function Is_Const (N : Net) return Boolean is
   begin
      case Get_Id (Get_Module (Get_Parent (N))) is
         when Id_Const_UB32 =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Const;

   function Get_Const (N : Net) return Int32
   is
      Inst : constant Instance := Get_Parent (N);
   begin
      case Get_Id (Get_Module (Inst)) is
         when Id_Const_UB32 =>
            return To_Int32 (Get_Param_Uns32 (Inst, 0));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Const;

   procedure Decompose_Mul_Add (Val : Net;
                                Inp : out Net;
                                Factor : out Int32;
                                Addend : out Int32)
   is
      Inst : Instance;
      Val_I0, Val_I1 : Net;
   begin
      Factor := 1;
      Addend := 0;
      Inp := Val;

      loop
         Inst := Get_Parent (Inp);
         if Get_Id (Get_Module (Inst)) = Id_Add then
            Val_I0 := Get_Input_Net (Inst, 0);
            Val_I1 := Get_Input_Net (Inst, 1);
            if Is_Const (Val_I0) then
               Addend := Addend + Get_Const (Val_I0) * Factor;
               Inp := Val_I1;
            elsif Is_Const (Val_I1) then
               Addend := Addend + Get_Const (Val_I1) * Factor;
               Inp := Val_I0;
            else
               --  It's an addition, but without any constant value.
               return;
            end if;
         elsif Get_Id (Get_Module (Inst)) = Id_Sub then
            Val_I0 := Get_Input_Net (Inst, 0);
            Val_I1 := Get_Input_Net (Inst, 1);
            if Is_Const (Val_I1) then
               Addend := Addend - Get_Const (Val_I1) * Factor;
               Inp := Val_I0;
            else
               --  It's a substraction, but without any constant value.
               return;
            end if;
         elsif Get_Id (Get_Module (Inst)) = Id_Mul then
            Val_I0 := Get_Input_Net (Inst, 0);
            Val_I1 := Get_Input_Net (Inst, 1);
            if Is_Const (Val_I0) then
               Factor := Factor * Get_Const (Val_I0);
               Inp := Val_I1;
            elsif Is_Const (Val_I1) then
               Factor := Factor * Get_Const (Val_I1);
               Inp := Val_I0;
            else
               --  A mul but without any constant value.
               return;
            end if;
         else
            --  Cannot decompose it.
            return;
         end if;
      end loop;
   end Decompose_Mul_Add;

   function Is_Same (L, R : Net) return Boolean is
   begin
      if L = R then
         return True;
      end if;

      if Get_Width (L) /= Get_Width (R) then
         return False;
      end if;

      declare
         Linst : constant Instance := Get_Parent (L);
         Rinst : constant Instance := Get_Parent (R);
      begin
         if Get_Id (Linst) /= Get_Id (Rinst) then
            return False;
         end if;
         if Get_Id (Linst) = Id_Uextend then
            return Is_Same (Get_Input_Net (Linst, 0),
                            Get_Input_Net (Rinst, 0));
         end if;
      end;
      return False;
   end Is_Same;

   --  Identify LEFT to/downto RIGHT as:
   --  INP * STEP + WIDTH - 1 + OFF to/downto INP * STEP + OFF
   procedure Synth_Extract_Dyn_Suffix (Loc : Node;
                                       Pfx_Bnd : Bound_Type;
                                       Left : Net;
                                       Right : Net;
                                       Inp : out Net;
                                       Step : out Uns32;
                                       Off : out Int32;
                                       Width : out Uns32)
   is
      L_Inp, R_Inp : Net;
      L_Fac, R_Fac : Int32;
      L_Add, R_Add : Int32;
   begin
      Inp := No_Net;
      Step := 0;
      Off := 0;
      Width := 0;

      if Left = Right then
         L_Inp := Left;
         R_Inp := Right;
         L_Fac := 1;
         R_Fac := 1;
         L_Add := 0;
         R_Add := 0;
      else
         Decompose_Mul_Add (Left, L_Inp, L_Fac, L_Add);
         Decompose_Mul_Add (Right, R_Inp, R_Fac, R_Add);
      end if;

      if not Is_Same (L_Inp, R_Inp) then
         Error_Msg_Synth
           (+Loc, "cannot extract same variable part for dynamic slice");
         return;
      end if;
      Inp := L_Inp;

      if L_Fac /= R_Fac then
         Error_Msg_Synth
           (+Loc, "cannot extract same constant factor for dynamic slice");
         return;
      end if;
      --  FIXME: what to do with negative values.
      Step := Uns32 (L_Fac);

      case Pfx_Bnd.Dir is
         when Iir_To =>
            Off := L_Add - Pfx_Bnd.Left;
            Width := Uns32 (R_Add - L_Add + 1);
         when Iir_Downto =>
            Off := R_Add - Pfx_Bnd.Right;
            Width := Uns32 (L_Add - R_Add + 1);
      end case;
   end Synth_Extract_Dyn_Suffix;

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Bound_Type;
                                 Res_Bnd : out Bound_Type;
                                 Inp : out Net;
                                 Step : out Uns32;
                                 Off : out Int32;
                                 Wd : out Uns32)
   is
      Expr : constant Node := Get_Suffix (Name);
      Left, Right : Value_Acc;
      Dir : Iir_Direction;
   begin
      Off := 0;

      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            Left := Synth_Expression (Syn_Inst, Get_Left_Limit (Expr));
            Right := Synth_Expression (Syn_Inst, Get_Right_Limit (Expr));
            Dir := Get_Direction (Expr);
         when others =>
            Error_Msg_Synth (+Expr, "only range supported for slices");
      end case;

      if Pfx_Bnd.Dir /= Dir then
         Error_Msg_Synth (+Name, "direction mismatch in slice");
         Step := 0;
         Wd := 0;
         return;
      end if;

      if not Is_Const (Left) or else not Is_Const (Right) then
         if Left.Kind /= Value_Net and Right.Kind /= Value_Net then
            Error_Msg_Synth
              (+Name, "left and right bounds of a slice must be "
                 & "either constant or dynamic");
            return;
         else
            Synth_Extract_Dyn_Suffix (Name, Pfx_Bnd, Left.N, Right.N,
                                      Inp, Step, Off, Wd);
         end if;
      else
         Inp := No_Net;
         Step := 0;

         if not In_Bounds (Pfx_Bnd, Int32 (Left.Scal))
           or else not In_Bounds (Pfx_Bnd, Int32 (Right.Scal))
         then
            Error_Msg_Synth (+Name, "index not within bounds");
            Wd := 0;
            Off := 0;
            return;
         end if;

         case Pfx_Bnd.Dir is
            when Iir_To =>
               Wd := Width (Right.Scal - Left.Scal + 1);
               Res_Bnd := (Dir => Iir_To,
                           Wlen => Wd,
                           Wbounds => Wd,
                           Len => Wd,
                           Left => Int32 (Left.Scal),
                           Right => Int32 (Right.Scal));
               Off := Pfx_Bnd.Right - Res_Bnd.Right;
            when Iir_Downto =>
               Wd := Width (Left.Scal - Right.Scal + 1);
               Res_Bnd := (Dir => Iir_Downto,
                           Wlen => Wd,
                           Wbounds => Wd,
                           Len => Wd,
                           Left => Int32 (Left.Scal),
                           Right => Int32 (Right.Scal));
               Off := Res_Bnd.Right - Pfx_Bnd.Right;
         end case;
      end if;
   end Synth_Slice_Suffix;

   function Synth_Slice_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                              return Value_Acc
   is
      Pfx_Node : constant Node := Get_Prefix (Name);
      Pfx : constant Value_Acc := Synth_Expression (Syn_Inst, Pfx_Node);
      Res_Bnd : Bound_Type;
      Res_Type : Type_Acc;
      Inp : Net;
      Step : Uns32;
      Off : Int32;
      Wd : Uns32;
      N : Net;
   begin
      Synth_Slice_Suffix
        (Syn_Inst, Name, Pfx.Typ.Vbound, Res_Bnd, Inp, Step, Off, Wd);
      if Inp /= No_Net then
         N := Build_Dyn_Extract (Build_Context, Get_Net (Pfx),
                                 Inp, Step, Off, Wd);
         Set_Location (N, Name);
         --  TODO: the bounds cannot be created as they are not known.
         return Create_Value_Net (N, null);
      else
         N := Build_Extract (Build_Context, Get_Net (Pfx), Uns32 (Off), Wd);
         Set_Location (N, Name);
         Res_Type := Create_Vector_Type (Res_Bnd, Pfx.Typ.Vec_El);
         return Create_Value_Net (N, Res_Type);
      end if;
   end Synth_Slice_Name;

   --  Match: clk_signal_name'event
   --  and return clk_signal_name.
   function Extract_Event_Expr_Prefix (Expr : Node) return Node is
   begin
      if Get_Kind (Expr) = Iir_Kind_Event_Attribute then
         return Get_Prefix (Expr);
      else
         return Null_Node;
      end if;
   end Extract_Event_Expr_Prefix;

   function Is_Same_Node (Left, Right : Node) return Boolean is
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
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Prefix : Node) return Net
   is
      Clk : Net;
      Imp : Node;
      Left, Right : Node;
      Lit : Node;
      Posedge : Boolean;
   begin
      Clk := Get_Net (Synth_Name (Syn_Inst, Prefix));
      if Get_Kind (Expr) /= Iir_Kind_Equality_Operator then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Imp := Get_Implementation (Expr);
      if Get_Implicit_Definition (Imp) /= Iir_Predefined_Enum_Equality then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Left := Get_Left (Expr);
      Right := Get_Right (Expr);
      if Get_Kind (Right) /= Iir_Kind_Character_Literal then
         Error_Msg_Synth
           (+Expr, "ill-formed clock-level, '0' or '1' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Lit := Get_Named_Entity (Right);
      if Lit = Vhdl.Std_Package.Bit_0
        or else Lit = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_0
      then
         Posedge := False;
      elsif Lit = Vhdl.Std_Package.Bit_1
        or else Lit = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_1
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
      if not Posedge then
         Clk := Build_Monadic (Build_Context, Id_Not, Clk);
      end if;
      return Build_Edge (Build_Context, Clk);
   end Extract_Clock_Level;

   --  Try to match: clk'event and clk = X
   --            or: clk = X and clk'event
   --  where X is '0' or '1'.
   function Synth_Clock_Edge
     (Syn_Inst : Synth_Instance_Acc; Left, Right : Node) return Net
   is
      Prefix : Node;
   begin
      --  Try with left.
      Prefix := Extract_Event_Expr_Prefix (Left);
      if Is_Valid (Prefix) then
         return Extract_Clock_Level (Syn_Inst, Right, Prefix);
      end if;

      --  Try with right.
      Prefix := Extract_Event_Expr_Prefix (Right);
      if Is_Valid (Prefix) then
         return Extract_Clock_Level (Syn_Inst, Left, Prefix);
      end if;

      return No_Net;
   end Synth_Clock_Edge;

   function Synth_Type_Conversion (Syn_Inst : Synth_Instance_Acc; Conv : Node)
                                  return Value_Acc
   is
      Expr : constant Node := Get_Expression (Conv);
      Conv_Type : constant Node := Get_Type (Conv);
      Val : Value_Acc;
   begin
      Val := Synth_Expression (Syn_Inst, Expr);
      case Get_Kind (Conv_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Is_Float (Val) then
               return Create_Value_Discrete
                 (Int64 (Val.Fp), Get_Value_Type (Syn_Inst, Conv_Type));
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to int)");
               return null;
            end if;
         when Iir_Kind_Floating_Subtype_Definition =>
            if Is_Const (Val) then
               return Create_Value_Float
                 (Fp64 (Val.Scal), Get_Value_Type (Syn_Inst, Conv_Type));
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to float)");
               return null;
            end if;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            if Is_Vector_Type (Conv_Type) then
               return Val;
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to array)");
               return Val;
            end if;
         when others =>
            Error_Msg_Synth (+Conv, "unhandled type conversion");
            return null;
      end case;
   end Synth_Type_Conversion;

   function Synth_Assoc_In (Syn_Inst : Synth_Instance_Acc;
                            Assoc : Node) return Value_Acc is
   begin
      if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression then
         return Synth_Expression (Syn_Inst, Get_Actual (Assoc));
      else
         Error_Kind ("synth_assoc_in", Assoc);
      end if;
   end Synth_Assoc_In;

   procedure Error_Unknown_Operator (Imp : Node; Loc : Node) is
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

   function Synth_String_Literal (Syn_Inst : Synth_Instance_Acc; Str : Node)
                                 return Value_Acc
   is
      pragma Assert (Get_Kind (Str) = Iir_Kind_String_Literal8);
      Id : constant String8_Id := Get_String8_Id (Str);

      Str_Type : constant Node := Get_Type (Str);
      El_Type : Type_Acc;
      Bounds : Bound_Type;
      Res_Type : Type_Acc;
      Res : Value_Acc;
      Pos : Nat8;
   begin
      Bounds := Synth_Array_Bounds (Syn_Inst, Str_Type, 0);
      El_Type := Get_Value_Type (Syn_Inst, Get_Element_Subtype (Str_Type));
      Res_Type := Create_Vector_Type (Bounds, El_Type);

      Res := Create_Value_Array (Res_Type);
      for I in Res.Arr.V'Range loop
         -- FIXME: use literal from type ??
         Pos := Str_Table.Element_String8 (Id, Pos32 (I));
         Res.Arr.V (I) := Create_Value_Discrete (Int64 (Pos), El_Type);
      end loop;

      return Res;
   end Synth_String_Literal;

   function Eval_To_Unsigned (Arg : Int64; Sz : Int64; Res_Type : Type_Acc)
                             return Value_Acc
   is
      Len : constant Iir_Index32 := Iir_Index32 (Sz);
      El_Type : constant Type_Acc := Get_Array_Element (Res_Type);
      Arr : Value_Array_Acc;
      Bnd : Type_Acc;
   begin
      Arr := Create_Value_Array (Len);
      for I in 1 .. Len loop
         Arr.V (Len - I + 1) := Create_Value_Discrete
           (Std_Logic_0_Pos + (Arg / 2 ** Natural (I - 1)) mod 2, El_Type);
      end loop;
      Bnd := Create_Vec_Type_By_Length (Width (Len), El_Type);
      return Create_Value_Array (Bnd, Arr);
   end Eval_To_Unsigned;

   function Synth_User_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Expr);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Bod : constant Node := Get_Subprogram_Body (Imp);
      Subprg_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
      Res : Value_Acc;
   begin
      --  Is it a call to an ieee function ?
      declare
         Pkg : constant Node := Get_Parent (Imp);
         Unit : Node;
         Lib : Node;
      begin
         if Get_Kind (Pkg) = Iir_Kind_Package_Declaration then
            Unit := Get_Parent (Pkg);
            if Get_Kind (Unit) = Iir_Kind_Design_Unit then
               Lib := Get_Library (Get_Design_File (Unit));
               if Get_Identifier (Lib) = Std_Names.Name_Ieee then
                  Error_Msg_Synth
                    (+Expr, "unhandled call to an ieee function");
                  raise Internal_Error;
               end if;
            end if;
         end if;
      end;

      Areapools.Mark (M, Instance_Pool.all);
      Subprg_Inst := Make_Instance (Syn_Inst, Get_Info (Bod));

      Subprg_Inst.Name := New_Internal_Name (Build_Context);

      Synth_Subprogram_Association
        (Subprg_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      Push_Phi;

      Decls.Synth_Declarations (Subprg_Inst, Get_Declaration_Chain (Bod));

      Synth_Sequential_Statements
        (Subprg_Inst, Get_Sequential_Statement_Chain (Bod));

      Res := Subprg_Inst.Return_Value;

      Pop_And_Merge_Phi (Build_Context, Bod);

      Free_Instance (Subprg_Inst);
      Areapools.Release (M, Instance_Pool.all);

      return Res;
   end Synth_User_Function_Call;

   function Synth_Predefined_Function_Call
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Expr);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Subprg_Inst : Synth_Instance_Acc;
      M : Areapools.Mark_Type;
   begin
      Areapools.Mark (M, Instance_Pool.all);
      Subprg_Inst := Make_Instance (Syn_Inst, Get_Info (Imp));

      Synth_Subprogram_Association
        (Subprg_Inst, Syn_Inst, Inter_Chain, Assoc_Chain);

      case Def is
         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns =>
            declare
               Arg : constant Value_Acc := Subprg_Inst.Objects (1);
               Size : constant Value_Acc := Subprg_Inst.Objects (2);
               Arg_Net : Net;
            begin
               if not Is_Const (Size) then
                  Error_Msg_Synth (+Expr, "to_unsigned size must be constant");
                  return Arg;
               else
                  --  FIXME: what if the arg is constant too ?
                  if Is_Const (Arg) then
                     return Eval_To_Unsigned
                       (Arg.Scal, Size.Scal,
                        Get_Value_Type (Syn_Inst, Get_Type (Imp)));
                  else
                     Arg_Net := Get_Net (Arg);
                     return Create_Value_Net
                       (Synth_Uresize (Arg_Net, Uns32 (Size.Scal)),
                        Create_Res_Bound (Arg, Arg_Net));
                  end if;
               end if;
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat =>
            --  UNSIGNED to Natural.
            declare
               Nat_Type : constant Type_Acc :=
                 Get_Value_Type (Syn_Inst,
                                 Vhdl.Std_Package.Natural_Subtype_Definition);
            begin
               return Create_Value_Net
                 (Synth_Uresize (Get_Net (Subprg_Inst.Objects (1)),
                                 Nat_Type.Drange.W),
                  Nat_Type);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Nat =>
            declare
               V : constant Value_Acc := Subprg_Inst.Objects (1);
               Sz : constant Value_Acc := Subprg_Inst.Objects (2);
               W : Width;
            begin
               if not Is_Const (Sz) then
                  Error_Msg_Synth (+Expr, "size must be constant");
                  return null;
               end if;
               W := Uns32 (Sz.Scal);
               return Create_Value_Net
                 (Synth_Uresize (Get_Net (V), W),
                  Create_Vec_Type_By_Length (W, Logic_Type));
            end;
         when Iir_Predefined_Ieee_Math_Real_Log2 =>
            declare
               V : constant Value_Acc := Subprg_Inst.Objects (1);

               function Log2 (Arg : Fp64) return Fp64;
               pragma Import (C, Log2);
            begin
               if not Is_Float (V) then
                  Error_Msg_Synth
                    (+Expr, "argument must be a float value");
                  return null;
               end if;
               return Create_Value_Float
                 (Log2 (V.Fp), Get_Value_Type (Syn_Inst, Get_Type (Imp)));
            end;
         when Iir_Predefined_Ieee_Math_Real_Ceil =>
            declare
               V : constant Value_Acc := Subprg_Inst.Objects (1);

               function Ceil (Arg : Fp64) return Fp64;
               pragma Import (C, Ceil);
            begin
               if not Is_Float (V) then
                  Error_Msg_Synth
                    (+Expr, "argument must be a float value");
                  return null;
               end if;
               return Create_Value_Float
                 (Ceil (V.Fp), Get_Value_Type (Syn_Inst, Get_Type (Imp)));
            end;
         when others =>
            Error_Msg_Synth
              (+Expr,
               "unhandled function: " & Iir_Predefined_Functions'Image (Def));
      end case;

      Free_Instance (Subprg_Inst);
      Areapools.Release (M, Instance_Pool.all);

      return null;
   end Synth_Predefined_Function_Call;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Node)
     return Value_Acc
   is
      Res : Value_Acc;
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Edge : Net;
            begin
               --  Match clock-edge
               if Def = Iir_Predefined_Boolean_And then
                  Edge := Synth_Clock_Edge (Syn_Inst,
                                            Get_Left (Expr), Get_Right (Expr));
                  if Edge /= No_Net then
                     return Create_Value_Net (Edge, Boolean_Type);
                  end if;
               end if;

               --  FIXME: short-circuit operators ?
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Dyadic_Operation
                    (Syn_Inst, Imp, Get_Left (Expr), Get_Right (Expr), Expr);
               else
                  Error_Unknown_Operator (Imp, Expr);
                  raise Internal_Error;
               end if;
            end;
         when Iir_Kinds_Monadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
            begin
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Monadic_Operation
                    (Syn_Inst, Def, Get_Operand (Expr), Expr);
               else
                  Error_Unknown_Operator (Imp, Expr);
                  raise Internal_Error;
               end if;
            end;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Interface_Signal_Declaration =>  -- For PSL...
            Res := Synth_Name (Syn_Inst, Expr);
            return Synth_Subtype_Conversion
              (Res, Get_Value_Type (Syn_Inst, Expr_Type), Expr);
         when Iir_Kind_Reference_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Expr));
         when Iir_Kind_Indexed_Name =>
            return Synth_Indexed_Name (Syn_Inst, Expr);
         when Iir_Kind_Slice_Name =>
            return Synth_Slice_Name (Syn_Inst, Expr);
         when Iir_Kind_Character_Literal =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Integer_Literal =>
            return Create_Value_Discrete
              (Get_Value (Expr), Get_Value_Type (Syn_Inst, Expr_Type));
         when Iir_Kind_Floating_Point_Literal =>
            return Create_Value_Float
              (Get_Fp_Value (Expr), Get_Value_Type (Syn_Inst, Expr_Type));
         when Iir_Kind_Physical_Int_Literal =>
            return Create_Value_Discrete
              (Get_Physical_Value (Expr),
               Get_Value_Type (Syn_Inst, Expr_Type));
         when Iir_Kind_String_Literal8 =>
            return Synth_String_Literal (Syn_Inst, Expr);
         when Iir_Kind_Enumeration_Literal =>
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Type_Conversion =>
            return Synth_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr), Get_Type (Expr));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Clk : Net;
               Edge : Net;
            begin
               if Imp = Vhdl.Ieee.Std_Logic_1164.Rising_Edge then
                  Clk := Get_Net
                    (Synth_Assoc_In
                       (Syn_Inst, Get_Parameter_Association_Chain (Expr)));
                  Edge := Build_Edge (Build_Context, Clk);
                  return Create_Value_Net (Edge, Boolean_Type);
               elsif Imp = Vhdl.Ieee.Std_Logic_1164.Falling_Edge then
                  Clk := Get_Net
                    (Synth_Assoc_In
                       (Syn_Inst, Get_Parameter_Association_Chain (Expr)));
                  Clk := Build_Monadic (Build_Context, Id_Not, Clk);
                  Edge := Build_Edge (Build_Context, Clk);
                  return Create_Value_Net (Edge, Boolean_Type);
               elsif Get_Implicit_Definition (Imp) /= Iir_Predefined_None then
                  return Synth_Predefined_Function_Call (Syn_Inst, Expr);
               else
                  return Synth_User_Function_Call (Syn_Inst, Expr);
               end if;
            end;
         when Iir_Kind_Aggregate =>
            return Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
         when others =>
            Error_Kind ("synth_expression", Expr);
      end case;
      raise Fatal_Error;
      return null;
   end Synth_Expression_With_Type;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Value_Acc is
   begin
      return Synth_Expression_With_Type (Syn_Inst, Expr, Get_Type (Expr));
   end Synth_Expression;

end Synth.Expr;
