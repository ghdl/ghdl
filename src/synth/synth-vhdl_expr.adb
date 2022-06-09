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

with Types_Utils; use Types_Utils;
with Std_Names;
with Mutils; use Mutils;
with Errorout; use Errorout;

with Vhdl.Types;
with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Annotations; use Vhdl.Annotations;

with PSL.Nodes;
with PSL.Errors;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Heap; use Elab.Vhdl_Heap;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Expr;
with Elab.Debugger;

with Synth.Errors; use Synth.Errors;
with Synth.Vhdl_Environment;
with Synth.Vhdl_Stmts; use Synth.Vhdl_Stmts;
with Synth.Vhdl_Oper; use Synth.Vhdl_Oper;
with Synth.Vhdl_Aggr;
with Synth.Vhdl_Context; use Synth.Vhdl_Context;

package body Synth.Vhdl_Expr is
   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Valtyp;

   procedure Set_Location (N : Net; Loc : Node)
     renames Synth.Source.Set_Location;

   function Get_Value_Memtyp (V : Valtyp) return Memtyp is
   begin
      case V.Val.Kind is
         when Value_Memory =>
            return (V.Typ, V.Val.Mem);
         when Value_Const =>
            return Get_Memtyp (V);
         when Value_Wire =>
            return Synth.Vhdl_Environment.Env.Get_Static_Wire
              (Get_Value_Wire (V.Val));
         when Value_Alias =>
            declare
               Res : Memtyp;
            begin
               Res := Get_Value_Memtyp ((V.Val.A_Typ, V.Val.A_Obj));
               return (V.Typ, Res.Mem + V.Val.A_Off.Mem_Off);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Value_Memtyp;

   function Get_Static_Discrete (V : Valtyp) return Int64 is
   begin
      case V.Val.Kind is
         when Value_Memory =>
            return Read_Discrete (V);
         when Value_Const =>
            return Read_Discrete (Get_Memtyp (V));
         when Value_Wire =>
            return Read_Discrete
              (Synth.Vhdl_Environment.Env.Get_Static_Wire
                 (Get_Value_Wire (V.Val)));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Static_Discrete;

   function Is_Positive (V : Valtyp) return Boolean
   is
      use Synth.Vhdl_Environment.Env;
      N : Net;
      Inst : Instance;
   begin
      pragma Assert (V.Typ.Kind = Type_Discrete);
      case V.Val.Kind is
         when Value_Const
           | Value_Memory =>
            return Read_Discrete (Get_Memtyp (V)) >= 0;
         when Value_Net =>
            N := Get_Value_Net (V.Val);
         when Value_Wire =>
            declare
               W : constant Wire_Id := Get_Value_Wire (V.Val);
            begin
               if Get_Kind (W) = Wire_Variable
                 and then Is_Static_Wire (W)
               then
                  return Read_Discrete (Get_Static_Wire (W)) >= 0;
               else
                  return False;
               end if;
            end;
         when others =>
            raise Internal_Error;
      end case;
      Inst := Get_Net_Parent (N);
      case Get_Id (Inst) is
         when Id_Uextend
           | Id_Const_UB32 =>
            return True;
         when others =>
            --  Be conservative.
            return False;
      end case;
   end Is_Positive;

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
     (Enum : Int64; Etype : Type_Acc; Val : out Uns32; Zx : out Uns32) is
   begin
      if Etype = Logic_Type then
         pragma Assert (Etype.Kind = Type_Logic);
         From_Std_Logic (Enum, Val, Zx);
      elsif Etype = Boolean_Type or Etype = Bit_Type then
         pragma Assert (Etype.Kind = Type_Bit);
         From_Bit (Enum, Val);
         Zx := 0;
      else
         raise Internal_Error;
      end if;
   end To_Logic;

   procedure Uns2logvec (Val : Uns64;
                         W : Width;
                         Vec : in out Logvec_Array;
                         Off : in out Uns32) is
   begin
      if W = 0 then
         return;
      end if;

      for I in 0 .. W - 1 loop
         declare
            B : constant Uns32 := Uns32 (Shift_Right (Val, Natural (I)) and 1);
            Idx : constant Digit_Index := Digit_Index (Off / 32);
            Pos : constant Natural := Natural (Off mod 32);
         begin
            Vec (Idx).Val := Vec (Idx).Val or Shift_Left (B, Pos);
         end;
         Off := Off + 1;
      end loop;
   end Uns2logvec;

   --  Insert bit from VAL into VEC at offset OFF.
   procedure Bit2logvec (Val : Uns32;
                         Off : Uns32;
                         Vec : in out Logvec_Array)
   is
      pragma Assert (Val <= 1);
      Idx : constant Digit_Index := Digit_Index (Off / 32);
      Pos : constant Natural := Natural (Off mod 32);
      Va : Uns32;
   begin
      Va := Shift_Left (Val, Pos);
      Vec (Idx).Val := Vec (Idx).Val or Va;
      Vec (Idx).Zx := 0;
   end Bit2logvec;

   --  Likewise for std_logic
   procedure Logic2logvec (Val : Int64;
                           Off : Uns32;
                           Vec : in out Logvec_Array;
                           Has_Zx : in out Boolean)
   is
      pragma Assert (Val <= 8);
      Idx : constant Digit_Index := Digit_Index (Off / 32);
      Pos : constant Natural := Natural (Off mod 32);
      Va : Uns32;
      Zx : Uns32;
   begin
      From_Std_Logic (Val, Va, Zx);
      Has_Zx := Has_Zx or Zx /= 0;
      Va := Shift_Left (Va, Pos);
      Zx := Shift_Left (Zx, Pos);
      Vec (Idx).Val := Vec (Idx).Val or Va;
      Vec (Idx).Zx := Vec (Idx).Zx or Zx;
   end Logic2logvec;

   --  Read W bits at offset OFF from MEM+TYP and write to VEC at VEC_OFF.
   --  Set HAS_ZX if one bit read is Z or X.
   --  OFF may be greather than the size of MEM.
   --  Update OFF, W, VEC_OFF according to the number of bits
   --  read (or skipped).
   procedure Value2logvec (Mem : Memory_Ptr;
                           Typ : Type_Acc;
                           Off : in out Uns32;
                           W : in out Width;
                           Vec : in out Logvec_Array;
                           Vec_Off : in out Uns32;
                           Has_Zx : in out Boolean) is
   begin
      if Off >= Typ.W then
         --  Offset not yet reached.
         Off := Off - Typ.W;
         return;
      end if;
      if W = 0 then
         --  Nothing to read.
         return;
      end if;

      case Typ.Kind is
         when Type_Bit =>
            --  Scalar bits cannot be cut.
            pragma Assert (Typ.W = 1);
            pragma Assert (Off = 0 and W >= 1);
            Bit2logvec (Uns32 (Read_U8 (Mem)), Vec_Off, Vec);
            --  One bit read and written.
            Vec_Off := Vec_Off + 1;
            W := W - 1;
         when Type_Logic =>
            --  Scalar bits cannot be cut.
            pragma Assert (Typ.W = 1);
            pragma Assert (Off = 0 and W >= 1);
            Logic2logvec (Int64 (Read_U8 (Mem)), Vec_Off, Vec, Has_Zx);
            --  One bit read and written.
            Vec_Off := Vec_Off + 1;
            W := W - 1;
         when Type_Discrete =>
            --  Scalar bits cannot be cut.
            pragma Assert (Off = 0 and W >= Typ.W);
            Uns2logvec (To_Uns64 (Read_Discrete (Memtyp'(Typ, Mem))),
                        Typ.W, Vec, Vec_Off);
            W := W - Typ.W;
         when Type_Float =>
            --  Fp64 is for sure 64 bits.  Assume the endianness of floats is
            --  the same as integers endianness.
            --  Scalar bits cannot be cut.
            pragma Assert (Off = 0 and W >= Typ.W);
            Uns2logvec (To_Uns64 (Read_Fp64 (Mem)), 64, Vec, Vec_Off);
            W := W - Typ.W;
         when Type_Vector =>
            declare
               Vlen : constant Uns32 := Uns32 (Vec_Length (Typ));
               Len : Uns32;
            begin
               pragma Assert (Off < Vlen);
               pragma Assert (Vlen > 0);

               if Vlen > Off + W then
                  --  The vector is longer than the number of bits to read.
                  --  Read less.
                  Len := Off + W;
               else
                  --  Read the whole vector.
                  Len := Vlen;
               end if;

               --  In memory MEM, bits are stored from left to right, so in
               --  big endian (MSB is written at offset 0, LSB at
               --  offset VLEN - 1).  Need to reverse: LSB is read first.
               case Typ.Arr_El.Kind is
                  when Type_Bit =>
                     --  TODO: optimize off mod 32 = 0.
                     for I in Off .. Len - 1 loop
                        Bit2logvec
                          (Uns32 (Read_U8 (Mem + Size_Type (Vlen - 1 - I))),
                           Vec_Off, Vec);
                        Vec_Off := Vec_Off + 1;
                     end loop;
                  when Type_Logic =>
                     for I in Off .. Len - 1 loop
                        Logic2logvec
                          (Int64 (Read_U8 (Mem + Size_Type (Vlen - 1 - I))),
                           Vec_Off, Vec, Has_Zx);
                        Vec_Off := Vec_Off + 1;
                     end loop;
                  when others =>
                     raise Internal_Error;
               end case;
               W := W - (Len - Off);
               Off := 0;
            end;
         when Type_Array =>
            declare
               Alen : constant Uns32 := Get_Bound_Length (Typ);
               El_Typ : constant Type_Acc := Typ.Arr_El;
            begin
               for I in reverse 1 .. Alen loop
                  Value2logvec (Mem + Size_Type (I - 1) * El_Typ.Sz, El_Typ,
                                Off, W, Vec, Vec_Off, Has_Zx);
                  exit when W = 0;
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Value2logvec (Mem + Typ.Rec.E (I).Offs.Mem_Off,
                             Typ.Rec.E (I).Typ, Off, W, Vec, Vec_Off, Has_Zx);
               exit when W = 0;
            end loop;
         when Type_Access =>
            --  Accesses cannot be indexed or sliced.
            --  Just fill with 'X'.
            pragma Assert (Off = 0 and W >= Typ.W);
            for I in 0 .. Typ.W - 1 loop
               declare
                  Idx : constant Digit_Index := Digit_Index (Vec_Off / 32);
                  Pos : constant Natural := Natural (Vec_Off mod 32);
               begin
                  Vec (Idx).Val := Vec (Idx).Val or Shift_Left (1, Pos);
                  Vec (Idx).Zx := Vec (Idx).Zx or Shift_Left (1, Pos);
               end;
               Vec_Off := Vec_Off + 1;
            end loop;
            W := W - Typ.W;
         when others =>
            raise Internal_Error;
      end case;
   end Value2logvec;

   procedure Value2logvec (Val : Memtyp;
                           Off : Uns32;
                           W : Width;
                           Vec : in out Logvec_Array;
                           Vec_Off : in out Uns32;
                           Has_Zx : in out Boolean)
   is
      Off1 : Uns32;
      W1 : Width;
   begin
      Off1 := Off;
      W1 := W;
      Value2logvec (Val.Mem, Val.Typ, Off1, W1, Vec, Vec_Off, Has_Zx);
      pragma Assert (Off1 = 0);
      pragma Assert (W1 = 0);
   end Value2logvec;

   --  Resize for a discrete value.
   function Synth_Resize
     (Ctxt : Context_Acc; Val : Valtyp; W : Width; Loc : Node) return Net
   is
      Wn : constant Width := Val.Typ.W;
      N : Net;
      Res : Net;
      V : Int64;
   begin
      if Is_Static (Val.Val)
        and then Wn /= W
      then
         --  Optimization: resize directly.
         V := Read_Discrete (Val);
         if Val.Typ.Drange.Is_Signed then
            Res := Build2_Const_Int (Ctxt, V, W);
         else
            Res := Build2_Const_Uns (Ctxt, To_Uns64 (V), W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      end if;

      N := Get_Net (Ctxt, Val);
      if Wn > W then
         return Build2_Trunc (Ctxt, Id_Utrunc, N, W, Get_Location (Loc));
      elsif Wn < W then
         if Val.Typ.Drange.Is_Signed then
            Res := Build_Extend (Ctxt, Id_Sextend, N, W);
         else
            Res := Build_Extend (Ctxt, Id_Uextend, N, W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      else
         return N;
      end if;
   end Synth_Resize;

   procedure Concat_Array (Ctxt : Context_Acc; Arr : in out Net_Array)
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
                 (Ctxt, Arr (Idx), Arr (Idx + 1));
               Idx := Idx + 2;
            elsif Idx + 2 = Last then
               Arr (New_Idx) := Build_Concat3
                 (Ctxt, Arr (Idx), Arr (Idx + 1), Arr (Idx + 2));
               Idx := Idx + 3;
            else
               Arr (New_Idx) := Build_Concat4
                 (Ctxt,
                  Arr (Idx), Arr (Idx + 1), Arr (Idx + 2), Arr (Idx + 3));
               Idx := Idx + 4;
            end if;
         end loop;
         Last := New_Idx;
      end loop;
   end Concat_Array;

   procedure Concat_Array
     (Ctxt : Context_Acc; Arr : in out Net_Array; N : out Net) is
   begin
      Concat_Array (Ctxt, Arr);
      N := Arr (Arr'First);
   end Concat_Array;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Dim_Type) return Bound_Type
   is
      Info : constant Sim_Info_Acc := Get_Info (Atype);
   begin
      if Info = null then
         pragma Assert (Get_Type_Declarator (Atype) = Null_Node);
         declare
            Index_Type : constant Node :=
              Get_Index_Type (Atype, Natural (Dim - 1));
         begin
            return Synth_Bounds_From_Range (Syn_Inst, Index_Type);
         end;
      else
         declare
            Bnds : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Atype);
         begin
            pragma Assert (Dim = 1);
            return Get_Array_Bound (Bnds);
         end;
      end if;
   end Synth_Array_Bounds;

   --  Change the bounds of VAL.
   function Reshape_Value (Val : Valtyp; Ntype : Type_Acc) return Valtyp is
   begin
      case Val.Val.Kind is
         when Value_Wire =>
            return Create_Value_Wire (Get_Value_Wire (Val.Val), Ntype);
         when Value_Net =>
            return Create_Value_Net (Get_Value_Net (Val.Val), Ntype);
         when Value_Alias =>
            return Create_Value_Alias
              ((Val.Val.A_Typ, Val.Val.A_Obj), Val.Val.A_Off, Ntype);
         when Value_Const =>
            return Reshape_Value ((Val.Typ, Val.Val.C_Val), Ntype);
         when Value_Memory =>
            return (Ntype, Val.Val);
         when others =>
            raise Internal_Error;
      end case;
   end Reshape_Value;

   function Synth_Subtype_Conversion (Ctxt : Context_Acc;
                                      Vt : Valtyp;
                                      Dtype : Type_Acc;
                                      Bounds : Boolean;
                                      Loc : Source.Syn_Src)
                                     return Valtyp
   is
      Vtype : constant Type_Acc := Vt.Typ;
   begin
      if Vt = No_Valtyp then
         --  Propagate error.
         return No_Valtyp;
      end if;
      if Dtype = Vtype then
         return Vt;
      end if;

      case Dtype.Kind is
         when Type_Bit =>
            pragma Assert (Vtype.Kind = Type_Bit);
            return Vt;
         when Type_Logic =>
            pragma Assert (Vtype.Kind = Type_Logic);
            return Vt;
         when Type_Discrete =>
            pragma Assert (Vtype.Kind in Type_All_Discrete);
            case Vt.Val.Kind is
               when Value_Net
                  | Value_Wire
                  | Value_Alias =>
                  if Vtype.W /= Dtype.W then
                     --  Truncate.
                     --  TODO: check overflow.
                     declare
                        N : Net;
                     begin
                        if Is_Static_Val (Vt.Val) then
                           return Create_Value_Discrete
                             (Get_Static_Discrete (Vt), Dtype);
                        end if;

                        N := Get_Net (Ctxt, Vt);
                        if Vtype.Drange.Is_Signed then
                           N := Build2_Sresize
                             (Ctxt, N, Dtype.W, Get_Location (Loc));
                        else
                           N := Build2_Uresize
                             (Ctxt, N, Dtype.W, Get_Location (Loc));
                        end if;
                        return Create_Value_Net (N, Dtype);
                     end;
                  else
                     return Vt;
                  end if;
               when Value_Const =>
                  return Synth_Subtype_Conversion
                    (Ctxt, (Vt.Typ, Vt.Val.C_Val), Dtype, Bounds, Loc);
               when Value_Memory =>
                  --  Check for overflow.
                  declare
                     Val : constant Int64 := Read_Discrete (Vt);
                  begin
                     if not In_Range (Dtype.Drange, Val) then
                        Error_Msg_Synth (+Loc, "value out of range");
                        return No_Valtyp;
                     end if;
                     return Create_Value_Discrete (Val, Dtype);
                  end;
               when others =>
                  raise Internal_Error;
            end case;
         when Type_Float =>
            pragma Assert (Vtype.Kind = Type_Float);
            --  TODO: check range
            return Vt;
         when Type_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or Vtype.Kind = Type_Slice);
            if Dtype.W /= Vtype.W then
               Error_Msg_Synth
                 (+Loc, "mismatching vector length; got %v, expect %v",
                  (Errorout."+" (Vtype.W), +Dtype.W));
               return No_Valtyp;
            end if;
            if Bounds then
               return Reshape_Value (Vt, Dtype);
            else
               return Vt;
            end if;
         when Type_Slice =>
            --  TODO: check width
            return Vt;
         when Type_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            --  Check bounds.
            declare
               Src_Typ, Dst_Typ : Type_Acc;
            begin
               Src_Typ := Vtype;
               Dst_Typ := Dtype;
               loop
                  pragma Assert (Src_Typ.Alast = Dst_Typ.Alast);
                  if Src_Typ.Abound.Len /= Dst_Typ.Abound.Len then
                     Error_Msg_Synth (+Loc, "mismatching array bounds");
                     return No_Valtyp;
                  end if;
                  exit when Src_Typ.Alast;
                  Src_Typ := Src_Typ.Arr_El;
                  Dst_Typ := Dst_Typ.Arr_El;
               end loop;
               --  TODO: check element.
               if Bounds then
                  return Reshape_Value (Vt, Dtype);
               else
                  return Vt;
               end if;
            end;
         when Type_Unbounded_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            return Vt;
         when Type_Unbounded_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or else Vtype.Kind = Type_Slice);
            return Vt;
         when Type_Record =>
            pragma Assert (Vtype.Kind = Type_Record);
            --  TODO: handle elements.
            return Vt;
         when Type_Unbounded_Record =>
            pragma Assert (Vtype.Kind = Type_Record);
            return Vt;
         when Type_Access =>
            return Vt;
         when Type_File
            | Type_Protected =>
            --  No conversion expected.
            --  As the subtype is identical, it is already handled by the
            --  above check.
            raise Internal_Error;
      end case;
   end Synth_Subtype_Conversion;

   function Synth_Subtype_Conversion (Syn_Inst : Synth_Instance_Acc;
                                      Vt : Valtyp;
                                      Dtype : Type_Acc;
                                      Bounds : Boolean;
                                      Loc : Source.Syn_Src)
                                     return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
   begin
      return Synth_Subtype_Conversion (Ctxt, Vt, Dtype, Bounds, Loc);
   end Synth_Subtype_Conversion;

   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Valtyp is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            return Get_Value (Syn_Inst, Name);
         when Iir_Kind_Enumeration_Literal =>
            declare
               Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Type (Name));
               Res : Valtyp;
            begin
               Res := Create_Value_Memory (Typ);
               Write_Discrete (Res, Int64 (Get_Enum_Pos (Name)));
               return Res;
            end;
         when Iir_Kind_Unit_Declaration =>
            declare
               Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Type (Name));
            begin
               return Create_Value_Discrete
                 (Vhdl.Evaluation.Get_Physical_Value (Name), Typ);
            end;
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            declare
               Val : Valtyp;
            begin
               Val := Synth_Expression (Syn_Inst, Get_Prefix (Name));
               return Elab.Vhdl_Heap.Synth_Dereference (Read_Access (Val));
            end;
         when others =>
            Error_Kind ("synth_name", Name);
      end case;
   end Synth_Name;

   procedure Bound_Error (Syn_Inst : Synth_Instance_Acc; Loc : Node) is
   begin
      Error_Msg_Synth (+Loc, "index not within bounds");
      Elab.Debugger.Debug_Error (Syn_Inst, Loc);
   end Bound_Error;

   --  Convert index IDX in PFX to an offset.
   --  SYN_INST and LOC are used in case of error.
   function Index_To_Offset
     (Syn_Inst : Synth_Instance_Acc; Bnd : Bound_Type; Idx : Int64; Loc : Node)
     return Value_Offsets
   is
      Res : Value_Offsets;
   begin
      if not In_Bounds (Bnd, Int32 (Idx)) then
         Bound_Error (Syn_Inst, Loc);
         return (0, 0);
      end if;

      --  The offset is from the LSB (bit 0).  Bit 0 is the rightmost one.
      case Bnd.Dir is
         when Dir_To =>
            Res.Net_Off := Uns32 (Bnd.Right - Int32 (Idx));
            Res.Mem_Off := Size_Type (Int32 (Idx) - Bnd.Left);
         when Dir_Downto =>
            Res.Net_Off := Uns32 (Int32 (Idx) - Bnd.Right);
            Res.Mem_Off := Size_Type (Bnd.Left - Int32 (Idx));
      end case;

      return Res;
   end Index_To_Offset;

   function Dyn_Index_To_Offset
     (Ctxt : Context_Acc; Bnd : Bound_Type; Idx_Val : Valtyp; Loc : Node)
     return Net
   is
      Idx2 : Net;
      Off : Net;
      Right : Net;
      Wbounds : Width;
   begin
      Wbounds := Clog2 (Bnd.Len);
      Idx2 := Synth_Resize (Ctxt, Idx_Val, Wbounds, Loc);

      if Bnd.Right = 0 and then Bnd.Dir = Dir_Downto then
         --  Simple case without adjustments.
         return Idx2;
      end if;

      Right := Build_Const_UB32 (Ctxt, To_Uns32 (Bnd.Right), Wbounds);
      Set_Location (Right, Loc);

      case Bnd.Dir is
         when Dir_To =>
            --  L <= I <= R    -->   off = R - I
            Off := Build_Dyadic (Ctxt, Id_Sub, Right, Idx2);
         when Dir_Downto =>
            --  L >= I >= R    -->   off = I - R
            Off := Build_Dyadic (Ctxt, Id_Sub, Idx2, Right);
      end case;
      Set_Location (Off, Loc);
      return Off;
   end Dyn_Index_To_Offset;

   procedure Synth_Indexes (Syn_Inst : Synth_Instance_Acc;
                            Indexes : Iir_Flist;
                            Dim : Natural;
                            Arr_Typ : Type_Acc;
                            El_Typ : out Type_Acc;
                            Voff : out Net;
                            Off : out Value_Offsets;
                            Stride : out Uns32;
                            Error : out Boolean)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Idx_Expr : Node;
      Idx_Val : Valtyp;
      Idx : Int64;
      Bnd : Bound_Type;
      Ivoff : Net;
      Idx_Off : Value_Offsets;
   begin
      if Dim > Flist_Last (Indexes) then
         Voff := No_Net;
         Off := (0, 0);
         Error := False;
         Stride := 1;
         El_Typ := Arr_Typ;
         return;
      else
         Synth_Indexes
           (Syn_Inst, Indexes, Dim + 1, Get_Array_Element (Arr_Typ),
            El_Typ, Voff, Off, Stride, Error);
      end if;

      Idx_Expr := Get_Nth_Element (Indexes, Dim);

      --  Use the base type as the subtype of the index is not synth-ed.
      Idx_Val := Synth_Expression_With_Basetype (Syn_Inst, Idx_Expr);
      if Idx_Val = No_Valtyp then
         --  Propagate error.
         Error := True;
         return;
      end if;

      Strip_Const (Idx_Val);

      Bnd := Get_Array_Bound (Arr_Typ);

      if Is_Static_Val (Idx_Val.Val) then
         Idx := Get_Static_Discrete (Idx_Val);
         if not In_Bounds (Bnd, Int32 (Idx)) then
            Bound_Error (Syn_Inst, Idx_Expr);
            Error := True;
         else
            Idx_Off := Index_To_Offset (Syn_Inst, Bnd, Idx, Idx_Expr);
            Off.Net_Off := Off.Net_Off
              + Idx_Off.Net_Off * Stride * El_Typ.W;
            Off.Mem_Off := Off.Mem_Off
              + Idx_Off.Mem_Off * Size_Type (Stride) * El_Typ.Sz;
         end if;
      else
         Ivoff := Dyn_Index_To_Offset (Ctxt, Bnd, Idx_Val, Idx_Expr);
         Ivoff := Build_Memidx
           (Get_Build (Syn_Inst), Ivoff, El_Typ.W * Stride,
            Bnd.Len - 1,
            Width (Clog2 (Uns64 (El_Typ.W * Stride * Bnd.Len))));
         Set_Location (Ivoff, Idx_Expr);

         if Voff = No_Net then
            Voff := Ivoff;
         else
            Voff := Build_Addidx (Get_Build (Syn_Inst), Ivoff, Voff);
            Set_Location (Voff, Idx_Expr);
         end if;
      end if;

      Stride := Stride * Bnd.Len;
   end Synth_Indexes;

   procedure Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Type : Type_Acc;
                                 El_Typ : out Type_Acc;
                                 Voff : out Net;
                                 Off : out Value_Offsets;
                                 Error : out Boolean)
   is
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      Stride : Uns32;
   begin
      Synth_Indexes (Syn_Inst, Indexes, Flist_First, Pfx_Type,
                     El_Typ, Voff, Off, Stride, Error);
   end Synth_Indexed_Name;

   function Is_Static (N : Net) return Boolean is
   begin
      case Get_Id (Get_Module (Get_Net_Parent (N))) is
         when Id_Const_UB32 =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Static;

   function Get_Const (N : Net) return Int32
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      case Get_Id (Get_Module (Inst)) is
         when Id_Const_UB32 =>
            return To_Int32 (Get_Param_Uns32 (Inst, 0));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Const;

   --  Decompose VAL as FACTOR * INP + ADDEND (where only INP is non-static).
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
         Inst := Get_Net_Parent (Inp);
         case Get_Id (Get_Module (Inst)) is
            when Id_Add =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I0) then
                  Addend := Addend + Get_Const (Val_I0) * Factor;
                  Inp := Val_I1;
               elsif Is_Static (Val_I1) then
                  Addend := Addend + Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               else
                  --  It's an addition, but without any constant value.
                  return;
               end if;
            when Id_Sub =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I1) then
                  Addend := Addend - Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               elsif Is_Static (Val_I0) then
                  Addend := Addend + Get_Const (Val_I0) * Factor;
                  Factor := -Factor;
                  Inp := Val_I1;
               else
                  --  It's a substraction, but without any constant value.
                  return;
               end if;
            when Id_Smul =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I0) then
                  Factor := Factor * Get_Const (Val_I0);
                  Inp := Val_I1;
               elsif Is_Static (Val_I1) then
                  Factor := Factor * Get_Const (Val_I1);
                  Inp := Val_I0;
               else
                  --  A mul but without any constant value.
                  return;
               end if;
            when Id_Utrunc
              | Id_Uextend =>
               Inp := Get_Input_Net (Inst, 0);
            when others =>
               --  Cannot decompose it.
               return;
         end case;
      end loop;
   end Decompose_Mul_Add;

   --  Identify LEFT to/downto RIGHT as:
   --  INP * STEP + WIDTH - 1 + OFF to/downto INP * STEP + OFF
   procedure Synth_Extract_Dyn_Suffix (Ctxt : Context_Acc;
                                       Loc : Node;
                                       Pfx_Bnd : Bound_Type;
                                       Left : Net;
                                       Right : Net;
                                       Inp : out Net;
                                       Step : out Uns32;
                                       Off : out Uns32;
                                       Width : out Uns32)
   is
      L_Inp, R_Inp : Net;
      L_Fac, R_Fac : Int32;
      L_Add, R_Add : Int32;
      Sstep : Int32;
      Soff : Int32;
      Bias : Int32;
      Bias_Net : Net;
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

      if not Same_Net (L_Inp, R_Inp) then
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

      --  Compute step and width.
      Sstep := abs L_Fac;
      Step := Uns32 (Sstep);
      case Pfx_Bnd.Dir is
         when Dir_To =>
            if R_Add < L_Add then
               Width := 0;
            else
               Width := Uns32 (R_Add - L_Add + 1);
            end if;
         when Dir_Downto =>
            if L_Add < R_Add then
               Width := 0;
            else
               Width := Uns32 (L_Add - R_Add + 1);
            end if;
      end case;

      if Width = 0 then
         Inp := No_Net;
         Off := 0;
         return;
      end if;

      --  TODO: degenerated dynamic slice.
      pragma Assert (L_Fac /= 0);

      case Pfx_Bnd.Dir is
         when Dir_To =>
            --  Transformations:
            --
            --  Bounds:       l to r
            --  Slice:  L+fac*i to L+(W-1)+fac*i
            --
            --  Bounds:       0 to len-1
            --  Slice:  (L-l)+fac*i to (L-l)+(W-1)+fac*i
            --
            --  Bounds:       len-1 downto 0
            --  Slice:  xxx         downto (len-1)-(L-l)-(W-1)-fac*i
            --          xxx         downto (r-l-L+l-W+1)-fac*i
            --                      downto (r-L-W+1)-fac*i
            Soff := Pfx_Bnd.Right - L_Add - Int32 (Width) + 1;
         when Dir_Downto =>
            --  Transformations:
            --
            --  Bounds:              l downto r
            --  Slice:   R+(W-1)+fac*i downto R+fac*i
            --
            --  Bounds:            len-1 downto 0
            --  Slice:   R-r+(W-1)+fac*i downto R-r+fac*i
            Soff := R_Add - Pfx_Bnd.Right;
      end case;

      --  So IDX = SOFF + INP * FAC
      --         = SOFF +/- INP * STEP
      --  We need to adjust for memidx:
      --     IDX = OFF + STEP * (B +/- INP)
      --     with OFF > 0, STEP > 0

      --  Ensure Off is between 0 and Step - 1
      if Soff >= 0 then
         Bias := Soff / Sstep;
         Off := Uns32 (Soff - Bias * Sstep);  --  mod
      else
         Bias := -((-Soff) / Sstep);
         --  Note: SOFF < 0, BIAS < 0.
         Soff := Soff - Bias * Sstep;
         if Soff < 0 then
            Soff := Soff + Sstep;
            Bias := Bias - 1;
         end if;
         Off := Uns32 (Soff);
      end if;
      pragma Assert (Off < Step);

      --  Assume input width large enough to cover all the values of the
      --  bounds.
      if (Pfx_Bnd.Dir = Dir_Downto and then L_Fac > 0)
        or else (Pfx_Bnd.Dir = Dir_To and then L_Fac < 0)
      then
         --  Same direction.
         if Bias /= 0 then
            Bias_Net := Build2_Const_Int (Ctxt, Int64 (Bias), Get_Width (Inp));
            Inp := Build_Dyadic (Ctxt, Id_Add, Inp, Bias_Net);
            Set_Location (Inp, Loc);
         end if;
      else
         if Bias /= 0 then
            Bias_Net := Build2_Const_Int (Ctxt, Int64 (Bias), Get_Width (Inp));
            Inp := Build_Dyadic (Ctxt, Id_Sub, Bias_Net, Inp);
         else
            Inp := Build_Monadic (Ctxt, Id_Neg, Inp);
         end if;
         Set_Location (Inp, Loc);
      end if;
   end Synth_Extract_Dyn_Suffix;

   procedure Synth_Slice_Const_Suffix (Syn_Inst: Synth_Instance_Acc;
                                       Expr : Node;
                                       Name : Node;
                                       Pfx_Bnd : Bound_Type;
                                       L, R : Int64;
                                       Dir : Direction_Type;
                                       El_Typ : Type_Acc;
                                       Res_Bnd : out Bound_Type;
                                       Off : out Value_Offsets)
   is
      Is_Null : Boolean;
      Len : Uns32;
   begin
      if Pfx_Bnd.Dir /= Dir then
         Error_Msg_Synth (+Name, "direction mismatch in slice");
         Off := (0, 0);
         if Dir = Dir_To then
            Res_Bnd := (Dir => Dir_To, Left => 1, Right => 0, Len => 0);
         else
            Res_Bnd := (Dir => Dir_Downto, Left => 0, Right => 1, Len => 0);
         end if;
         return;
      end if;

      --  Might be a null slice.
      case Pfx_Bnd.Dir is
         when Dir_To =>
            Is_Null := L > R;
         when Dir_Downto =>
            Is_Null := L < R;
      end case;
      if Is_Null then
         Len := 0;
         Off := (0, 0);
      else
         if not In_Bounds (Pfx_Bnd, Int32 (L))
           or else not In_Bounds (Pfx_Bnd, Int32 (R))
         then
            Error_Msg_Synth (+Name, "index not within bounds");
            Elab.Debugger.Debug_Error (Syn_Inst, Expr);
            Off := (0, 0);
            return;
         end if;

         case Pfx_Bnd.Dir is
            when Dir_To =>
               Len := Uns32 (R - L + 1);
               Off.Net_Off := Uns32 (Pfx_Bnd.Right - Int32 (R)) * El_Typ.W;
               Off.Mem_Off := Size_Type (Int32 (L) - Pfx_Bnd.Left) * El_Typ.Sz;
            when Dir_Downto =>
               Len := Uns32 (L - R + 1);
               Off.Net_Off := Uns32 (Int32 (R) - Pfx_Bnd.Right) * El_Typ.W;
               Off.Mem_Off := Size_Type (Pfx_Bnd.Left - Int32 (L)) * El_Typ.Sz;
         end case;
      end if;
      Res_Bnd := (Dir => Pfx_Bnd.Dir,
                  Len => Len,
                  Left => Int32 (L),
                  Right => Int32 (R));
   end Synth_Slice_Const_Suffix;

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Bound_Type;
                                 El_Typ : Type_Acc;
                                 Res_Bnd : out Bound_Type;
                                 Inp : out Net;
                                 Off : out Value_Offsets)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Expr : constant Node := Get_Suffix (Name);
      Left, Right : Valtyp;
      Dir : Direction_Type;
      Step : Uns32;
      Max : Uns32;
      Inp_W : Width;
   begin
      Off := (0, 0);
      Inp := No_Net;

      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            --  As the range may be dynamic, cannot use synth_discrete_range.
            Left := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Left_Limit (Expr));
            Right := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Right_Limit (Expr));
            Dir := Get_Direction (Expr);

         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kinds_Denoting_Name =>
            declare
               Rng : Discrete_Range_Type;
            begin
               Synth_Discrete_Range (Syn_Inst, Expr, Rng);
               Synth_Slice_Const_Suffix (Syn_Inst, Expr,
                                         Name, Pfx_Bnd,
                                         Rng.Left, Rng.Right, Rng.Dir,
                                         El_Typ, Res_Bnd, Off);
               return;
            end;
         when others =>
            Error_Msg_Synth
              (+Expr, "only range expression supported for slices");
            Res_Bnd := (Dir => Dir_To, Left => 1, Right => 0, Len => 0);
            return;
      end case;

      if Is_Static_Val (Left.Val) and then Is_Static_Val (Right.Val) then
         Synth_Slice_Const_Suffix (Syn_Inst, Expr,
                                   Name, Pfx_Bnd,
                                   Get_Static_Discrete (Left),
                                   Get_Static_Discrete (Right),
                                   Dir,
                                   El_Typ, Res_Bnd, Off);
      else
         if Pfx_Bnd.Dir /= Dir then
            Error_Msg_Synth (+Name, "direction mismatch in slice");
            if Dir = Dir_To then
               Res_Bnd := (Dir => Dir_To, Left => 1, Right => 0, Len => 0);
            else
               Res_Bnd := (Dir => Dir_Downto, Left => 0, Right => 1, Len => 0);
            end if;
            return;
         end if;

         if Is_Static (Left.Val) or else Is_Static (Right.Val) then
            Error_Msg_Synth
              (+Name, "left and right bounds of a slice must be "
                 & "either constant or dynamic");
            return;
         end if;

         Synth_Extract_Dyn_Suffix
           (Ctxt, Name, Pfx_Bnd, Get_Net (Ctxt, Left), Get_Net (Ctxt, Right),
            Inp, Step, Off.Net_Off, Res_Bnd.Len);
         if Inp = No_Net then
            return;
         end if;
         Inp_W := Get_Width (Inp);
         --  FIXME: convert range to offset.
         --  Extract max from the range.
         --  example: len=128  wd=8  step=8  => max=16
         --           len=8    wd=4  step=1  => max=4
         --  max so that max*step+wd <= len - off
         --              max <= (len - off - wd) / step
         Max := (Pfx_Bnd.Len - Off.Net_Off - Res_Bnd.Len) / Step;
         if Max > 2**Natural (Inp_W) - 1 then
            --  The width of Inp limits the max.
            Max := 2**Natural (Inp_W) - 1;
         end if;
         Inp := Build_Memidx
           (Ctxt, Inp, Step * El_Typ.W, Max,
            Inp_W + Width (Clog2 (Uns64 (Step * El_Typ.W))));
         Set_Location (Inp, Name);
      end if;
   end Synth_Slice_Suffix;

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

   function Is_Same_Clock (Syn_Inst : Synth_Instance_Acc;
                           Left, Right : Node;
                           Clk_Left : Net) return Boolean
   is
      N : Net;
   begin
      --  Handle directly the common case (when clock is a simple name).
      if Get_Kind (Left) = Iir_Kind_Simple_Name
        and then Get_Kind (Right) = Iir_Kind_Simple_Name
      then
         return Get_Named_Entity (Left) = Get_Named_Entity (Right);
      end if;

      N := Get_Net (Get_Build (Syn_Inst), Synth_Expression (Syn_Inst, Right));

      return Same_Net (Clk_Left, N);
   end Is_Same_Clock;

   --  Match: clk_signal_name = '1' | clk_signal_name = '0'
   function Extract_Clock_Level
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Prefix : Node) return Net
   is
      Ctxt        : constant Context_Acc := Get_Build (Syn_Inst);
      Clk         : Net;
      Imp         : Node;
      Left, Right : Node;
      Lit         : Valtyp;
      Lit_Type    : Node;
      Posedge     : Boolean;
      Res         : Net;
   begin
      Clk := Get_Net (Ctxt, Synth_Expression (Syn_Inst, Prefix));
      if Get_Kind (Expr) /= Iir_Kind_Equality_Operator then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         Res := Build_Posedge (Ctxt, Clk);
         Set_Location (Res, Expr);
         return Res;
      end if;
      Imp := Get_Implementation (Expr);
      if Get_Implicit_Definition (Imp) /= Iir_Predefined_Enum_Equality then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         Res := Build_Posedge (Ctxt, Clk);
         Set_Location (Res, Expr);
         return Res;
      end if;

      Left := Get_Left (Expr);
      if not Is_Same_Clock (Syn_Inst, Prefix, Left, Clk) then
         Error_Msg_Synth (+Left, "clock signal name doesn't match");
      end if;

      Right := Get_Right (Expr);
      Lit_Type := Get_Base_Type (Get_Type (Right));
      Lit := Synth_Expression (Syn_Inst, Right);
      if Lit.Val.Kind /= Value_Memory then
         Error_Msg_Synth (+Right, "clock-level is not a constant");
         Posedge := True;
      else
         if Lit_Type = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type then
            case Read_U8 (Lit.Val.Mem) is
               when Vhdl.Ieee.Std_Logic_1164.Std_Logic_0_Pos =>
                  Posedge := False;
               when Vhdl.Ieee.Std_Logic_1164.Std_Logic_1_Pos =>
                  Posedge := True;
               when others =>
                  Error_Msg_Synth
                    (+Right, "clock-level must be either '0' or '1'");
                  Posedge := True;
            end case;
         else
            pragma Assert (Lit_Type = Vhdl.Std_Package.Bit_Type_Definition);
            case Read_U8 (Lit.Val.Mem) is
               when 0 =>
                  Posedge := False;
               when 1 =>
                  Posedge := True;
               when others =>
                  raise Internal_Error;
            end case;
         end if;
      end if;
      if Posedge then
         Res := Build_Posedge (Ctxt, Clk);
      else
         Res := Build_Negedge (Ctxt, Clk);
      end if;
      Set_Location (Res, Expr);
      return Res;
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

   function Synth_Type_Conversion
     (Syn_Inst : Synth_Instance_Acc; Conv : Node) return Valtyp
   is
      Expr : constant Node := Get_Expression (Conv);
      Conv_Type : constant Node := Get_Type (Conv);
      Conv_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Conv_Type);
      Val : Valtyp;
   begin
      Val := Synth_Expression_With_Basetype (Syn_Inst, Expr);
      if Val = No_Valtyp then
         return No_Valtyp;
      end if;
      Strip_Const (Val);
      case Get_Kind (Conv_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Val.Typ.Kind = Type_Discrete then
               --  Int to int.
               return Val;
            elsif Val.Typ.Kind = Type_Float then
               return Create_Value_Discrete
                 (Int64 (Read_Fp64 (Val)), Conv_Typ);
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to int)");
               return No_Valtyp;
            end if;
         when Iir_Kind_Floating_Subtype_Definition =>
            if Is_Static (Val.Val) then
               return Create_Value_Float
                 (Fp64 (Read_Discrete (Val)), Conv_Typ);
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to float)");
               return No_Valtyp;
            end if;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            case Conv_Typ.Kind is
               when Type_Vector
                 | Type_Unbounded_Vector =>
                  return Val;
               when Type_Array
                 | Type_Unbounded_Array =>
                  return Val;
               when others =>
                  Error_Msg_Synth
                    (+Conv, "unhandled type conversion (to array)");
                  return No_Valtyp;
            end case;
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            pragma Assert (Get_Base_Type (Get_Type (Expr))
                             = Get_Base_Type (Conv_Type));
            return Val;
         when others =>
            Error_Msg_Synth (+Conv, "unhandled type conversion");
            return No_Valtyp;
      end case;
   end Synth_Type_Conversion;

   function Error_Ieee_Operator (Imp : Node; Loc : Node) return Boolean
   is
      use Std_Names;
      Parent : constant Iir := Get_Parent (Imp);
   begin
      if Get_Kind (Parent) = Iir_Kind_Package_Declaration
        and then (Get_Identifier
                    (Get_Library (Get_Design_File (Get_Design_Unit (Parent))))
                    = Name_Ieee)
      then
         case Get_Identifier (Parent) is
            when Name_Std_Logic_1164
               | Name_Std_Logic_Arith
               | Name_Std_Logic_Signed
               | Name_Std_Logic_Unsigned
               | Name_Std_Logic_Misc
               | Name_Numeric_Std
               | Name_Numeric_Bit
               | Name_Math_Real =>
               Error_Msg_Synth
                 (+Loc, "unhandled predefined IEEE operator %i", +Imp);
               Error_Msg_Synth
                 (+Imp, " declared here");
               return True;
            when others =>
               --  ieee 2008 packages are handled like regular packages.
               null;
         end case;
      end if;

      return False;
   end Error_Ieee_Operator;

   --  Return the left bound if the direction of the range is LEFT_DIR.
   function Synth_Low_High_Type_Attribute
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Left_Dir : Direction_Type)
     return Valtyp
   is
      Typ : Type_Acc;
      R : Int64;
   begin
      Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Get_Prefix (Expr)));
      pragma Assert (Typ.Kind = Type_Discrete);
      if Typ.Drange.Dir = Left_Dir then
         R := Typ.Drange.Left;
      else
         R := Typ.Drange.Right;
      end if;
      return Create_Value_Discrete (R, Typ);
   end Synth_Low_High_Type_Attribute;

   function Synth_PSL_Expression
     (Syn_Inst : Synth_Instance_Acc; Expr : PSL.Types.PSL_Node) return Net
   is
      use PSL.Types;
      use PSL.Nodes;

      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Loc : constant Location_Type := Get_Location (Expr);
      Res : Net;
   begin
      case Get_Kind (Expr) is
         when N_HDL_Bool =>
            declare
               E : constant Vhdl.Types.Vhdl_Node := Get_HDL_Node (Expr);
            begin
               return Get_Net (Ctxt, Synth_Expression (Syn_Inst, E));
            end;
         when N_Not_Bool =>
            pragma Assert (Loc /= No_Location);
            Res := Build_Monadic
              (Ctxt, Id_Not,
               Synth_PSL_Expression (Syn_Inst, Get_Boolean (Expr)));
         when N_And_Bool =>
            pragma Assert (Loc /= No_Location);
            declare
               L : constant PSL_Node := Get_Left (Expr);
               R : constant PSL_Node := Get_Right (Expr);
               Edge : Net;
            begin
               --  Handle edge (as it can be in default clock).
               if Get_Kind (L) in N_HDLs and then Get_Kind (R) in N_HDLs then
                  Edge := Synth_Clock_Edge
                    (Syn_Inst, Get_HDL_Node (L), Get_HDL_Node (R));
                  if Edge /= No_Net then
                     return Edge;
                  end if;
               end if;
               if Get_Kind (R) = N_EOS then
                  --  It is never EOS!
                  Res := Build_Const_UB32 (Ctxt, 0, 1);
               else
                  Res := Build_Dyadic (Ctxt, Id_And,
                                       Synth_PSL_Expression (Syn_Inst, L),
                                       Synth_PSL_Expression (Syn_Inst, R));
               end if;
            end;
         when N_Or_Bool =>
            pragma Assert (Loc /= No_Location);
            Res := Build_Dyadic
              (Ctxt, Id_Or,
               Synth_PSL_Expression (Syn_Inst, Get_Left (Expr)),
               Synth_PSL_Expression (Syn_Inst, Get_Right (Expr)));
         when N_True =>
            Res := Build_Const_UB32 (Ctxt, 1, 1);
         when N_False
           | N_EOS =>
            Res := Build_Const_UB32 (Ctxt, 0, 1);
         when others =>
            PSL.Errors.Error_Kind ("synth_psl_expr", Expr);
            return No_Net;
      end case;
      Netlists.Locations.Set_Location (Get_Net_Parent (Res), Loc);
      return Res;
   end Synth_PSL_Expression;

   function Synth_Psl_Function_Clock
     (Syn_Inst : Synth_Instance_Acc; Call : Node; Ctxt : Context_Acc)
     return Net
   is
      Clock   : Node;
      Clk     : Valtyp;
      Clk_Net : Net;
   begin
      Clock := Get_Clock_Expression (Call);
      if Clock /= Null_Node then
         Clk := Synth_Expression (Syn_Inst, Clock);
         Clk_Net := Get_Net (Ctxt, Clk);
      else
         Clock := Get_Default_Clock (Call);
         pragma Assert (Clock /= Null_Node);
         Clk_Net := Synth_PSL_Expression (Syn_Inst, Get_Psl_Boolean (Clock));
      end if;
      return Clk_Net;
   end Synth_Psl_Function_Clock;

   function Synth_Psl_Prev (Syn_Inst : Synth_Instance_Acc; Call : Node)
                            return Valtyp
   is
      Ctxt      : constant Context_Acc := Get_Build (Syn_Inst);
      Count     : constant Node := Get_Count_Expression (Call);
      Count_Val : Valtyp;
      Dff       : Net;
      Expr      : Valtyp;
      Clk_Net   : Net;
      Num       : Int64;
   begin
      Expr := Synth_Expression_With_Basetype (Syn_Inst, Get_Expression (Call));

      Clk_Net := Synth_Psl_Function_Clock (Syn_Inst, Call, Ctxt);

      if Count /= Null_Node then
         Count_Val := Synth_Expression (Syn_Inst, Count);
         Num := Read_Discrete (Count_Val);
         pragma Assert (Num >= 1);
      else
         Num := 1;
      end if;

      Dff := Get_Net (Ctxt, Expr);
      for I in 1 .. Num loop
         Dff := Build_Dff (Ctxt, Clk_Net, Dff);
         Set_Location (Dff, Call);
      end loop;

      return Create_Value_Net (Dff, Expr.Typ);
   end Synth_Psl_Prev;

   function Synth_Psl_Stable (Syn_Inst : Synth_Instance_Acc; Call : Node)
                              return Valtyp
   is
      Ctxt    : constant Context_Acc := Get_Build (Syn_Inst);
      DffCurr : Net;
      Dff     : Net;
      Expr    : Valtyp;
      Clk_Net : Net;
      Res     : Net;
   begin
      Expr := Synth_Expression_With_Basetype (Syn_Inst, Get_Expression (Call));

      Clk_Net := Synth_Psl_Function_Clock (Syn_Inst, Call, Ctxt);

      DffCurr := Get_Net (Ctxt, Expr);
      Set_Location (DffCurr, Call);
      Dff := Build_Dff (Ctxt, Clk_Net, DffCurr);
      Set_Location (Dff, Call);

      Res := Build_Compare(Ctxt, Id_Eq, DffCurr, Dff);
      Set_Location (Res, Call);

      return Create_Value_Net (Res, Boolean_Type);

   end Synth_Psl_Stable;

   function Synth_Psl_Rose (Syn_Inst : Synth_Instance_Acc; Call : Node)
                            return Valtyp
   is
      Ctxt    : constant Context_Acc := Get_Build (Syn_Inst);
      DffCurr : Net;
      Dff     : Net;
      NotDff  : Net;
      Clk_Net : Net;
      Expr    : Valtyp;
      Res     : Net;
   begin
      Expr := Synth_Expression (Syn_Inst, Get_Expression (Call));

      Clk_Net := Synth_Psl_Function_Clock (Syn_Inst, Call, Ctxt);

      DffCurr := Get_Net (Ctxt, Expr);
      Set_Location (DffCurr, Call);
      Dff := Build_Dff (Ctxt, Clk_Net, DffCurr);
      Set_Location (Dff, Call);

      NotDff := Build_Monadic (Ctxt, Id_Not, Dff);
      Set_Location (NotDff, Call);

      Res := Build_Dyadic (Ctxt, Id_And,
             NotDff, DffCurr);
      Set_Location (Res, Call);

      return Create_Value_Net (Res, Boolean_Type);

   end Synth_Psl_Rose;

   function Synth_Psl_Fell (Syn_Inst : Synth_Instance_Acc; Call : Node)
                            return Valtyp
   is
      Ctxt       : constant Context_Acc := Get_Build (Syn_Inst);
      DffCurr    : Net;
      NotDffCurr : Net;
      Dff        : Net;
      Clk_Net    : Net;
      Expr       : Valtyp;
      Res        : Net;
   begin
      Expr := Synth_Expression (Syn_Inst, Get_Expression (Call));

      Clk_Net := Synth_Psl_Function_Clock(Syn_Inst, Call, Ctxt);

      DffCurr := Get_Net (Ctxt, Expr);
      Set_Location (DffCurr, Call);
      Dff := Build_Dff (Ctxt, Clk_Net, DffCurr);
      Set_Location (Dff, Call);

      NotDffCurr := Build_Monadic (Ctxt, Id_Not, DffCurr);
      Set_Location (NotDffCurr, Call);

      Res := Build_Dyadic (Ctxt, Id_And, Dff, NotDffCurr);
      Set_Location (Res, Call);

      return Create_Value_Net (Res, Boolean_Type);

   end Synth_Psl_Fell;

   function Synth_Onehot0 (Ctxt : Context_Acc; DffCurr : Net; Call : Node;
                           Vlen : Uns32)
                           return Net
   is
      DffZero    : Net;
      DffOne     : Net;
      DffOneHot0 : Net;
      Res        : Net;
   begin
      -- Create a constant vector of 0 for comparing
      DffZero := Build2_Const_Uns(Ctxt, 0,  Vlen);

      -- Create vector of value 1 for subtraction
      DffOne := Build2_Const_Uns(Ctxt, 1,  Vlen);

      -- Subtraction -> v - 1
      DffOneHot0 := Build_Dyadic (Ctxt, Id_Sub, DffCurr, DffOne);
      Set_Location (DffOneHot0, Call);

      -- Binary And -> v & (v - 1)
      DffOneHot0 := Build_Dyadic (Ctxt, Id_And, DffCurr, DffOneHot0);
      Set_Location (DffOneHot0, Call);

      -- Compare with 0 -> (v & (v - 1)) == 0
      Res := Build_Compare (Ctxt, Id_Eq, DffOneHot0, DffZero);
      Set_Location (Res, Call);

      return Res;
   end Synth_Onehot0;

   function Synth_Psl_Onehot (Syn_Inst : Synth_Instance_Acc; Call : Node)
                              return Valtyp
   is
      Ctxt             : constant Context_Acc := Get_Build (Syn_Inst);
      Expr             : Valtyp;
      DffCurr          : Net;
      DffCurrIsNotZero : Net;
      DffOneHot0       : Net;
      Res              : Net;
      Vlen             : Uns32;
   begin
      -- Get parameter & its length
      Expr := Synth_Expression (Syn_Inst, Get_Expression (Call));
      Vlen := Expr.Typ.W;

      -- First get net of parameter
      DffCurr := Get_Net (Ctxt, Expr);
      Set_Location (DffCurr, Call);

      -- Compare parameter with 0 -> v != 0
      DffCurrIsNotZero := Build_Compare (Ctxt, Id_Ne, DffCurr,
                                         Build2_Const_Uns(Ctxt, 0, Vlen));
      Set_Location (DffCurrIsNotZero, Call);

      -- Synth onehot0
      DffOneHot0 := Synth_Onehot0 (Ctxt, DffCurr, Call, Vlen);
      Set_Location (DffOneHot0, Call);

      -- Final Binary And -> (v != 0) & ((v & (v - 1)) == 0)
      Res := Build_Dyadic (Ctxt, Id_And, DffOneHot0, DffCurrIsNotZero);
      Set_Location (Res, Call);

      return Create_Value_Net (Res, Boolean_Type);
   end Synth_Psl_Onehot;

   function Synth_Psl_Onehot0 (Syn_Inst : Synth_Instance_Acc; Call : Node)
                               return Valtyp
   is
      Ctxt    : constant Context_Acc := Get_Build (Syn_Inst);
      Expr    : Valtyp;
      Vlen    : Uns32;
      DffCurr : Net;
      Res     : Net;
   begin
      -- Get parameter & its length
      Expr := Synth_Expression (Syn_Inst, Get_Expression (Call));
      Vlen := Expr.Typ.W;

      -- First get net of parameter
      DffCurr := Get_Net (Ctxt, Expr);
      Set_Location (DffCurr, Call);

      -- Synth onehot0
      Res := Synth_Onehot0 (Ctxt, DffCurr, Call, Vlen);

      return Create_Value_Net (Res, Boolean_Type);
   end Synth_Psl_Onehot0;

   subtype And_Or_Module_Id is Module_Id range Id_And .. Id_Or;

   function Synth_Short_Circuit (Syn_Inst : Synth_Instance_Acc;
                                 Id : And_Or_Module_Id;
                                 Left_Expr : Node;
                                 Right_Expr : Node;
                                 Typ : Type_Acc;
                                 Expr : Node) return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Left : Valtyp;
      Right : Valtyp;
      Val : Int64;
      N : Net;
   begin
      --  The short-circuit value.
      case Id is
         when Id_And =>
            Val := 0;
         when Id_Or =>
            Val := 1;
      end case;

      Left := Synth_Expression_With_Type (Syn_Inst, Left_Expr, Typ);
      if Left = No_Valtyp then
         --  Propagate error.
         return No_Valtyp;
      end if;
      if Is_Static_Val (Left.Val)
        and then Get_Static_Discrete (Left) = Val
      then
         --  Short-circuit when the left operand determines the result.
         return Create_Value_Discrete (Val, Typ);
      end if;

      Strip_Const (Left);
      Right := Synth_Expression_With_Type (Syn_Inst, Right_Expr, Typ);
      if Right = No_Valtyp then
         --  Propagate error.
         return No_Valtyp;
      end if;
      Strip_Const (Right);

      if Is_Static_Val (Right.Val)
        and then Get_Static_Discrete (Right) = Val
      then
         --  If the right operand can determine the result, return it.
         return Create_Value_Discrete (Val, Typ);
      end if;

      --  Return a static value if both operands are static.
      --  Note: we know the value of left if it is not constant.
      if Is_Static_Val (Left.Val) and then Is_Static_Val (Right.Val) then
         Val := Get_Static_Discrete (Right);
         return Create_Value_Discrete (Val, Typ);
      end if;

      --  Non-static result.
      N := Build_Dyadic (Ctxt, Id,
                         Get_Net (Ctxt, Left), Get_Net (Ctxt, Right));
      Set_Location (N, Expr);
      return Create_Value_Net (N, Typ);
   end Synth_Short_Circuit;

   function Synth_Expression_With_Type (Syn_Inst : Synth_Instance_Acc;
                                        Expr : Node;
                                        Expr_Type : Type_Acc) return Valtyp is
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Edge : Net;
            begin
               --  Match clock-edge (only for synthesis)
               if Def = Iir_Predefined_Boolean_And
                 and then Hook_Signal_Expr = null
               then
                  Edge := Synth_Clock_Edge (Syn_Inst,
                                            Get_Left (Expr), Get_Right (Expr));
                  if Edge /= No_Net then
                     return Create_Value_Net (Edge, Boolean_Type);
                  end if;
               end if;

               --  Specially handle short-circuit operators.
               case Def is
                  when Iir_Predefined_Boolean_And =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_And, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type, Expr);
                  when Iir_Predefined_Boolean_Or =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_Or, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type, Expr);
                  when Iir_Predefined_Bit_And =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_And, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type, Expr);
                  when Iir_Predefined_Bit_Or =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_Or, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type, Expr);
                  when Iir_Predefined_None =>
                     if Error_Ieee_Operator (Imp, Expr) then
                        return No_Valtyp;
                     else
                        return Synth_User_Operator
                          (Syn_Inst, Get_Left (Expr), Get_Right (Expr), Expr);
                     end if;
                  when others =>
                     return Synth_Dyadic_Operation
                       (Syn_Inst, Imp,
                        Get_Left (Expr), Get_Right (Expr), Expr);
               end case;
            end;
         when Iir_Kinds_Monadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
            begin
               if Def = Iir_Predefined_None then
                  if Error_Ieee_Operator (Imp, Expr) then
                     return No_Valtyp;
                  else
                     return Synth_User_Operator
                       (Syn_Inst, Get_Operand (Expr), Null_Node, Expr);
                  end if;
               else
                  return Synth_Monadic_Operation
                    (Syn_Inst, Imp, Get_Operand (Expr), Expr);
               end if;
            end;
         when Iir_Kind_Simple_Name
            | Iir_Kind_Selected_Name
            | Iir_Kind_Interface_Signal_Declaration --  For PSL.
            | Iir_Kind_Signal_Declaration   -- For PSL.
            | Iir_Kind_Object_Alias_Declaration   -- For PSL
            | Iir_Kind_Non_Object_Alias_Declaration   -- For PSL
            | Iir_Kind_Implicit_Dereference
            | Iir_Kind_Dereference =>
            declare
               Res : Valtyp;
            begin
               Res := Synth_Name (Syn_Inst, Expr);
               if Res.Val /= null
                 and then
                 (Res.Val.Kind = Value_Signal
                    or else (Res.Val.Kind = Value_Alias
                               and then Res.Val.A_Obj.Kind = Value_Signal))
               then
                  if Hook_Signal_Expr /= null then
                     return Hook_Signal_Expr (Res);
                  end if;
                  Error_Msg_Synth
                    (+Expr, "cannot use signal value during elaboration");
                  return No_Valtyp;
               end if;
               if Res.Typ /= null
                 and then Res.Typ.W = 0 and then Res.Val.Kind /= Value_Memory
               then
                  --  This is a null object.  As nothing can be done about it,
                  --  returns 0.
                  return Create_Value_Memtyp (Create_Memory_Zero (Res.Typ));
               end if;
               return Res;
            end;
         when Iir_Kind_Reference_Name =>
            --  Only used for anonymous signals in internal association.
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            declare
               Base : Valtyp;
               Typ : Type_Acc;
               Off : Value_Offsets;
               Res : Valtyp;

               Dyn : Dyn_Name;
            begin
               Synth_Assignment_Prefix (Syn_Inst, Expr, Base, Typ, Off, Dyn);
               if Base = No_Valtyp then
                  --  Propagate error.
                  return No_Valtyp;
               end if;
               if Base.Val.Kind = Value_Signal
                 and then Hook_Signal_Expr /= null
               then
                  Base := Hook_Signal_Expr (Base);
               end if;
               if Dyn.Voff = No_Net and then Is_Static (Base.Val) then
                  Res := Create_Value_Memtyp
                    ((Typ, Base.Val.Mem + Off.Mem_Off));
                  return Res;
               end if;
               return Synth_Read_Memory
                 (Syn_Inst, Base, Typ, Off.Net_Off, Dyn, Expr);
            end;
         when Iir_Kind_Selected_Element =>
            declare
               Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Expr));
               Pfx : constant Node := Get_Prefix (Expr);
               Res_Typ : Type_Acc;
               N : Net;
               Val : Valtyp;
               Res : Valtyp;
            begin
               Val := Synth_Expression (Syn_Inst, Pfx);
               Strip_Const (Val);
               Res_Typ := Val.Typ.Rec.E (Idx + 1).Typ;
               if Res_Typ.W = 0 and then Val.Val.Kind /= Value_Memory then
                  --  This is a null object.  As nothing can be done about it,
                  --  returns 0.
                  return Create_Value_Memtyp (Create_Memory_Zero (Res_Typ));
               elsif Is_Static (Val.Val) then
                  Res := Create_Value_Memory (Res_Typ);
                  Copy_Memory
                    (Res.Val.Mem,
                     Val.Val.Mem + Val.Typ.Rec.E (Idx + 1).Offs.Mem_Off,
                     Res_Typ.Sz);
                  return Res;
               else
                  N := Build_Extract (Ctxt, Get_Net (Ctxt, Val),
                                      Val.Typ.Rec.E (Idx + 1).Offs.Net_Off,
                                      Get_Type_Width (Res_Typ));
                  Set_Location (N, Expr);
                  return Create_Value_Net (N, Res_Typ);
               end if;
            end;
         when Iir_Kind_Character_Literal =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Integer_Literal =>
            declare
               Res : Valtyp;
            begin
               Res := Create_Value_Memory (Expr_Type);
               Write_Discrete (Res, Get_Value (Expr));
               return Res;
            end;
         when Iir_Kind_Floating_Point_Literal =>
            return Create_Value_Float (Get_Fp_Value (Expr), Expr_Type);
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return Create_Value_Discrete
              (Get_Physical_Value (Expr), Expr_Type);
         when Iir_Kind_String_Literal8 =>
            return Elab.Vhdl_Expr.Exec_String_Literal
              (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Enumeration_Literal =>
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Type_Conversion =>
            return Synth_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr),
               Get_Subtype_Object (Syn_Inst, Get_Type (Get_Type_Mark (Expr))));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
            begin
               case Get_Implicit_Definition (Imp) is
                  when Iir_Predefined_Operators
                     | Iir_Predefined_Ieee_Numeric_Std_Binary_Operators
                     | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Operators =>
                     return Synth_Operator_Function_Call (Syn_Inst, Expr);
                  when Iir_Predefined_None =>
                     return Synth_User_Function_Call (Syn_Inst, Expr);
                  when others =>
                     return Synth_Predefined_Function_Call (Syn_Inst, Expr);
               end case;
            end;
         when Iir_Kind_Aggregate =>
            return Synth.Vhdl_Aggr.Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Simple_Aggregate =>
            return Elab.Vhdl_Expr.Exec_Simple_Aggregate (Syn_Inst, Expr);
         when Iir_Kind_Parenthesis_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr), Expr_Type);
         when Iir_Kind_Left_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Left), Expr_Type);
            end;
         when Iir_Kind_Right_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Right), Expr_Type);
            end;
         when Iir_Kind_High_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Dir_To =>
                     V := B.Right;
                  when Dir_Downto =>
                     V := B.Left;
               end case;
               return Create_Value_Discrete (Int64 (V), Expr_Type);
            end;
         when Iir_Kind_Low_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Dir_To =>
                     V := B.Left;
                  when Dir_Downto =>
                     V := B.Right;
               end case;
               return Create_Value_Discrete (Int64 (V), Expr_Type);
            end;
         when Iir_Kind_Length_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Len), Expr_Type);
            end;
         when Iir_Kind_Ascending_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int64;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Dir_To =>
                     V := 1;
                  when Dir_Downto =>
                     V := 0;
               end case;
               return Create_Value_Discrete (V, Expr_Type);
            end;

         when Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute =>
            declare
               Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
               Param : constant Node := Get_Parameter (Expr);
               V : Valtyp;
               Dtype : Type_Acc;
            begin
               V := Synth_Expression (Syn_Inst, Param);
               Dtype := Get_Subtype_Object (Syn_Inst, Get_Type (Expr));
               --  FIXME: to be generalized.  Not always as simple as a
               --  subtype conversion.
               return Synth_Subtype_Conversion (Ctxt, V, Dtype, False, Expr);
            end;
         when Iir_Kind_Low_Type_Attribute =>
            return Synth_Low_High_Type_Attribute (Syn_Inst, Expr, Dir_To);
         when Iir_Kind_High_Type_Attribute =>
            return Synth_Low_High_Type_Attribute (Syn_Inst, Expr, Dir_Downto);
         when Iir_Kind_Value_Attribute =>
            return Elab.Vhdl_Expr.Exec_Value_Attribute (Syn_Inst, Expr);
         when Iir_Kind_Image_Attribute =>
            return Elab.Vhdl_Expr.Exec_Image_Attribute (Syn_Inst, Expr);
         when Iir_Kind_Instance_Name_Attribute =>
            return Elab.Vhdl_Expr.Exec_Instance_Name_Attribute
              (Syn_Inst, Expr);
         when Iir_Kind_Null_Literal =>
            return Create_Value_Access (Null_Heap_Index, Expr_Type);
         when Iir_Kind_Allocator_By_Subtype =>
            declare
               T : Type_Acc;
               Acc : Heap_Index;
            begin
               T := Synth_Subtype_Indication
                 (Syn_Inst, Get_Subtype_Indication (Expr));
               Acc := Allocate_By_Type (T);
               return Create_Value_Access (Acc, Expr_Type);
            end;
         when Iir_Kind_Allocator_By_Expression =>
            declare
               V : Valtyp;
               Acc : Heap_Index;
            begin
               V := Synth_Expression_With_Type
                 (Syn_Inst, Get_Expression (Expr), Expr_Type.Acc_Acc);
               Acc := Allocate_By_Value (V);
               return Create_Value_Access (Acc, Expr_Type);
            end;
         when Iir_Kind_Stable_Attribute =>
            Error_Msg_Synth (+Expr, "signal attribute not supported");
            return No_Valtyp;
         when Iir_Kind_Psl_Prev =>
            return Synth_Psl_Prev (Syn_Inst, Expr);
         when Iir_Kind_Psl_Stable =>
            return Synth_Psl_Stable (Syn_Inst, Expr);
         when Iir_Kind_Psl_Rose =>
            return Synth_Psl_Rose(Syn_Inst, Expr);
         when Iir_Kind_Psl_Fell =>
            return Synth_Psl_Fell(Syn_Inst, Expr);
         when Iir_Kind_Psl_Onehot =>
            return Synth_Psl_Onehot(Syn_Inst, Expr);
         when Iir_Kind_Psl_Onehot0 =>
            return Synth_Psl_Onehot0(Syn_Inst, Expr);
         when Iir_Kind_Overflow_Literal =>
            Error_Msg_Synth (+Expr, "out of bound expression");
            return No_Valtyp;
         when Iir_Kind_Event_Attribute =>
            if Hook_Signal_Attribute /= null then
               return Hook_Signal_Attribute (Syn_Inst, Expr);
            end if;
            Error_Msg_Synth (+Expr, "signal attributes not allowed");
            return No_Valtyp;
         when others =>
            Error_Kind ("synth_expression_with_type", Expr);
      end case;
   end Synth_Expression_With_Type;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Valtyp
   is
      Etype : Node;
   begin
      Etype := Get_Type (Expr);

      case Get_Kind (Expr) is
         when Iir_Kind_High_Array_Attribute
           |  Iir_Kind_Low_Array_Attribute
           |  Iir_Kind_Indexed_Name
           |  Iir_Kind_Integer_Literal =>
            --  For array attributes: the type is the type of the index, which
            --  is not synthesized as a type (only as an index).
            --
            --  Likewise for indexed names.
            --
            --  For integer_literal, the type is not really needed, and it
            --  may be created by static evaluation of an array attribute.
            Etype := Get_Base_Type (Etype);
         when others =>
            null;
      end case;

      return Synth_Expression_With_Type
        (Syn_Inst, Expr, Get_Subtype_Object (Syn_Inst, Etype));
   end Synth_Expression;

   function Synth_Expression_With_Basetype
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp
   is
      Basetype : Type_Acc;
   begin
      Basetype := Get_Subtype_Object
        (Syn_Inst, Get_Base_Type (Get_Type (Expr)));
      return Synth_Expression_With_Type (Syn_Inst, Expr, Basetype);
   end Synth_Expression_With_Basetype;
end Synth.Vhdl_Expr;
