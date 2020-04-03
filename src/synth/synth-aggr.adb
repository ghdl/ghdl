--  Aggregates synthesis.
--  Copyright (C) 2020 Tristan Gingold
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

with Types; use Types;

with Netlists; use Netlists;
with Netlists.Utils; use Netlists.Utils;

with Vhdl.Errors; use Vhdl.Errors;

with Synth.Errors; use Synth.Errors;
with Synth.Expr; use Synth.Expr;
with Synth.Stmts; use Synth.Stmts;
with Synth.Decls; use Synth.Decls;

package body Synth.Aggr is
   type Stride_Array is array (Dim_Type range <>) of Nat32;

   function Get_Index_Offset
     (Index : Int64; Bounds : Bound_Type; Expr : Iir) return Uns32
   is
      Left : constant Int64 := Int64 (Bounds.Left);
      Right : constant Int64 := Int64 (Bounds.Right);
   begin
      case Bounds.Dir is
         when Iir_To =>
            if Index >= Left and then Index <= Right then
               -- to
               return Uns32 (Index - Left);
            end if;
         when Iir_Downto =>
            if Index <= Left and then Index >= Right then
               -- downto
               return Uns32 (Left - Index);
            end if;
      end case;
      Error_Msg_Synth (+Expr, "index out of bounds");
      return 0;
   end Get_Index_Offset;

   function Get_Index_Offset
     (Index : Valtyp; Bounds : Bound_Type; Expr : Iir) return Uns32 is
   begin
      return Get_Index_Offset (Read_Discrete (Index), Bounds, Expr);
   end Get_Index_Offset;

   function Fill_Stride (Typ : Type_Acc) return Stride_Array is
   begin
      case Typ.Kind is
         when Type_Vector =>
            return (1 => 1);
         when Type_Array =>
            declare
               Bnds : constant Bound_Array_Acc := Typ.Abounds;
               Res : Stride_Array (1 .. Bnds.Ndim);
               Stride : Nat32;
            begin
               Stride := 1;
               for I in reverse 2 .. Bnds.Ndim loop
                  Res (Dim_Type (I)) := Stride;
                  Stride := Stride * Nat32 (Bnds.D (I).Len);
               end loop;
               Res (1) := Stride;
               return Res;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Fill_Stride;

   procedure Fill_Array_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Res : Valtyp_Array_Acc;
                                   Typ : Type_Acc;
                                   First_Pos : Nat32;
                                   Strides : Stride_Array;
                                   Dim : Dim_Type;
                                   Const_P : out Boolean)
   is
      Bound : constant Bound_Type := Get_Array_Bound (Typ, Dim);
      El_Typ : constant Type_Acc := Get_Array_Element (Typ);
      Stride : constant Nat32 := Strides (Dim);
      Value : Node;
      Assoc : Node;

      procedure Set_Elem (Pos : Nat32)
      is
         Sub_Const : Boolean;
         Val : Valtyp;
      begin
         if Dim = Strides'Last then
            Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Typ);
            Val := Synth_Subtype_Conversion (Val, El_Typ, False, Value);
            pragma Assert (Res (Pos) = No_Valtyp);
            Res (Pos) := Val;
            if Const_P and then not Is_Static (Val.Val) then
               Const_P := False;
            end if;
         else
            Fill_Array_Aggregate
              (Syn_Inst, Value, Res, Typ, Pos, Strides, Dim + 1, Sub_Const);
            if not Sub_Const then
               Const_P := False;
            end if;
         end if;
      end Set_Elem;

      procedure Set_Vector (Pos : Nat32; Len : Nat32; Val : Valtyp) is
      begin
         pragma Assert (Dim = Strides'Last);
         if Len = 0 then
            return;
         end if;
         pragma Assert (Res (Pos) = No_Valtyp);
         Res (Pos) := Val;

         --  Mark following slots as busy so that 'others => x' won't fill
         --  them.
         for I in 2 .. Len loop
            Res (Pos + I - 1).Typ := Val.Typ;
         end loop;

         if Const_P and then not Is_Static (Val.Val) then
            Const_P := False;
         end if;
      end Set_Vector;

      Pos : Nat32;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := First_Pos;
      Const_P := True;
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  if Get_Element_Type_Flag (Assoc) then
                     if Pos >= First_Pos + Stride * Nat32 (Bound.Len) then
                        Error_Msg_Synth (+Assoc, "element out of array bound");
                     else
                        Set_Elem (Pos);
                        Pos := Pos + Stride;
                     end if;
                  else
                     declare
                        Val : Valtyp;
                        Val_Len : Uns32;
                     begin
                        Val := Synth_Expression_With_Basetype
                          (Syn_Inst, Value);
                        Val_Len := Get_Bound_Length (Val.Typ, 1);
                        pragma Assert (Stride = 1);
                        if Pos - First_Pos > Nat32 (Bound.Len - Val_Len) then
                           Error_Msg_Synth
                             (+Assoc, "element out of array bound");
                        else
                           Set_Vector (Pos, Nat32 (Val_Len), Val);
                           Pos := Pos + Nat32 (Val_Len);
                        end if;
                     end;
                  end if;
               when Iir_Kind_Choice_By_Others =>
                  pragma Assert (Get_Element_Type_Flag (Assoc));
                  declare
                     Last_Pos : constant Nat32 :=
                       First_Pos + Nat32 (Bound.Len) * Stride;
                  begin
                     while Pos < Last_Pos loop
                        if Res (Pos) = No_Valtyp then
                           --  FIXME: the check is not correct if there is
                           --   an array.
                           Set_Elem (Pos);
                        end if;
                        Pos := Pos + Stride;
                     end loop;
                  end;
               when Iir_Kind_Choice_By_Expression =>
                  pragma Assert (Get_Element_Type_Flag (Assoc));
                  declare
                     Ch : constant Node := Get_Choice_Expression (Assoc);
                     Idx : Valtyp;
                     Off : Nat32;
                  begin
                     Idx := Synth_Expression (Syn_Inst, Ch);
                     if not Is_Static (Idx.Val) then
                        Error_Msg_Synth (+Ch, "choice is not static");
                     else
                        Off := Nat32 (Get_Index_Offset (Idx, Bound, Ch));
                        Set_Elem (First_Pos + Off * Stride);
                     end if;
                  end;
               when Iir_Kind_Choice_By_Range =>
                  declare
                     Ch : constant Node := Get_Choice_Range (Assoc);
                     Rng : Discrete_Range_Type;
                     Val : Valtyp;
                     Rng_Len : Width;
                     Off : Nat32;
                  begin
                     Synth_Discrete_Range (Syn_Inst, Ch, Rng);
                     if Get_Element_Type_Flag (Assoc) then
                        Val := Create_Value_Discrete
                          (Rng.Left,
                           Get_Subtype_Object (Syn_Inst,
                                               Get_Base_Type (Get_Type (Ch))));
                        while In_Range (Rng, Read_Discrete (Val)) loop
                           Off := Nat32 (Get_Index_Offset (Val, Bound, Ch));
                           Set_Elem (First_Pos + Off * Stride);
                           Update_Index (Rng, Val);
                        end loop;
                     else
                        --  The direction must be the same.
                        if Rng.Dir /= Bound.Dir then
                           Error_Msg_Synth
                             (+Assoc, "direction of range does not match "
                                & "direction of array");
                        end if;
                        --  FIXME: can the expression be unbounded ?
                        Val := Synth_Expression_With_Basetype
                          (Syn_Inst, Value);
                        --  The length must match the range.
                        Rng_Len := Get_Range_Length (Rng);
                        if Get_Bound_Length (Val.Typ, 1) /= Rng_Len then
                           Error_Msg_Synth
                             (+Value, "length doesn't match range");
                        end if;
                        pragma Assert (Stride = 1);
                        Off := Nat32 (Get_Index_Offset (Rng.Left, Bound, Ch));
                        Set_Vector (First_Pos + Off, Nat32 (Rng_Len), Val);
                     end if;
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

   procedure Fill_Record_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node;
                                    Rec : Valtyp_Array_Acc;
                                    Const_P : out Boolean)
   is
      El_List : constant Node_Flist :=
        Get_Elements_Declaration_List (Get_Type (Aggr));
      Value : Node;
      Assoc : Node;
      Pos : Nat32;

      procedure Set_Elem (Pos : Nat32)
      is
         Val : Valtyp;
         El_Type : Type_Acc;
      begin
         El_Type := Get_Subtype_Object
           (Syn_Inst, Get_Type (Get_Nth_Element (El_List, Natural (Pos))));
         Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Type);
         if Const_P and not Is_Static (Val.Val) then
            Const_P := False;
         end if;
         Val := Synth_Subtype_Conversion (Val, El_Type, False, Value);
         Rec (Nat32 (Pos + 1)) := Val;
      end Set_Elem;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := 0;
      Const_P := True;
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  Set_Elem (Pos);
                  Pos := Pos + 1;
               when Iir_Kind_Choice_By_Others =>
                  for I in Rec'Range loop
                     if Rec (I) = No_Valtyp then
                        Set_Elem (I - 1);
                     end if;
                  end loop;
               when Iir_Kind_Choice_By_Name =>
                  Pos := Nat32 (Get_Element_Position
                                  (Get_Named_Entity
                                     (Get_Choice_Name (Assoc))));
                  Set_Elem (Pos);
               when others =>
                  Error_Msg_Synth
                    (+Assoc, "unhandled association form");
            end case;
            Assoc := Get_Chain (Assoc);
            exit when Is_Null (Assoc);
            exit when not Get_Same_Alternative_Flag (Assoc);
         end loop;
      end loop;
   end Fill_Record_Aggregate;

   function Valtyp_Array_To_Net (Tab : Valtyp_Array) return Net
   is
      Res : Net;
      Arr : Net_Array_Acc;
      Idx : Nat32;
   begin
      Arr := new Net_Array (Tab'Range);
      Idx := 0;
      for I in Arr'Range loop
         if Tab (I).Val /= null then
            Idx := Idx + 1;
            Arr (Idx) := Get_Net (Tab (I));
         end if;
      end loop;
      Concat_Array (Arr (1 .. Idx), Res);
      Free_Net_Array (Arr);
      return Res;
   end Valtyp_Array_To_Net;

   function Synth_Aggregate_Array (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Aggr_Type : Type_Acc) return Valtyp
   is
      Strides : constant Stride_Array := Fill_Stride (Aggr_Type);
      Flen : constant Iir_Index32 := Get_Array_Flat_Length (Aggr_Type);
      Tab_Res : Valtyp_Array_Acc;
      Const_P : Boolean;
      Res : Valtyp;
   begin
      Tab_Res := new Valtyp_Array'(1 .. Nat32 (Flen) => No_Valtyp);

      Fill_Array_Aggregate
        (Syn_Inst, Aggr, Tab_Res, Aggr_Type, 1, Strides, 1, Const_P);

      --  TODO: check all element types have the same bounds ?

      if Const_P then
         declare
            Off : Size_Type;
         begin
            Res := Create_Value_Memory (Aggr_Type);
            Off := 0;
            for I in Tab_Res'Range loop
               if Tab_Res (I).Val /= null then
                  --  There can be holes due to sub-arrays.
                  Write_Value (Res.Val.Mem + Off, Tab_Res (I));
                  Off := Off + Tab_Res (I).Typ.Sz;
               end if;
            end loop;
            pragma Assert (Off = Aggr_Type.Sz);
         end;
      else
         Res := Create_Value_Net
           (Valtyp_Array_To_Net (Tab_Res.all), Aggr_Type);
      end if;

      Free_Valtyp_Array (Tab_Res);

      return Res;
   end Synth_Aggregate_Array;

   function Synth_Aggregate_Record (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node;
                                    Aggr_Type : Type_Acc) return Valtyp
   is
      Tab_Res : Valtyp_Array_Acc;
      Res : Valtyp;
      Const_P : Boolean;
   begin
      --  Allocate the result.
      Tab_Res :=
        new Valtyp_Array'(1 .. Nat32 (Aggr_Type.Rec.Len) => No_Valtyp);

      Fill_Record_Aggregate (Syn_Inst, Aggr, Tab_Res, Const_P);

      if Const_P then
         Res := Create_Value_Memory (Aggr_Type);
         for I in Aggr_Type.Rec.E'Range loop
            Write_Value (Res.Val.Mem + Aggr_Type.Rec.E (I).Moff,
                         Tab_Res (Nat32 (I)));
         end loop;
      else
         Res := Create_Value_Net
           (Valtyp_Array_To_Net (Tab_Res.all), Aggr_Type);
      end if;

      Free_Valtyp_Array (Tab_Res);

      return Res;
   end Synth_Aggregate_Record;

   --  Aggr_Type is the type from the context.
   function Synth_Aggregate (Syn_Inst : Synth_Instance_Acc;
                             Aggr : Node;
                             Aggr_Type : Type_Acc) return Valtyp is
   begin
      case Aggr_Type.Kind is
         when Type_Unbounded_Array | Type_Unbounded_Vector =>
            declare
               Res_Type : Type_Acc;
            begin
               Res_Type := Decls.Synth_Array_Subtype_Indication
                 (Syn_Inst, Get_Type (Aggr));
               return Synth_Aggregate_Array (Syn_Inst, Aggr, Res_Type);
            end;
         when Type_Vector | Type_Array =>
            return Synth_Aggregate_Array (Syn_Inst, Aggr, Aggr_Type);
         when Type_Record =>
            return Synth_Aggregate_Record (Syn_Inst, Aggr, Aggr_Type);
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Aggregate;

end Synth.Aggr;
