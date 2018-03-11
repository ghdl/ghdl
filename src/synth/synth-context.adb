--  Synthesis context.
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

with Ada.Unchecked_Deallocation;

with Types; use Types;
with Grt.Types; use Grt.Types;
with Errorout; use Errorout;
with Iirs_Utils;

with Std_Package;
with Ieee.Std_Logic_1164;

with Simul.Annotations; use Simul.Annotations;
with Simul.Execution;

with Netlists.Builders; use Netlists.Builders;

with Synth.Types; use Synth.Types;
with Synth.Errors; use Synth.Errors;
with Synth.Expr; use Synth.Expr;

package body Synth.Context is
   function Make_Instance (Sim_Inst : Block_Instance_Acc)
                          return Synth_Instance_Acc
   is
      Res : Synth_Instance_Acc;
   begin
      Res := new Synth_Instance_Type'(Max_Objs => Sim_Inst.Max_Objs,
                                      M => No_Module,
                                      Name => No_Sname,
                                      Sim => Sim_Inst,
                                      Objects => (others => null));
      pragma Assert (Instance_Map (Sim_Inst.Id) = null);
      Instance_Map (Sim_Inst.Id) := Res;
      return Res;
   end Make_Instance;

   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Synth_Instance_Type, Synth_Instance_Acc);
   begin
      Instance_Map (Synth_Inst.Sim.Id) := null;
      Deallocate (Synth_Inst);
   end Free_Instance;

   function Alloc_Wire (Kind : Wire_Kind; Obj : Iir; Rng : Value_Range_Acc)
                       return Value_Acc is
   begin
      Wire_Id_Table.Append ((Kind => Kind,
                             Mark_Flag => False,
                             Decl => Obj,
                             Gate => No_Net,
                             Cur_Assign => No_Assign));
      return Create_Value_Wire (Wire_Id_Table.Last, Rng);
   end Alloc_Wire;

   function Alloc_Object
     (Kind : Wire_Kind; Obj : Iir; Val : Iir_Value_Literal_Acc)
     return Value_Acc
   is
      Obj_Type : constant Iir := Get_Type (Obj);
      Btype : constant Iir := Get_Base_Type (Obj_Type);
   begin
      case Get_Kind (Btype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            if Is_Bit_Type (Btype) then
               return Alloc_Wire (Kind, Obj, null);
            else
               --  TODO
               raise Internal_Error;
            end if;
         when Iir_Kind_Array_Type_Definition =>
            --  Well known array types.
            if Btype = Ieee.Std_Logic_1164.Std_Logic_Vector_Type
              or else Btype = Ieee.Std_Logic_1164.Std_Ulogic_Vector_Type
            then
               return Alloc_Wire
                 (Kind, Obj, Bounds_To_Range (Val.Bounds.D (1)));
            end if;
            if Is_Bit_Type (Get_Element_Subtype (Btype))
              and then Iirs_Utils.Get_Nbr_Dimensions (Btype) = 1
            then
               --  A vector of bits.
               return Alloc_Wire
                 (Kind, Obj, Bounds_To_Range (Val.Bounds.D (1)));
            else
               raise Internal_Error;
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Alloc_Object;

   procedure Make_Object (Syn_Inst : Synth_Instance_Acc;
                          Kind : Wire_Kind;
                          Obj : Iir)
   is
      Otype : constant Iir := Get_Type (Obj);
      Slot : constant Object_Slot_Type := Get_Info (Obj).Slot;
      Val : Value_Acc;
   begin
      Val := Alloc_Object (Kind, Obj, Syn_Inst.Sim.Objects (Slot));
      if Val = null then
         Error_Msg_Synth (+Obj, "%n is not supported", +Otype);
         return;
      end if;

      pragma Assert (Syn_Inst.Objects (Slot) = null);
      Syn_Inst.Objects (Slot) := Val;
   end Make_Object;

   function Get_Net (Val : Value_Acc) return Net is
   begin
      case Val.Kind is
         when Value_Wire =>
            return Get_Current_Value (Val.W);
         when Value_Net =>
            return Val.N;
         when Value_Lit =>
            case Val.Lit.Kind is
               when Iir_Value_B1 =>
                  pragma Assert
                    (Val.Lit_Type = Std_Package.Boolean_Type_Definition
                       or else Val.Lit_Type = Std_Package.Bit_Type_Definition);
                  return Build_Const_UB32
                    (Build_Context, Ghdl_B1'Pos (Val.Lit.B1), 1);
               when Iir_Value_E8 =>
                  if Is_Bit_Type (Val.Lit_Type) then
                     declare
                        V, Xz : Uns32;
                     begin
                        To_Logic (Val.Lit, V, Xz);
                        if Xz = 0 then
                           return Build_Const_UB32 (Build_Context, V, 1);
                        else
                           return Build_Const_UL32 (Build_Context, V, Xz, 1);
                        end if;
                     end;
                  else
                     --  State machine.
                     raise Internal_Error;
                  end if;
               when Iir_Value_I64 =>
                  if Val.Lit.I64 >= 0 then
                     for I in 1 .. 32 loop
                        if Val.Lit.I64 < (2**I) then
                           return Build_Const_UB32
                             (Build_Context, Uns32 (Val.Lit.I64), Width (I));
                        end if;
                     end loop;
                     --  Need Uconst64
                     raise Internal_Error;
                  else
                     --  Need Sconst32/Sconst64
                     raise Internal_Error;
                  end if;
               when Iir_Value_Array =>
                  if Is_Vector_Type (Val.Lit_Type) then
                     if Val.Lit.Bounds.D (1).Length <= 32 then
                        declare
                           Len : constant Iir_Index32 := Val.Lit.Val_Array.Len;
                           R_Val, R_Xz : Uns32;
                           V, Xz : Uns32;
                        begin
                           R_Val := 0;
                           R_Xz := 0;
                           for I in 1 .. Len loop
                              To_Logic (Val.Lit.Val_Array.V (I), V, Xz);
                              R_Val :=
                                R_Val or Shift_Left (V, Natural (Len - I));
                              R_Xz :=
                                R_Xz or Shift_Left (Xz, Natural (Len - I));
                           end loop;
                           if R_Xz = 0 then
                              return Build_Const_UB32
                                (Build_Context, R_Val, Uns32 (Len));
                           else
                              return Build_Const_UL32
                                (Build_Context, R_Val, R_Xz, Uns32 (Len));
                           end if;
                        end;
                     else
                        --  Need Uconst64 / UconstBig
                        raise Internal_Error;
                     end if;
                  else
                     raise Internal_Error;
                  end if;
               when others =>
                  raise Internal_Error;
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Net;

   function Get_Value (Inst : Synth_Instance_Acc; Obj : Iir) return Value_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Obj);
      Sim_Inst : constant Block_Instance_Acc :=
        Simul.Execution.Get_Instance_By_Scope (Inst.Sim, Info.Obj_Scope);
      Val : Value_Acc;
   begin
      Val := Instance_Map (Sim_Inst.Id).Objects (Info.Slot);
      pragma Assert (Val /= null);
      return Val;
   end Get_Value;

end Synth.Context;
