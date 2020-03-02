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
with Tables;
with Types_Utils; use Types_Utils;
with Name_Table; use Name_Table;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils;

with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Concats;

with Synth.Expr; use Synth.Expr;
with Netlists.Locations;

package body Synth.Context is
   package Packages_Table is new Tables
     (Table_Component_Type => Synth_Instance_Acc,
      Table_Index_Type => Instance_Id,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Make_Base_Instance return Synth_Instance_Acc
   is
      Base : Base_Instance_Acc;
      Top_Module : Module;
      Res : Synth_Instance_Acc;
   begin
      Top_Module :=
        New_Design (New_Sname_Artificial (Get_Identifier ("top"), No_Sname));
      pragma Assert (Build_Context = null);
      Build_Context := Build_Builders (Top_Module);

      Base := new Base_Instance_Type'(Builder => Build_Context,
                                      Top_Module => Top_Module,
                                      Cur_Module => No_Module,
                                      Bit0 => No_Net,
                                      Bit1 => No_Net);

      Res := new Synth_Instance_Type'(Max_Objs => Global_Info.Nbr_Objects,
                                      Is_Const => False,
                                      Base => Base,
                                      Name => No_Sname,
                                      Block_Scope => Global_Info,
                                      Up_Block => null,
                                      Source_Scope => Null_Node,
                                      Elab_Objects => 0,
                                      Objects => (others => null));
      return Res;
   end Make_Base_Instance;

   procedure Free_Base_Instance is
   begin
      --  TODO: really free.
      Build_Context := null;
      Packages_Table.Init;
   end Free_Base_Instance;

   function Make_Instance (Parent : Synth_Instance_Acc;
                           Blk : Node;
                           Name : Sname := No_Sname)
                          return Synth_Instance_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Blk);
      Scope : Sim_Info_Acc;
      Res : Synth_Instance_Acc;
   begin
      if Get_Kind (Blk) = Iir_Kind_Architecture_Body then
         --  Architectures are extensions of entities.
         Scope := Get_Info (Vhdl.Utils.Get_Entity (Blk));
      else
         Scope := Info;
      end if;

      Res := new Synth_Instance_Type'(Max_Objs => Info.Nbr_Objects,
                                      Is_Const => False,
                                      Base => Parent.Base,
                                      Name => Name,
                                      Block_Scope => Scope,
                                      Up_Block => Parent,
                                      Source_Scope => Blk,
                                      Elab_Objects => 0,
                                      Objects => (others => null));
      return Res;
   end Make_Instance;

   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Synth_Instance_Type, Synth_Instance_Acc);
   begin
      Deallocate (Synth_Inst);
   end Free_Instance;

   procedure Set_Instance_Module (Inst : Synth_Instance_Acc; M : Module)
   is
      Prev_Base : constant Base_Instance_Acc := Inst.Base;
      Base : Base_Instance_Acc;
      Self_Inst : Instance;
   begin
      Base := new Base_Instance_Type'(Builder => Prev_Base.Builder,
                                      Top_Module => Prev_Base.Top_Module,
                                      Cur_Module => M,
                                      Bit0 => No_Net,
                                      Bit1 => No_Net);
      Builders.Set_Parent (Base.Builder, M);

      Self_Inst := Create_Self_Instance (M);
      pragma Unreferenced (Self_Inst);

      Base.Bit0 := Build_Const_UB32 (Base.Builder, 0, 1);
      Base.Bit1 := Build_Const_UB32 (Base.Builder, 1, 1);
      Inst.Base := Base;
   end Set_Instance_Module;

   function Get_Instance_Module (Inst : Synth_Instance_Acc) return Module is
   begin
      return Inst.Base.Cur_Module;
   end Get_Instance_Module;

   function Get_Source_Scope (Inst : Synth_Instance_Acc) return Node is
   begin
      return Inst.Source_Scope;
   end Get_Source_Scope;

   function Get_Top_Module (Inst : Synth_Instance_Acc) return Module is
   begin
      return Inst.Base.Top_Module;
   end Get_Top_Module;

   function Get_Sname (Inst : Synth_Instance_Acc) return Sname is
   begin
      return Inst.Name;
   end Get_Sname;

   function Get_Build (Inst : Synth_Instance_Acc)
                      return Netlists.Builders.Context_Acc is
   begin
      return Inst.Base.Builder;
   end Get_Build;

   function Get_Inst_Bit0 (Inst : Synth_Instance_Acc) return Net is
   begin
      return Inst.Base.Bit0;
   end Get_Inst_Bit0;

   function Get_Inst_Bit1 (Inst : Synth_Instance_Acc) return Net is
   begin
      return Inst.Base.Bit1;
   end Get_Inst_Bit1;

   function Get_Instance_Const (Inst : Synth_Instance_Acc) return Boolean is
   begin
      return Inst.Is_Const;
   end Get_Instance_Const;

   function Check_Set_Instance_Const (Inst : Synth_Instance_Acc)
                                     return Boolean is
   begin
      for I in 1 .. Inst.Elab_Objects loop
         if Inst.Objects (I).Kind /= Value_Subtype then
            return False;
         end if;
      end loop;
      return True;
   end Check_Set_Instance_Const;

   procedure Set_Instance_Const (Inst : Synth_Instance_Acc; Val : Boolean) is
   begin
      pragma Assert (not Val or else Check_Set_Instance_Const (Inst));
      Inst.Is_Const := Val;
   end Set_Instance_Const;

   function Create_Value_Instance (Inst : Synth_Instance_Acc)
                                  return Value_Acc is
   begin
      Packages_Table.Append (Inst);
      return Create_Value_Instance (Packages_Table.Last);
   end Create_Value_Instance;

   function Get_Value_Instance (Inst : Instance_Id)
                               return Synth_Instance_Acc is
   begin
      pragma Assert (Inst in Packages_Table.First .. Packages_Table.Last);
      return Packages_Table.Table (Inst);
   end Get_Value_Instance;

   procedure Create_Object (Syn_Inst : Synth_Instance_Acc;
                            Slot : Object_Slot_Type;
                            Num : Object_Slot_Type := 1) is
   begin
      --  Check elaboration order.
      --  Note: this is not done for package since objects from package are
      --  commons (same scope), and package annotation order can be different
      --  from package elaboration order (eg: body).
      if Slot /= Syn_Inst.Elab_Objects + 1
        or else Syn_Inst.Objects (Slot) /= null
      then
         Error_Msg_Elab ("synth: bad elaboration order of objects");
         raise Internal_Error;
      end if;
      Syn_Inst.Elab_Objects := Slot + Num - 1;
   end Create_Object;

   procedure Create_Object_Force
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Val : Value_Acc)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      pragma Assert (Val = null or else Syn_Inst.Objects (Info.Slot) = null);
      Syn_Inst.Objects (Info.Slot) := Val;
   end Create_Object_Force;

   procedure Create_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Val : Value_Acc)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      Create_Object (Syn_Inst, Info.Slot, 1);
      Create_Object_Force (Syn_Inst, Decl, Val);
   end Create_Object;

   procedure Create_Package_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Val : Value_Acc)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      pragma Assert (Syn_Inst.Objects (Info.Pkg_Slot) = null);
      Syn_Inst.Objects (Info.Pkg_Slot) := Val;
   end Create_Package_Object;

   function Get_Package_Object
     (Syn_Inst : Synth_Instance_Acc; Info : Sim_Info_Acc) return Value_Acc
   is
      Parent : Synth_Instance_Acc;
   begin
      Parent := Get_Instance_By_Scope (Syn_Inst, Info.Pkg_Parent);
      return Parent.Objects (Info.Pkg_Slot);
   end Get_Package_Object;

   function Get_Package_Object
     (Syn_Inst : Synth_Instance_Acc; Pkg : Node) return Value_Acc is
   begin
      return Get_Package_Object (Syn_Inst, Get_Info (Pkg));
   end Get_Package_Object;

   procedure Destroy_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
      Slot : constant Object_Slot_Type := Info.Slot;
   begin
      if Slot /= Syn_Inst.Elab_Objects
        or else Info.Obj_Scope /= Syn_Inst.Block_Scope
      then
         Error_Msg_Elab ("synth: bad destroy order");
      end if;
      Syn_Inst.Objects (Slot) := null;
      Syn_Inst.Elab_Objects := Slot - 1;
   end Destroy_Object;

   procedure Create_Wire_Object (Syn_Inst : Synth_Instance_Acc;
                                 Kind : Wire_Kind;
                                 Obj : Node)
   is
      Obj_Type : constant Node := Get_Type (Obj);
      Otyp : constant Type_Acc := Get_Value_Type (Syn_Inst, Obj_Type);
      Val : Value_Acc;
      Wid : Wire_Id;
   begin
      if Kind = Wire_None then
         Wid := No_Wire_Id;
      else
         Wid := Alloc_Wire (Kind, Obj);
      end if;
      Val := Create_Value_Wire (Wid, Otyp);

      Create_Object (Syn_Inst, Obj, Val);
   end Create_Wire_Object;

   function Get_Instance_By_Scope
     (Syn_Inst: Synth_Instance_Acc; Scope: Sim_Info_Acc)
     return Synth_Instance_Acc is
   begin
      case Scope.Kind is
         when Kind_Block
           | Kind_Frame
           | Kind_Process =>
            declare
               Current : Synth_Instance_Acc;
            begin
               Current := Syn_Inst;
               while Current /= null loop
                  if Current.Block_Scope = Scope then
                     return Current;
                  end if;
                  Current := Current.Up_Block;
               end loop;
               raise Internal_Error;
            end;
         when Kind_Package =>
            if Scope.Pkg_Parent = null then
               --  This is a scope for an uninstantiated package.
               raise Internal_Error;
            else
               --  Instantiated package.
               declare
                  Inst : Value_Acc;
               begin
                  Inst := Get_Package_Object (Syn_Inst, Scope);
                  return Get_Value_Instance (Inst.Instance);
               end;
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Instance_By_Scope;

   function Get_Value (Syn_Inst: Synth_Instance_Acc; Obj : Node)
                      return Value_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Obj);
      Obj_Inst : Synth_Instance_Acc;
   begin
      Obj_Inst := Get_Instance_By_Scope (Syn_Inst, Info.Obj_Scope);
      return Obj_Inst.Objects (Info.Slot);
   end Get_Value;

   function Get_Value_Type (Syn_Inst : Synth_Instance_Acc; Atype : Node)
                           return Type_Acc
   is
      Val : Value_Acc;
   begin
      Val := Get_Value (Syn_Inst, Atype);
      return Val.Typ;
   end Get_Value_Type;

   function Vec2net (Val : Value_Acc) return Net is
   begin
      if Val.Typ.Vbound.Len <= 32 then
         declare
            Len : constant Iir_Index32 := Iir_Index32 (Val.Typ.Vbound.Len);
            R_Val, R_Zx : Uns32;
            V, Zx : Uns32;
         begin
            R_Val := 0;
            R_Zx := 0;
            for I in 1 .. Len loop
               To_Logic (Val.Arr.V (I).Scal, Val.Typ.Vec_El, V, Zx);
               R_Val := R_Val or Shift_Left (V, Natural (Len - I));
               R_Zx := R_Zx or Shift_Left (Zx, Natural (Len - I));
            end loop;
            if R_Zx = 0 then
               return Build_Const_UB32 (Build_Context, R_Val, Uns32 (Len));
            else
               return Build_Const_UL32
                 (Build_Context, R_Val, R_Zx, Uns32 (Len));
            end if;
         end;
      else
         --  Need Uconst64 / UconstBig
         raise Internal_Error;
      end if;
   end Vec2net;

   pragma Unreferenced (Vec2net);

   procedure Value2net
     (Val : Value_Acc; W : Width; Vec : in out Logvec_Array; Res : out Net)
   is
      Off : Uns32;
      Has_Zx : Boolean;
      Inst : Instance;
   begin
      Has_Zx := False;
      Off := 0;
      Value2logvec (Val, Vec, Off, Has_Zx);
      if W = 0 then
         --  For null range (like the null string literal "")
         Res := Build_Const_UB32 (Build_Context, 0, 0);
      elsif W <= 32 then
         --  32 bit result.
         if not Has_Zx then
            Res := Build_Const_UB32 (Build_Context, Vec (0).Val, W);
         else
            Res := Build_Const_UL32
              (Build_Context, Vec (0).Val, Vec (0).Zx, W);
         end if;
         return;
      else
         if not Has_Zx then
            Inst := Build_Const_Bit (Build_Context, W);
            for I in Vec'Range loop
               Set_Param_Uns32 (Inst, Param_Idx (I), Vec (I).Val);
            end loop;
            Res := Get_Output (Inst, 0);
         else
            Inst := Build_Const_Log (Build_Context, W);
            for I in Vec'Range loop
               Set_Param_Uns32 (Inst, Param_Idx (2 * I), Vec (I).Val);
               Set_Param_Uns32 (Inst, Param_Idx (2 * I + 1), Vec (I).Zx);
            end loop;
            Res := Get_Output (Inst, 0);
         end if;
      end if;
   end Value2net;

   function Get_Net (Val : Value_Acc) return Net is
   begin
      case Val.Kind is
         when Value_Wire =>
            return Get_Current_Value (Build_Context, Val.W);
         when Value_Net =>
            return Val.N;
         when Value_Discrete =>
            case Val.Typ.Kind is
               when Type_Bit
                 | Type_Logic =>
                  declare
                     V : Logvec_Array (0 .. 0) := (0 => (0, 0));
                     Res : Net;
                  begin
                     Value2net (Val, 1, V, Res);
                     return Res;
                  end;
               when Type_Discrete =>
                  if Val.Typ.W <= 64 then
                     declare
                        Sh : constant Natural := 64 - Natural (Val.Typ.W);
                        V : Uns64;
                     begin
                        V := To_Uns64 (Val.Scal);
                        --  Keep only Val.Typ.W bits of the value.
                        V := Shift_Right (Shift_Left (V, Sh), Sh);
                        return Build2_Const_Uns
                          (Build_Context, V, Val.Typ.W);
                     end;
                  else
                     raise Internal_Error;
                  end if;
               when others =>
                  raise Internal_Error;
            end case;
         when Value_Const_Array
           | Value_Const_Record =>
            declare
               W : constant Width := Get_Type_Width (Val.Typ);
               Nd : constant Digit_Index := Digit_Index ((W + 31) / 32);
               Res : Net;
            begin
               if Nd > 64 then
                  declare
                     Vecp : Logvec_Array_Acc;
                  begin
                     Vecp := new Logvec_Array'(0 .. Nd - 1 => (0, 0));
                     Value2net (Val, W, Vecp.all, Res);
                     Free_Logvec_Array (Vecp);
                     return Res;
                  end;
               else
                  declare
                     Vec : Logvec_Array (0 .. Nd - 1) := (others => (0, 0));
                  begin
                     Value2net (Val, W, Vec, Res);
                     return Res;
                  end;
               end if;
            end;
         when Value_Array =>
            declare
               use Netlists.Concats;
               C : Concat_Type;
               Res : Net;
            begin
               for I in reverse Val.Arr.V'Range loop
                  Append (C, Get_Net (Val.Arr.V (I)));
               end loop;
               Build (Build_Context, C, Res);
               return Res;
            end;
         when Value_Record =>
            declare
               use Netlists.Concats;
               C : Concat_Type;
               Res : Net;
            begin
               for I in Val.Rec.V'Range loop
                  Append (C, Get_Net (Val.Rec.V (I)));
               end loop;
               Build (Build_Context, C, Res);
               return Res;
            end;
         when Value_Alias =>
            declare
               Res : Net;
            begin
               if Val.A_Obj.Kind = Value_Wire then
                  Res := Get_Current_Value (Build_Context, Val.A_Obj.W);
                  return Build_Extract (Build_Context, Res, Val.A_Off,
                                        Get_Type_Width (Val.Typ));
               else
                  pragma Assert (Val.A_Off = 0);
                  return Get_Net (Val.A_Obj);
               end if;
            end;
         when Value_Const =>
            if Val.C_Net = No_Net then
               Val.C_Net := Get_Net (Val.C_Val);
               Locations.Set_Location (Get_Net_Parent (Val.C_Net),
                                       Get_Location (Val.C_Loc));
            end if;
            return Val.C_Net;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Net;
end Synth.Context;
