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

with Name_Table; use Name_Table;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils;

with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;

with Synth.Expr; use Synth.Expr;
with Netlists.Locations;

package body Synth.Context is
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
                                      Is_Error => False,
                                      Base => Base,
                                      Name => No_Sname,
                                      Block_Scope => Global_Info,
                                      Up_Block => null,
                                      Uninst_Scope => null,
                                      Source_Scope => Null_Node,
                                      Elab_Objects => 0,
                                      Objects => (others =>
                                                    (Kind => Obj_None)));
      return Res;
   end Make_Base_Instance;

   procedure Free_Base_Instance is
   begin
      --  TODO: really free.
      Build_Context := null;
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
                                      Is_Error => False,
                                      Base => Parent.Base,
                                      Name => Name,
                                      Block_Scope => Scope,
                                      Up_Block => Parent,
                                      Uninst_Scope => null,
                                      Source_Scope => Blk,
                                      Elab_Objects => 0,
                                      Objects => (others =>
                                                    (Kind => Obj_None)));
      return Res;
   end Make_Instance;

   procedure Set_Instance_Base (Inst : Synth_Instance_Acc;
                                Base : Synth_Instance_Acc) is
   begin
      Inst.Base := Base.Base;
   end Set_Instance_Base;

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

   function Is_Error (Inst : Synth_Instance_Acc) return Boolean is
   begin
      return Inst.Is_Error;
   end Is_Error;

   procedure Set_Error (Inst : Synth_Instance_Acc) is
   begin
      Inst.Is_Error := True;
   end Set_Error;

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
         if Inst.Objects (I).Kind /= Obj_Subtype then
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

   procedure Create_Object (Syn_Inst : Synth_Instance_Acc;
                            Slot : Object_Slot_Type;
                            Num : Object_Slot_Type := 1) is
   begin
      --  Check elaboration order.
      --  Note: this is not done for package since objects from package are
      --  commons (same scope), and package annotation order can be different
      --  from package elaboration order (eg: body).
      if Slot /= Syn_Inst.Elab_Objects + 1
        or else Syn_Inst.Objects (Slot).Kind /= Obj_None
      then
         Error_Msg_Elab ("synth: bad elaboration order of objects");
         raise Internal_Error;
      end if;
      Syn_Inst.Elab_Objects := Slot + Num - 1;
   end Create_Object;

   procedure Create_Object_Force
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      pragma Assert
        (Syn_Inst.Objects (Info.Slot).Kind = Obj_None
           or else Vt = (null, null)
           or else Syn_Inst.Objects (Info.Slot) = (Kind => Obj_Object,
                                                   Obj => No_Valtyp));
      Syn_Inst.Objects (Info.Slot) := (Kind => Obj_Object, Obj => Vt);
   end Create_Object_Force;

   procedure Create_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      Create_Object (Syn_Inst, Info.Slot, 1);
      Create_Object_Force (Syn_Inst, Decl, Vt);
   end Create_Object;

   procedure Create_Subtype_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Typ : Type_Acc)
   is
      pragma Assert (Typ /= null);
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      Create_Object (Syn_Inst, Info.Slot, 1);
      pragma Assert (Syn_Inst.Objects (Info.Slot).Kind = Obj_None);
      Syn_Inst.Objects (Info.Slot) := (Kind => Obj_Subtype, T_Typ => Typ);
   end Create_Subtype_Object;

   procedure Create_Package_Object (Syn_Inst : Synth_Instance_Acc;
                                    Decl : Node;
                                    Inst : Synth_Instance_Acc;
                                    Is_Global : Boolean)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      if Is_Global then
         pragma Assert (Syn_Inst.Objects (Info.Pkg_Slot).Kind = Obj_None);
         pragma Assert (Syn_Inst.Up_Block = null);
         null;
      else
         pragma Assert (Syn_Inst.Up_Block /= null);
         Create_Object (Syn_Inst, Info.Slot, 1);
      end if;
      Syn_Inst.Objects (Info.Pkg_Slot) := (Kind => Obj_Instance,
                                           I_Inst => Inst);
   end Create_Package_Object;

   function Get_Package_Object
     (Syn_Inst : Synth_Instance_Acc; Info : Sim_Info_Acc)
     return Synth_Instance_Acc
   is
      Parent : Synth_Instance_Acc;
   begin
      Parent := Get_Instance_By_Scope (Syn_Inst, Info.Pkg_Parent);
      return Parent.Objects (Info.Pkg_Slot).I_Inst;
   end Get_Package_Object;

   function Get_Package_Object
     (Syn_Inst : Synth_Instance_Acc; Pkg : Node) return Synth_Instance_Acc is
   begin
      return Get_Package_Object (Syn_Inst, Get_Info (Pkg));
   end Get_Package_Object;

   procedure Set_Uninstantiated_Scope
     (Syn_Inst : Synth_Instance_Acc; Bod : Node) is
   begin
      Syn_Inst.Uninst_Scope := Get_Info (Bod);
   end Set_Uninstantiated_Scope;

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
      Syn_Inst.Objects (Slot) := (Kind => Obj_None);
      Syn_Inst.Elab_Objects := Slot - 1;
   end Destroy_Object;

   procedure Create_Wire_Object (Syn_Inst : Synth_Instance_Acc;
                                 Kind : Wire_Kind;
                                 Obj : Node)
   is
      Obj_Type : constant Node := Get_Type (Obj);
      Otyp : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Obj_Type);
      Val : Valtyp;
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
               declare
                  Current : Synth_Instance_Acc;
               begin
                  Current := Syn_Inst;
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
               return Get_Package_Object (Syn_Inst, Scope);
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Instance_By_Scope;

   function Get_Parent_Scope (Blk : Node) return Sim_Info_Acc
   is
      Parent : Node;
   begin
      Parent := Get_Parent (Blk);
      if Get_Kind (Parent) = Iir_Kind_Architecture_Body then
         Parent := Vhdl.Utils.Get_Entity (Parent);
      end if;
      return Get_Info (Parent);
   end Get_Parent_Scope;

   function Get_Value (Syn_Inst: Synth_Instance_Acc; Obj : Node)
                      return Valtyp
   is
      Info : constant Sim_Info_Acc := Get_Info (Obj);
      Obj_Inst : Synth_Instance_Acc;
   begin
      Obj_Inst := Get_Instance_By_Scope (Syn_Inst, Info.Obj_Scope);
      return Obj_Inst.Objects (Info.Slot).Obj;
   end Get_Value;

   function Get_Subtype_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node) return Type_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
      Obj_Inst : Synth_Instance_Acc;
   begin
      Obj_Inst := Get_Instance_By_Scope (Syn_Inst, Info.Obj_Scope);
      return Obj_Inst.Objects (Info.Slot).T_Typ;
   end Get_Subtype_Object;

   --  Set Is_0 to True iff VEC is 000...
   --  Set Is_X to True iff VEC is XXX...
   procedure Is_Full (Vec : Logvec_Array;
                      Is_0 : out Boolean;
                      Is_X : out Boolean)
   is
      Val : Uns32;
      Zx : Uns32;
   begin
      Val := Vec (0).Val;
      Zx := Vec (0).Zx;
      Is_0 := False;
      Is_X := False;
      if Val = 0 and Zx = 0 then
         Is_0 := True;
         Is_X := False;
      elsif Val = not 0 and Zx = not 0 then
         Is_0 := False;
         Is_X := True;
      end if;

      for I in 1 .. Vec'Last loop
         if Vec (I).Val /= Val or else Vec (I).Zx /= Zx then
            Is_0 := False;
            Is_X := False;
            return;
         end if;
      end loop;
   end Is_Full;

   procedure Value2net (Val : Memtyp;
                        Off : Uns32;
                        W : Width;
                        Vec : in out Logvec_Array;
                        Res : out Net)
   is
      Vec_Off : Uns32;
      Has_Zx : Boolean;
      Inst : Instance;
      Is_0, Is_X : Boolean;
   begin
      --  First convert to logvec.
      Has_Zx := False;
      Vec_Off := 0;
      Value2logvec (Val, Off, W, Vec, Vec_Off, Has_Zx);
      pragma Assert (Vec_Off = W);

      --  Then convert logvec to net.
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
         Is_Full (Vec, Is_0, Is_X);
         if Is_0 then
            Res := Build_Const_UB32 (Build_Context, 0, W);
         elsif Is_X then
            Res := Build_Const_X (Build_Context, W);
         elsif not Has_Zx then
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

   function Get_Partial_Memtyp_Net (Val : Memtyp; Off : Uns32; Wd : Width)
                                   return Net
   is
      Nd : constant Digit_Index := Digit_Index ((Wd + 31) / 32);
      Res : Net;
   begin
      if Nd > 64 then
         declare
            Vecp : Logvec_Array_Acc;
         begin
            Vecp := new Logvec_Array'(0 .. Nd - 1 => (0, 0));
            Value2net (Val, Off, Wd, Vecp.all, Res);
            Free_Logvec_Array (Vecp);
            return Res;
         end;
      else
         declare
            Vec : Logvec_Array (0 .. Nd - 1) := (others => (0, 0));
         begin
            Value2net (Val, Off, Wd, Vec, Res);
            return Res;
         end;
      end if;
   end Get_Partial_Memtyp_Net;

   function Get_Memtyp_Net (Val : Memtyp) return Net is
   begin
      return Get_Partial_Memtyp_Net (Val, 0, Val.Typ.W);
   end Get_Memtyp_Net;

   function Get_Net (Val : Valtyp) return Net is
   begin
      case Val.Val.Kind is
         when Value_Wire =>
            return Get_Current_Value (Build_Context, Val.Val.W);
         when Value_Net =>
            return Val.Val.N;
         when Value_Alias =>
            declare
               Res : Net;
            begin
               if Val.Val.A_Obj.Kind = Value_Wire then
                  Res := Get_Current_Value (Build_Context, Val.Val.A_Obj.W);
                  return Build2_Extract
                    (Build_Context, Res, Val.Val.A_Off.Net_Off, Val.Typ.W);
               else
                  pragma Assert (Val.Val.A_Off.Net_Off = 0);
                  return Get_Net ((Val.Typ, Val.Val.A_Obj));
               end if;
            end;
         when Value_Const =>
            if Val.Val.C_Net = No_Net then
               Val.Val.C_Net := Get_Net ((Val.Typ, Val.Val.C_Val));
               Locations.Set_Location (Get_Net_Parent (Val.Val.C_Net),
                                       Get_Location (Val.Val.C_Loc));
            end if;
            return Val.Val.C_Net;
         when Value_Memory =>
            return Get_Memtyp_Net (Get_Memtyp (Val));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Net;
end Synth.Context;
