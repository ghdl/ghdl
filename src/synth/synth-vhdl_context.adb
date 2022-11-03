--  Synthesis context.
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

with Ada.Unchecked_Conversion;
with Tables;
with Types_Utils; use Types_Utils;

with Netlists.Folds; use Netlists.Folds;

with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Netlists.Locations;

package body Synth.Vhdl_Context is
   package Extra_Tables is new Tables
     (Table_Component_Type => Extra_Vhdl_Instance_Type,
      Table_Index_Type => Instance_Id_Type,
      Table_Low_Bound => First_Instance_Id,
      Table_Initial => 16);

   procedure Resize_Extra_Tables (Id : Instance_Id_Type) is
   begin
      while Id > Extra_Tables.Last loop
         Extra_Tables.Append ((Base => null, Name => No_Sname));
      end loop;
   end Resize_Extra_Tables;

   procedure Set_Extra (Inst : Synth_Instance_Acc;
                        Extra : Extra_Vhdl_Instance_Type)
   is
      Id : constant Instance_Id_Type := Get_Instance_Id (Inst);
   begin
      Resize_Extra_Tables (Id);
      Extra_Tables.Table (Id) := Extra;
   end Set_Extra;

   procedure Make_Base_Instance (Base : Base_Instance_Acc) is
   begin
      Set_Extra (Root_Instance, (Base => Base, Name => No_Sname));
   end Make_Base_Instance;

   procedure Free_Base_Instance is
   begin
      --  TODO: really free.
      null;
   end Free_Base_Instance;

   function Get_Instance_Extra (Inst : Synth_Instance_Acc)
                               return Extra_Vhdl_Instance_Type is
   begin
      return Extra_Tables.Table (Get_Instance_Id (Inst));
   end Get_Instance_Extra;

   procedure Set_Extra (Inst : Synth_Instance_Acc;
                        Base : Base_Instance_Acc;
                        Name : Sname := No_Sname) is
   begin
      Set_Extra (Inst, (Base => Base, Name => Name));
   end Set_Extra;

   procedure Set_Extra (Inst : Synth_Instance_Acc;
                        Parent : Synth_Instance_Acc;
                        Name : Sname := No_Sname)
   is
      Id : constant Instance_Id_Type := Get_Instance_Id (Inst);
   begin
      Resize_Extra_Tables (Id);
      Extra_Tables.Table (Id) := (Base => Get_Instance_Extra (Parent).Base,
                                  Name => Name);
   end Set_Extra;

   function Make_Instance (Parent : Synth_Instance_Acc;
                           Blk : Node;
                           Name : Sname := No_Sname)
                          return Synth_Instance_Acc
   is
      Res : Synth_Instance_Acc;
   begin
      Res := Make_Elab_Instance (Parent, Blk, Null_Node);
      Set_Extra (Res, Parent, Name);
      return Res;
   end Make_Instance;

   procedure Set_Instance_Base (Inst : Synth_Instance_Acc;
                                Base : Base_Instance_Acc) is
   begin
      Extra_Tables.Table (Get_Instance_Id (Inst)).Base := Base;
   end Set_Instance_Base;

   procedure Set_Instance_Base (Inst : Synth_Instance_Acc;
                                Base : Synth_Instance_Acc) is
   begin
      Set_Instance_Base (Inst, Get_Instance_Extra (Base).Base);
   end Set_Instance_Base;

   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc) is
   begin
      if Get_Instance_Id (Synth_Inst) = Extra_Tables.Last then
         Extra_Tables.Decrement_Last;
      end if;
      Free_Elab_Instance (Synth_Inst);
   end Free_Instance;

   procedure Set_Instance_Module (Inst : Synth_Instance_Acc; M : Module)
   is
      Prev_Base : constant Base_Instance_Acc := Get_Instance_Extra (Inst).Base;
      Base : Base_Instance_Acc;
      Self_Inst : Instance;
   begin
      Base := new Base_Instance_Type'(Builder => Prev_Base.Builder,
                                      Top_Module => Prev_Base.Top_Module,
                                      Cur_Module => M);
      Builders.Set_Parent (Base.Builder, M);

      Self_Inst := Create_Self_Instance (M);
      pragma Unreferenced (Self_Inst);

      Set_Instance_Base (Inst, Base);
   end Set_Instance_Module;

   function Get_Instance_Module (Inst : Synth_Instance_Acc) return Module is
   begin
      return Extra_Tables.Table (Get_Instance_Id (Inst)).Base.Cur_Module;
   end Get_Instance_Module;

   function Get_Top_Module (Inst : Synth_Instance_Acc) return Module is
   begin
      return Extra_Tables.Table (Get_Instance_Id (Inst)).Base.Top_Module;
   end Get_Top_Module;

   function Get_Sname (Inst : Synth_Instance_Acc) return Sname is
   begin
      return Extra_Tables.Table (Get_Instance_Id (Inst)).Name;
   end Get_Sname;

   function Get_Build (Inst : Synth_Instance_Acc)
                      return Netlists.Builders.Context_Acc
   is
      Id : constant Instance_Id_Type := Get_Instance_Id (Inst);
      Base : Base_Instance_Acc;
   begin
      if Id > Extra_Tables.Last then
         --  Not yet built.
         return null;
      end if;

      Base := Extra_Tables.Table (Id).Base;
      if Base = null then
         return null;
      end if;

      return Base.Builder;
   end Get_Build;

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
         Wid := Alloc_Wire (Kind, (Obj, Otyp));
      end if;
      Val := Create_Value_Wire (Wid, Otyp, Current_Pool);

      Create_Object (Syn_Inst, Obj, Val);
   end Create_Wire_Object;

   --  Set Is_0 to True iff VEC is 000...
   --  Set Is_X to True iff VEC is XXX...
   procedure Is_Full (Vec : Logvec_Array;
                      W : Width;
                      Is_0 : out Boolean;
                      Is_X : out Boolean;
                      Is_Z : out Boolean)
   is
      Val : Uns32;
      Zx : Uns32;
      Mask : Uns32;
   begin
      --  Check the first word.
      pragma Assert (W >= 32);
      Val := Vec (0).Val;
      Zx := Vec (0).Zx;
      Is_0 := False;
      Is_X := False;
      Is_Z := False;
      if Val = 0 and Zx = 0 then
         Is_0 := True;
      elsif Zx = not 0 then
         if Val = not 0 then
            Is_X := True;
         elsif Val = 0 then
            Is_Z := True;
         else
            return;
         end if;
      else
         return;
      end if;

      for I in 1 .. Vec'Last - 1 loop
         if Vec (I).Val /= Val or else Vec (I).Zx /= Zx then
            --  Clear flags.
            Is_0 := False;
            Is_X := False;
            Is_Z := False;
            return;
         end if;
      end loop;

      pragma Assert (Vec'Last = Digit_Index ((W - 1) / 32));
      Mask := Shift_Right (not 0, (32 - Natural (W mod 32)) mod 32);
      if (Vec (Vec'Last).Val and Mask) /= (Val and Mask)
        or else (Vec (Vec'Last).Zx and Mask) /= (Zx and Mask)
      then
         Is_0 := False;
         Is_X := False;
         Is_Z := False;
      end if;
   end Is_Full;

   procedure Value2net (Ctxt : Context_Acc;
                        Val : Memtyp;
                        Off : Uns32;
                        W : Width;
                        Vec : in out Logvec_Array;
                        Res : out Net)
   is
      Vec_Off : Uns32;
      Has_Zx : Boolean;
      Inst : Instance;
      Is_0, Is_X, Is_Z : Boolean;
   begin
      --  First convert to logvec.
      Has_Zx := False;
      Vec_Off := 0;
      Value2logvec (Val, Off, W, Vec, Vec_Off, Has_Zx);
      pragma Assert (Vec_Off = W);

      --  Then convert logvec to net.
      if W = 0 then
         --  For null range (like the null string literal "")
         Res := Build_Const_UB32 (Ctxt, 0, 0);
      elsif W <= 32 then
         --  32 bit result.
         if not Has_Zx then
            Res := Build_Const_UB32 (Ctxt, Vec (0).Val, W);
            return;
         end if;

         if Sext (Vec (0).Zx, Natural (W)) = not 0 then
            --  All bits are either Z or X.
            if Vec (0).Val = 0 then
               --  All bits are Z.
               Res := Build_Const_Z (Ctxt, W);
               return;
            end if;
            if Sext (Vec (0).Val, Natural (W)) = not 0 then
               --  All bits are X.
               Res := Build_Const_X (Ctxt, W);
               return;
            end if;
            --  Mix of Z and X.
         end if;
         --  Generic.
         Res := Build_Const_UL32 (Ctxt, Vec (0).Val, Vec (0).Zx, W);
         return;
      else
         Is_Full (Vec, W, Is_0, Is_X, Is_Z);
         if Is_0 then
            Res := Build_Const_UB32 (Ctxt, 0, W);
         elsif Is_X then
            Res := Build_Const_X (Ctxt, W);
         elsif Is_Z then
            Res := Build_Const_Z (Ctxt, W);
         elsif not Has_Zx then
            Inst := Build_Const_Bit (Ctxt, W);
            for I in Vec'Range loop
               Set_Param_Uns32 (Inst, Param_Idx (I), Vec (I).Val);
            end loop;
            Res := Get_Output (Inst, 0);
         else
            Inst := Build_Const_Log (Ctxt, W);
            for I in Vec'Range loop
               Set_Param_Uns32 (Inst, Param_Idx (2 * I), Vec (I).Val);
               Set_Param_Uns32 (Inst, Param_Idx (2 * I + 1), Vec (I).Zx);
            end loop;
            Res := Get_Output (Inst, 0);
         end if;
      end if;
   end Value2net;

   function Get_Partial_Memtyp_Net
     (Ctxt : Context_Acc; Val : Memtyp; Off : Uns32; Wd : Width) return Net
   is
      Nd : constant Digit_Index := Digit_Index ((Wd + 31) / 32);
      Res : Net;
   begin
      if Nd > 64 then
         declare
            Vecp : Logvec_Array_Acc;
         begin
            Vecp := new Logvec_Array'(0 .. Nd - 1 => (0, 0));
            Value2net (Ctxt, Val, Off, Wd, Vecp.all, Res);
            Free_Logvec_Array (Vecp);
            return Res;
         end;
      else
         declare
            Vec : Logvec_Array (0 .. Nd - 1) := (others => (0, 0));
         begin
            Value2net (Ctxt, Val, Off, Wd, Vec, Res);
            return Res;
         end;
      end if;
   end Get_Partial_Memtyp_Net;

   function Get_Memtyp_Net (Ctxt : Context_Acc; Val : Memtyp) return Net is
   begin
      return Get_Partial_Memtyp_Net (Ctxt, Val, 0, Val.Typ.W);
   end Get_Memtyp_Net;

   function To_Net is new Ada.Unchecked_Conversion (Uns32, Net);
   function To_Uns32 is new Ada.Unchecked_Conversion (Net, Uns32);

   function Get_Value_Net (Val : Value_Acc) return Net is
   begin
      return To_Net (Val.N);
   end Get_Value_Net;

   procedure Set_Value_Net (Val : Value_Acc; N : Net) is
   begin
      Val.N := To_Uns32 (N);
   end Set_Value_Net;

   function Get_Value_Wire (Val : Value_Acc) return Wire_Id
   is
      function To_Wire_Id is new Ada.Unchecked_Conversion (Uns32, Wire_Id);
   begin
      return To_Wire_Id (Val.N);
   end Get_Value_Wire;

   procedure Set_Value_Wire (Val : Value_Acc; W : Wire_Id)
   is
      function To_Uns32 is new Ada.Unchecked_Conversion (Wire_Id, Uns32);
   begin
      Val.N := To_Uns32 (W);
   end Set_Value_Wire;

   function Create_Value_Wire (W : Wire_Id; Pool : Areapool_Acc)
                              return Value_Acc
   is
      function To_Uns32 is new Ada.Unchecked_Conversion (Wire_Id, Uns32);
   begin
      return Create_Value_Wire (To_Uns32 (W), Pool);
   end Create_Value_Wire;

   function Create_Value_Wire (W : Wire_Id;
                               Wtype : Type_Acc;
                               Pool : Areapool_Acc) return Valtyp
   is
      pragma Assert (Wtype /= null);
   begin
      return (Wtype, Create_Value_Wire (W, Pool));
   end Create_Value_Wire;

   function Create_Value_Net (N : Net) return Value_Acc
   is
      function To_Uns32 is new Ada.Unchecked_Conversion (Net, Uns32);
   begin
      return Create_Value_Net (To_Uns32 (N));
   end Create_Value_Net;

   function Create_Value_Net (N : Net; Ntype : Type_Acc) return Valtyp
   is
      pragma Assert (Ntype /= null);
   begin
      return (Ntype, Create_Value_Net (N));
   end Create_Value_Net;

   function Create_Value_Dyn_Alias (Obj : Value_Acc;
                                    Poff : Uns32;
                                    Ptyp : Type_Acc;
                                    Voff : Net;
                                    Eoff : Uns32;
                                    Typ : Type_Acc;
                                    Pool : Areapools.Areapool_Acc)
                                   return Valtyp is
   begin
      return (Typ,
              Create_Value_Dyn_Alias (Obj, Poff, Ptyp, To_Uns32 (Voff), Eoff,
                                      Pool));
   end Create_Value_Dyn_Alias;

   function Get_Value_Dyn_Alias_Voff (Val : Value_Acc) return Net is
   begin
      return To_Net (Val.D_Voff);
   end Get_Value_Dyn_Alias_Voff;

   function Get_Net (Ctxt : Context_Acc; Val : Valtyp) return Net is
   begin
      case Val.Val.Kind is
         when Value_Wire =>
            return Get_Current_Value (Ctxt, Get_Value_Wire (Val.Val));
         when Value_Net =>
            return Get_Value_Net (Val.Val);
         when Value_Alias =>
            declare
               Res : Net;
            begin
               if Val.Val.A_Obj.Kind = Value_Wire then
                  Res := Get_Current_Value
                    (Ctxt, Get_Value_Wire (Val.Val.A_Obj));
               else
                  Res := Get_Net (Ctxt, (Val.Typ, Val.Val.A_Obj));
               end if;
               return Build2_Extract
                 (Ctxt, Res, Val.Val.A_Off.Net_Off, Val.Typ.W);
            end;
         when Value_Const =>
            declare
               N : Net;
            begin
               N := To_Net (Val.Val.C_Net);
               if N = No_Net then
                  N := Get_Net (Ctxt, (Val.Typ, Val.Val.C_Val));
                  Val.Val.C_Net := To_Uns32 (N);
                  Locations.Set_Location (Get_Net_Parent (N),
                                          Get_Location (Val.Val.C_Loc));
               end if;
               return N;
            end;
         when Value_Memory =>
            return Get_Memtyp_Net (Ctxt, Get_Memtyp (Val));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Net;

   function Is_Static_Val (Val : Value_Acc) return Boolean is
   begin
      case Val.Kind is
         when Value_Memory =>
            return True;
         when Value_Net
           | Value_Signal
           | Value_Dyn_Alias =>
            return False;
         when Value_Quantity
           | Value_Terminal =>
            return False;
         when Value_Wire =>
            declare
               W : constant Wire_Id := Get_Value_Wire (Val);
            begin
               if Get_Kind (W) = Wire_Variable then
                  return Is_Static_Wire (W);
               else
                  --  A signal does not have static values.
                  return False;
               end if;
            end;
         when Value_File =>
            return True;
         when Value_Const =>
            return True;
         when Value_Alias =>
            return Is_Static_Val (Val.A_Obj);
      end case;
   end Is_Static_Val;

end Synth.Vhdl_Context;
