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

with Ada.Unchecked_Deallocation;

with Tables;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils;

package body Elab.Vhdl_Context is

   Sig_Nbr : Signal_Index_Type := No_Signal_Index;

   function Get_Nbr_Signal return Signal_Index_Type is
   begin
      return Sig_Nbr;
   end Get_Nbr_Signal;

   package Inst_Tables is new Tables
     (Table_Component_Type => Synth_Instance_Acc,
      Table_Index_Type => Instance_Id_Type,
      Table_Low_Bound => First_Instance_Id,
      Table_Initial => 16);

   function Get_Instance_Id (Inst : Synth_Instance_Acc)
                            return Instance_Id_Type is
   begin
      return Inst.Id;
   end Get_Instance_Id;

   procedure Make_Root_Instance is
   begin
      --  Allow multiple elaborations
      --  pragma Assert (Root_Instance = null);

      Root_Instance :=
        new Synth_Instance_Type'(Max_Objs => Global_Info.Nbr_Objects,
                                 Is_Const => False,
                                 Is_Error => False,
                                 Id => Inst_Tables.Last + 1,
                                 Block_Scope => Global_Info,
                                 Up_Block => null,
                                 Uninst_Scope => null,
                                 Source_Scope => Null_Node,
                                 Caller       => null,
                                 Config       => Null_Node,
                                 Foreign      => 0,
                                 Extra_Units  => null,
                                 Extra_Link   => null,
                                 Elab_Objects => 0,
                                 Objects => (others => (Kind => Obj_None)));
      Inst_Tables.Append (Root_Instance);
   end Make_Root_Instance;

   procedure Free_Base_Instance is
   begin
      --  TODO: really free.
      null;
   end Free_Base_Instance;

   function Make_Elab_Instance
     (Parent : Synth_Instance_Acc; Blk : Node; Config : Node)
     return Synth_Instance_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Blk);
      Scope : Sim_Info_Acc;
      Nbr_Objs : Object_Slot_Type;
      Res : Synth_Instance_Acc;
   begin
      if Get_Kind (Blk) = Iir_Kind_Architecture_Body then
         --  Architectures are extensions of entities.
         Scope := Get_Info (Vhdl.Utils.Get_Entity (Blk));
      else
         Scope := Info;
      end if;

      if Scope = null then
         --  Foreign modules are not annotated.
         pragma Assert (Get_Kind (Blk) = Iir_Kind_Foreign_Module);
         Nbr_Objs := 0;
      else
         Nbr_Objs := Info.Nbr_Objects;
      end if;

      Res := new Synth_Instance_Type'(Max_Objs => Nbr_Objs,
                                      Is_Const => False,
                                      Is_Error => False,
                                      Id => Inst_Tables.Last + 1,
                                      Block_Scope => Scope,
                                      Up_Block => Parent,
                                      Uninst_Scope => null,
                                      Source_Scope => Blk,
                                      Caller       => null,
                                      Config       => Config,
                                      Foreign      => 0,
                                      Extra_Units  => null,
                                      Extra_Link   => null,
                                      Elab_Objects => 0,
                                      Objects => (others =>
                                                    (Kind => Obj_None)));
      Inst_Tables.Append (Res);
      return Res;
   end Make_Elab_Instance;

   procedure Free_Elab_Instance (Synth_Inst : in out Synth_Instance_Acc)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Synth_Instance_Type, Synth_Instance_Acc);
      Id : constant Instance_Id_Type := Synth_Inst.Id;
   begin
      Deallocate (Synth_Inst);
      if Id = Inst_Tables.Last then
         Inst_Tables.Decrement_Last;
      else
         Inst_Tables.Table (Id) := null;
      end if;
   end Free_Elab_Instance;

   function Make_Elab_Generate_Instance
     (Parent : Synth_Instance_Acc; Blk : Node; Config : Node; Len : Natural)
     return Synth_Instance_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Blk);
      Res : Synth_Instance_Acc;
   begin
      Res := new Synth_Instance_Type'(Max_Objs => Object_Slot_Type (Len),
                                      Is_Const => False,
                                      Is_Error => False,
                                      Id => Inst_Tables.Last + 1,
                                      Block_Scope => Info,
                                      Up_Block => Parent,
                                      Uninst_Scope => null,
                                      Source_Scope => Blk,
                                      Caller       => null,
                                      Config       => Config,
                                      Foreign      => 0,
                                      Extra_Units  => null,
                                      Extra_Link   => null,
                                      Elab_Objects => 0,
                                      Objects => (others =>
                                                    (Kind => Obj_None)));
      Inst_Tables.Append (Res);
      return Res;
   end Make_Elab_Generate_Instance;

   function Get_Generate_Sub_Instance
     (Parent : Synth_Instance_Acc; Idx : Positive) return Synth_Instance_Acc is
   begin
      return Parent.Objects (Object_Slot_Type (Idx)).I_Inst;
   end Get_Generate_Sub_Instance;

   procedure Set_Generate_Sub_Instance
     (Parent : Synth_Instance_Acc; Idx : Positive; Child : Synth_Instance_Acc)
   is
   begin
      Parent.Objects (Object_Slot_Type (Idx)) := (Obj_Instance, Child);
   end Set_Generate_Sub_Instance;

   function Is_Error (Inst : Synth_Instance_Acc) return Boolean is
   begin
      return Inst.Is_Error;
   end Is_Error;

   procedure Set_Error (Inst : Synth_Instance_Acc) is
   begin
      Inst.Is_Error := True;
   end Set_Error;

   function Get_Source_Scope (Inst : Synth_Instance_Acc) return Node is
   begin
      return Inst.Source_Scope;
   end Get_Source_Scope;

   function Get_Instance_Parent (Inst : Synth_Instance_Acc)
                                return Synth_Instance_Acc is
   begin
      return Inst.Up_Block;
   end Get_Instance_Parent;

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

   procedure Set_Instance_Config (Inst : Synth_Instance_Acc; Config : Node) is
   begin
      pragma Assert (Inst.Config = Null_Node);
      Inst.Config := Config;
   end Set_Instance_Config;

   function Get_Instance_Config (Inst : Synth_Instance_Acc) return Node is
   begin
      return Inst.Config;
   end Get_Instance_Config;

   procedure Set_Instance_Foreign (Inst : Synth_Instance_Acc; N : Int32) is
   begin
      pragma Assert (Inst.Foreign = 0);
      Inst.Foreign := N;
   end Set_Instance_Foreign;

   function Get_Instance_Foreign (Inst : Synth_Instance_Acc) return Int32 is
   begin
      return Inst.Foreign;
   end Get_Instance_Foreign;

   procedure Add_Extra_Instance (Inst : Synth_Instance_Acc;
                                 Extra : Synth_Instance_Acc) is
   begin
      pragma Assert (Extra.Extra_Link = null);
      Extra.Extra_Link := Inst.Extra_Units;
      Inst.Extra_Units := Extra;
   end Add_Extra_Instance;

   function Get_First_Extra_Instance (Inst : Synth_Instance_Acc)
                                     return Synth_Instance_Acc is
   begin
      return Inst.Extra_Units;
   end Get_First_Extra_Instance;

   function Get_Next_Extra_Instance (Inst : Synth_Instance_Acc)
                                    return Synth_Instance_Acc is
   begin
      return Inst.Extra_Link;
   end Get_Next_Extra_Instance;

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

   procedure Create_Object_Marker
     (Syn_Inst : Synth_Instance_Acc; N : Node; Pool : Areapools.Areapool_Acc)
   is
      use Areapools;
      Info : constant Sim_Info_Acc := Get_Info (N);
   begin
      Create_Object (Syn_Inst, Info.Slot, 1);
      Syn_Inst.Objects (Info.Slot) := (Kind => Obj_Marker,
                                       M_Mark => Empty_Marker);
      Mark (Syn_Inst.Objects (Info.Slot).M_Mark, Pool.all);
   end Create_Object_Marker;

   procedure Create_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      Create_Object (Syn_Inst, Info.Slot, 1);
      Create_Object_Force (Syn_Inst, Decl, Vt);
   end Create_Object;

   procedure Create_Signal (Syn_Inst : Synth_Instance_Acc;
                            Decl : Node;
                            Typ : Type_Acc;
                            Init : Value_Acc)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
      Vt : Valtyp;
   begin
      Create_Object (Syn_Inst, Info.Slot, 1);
      Sig_Nbr := Sig_Nbr + 1;
      Vt := (Typ, Create_Value_Signal (Sig_Nbr, Init));
      Syn_Inst.Objects (Info.Slot) := (Kind => Obj_Object, Obj => Vt);
   end Create_Signal;

   procedure Replace_Signal
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
      Obj : Obj_Type renames Syn_Inst.Objects (Info.Slot);
   begin
      pragma Assert (Obj.Kind = Obj_Object);
      pragma Assert (Obj.Obj.Typ = Vt.Typ);
      pragma Assert (Obj.Obj.Val.Kind = Value_Signal);

      Obj.Obj := Vt;

      --  TODO: free old signal ?
   end Replace_Signal;

   procedure Mutate_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
      Obj : Obj_Type renames Syn_Inst.Objects (Info.Slot);
   begin
      pragma Assert (Obj.Kind = Obj_Object);
      pragma Assert (Obj.Obj.Typ = Vt.Typ);

      Obj.Obj := Vt;
   end Mutate_Object;

   procedure Create_Sub_Instance (Syn_Inst : Synth_Instance_Acc;
                                  Stmt : Node;
                                  Sub_Inst : Synth_Instance_Acc)
   is
      Info : constant Sim_Info_Acc := Get_Info (Stmt);
   begin
      Create_Object (Syn_Inst, Info.Inst_Slot, 1);
      pragma Assert (Syn_Inst.Objects (Info.Inst_Slot).Kind = Obj_None);
      Syn_Inst.Objects (Info.Inst_Slot) := (Kind => Obj_Instance,
                                            I_Inst => Sub_Inst);
   end Create_Sub_Instance;

   procedure Create_Component_Instance (Syn_Inst : Synth_Instance_Acc;
                                        Sub_Inst : Synth_Instance_Acc)
   is
      Slot : constant Object_Slot_Type := Syn_Inst.Max_Objs;
   begin
      pragma Assert (Slot > 0);
      pragma Assert (Syn_Inst.Objects (Slot).Kind = Obj_None);
      Create_Object (Syn_Inst, Slot, 1);
      Syn_Inst.Objects (Slot) := (Kind => Obj_Instance,
                                  I_Inst => Sub_Inst);
   end Create_Component_Instance;

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
         Create_Object (Syn_Inst, Info.Pkg_Slot, 1);
      end if;
      Syn_Inst.Objects (Info.Pkg_Slot) := (Kind => Obj_Instance,
                                           I_Inst => Inst);
   end Create_Package_Object;

   procedure Create_Package_Interface (Syn_Inst : Synth_Instance_Acc;
                                       Decl     : Node;
                                       Inst     : Synth_Instance_Acc)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      pragma Assert (Syn_Inst.Up_Block /= null);
      Create_Object (Syn_Inst, Info.Pkg_Slot, 1);
      Syn_Inst.Objects (Info.Pkg_Slot) := (Kind => Obj_Instance,
                                           I_Inst => Inst);
   end Create_Package_Interface;

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

   function Create_Package_Instance (Parent_Inst : Synth_Instance_Acc;
                                     Pkg : Node)
                                    return Synth_Instance_Acc
   is
      Syn_Inst : Synth_Instance_Acc;
   begin
      Syn_Inst := Make_Elab_Instance (Parent_Inst, Pkg, Null_Node);
      if Get_Kind (Get_Parent (Pkg)) = Iir_Kind_Design_Unit then
         --  Global package.
         Create_Package_Object (Parent_Inst, Pkg, Syn_Inst, True);
      else
         --  Local package: check elaboration order.
         Create_Package_Object (Parent_Inst, Pkg, Syn_Inst, False);
      end if;
      return Syn_Inst;
   end Create_Package_Instance;

   function Get_Sub_Instance
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node) return Synth_Instance_Acc
   is
      Info : constant Sim_Info_Acc := Get_Info (Stmt);
   begin
      return Syn_Inst.Objects (Info.Inst_Slot).I_Inst;
   end Get_Sub_Instance;

   function Get_Component_Instance
     (Syn_Inst : Synth_Instance_Acc) return Synth_Instance_Acc
   is
      Slot : constant Object_Slot_Type := Syn_Inst.Max_Objs;
   begin
      return Syn_Inst.Objects (Slot).I_Inst;
   end Get_Component_Instance;

   procedure Set_Uninstantiated_Scope
     (Syn_Inst : Synth_Instance_Acc; Bod : Node) is
   begin
      Syn_Inst.Uninst_Scope := Get_Info (Bod);
   end Set_Uninstantiated_Scope;

   procedure Destroy_Init (D : out Destroy_Type;
                           Syn_Inst : Synth_Instance_Acc) is
   begin
      D := (Inst => Syn_Inst,
            First => Object_Slot_Type'Last,
            Last => Syn_Inst.Elab_Objects);
   end Destroy_Init;

   procedure Destroy_Check (D : in out Destroy_Type; Info : Sim_Info_Acc)
   is
      Slot : constant Object_Slot_Type := Info.Slot;
   begin
      if Info.Obj_Scope /= D.Inst.Block_Scope then
         --  Bad context.
         raise Internal_Error;
      end if;
      if Slot > D.Last then
         --  Not elaborated object ?
         raise Internal_Error;
      end if;
      if D.Inst.Objects (Slot).Kind = Obj_None then
         --  Already destroyed.
         raise Internal_Error;
      end if;
      if Slot < D.First then
         D.First := Slot;
      end if;
   end Destroy_Check;

   procedure Destroy_Object (D : in out Destroy_Type; Decl : Node)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      Destroy_Check (D, Info);
      D.Inst.Objects (Info.Slot) := (Kind => Obj_None);
   end Destroy_Object;

   procedure Destroy_Marker
     (D : in out Destroy_Type; N : Node; Pool : Areapools.Areapool_Acc)
   is
      use Areapools;
      Info : constant Sim_Info_Acc := Get_Info (N);
      Slot : constant Object_Slot_Type := Info.Slot;
   begin
      Destroy_Check (D, Info);
      Release (D.Inst.Objects (Slot).M_Mark, Pool.all);
      D.Inst.Objects (Slot) := (Kind => Obj_None);
   end Destroy_Marker;

   procedure Destroy_Finish (D : in out Destroy_Type) is
   begin
      if D.First = Object_Slot_Type'Last then
         --  No object destroyed.
         return;
      end if;

      if D.Last /= D.Inst.Elab_Objects then
         --  Two destroys at the same time.
         raise Internal_Error;
      end if;

      --  Check all objects have been destroyed.
      for I in D.First .. D.Last loop
         if D.Inst.Objects (I).Kind /= Obj_None then
            raise Internal_Error;
         end if;
      end loop;

      D.Inst.Elab_Objects := D.First - 1;
   end Destroy_Finish;

   function Get_Instance_By_Scope
     (Syn_Inst: Synth_Instance_Acc; Scope: Sim_Info_Acc)
     return Synth_Instance_Acc
   is
      pragma Assert (Scope /= null);
   begin
      case Scope.Kind is
         when Kind_Block
           | Kind_Frame
           | Kind_Process
           | Kind_Protected =>
            declare
               Current : Synth_Instance_Acc;
            begin
               Current := Syn_Inst;
               while Current /= null loop
                  if Current.Block_Scope = Scope
                    or else Current.Uninst_Scope = Scope
                  then
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

   procedure Set_Caller_Instance (Syn_Inst : Synth_Instance_Acc;
                                  Caller : Synth_Instance_Acc) is
   begin
      pragma Assert (Syn_Inst.Caller = null);
      Syn_Inst.Caller := Caller;
   end Set_Caller_Instance;

   function Get_Caller_Instance (Syn_Inst : Synth_Instance_Acc)
                                return Synth_Instance_Acc is
   begin
      return Syn_Inst.Caller;
   end Get_Caller_Instance;

   procedure Iterate_Top_Level (It : in out Iterator_Top_Level_Type;
                                Res : out Synth_Instance_Acc)
   is
      Obj : Obj_Type;
   begin
      loop
         if It.Next_Idx > Root_Instance.Max_Objs then
            Res := null;
            exit;
         end if;

         Obj := Root_Instance.Objects (It.Next_Idx);
         It.Next_Idx := It.Next_Idx + 1;

         if Obj.Kind = Obj_Instance then
            Res := Obj.I_Inst;
            return;
         end if;
      end loop;
   end Iterate_Top_Level;

end Elab.Vhdl_Context;
