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

with Types; use Types;

with Netlists; use Netlists;
with Netlists.Builders;

with Vhdl.Annotations; use Vhdl.Annotations;
with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Environment; use Synth.Environment;
with Synth.Objtypes; use Synth.Objtypes;
with Synth.Values; use Synth.Values;

package Synth.Context is
   --  Values are stored into Synth_Instance, which is parallel to simulation
   --  Block_Instance_Type.

   type Synth_Instance_Type (<>) is limited private;
   type Synth_Instance_Acc is access Synth_Instance_Type;

   --  Global context.
   Build_Context : Netlists.Builders.Context_Acc;

   function Get_Instance_By_Scope
     (Syn_Inst: Synth_Instance_Acc; Scope: Sim_Info_Acc)
     return Synth_Instance_Acc;

   --  Create the first instance.
   function Make_Base_Instance return Synth_Instance_Acc;

   --  Free the first instance.
   procedure Free_Base_Instance;

   --  Create and free the corresponding synth instance.
   function Make_Instance (Parent : Synth_Instance_Acc;
                           Blk : Node;
                           Name : Sname := No_Sname)
                          return Synth_Instance_Acc;

   --  Only useful for subprograms: set the base (which can be different from
   --  the parent).  Ideally it should be part of Make_Instance, but in most
   --  cases they are the same (except sometimes for subprograms).
   procedure Set_Instance_Base (Inst : Synth_Instance_Acc;
                                Base : Synth_Instance_Acc);
   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc);

   function Is_Error (Inst : Synth_Instance_Acc) return Boolean;
   pragma Inline (Is_Error);

   procedure Set_Error (Inst : Synth_Instance_Acc);

   function Get_Sname (Inst : Synth_Instance_Acc) return Sname;
   pragma Inline (Get_Sname);

   function Get_Build (Inst : Synth_Instance_Acc)
                      return Netlists.Builders.Context_Acc;
   pragma Inline (Get_Build);

   function Get_Top_Module (Inst : Synth_Instance_Acc) return Module;

   function Get_Instance_Module (Inst : Synth_Instance_Acc) return Module;
   pragma Inline (Get_Instance_Module);

   --  Each base instance creates bit0 and bit1, which are used for control
   --  flow.
   function Get_Inst_Bit0 (Inst : Synth_Instance_Acc) return Net;
   function Get_Inst_Bit1 (Inst : Synth_Instance_Acc) return Net;
   pragma Inline (Get_Inst_Bit0, Get_Inst_Bit1);

   --  Start the definition of module M (using INST).
   procedure Set_Instance_Module (Inst : Synth_Instance_Acc; M : Module);

   function Get_Instance_Const (Inst : Synth_Instance_Acc) return Boolean;
   procedure Set_Instance_Const (Inst : Synth_Instance_Acc; Val : Boolean);

   --  Get the corresponding source for the scope of the instance.
   function Get_Source_Scope (Inst : Synth_Instance_Acc) return Node;

   procedure Create_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp);

   procedure Create_Package_Object (Syn_Inst : Synth_Instance_Acc;
                                    Decl : Node;
                                    Inst : Synth_Instance_Acc;
                                    Is_Global : Boolean);

   procedure Create_Subtype_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Typ : Type_Acc);

   --  Force the value of DECL, without checking for elaboration order.
   --  It is for deferred constants.
   procedure Create_Object_Force
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp);

   procedure Destroy_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node);

   --  Build the value for object OBJ.
   --  KIND must be Wire_Variable or Wire_Signal.
   procedure Create_Wire_Object (Syn_Inst : Synth_Instance_Acc;
                                 Kind : Wire_Kind;
                                 Obj : Node);

   --  Get the value of OBJ.
   function Get_Value (Syn_Inst : Synth_Instance_Acc; Obj : Node)
                      return Valtyp;

   --  Get a net from a scalar/vector value.  This will automatically create
   --  a net for literals.
   function Get_Net (Val : Valtyp) return Net;
   function Get_Partial_Memtyp_Net (Val : Memtyp; Off : Uns32; Wd : Width)
                                   return Net;
   function Get_Memtyp_Net (Val : Memtyp) return Net;

   function Get_Package_Object
     (Syn_Inst : Synth_Instance_Acc; Pkg : Node) return Synth_Instance_Acc;

   --  Return the type for DECL (a subtype indication).
   function Get_Subtype_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node) return Type_Acc;

   --  Return the scope of the parent of BLK.  Deals with architecture bodies.
   function Get_Parent_Scope (Blk : Node) return Sim_Info_Acc;

   procedure Set_Uninstantiated_Scope
     (Syn_Inst : Synth_Instance_Acc; Bod : Node);
private
   type Obj_Kind is
     (
      Obj_None,
      Obj_Object,
      Obj_Subtype,
      Obj_Instance
     );

   type Obj_Type (Kind : Obj_Kind := Obj_None) is record
      case Kind is
         when Obj_None =>
            null;
         when Obj_Object =>
            Obj : Valtyp;
         when Obj_Subtype =>
            T_Typ : Type_Acc;
         when Obj_Instance =>
            I_Inst : Synth_Instance_Acc;
      end case;
   end record;

   type Objects_Array is array (Object_Slot_Type range <>) of Obj_Type;

   type Base_Instance_Type is limited record
      Builder : Netlists.Builders.Context_Acc;
      Top_Module : Module;

      Cur_Module : Module;
      Bit0 : Net;
      Bit1 : Net;
   end record;

   type Base_Instance_Acc is access Base_Instance_Type;

   type Synth_Instance_Type (Max_Objs : Object_Slot_Type) is limited record
      Is_Const : Boolean;

      --  True if a fatal error has been detected that aborts the synthesis
      --  of this instance.
      Is_Error : Boolean;

      Base : Base_Instance_Acc;

      --  Name prefix for declarations.
      Name : Sname;

      --  The corresponding info for this instance.
      --  This is used for lookup.
      Block_Scope : Sim_Info_Acc;

      --  The corresponding info the the uninstantiated specification of
      --  an instantiated package.  When an object is looked for from the
      --  uninstantiated body, the scope of the uninstantiated specification
      --  is used.  And it is different from Block_Scope.
      --  This is used for lookup of uninstantiated specification.
      Uninst_Scope : Sim_Info_Acc;

      --  Instance of the parent scope.
      Up_Block : Synth_Instance_Acc;

      --  Source construct corresponding to this instance/
      Source_Scope : Node;

      Elab_Objects : Object_Slot_Type;

      --  Instance for synthesis.
      Objects : Objects_Array (1 .. Max_Objs);
   end record;
end Synth.Context;
