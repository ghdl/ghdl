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

with Types; use Types;

with Vhdl.Annotations; use Vhdl.Annotations;
with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

package Elab.Vhdl_Context is
   --  Values are stored into Synth_Instance, which is parallel to simulation
   --  Block_Instance_Type.

   type Synth_Instance_Type (<>) is limited private;
   type Synth_Instance_Acc is access Synth_Instance_Type;

   Root_Instance : Synth_Instance_Acc;

   --  Unique per instance id.  Used to create parallel tables.
   type Instance_Id_Type is new Natural;
   First_Instance_Id : constant Instance_Id_Type := 1;

   function Get_Instance_Id (Inst : Synth_Instance_Acc)
                            return Instance_Id_Type;
   pragma Inline (Get_Instance_Id);

   function Get_Instance_By_Scope
     (Syn_Inst: Synth_Instance_Acc; Scope: Sim_Info_Acc)
     return Synth_Instance_Acc;

   --  Create the root instance (which contains the packages).
   --  Assign ROOT_INSTANCE.
   procedure Make_Root_Instance;

   --  Free the first instance.
   procedure Free_Base_Instance;

   --  Create and free the corresponding synth instance.
   function Make_Elab_Instance
     (Parent : Synth_Instance_Acc; Blk : Node; Config : Node)
     return Synth_Instance_Acc;

   procedure Free_Elab_Instance (Synth_Inst : in out Synth_Instance_Acc);

   function Make_Elab_Generate_Instance
     (Parent : Synth_Instance_Acc; Blk : Node; Config : Node; Len : Natural)
     return Synth_Instance_Acc;

   function Get_Generate_Sub_Instance
     (Parent : Synth_Instance_Acc; Idx : Positive) return Synth_Instance_Acc;
   procedure Set_Generate_Sub_Instance
     (Parent : Synth_Instance_Acc; Idx : Positive; Child : Synth_Instance_Acc);

   function Is_Error (Inst : Synth_Instance_Acc) return Boolean;
   pragma Inline (Is_Error);

   procedure Set_Error (Inst : Synth_Instance_Acc);

   function Get_Instance_Const (Inst : Synth_Instance_Acc) return Boolean;
   procedure Set_Instance_Const (Inst : Synth_Instance_Acc; Val : Boolean);

   --  Get the corresponding source for the scope of the instance.
   function Get_Source_Scope (Inst : Synth_Instance_Acc) return Node;

   --  Get parent_instance.
   function Get_Instance_Parent (Inst : Synth_Instance_Acc)
                                return Synth_Instance_Acc;

   procedure Set_Instance_Config (Inst : Synth_Instance_Acc; Config : Node);
   function Get_Instance_Config (Inst : Synth_Instance_Acc) return Node;

   procedure Set_Instance_Foreign (Inst : Synth_Instance_Acc; N : Int32);
   function Get_Instance_Foreign (Inst : Synth_Instance_Acc) return Int32;

   --  Add/Get extra instances.
   --  Those instances are verification units.
   procedure Add_Extra_Instance (Inst : Synth_Instance_Acc;
                                 Extra : Synth_Instance_Acc);
   function Get_First_Extra_Instance (Inst : Synth_Instance_Acc)
                                     return Synth_Instance_Acc;
   function Get_Next_Extra_Instance (Inst : Synth_Instance_Acc)
                                     return Synth_Instance_Acc;

   procedure Create_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp);

   procedure Create_Signal (Syn_Inst : Synth_Instance_Acc;
                            Decl : Node;
                            Typ : Type_Acc;
                            Init : Value_Acc);

   --  Number of created signals.
   function Get_Nbr_Signal return Signal_Index_Type;

   --  Create a sub instance: either a direct entity instantiation, or
   --  a component instantiation.
   procedure Create_Sub_Instance (Syn_Inst : Synth_Instance_Acc;
                                  Stmt : Node;
                                  Sub_Inst : Synth_Instance_Acc);

   --  Create a sub instance for a component.
   procedure Create_Component_Instance (Syn_Inst : Synth_Instance_Acc;
                                        Sub_Inst : Synth_Instance_Acc);

   procedure Create_Package_Object (Syn_Inst : Synth_Instance_Acc;
                                    Decl : Node;
                                    Inst : Synth_Instance_Acc;
                                    Is_Global : Boolean);

   function Create_Package_Instance (Parent_Inst : Synth_Instance_Acc;
                                     Pkg : Node)
                                    return Synth_Instance_Acc;

   procedure Create_Package_Interface (Syn_Inst : Synth_Instance_Acc;
                                       Decl     : Node;
                                       Inst     : Synth_Instance_Acc);

   procedure Create_Subtype_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Typ : Type_Acc);

   --  Force the value of DECL, without checking for elaboration order.
   --  It is for deferred constants.
   procedure Create_Object_Force
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp);

   procedure Replace_Signal
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp);
   procedure Mutate_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Vt : Valtyp);

   type Destroy_Type is limited private;
   procedure Destroy_Init (D : out Destroy_Type;
                           Syn_Inst : Synth_Instance_Acc);
   procedure Destroy_Object (D : in out Destroy_Type; Decl : Node);
   procedure Destroy_Finish (D : in out Destroy_Type);

   --  Get the value of OBJ.
   function Get_Value (Syn_Inst : Synth_Instance_Acc; Obj : Node)
                      return Valtyp;

   function Get_Package_Object
     (Syn_Inst : Synth_Instance_Acc; Pkg : Node) return Synth_Instance_Acc;

   --  Return the type for DECL (a subtype indication).
   function Get_Subtype_Object
     (Syn_Inst : Synth_Instance_Acc; Decl : Node) return Type_Acc;

   function Get_Sub_Instance
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node) return Synth_Instance_Acc;
   function Get_Component_Instance
     (Syn_Inst : Synth_Instance_Acc) return Synth_Instance_Acc;

   --  Return the scope of the parent of BLK.  Deals with architecture bodies.
   function Get_Parent_Scope (Blk : Node) return Sim_Info_Acc;

   procedure Set_Uninstantiated_Scope
     (Syn_Inst : Synth_Instance_Acc; Bod : Node);

   --  For debugging purpose: update the caller of an instance.
   procedure Set_Caller_Instance (Syn_Inst : Synth_Instance_Acc;
                                  Caller : Synth_Instance_Acc);
   function Get_Caller_Instance (Syn_Inst : Synth_Instance_Acc)
                                return Synth_Instance_Acc;
private
   type Destroy_Type is record
      Inst : Synth_Instance_Acc;
      First : Object_Slot_Type;
      Last : Object_Slot_Type;
   end record;

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

   type Synth_Instance_Type (Max_Objs : Object_Slot_Type) is limited record
      Is_Const : Boolean;

      --  True if a fatal error has been detected that aborts the synthesis
      --  of this instance.
      Is_Error : Boolean;

      Id : Instance_Id_Type;

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

      --  For a subprogram instance, the instance of the caller.
      --  Used only fo debugging purpose.
      Caller : Synth_Instance_Acc;

      --  Source construct corresponding to this instance.
      Source_Scope : Node;

      --  Block configuration (unless the instance is for a package).
      Config : Node;
      Foreign : Int32;

      --  Chain of verification units that applies to this one.
      Extra_Units : Synth_Instance_Acc;
      Extra_Link : Synth_Instance_Acc;

      --  Last elaborated object.  Detect elaboration issues.
      Elab_Objects : Object_Slot_Type;

      --  Instance for synthesis.
      Objects : Objects_Array (1 .. Max_Objs);
   end record;
end Elab.Vhdl_Context;
