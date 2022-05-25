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

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Context; use Synth.Context;
with Synth.Vhdl_Environment; use Synth.Vhdl_Environment.Env;

package Synth.Vhdl_Context is
   --  Values are stored into Synth_Instance, which is parallel to simulation
   --  Block_Instance_Type.

   --  Create the root instance.
   procedure Make_Base_Instance (Base : Base_Instance_Acc);

   --  Free the first instance.
   procedure Free_Base_Instance;

   --  Create a synth instance.
   procedure Set_Extra (Inst : Synth_Instance_Acc;
                        Base : Base_Instance_Acc;
                        Name : Sname := No_Sname);

   procedure Set_Extra (Inst :Synth_Instance_Acc;
                        Parent : Synth_Instance_Acc;
                        Name : Sname := No_Sname);

   function Make_Instance (Parent : Synth_Instance_Acc;
                           Blk : Node;
                           Name : Sname := No_Sname)
                          return Synth_Instance_Acc;

   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc);

   --  Only useful for subprograms: set the base (which can be different from
   --  the parent).  Ideally it should be part of Make_Instance, but in most
   --  cases they are the same (except sometimes for subprograms).
   procedure Set_Instance_Base (Inst : Synth_Instance_Acc;
                                Base : Synth_Instance_Acc);

   function Get_Sname (Inst : Synth_Instance_Acc) return Sname;
   pragma Inline (Get_Sname);

   function Get_Build (Inst : Synth_Instance_Acc) return Context_Acc;
   pragma Inline (Get_Build);

   function Get_Top_Module (Inst : Synth_Instance_Acc) return Module;

   function Get_Instance_Module (Inst : Synth_Instance_Acc) return Module;
   pragma Inline (Get_Instance_Module);

   --  Start the definition of module M (using INST).
   procedure Set_Instance_Module (Inst : Synth_Instance_Acc; M : Module);

   --  Build the value for object OBJ.
   --  KIND must be Wire_Variable or Wire_Signal.
   procedure Create_Wire_Object (Syn_Inst : Synth_Instance_Acc;
                                 Kind : Wire_Kind;
                                 Obj : Node);

   --  Get a net from a scalar/vector value.  This will automatically create
   --  a net for literals.
   function Get_Net (Ctxt : Context_Acc; Val : Valtyp) return Net;
   function Get_Partial_Memtyp_Net
     (Ctxt : Context_Acc; Val : Memtyp; Off : Uns32; Wd : Width) return Net;
   function Get_Memtyp_Net (Ctxt : Context_Acc; Val : Memtyp) return Net;

   --  Can also return true for nets and wires.
   --  Use Get_Static_Discrete to get the value.
   function Is_Static_Val (Val : Value_Acc) return Boolean;

   function Get_Value_Net (Val : Value_Acc) return Net;
   pragma Inline (Get_Value_Net);
   procedure Set_Value_Net (Val : Value_Acc; N : Net);
   pragma Inline (Set_Value_Net);
   function Get_Value_Wire (Val : Value_Acc) return Wire_Id;
   pragma Inline (Get_Value_Wire);
   procedure Set_Value_Wire (Val : Value_Acc; W : Wire_Id);
   pragma Inline (Set_Value_Wire);

   --  Create a Value_Net.
   function Create_Value_Net (N : Net; Ntype : Type_Acc) return Valtyp;

   --  Create a Value_Wire.  For a bit wire, RNG must be null.
   function Create_Value_Wire (W : Wire_Id; Wtype : Type_Acc) return Valtyp;

   --  Create a Value_Dyn_Alias
   function Create_Value_Dyn_Alias (Obj : Value_Acc;
                                    Poff : Uns32;
                                    Ptyp : Type_Acc;
                                    Voff : Net;
                                    Eoff : Uns32;
                                    Typ : Type_Acc) return Valtyp;

   function Get_Value_Dyn_Alias_Voff (Val : Value_Acc) return Net;
private
   type Extra_Vhdl_Instance_Type is record
      Base : Base_Instance_Acc;

      --  Name prefix for declarations.
      Name : Sname;
   end record;
end Synth.Vhdl_Context;
