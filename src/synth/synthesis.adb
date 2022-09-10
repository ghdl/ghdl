--  Synthesis.
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

with Errorout; use Errorout;
with Name_Table; use Name_Table;

with Netlists.Cleanup;
with Netlists.Memories;
with Netlists.Expands;

with Elab.Vhdl_Values.Debug;
pragma Unreferenced (Elab.Vhdl_Values.Debug);
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;

with Synth.Vhdl_Insts; use Synth.Vhdl_Insts;

package body Synthesis is
   function Make_Base_Instance return Base_Instance_Acc
   is
      Base : Base_Instance_Acc;
      Top_Module : Module;
      Ctxt : Context_Acc;
   begin
      Top_Module :=
        New_Design (New_Sname_Artificial (Get_Identifier ("top"), No_Sname));
      Ctxt := Build_Builders (Top_Module);

      Base := new Base_Instance_Type'(Builder => Ctxt,
                                      Top_Module => Top_Module,
                                      Cur_Module => No_Module);
      return Base;
   end Make_Base_Instance;

   function Synth_Design (Design : Iir;
                          Inst : Synth_Instance_Acc;
                          Encoding : Name_Encoding) return Module
   is
      Base : Base_Instance_Acc;
      Unit : Iir;
   begin
      Base := Make_Base_Instance;

      if Synth_Initialize_Foreign /= null then
         Synth_Initialize_Foreign.all;
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      Unit := Get_Library_Unit (Design);
      if Get_Kind (Unit) = Iir_Kind_Foreign_Module then
         if Synth_Top_Foreign = null then
            raise Internal_Error;
         end if;
         Synth_Top_Foreign (Base, Get_Foreign_Node (Unit), Encoding);
      else
         Synth_Top_Entity (Base, Design, Encoding, Inst);
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      Synth.Vhdl_Insts.Synth_All_Instances;

      pragma Assert (Is_Expr_Pool_Empty);

      if Errorout.Nbr_Errors > 0 then
         return No_Module;
      end if;

      return Base.Top_Module;
   end Synth_Design;

   procedure Instance_Passes (Ctxt : Context_Acc; M : Module) is
   begin
      if not Synth.Flags.Flag_Debug_Nonull then
         Netlists.Cleanup.Replace_Null_Inputs (Ctxt, M);
      end if;

      --  Remove unused gates.  This is not only an optimization but also
      --  a correctness point: there might be some unsynthesizable gates, like
      --  the one created for 'rising_egde (clk) and not rst'.
      if not Synth.Flags.Flag_Debug_Nocleanup then
         --  Netlists.Cleanup.Remove_Unconnected_Instances (Inst.M);
         Netlists.Cleanup.Mark_And_Sweep (M);
         Netlists.Cleanup.Remove_Output_Gates (M);
      end if;

      if not Synth.Flags.Flag_Debug_Nomemory2 then
         Netlists.Memories.Extract_Memories (Ctxt, M);
         --  Remove remaining clock edge gates.
         Netlists.Cleanup.Mark_And_Sweep (M);
      end if;

      if not Synth.Flags.Flag_Debug_Noexpand then
         Netlists.Expands.Expand_Gates (Ctxt, M);
         --  Remove unused dyn_expand inputs.
         Netlists.Cleanup.Mark_And_Sweep (M);
      end if;
   end Instance_Passes;

end Synthesis;
