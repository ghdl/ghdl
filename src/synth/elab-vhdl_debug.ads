--  Debug utilities on elaborated design
--  Copyright (C) 2019 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Nodes_Walk; use Vhdl.Nodes_Walk;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Elab.Vhdl_Debug is
   procedure Disp_Memtyp (M : Memtyp; Vtype : Node);
   function Walk_Declarations (Cb : Walk_Cb) return Walk_Status;

   procedure Disp_Integer_Value (Val : Int64; Btype : Node);
   procedure Disp_Enumeration_Value (Val : Int64; Btype : Node);
   procedure Disp_Physical_Value (Val : Int64; Btype : Node);
   procedure Disp_Float_Value (Val : Fp64; Btype : Node);

   procedure Disp_Discrete_Value (Val : Int64; Btype : Node);

   procedure Disp_Declaration_Objects
     (Instance : Synth_Instance_Acc; Decl_Chain : Iir; Indent : Natural := 0);

   procedure Disp_Hierarchy (Inst : Synth_Instance_Acc;
                             Recurse : Boolean;
                             With_Objs : Boolean);

   --  Get sub-instance NAME of INST.  Return null if not found.
   function Get_Sub_Instance_By_Name (Inst : Synth_Instance_Acc; Name : String)
                                     return Synth_Instance_Acc;

   function Get_Instance_Path_Parent (Inst : Synth_Instance_Acc)
                                     return Synth_Instance_Acc;

   --  Disp full path name of INST.
   --  If COMPONENTS is true, also display components
   procedure Disp_Instance_Path (Inst : Synth_Instance_Acc;
                                 Components : Boolean := False);

   procedure Put_Stmt_Trace (Stmt : Iir);

   procedure Append_Commands;
end Elab.Vhdl_Debug;
