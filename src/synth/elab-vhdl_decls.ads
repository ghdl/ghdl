--  Create declarations for synthesis.
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

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Elab.Vhdl_Decls is
   procedure Elab_Subprogram_Declaration
     (Syn_Inst : Synth_Instance_Acc; Subprg : Node);
   procedure Elab_File_Declaration (Syn_Inst : Synth_Instance_Acc;
                                    Decl : Node);

   procedure Elab_Declaration (Syn_Inst : Synth_Instance_Acc;
                               Decl : Node;
                               Force_Init : Boolean;
                               Last_Type : in out Node);

   procedure Elab_Declarations (Syn_Inst : Synth_Instance_Acc;
                                Decls : Iir;
                                Force_Init : Boolean := False);

   procedure Finalize_Declaration (Syn_Inst : Synth_Instance_Acc;
                                   Decl : Iir;
                                   Is_Subprg : Boolean);
   procedure Finalize_Declarations (Syn_Inst : Synth_Instance_Acc;
                                    Decls : Iir;
                                    Is_Subprg : Boolean := False);

   --  Create a signal.
   --  It's a wrapper around elab.vhdl_context.create_signal but it computes
   --  the default value.
   --  Also used to create signals for ports.
   procedure Create_Signal (Syn_Inst : Synth_Instance_Acc;
                            Decl : Node;
                            Typ : Type_Acc);
end Elab.Vhdl_Decls;
