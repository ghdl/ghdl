--  Design elaboration
--  Copyright (C) 2021 Tristan Gingold
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

with Tables;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Elab.Vhdl_Insts is
   Flag_Macro_Expand_Instance : Boolean := False;

   --  The result of Elab_Top_Unit, to handle external names.
   Top_Instance : Synth_Instance_Acc;

   --  Table of library units for the elaborated design.
   --  This is similar to the design unit table in vhdl.configuration,
   --  except unsued units are not present and instantiated units are added.
   package Elab_Units is new Tables
     (Table_Component_Type => Node,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Elab_Top_Unit (Config : Node) return Synth_Instance_Acc;

   procedure Elab_Generics_Association (Sub_Inst : Synth_Instance_Acc;
                                        Syn_Inst : Synth_Instance_Acc;
                                        Inter_Chain : Node;
                                        Assoc_Chain : Node);

   procedure Elab_Ports_Association_Type (Sub_Inst : Synth_Instance_Acc;
                                          Syn_Inst : Synth_Instance_Acc;
                                          Inter_Chain : Node;
                                          Assoc_Chain : Node);

   procedure Elab_Package_Declaration
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node);
   procedure Elab_Package_Body
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node; Bod : Node);

   --  Return the body to be elaborated.
   procedure Elab_Package_Instantiation_Assoc
     (Parent_Inst : Synth_Instance_Acc;
      Pkg : Node;
      Sub_Inst : out Synth_Instance_Acc;
      Bod : out Node);
   --  Elaborate the body.
   procedure Elab_Package_Instantiation
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node);
   procedure Elab_Package_Instantiation_Body
     (Parent_Inst : Synth_Instance_Acc; Bod : Node);

   type Configs_Rec is record
      Cfg : Iir_Array_Acc;
      Idx : Natural;
   end record;

   procedure Free_Configs_Rec (Cfg : in out Configs_Rec);

   --  Apply block configuration CFG to BLK, return the list of block
   --  configuration sorted by position.
   --  Must be done before synthesis of BLK.
   function Apply_Block_Configuration (Cfg : Node; Blk : Node)
                                      return Configs_Rec;

   --  Get the next block configuration from CFG
   --  There is an assertion to check it corresponds to STMT.
   procedure Get_Next_Block_Configuration (Cfg : in out Configs_Rec;
                                           Res : out Node);

   procedure Elab_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Cfgs : in out Configs_Rec);
   procedure Elab_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);

   type Elab_Foreign_Instance_Acc is access
     procedure (Syn_Inst : Synth_Instance_Acc;
                Comp_Inst : Synth_Instance_Acc;
                Bind : Node;
                Module : Node);

   Elab_Foreign_Instance : Elab_Foreign_Instance_Acc;
end Elab.Vhdl_Insts;
