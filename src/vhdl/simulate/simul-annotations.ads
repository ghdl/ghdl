--  Annotations for interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Iirs; use Iirs;
with Simul.Environments; use Simul.Environments;

package Simul.Annotations is
   --  Decorate the tree in order to be usable with the internal simulator.
   procedure Annotate (Unit : Iir_Design_Unit);

   --  Disp annotations for an iir node.
   procedure Disp_Vhdl_Info (Node : Iir);
   procedure Disp_Tree_Info (Node : Iir);

   Global_Info : Sim_Info_Acc;

   -- Annotations are used to collect informations for elaboration and to
   -- locate iir_value_literal for signals, variables or constants.

   -- Get/Set annotation fied from/to an iir.
   procedure Set_Info (Target : Iir; Info : Sim_Info_Acc);
   pragma Inline (Set_Info);
   function Get_Info (Target : Iir) return Sim_Info_Acc;
   pragma Inline (Get_Info);

   --  Expand the annotation table.  This is automatically done by Annotate,
   --  to be used only by debugger.
   procedure Annotate_Expand_Table;
end Simul.Annotations;
