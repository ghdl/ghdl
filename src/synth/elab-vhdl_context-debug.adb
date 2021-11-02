--  Synthesis context.
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

with Types; use Types;
with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;

with Vhdl.Errors;
with Elab.Vhdl_Values.Debug; use Elab.Vhdl_Values.Debug;

package body Elab.Vhdl_Context.Debug is
   procedure Debug_Synth_Instance (Inst : Synth_Instance_Acc) is
   begin
      Put_Line ("instance for: "
                  & Vhdl.Errors.Disp_Node (Get_Source_Scope (Inst)));
      for I in Inst.Objects'Range loop
         Put_Uns32 (Uns32 (I));
         Put (": ");
         case Inst.Objects (I).Kind is
            when Obj_None =>
               Put_Line ("none");
            when Obj_Object =>
               Put ("object");
               Put (": ");
               Debug_Valtyp (Inst.Objects (I).Obj);
            when Obj_Subtype =>
               Put ("subtype");
               Put (": ");
               Debug_Typ (Inst.Objects (I).T_Typ);
            when Obj_Instance =>
               Put ("instance");
               New_Line;
         end case;
      end loop;
   end Debug_Synth_Instance;

   procedure Debug_Elab_Tree_1 (Inst : Synth_Instance_Acc; Level : Natural) is
   begin
      Put_Indent (Level);
      if Inst = null then
         Put_Line ("*null*");
         return;
      end if;

      Put_Line (Vhdl.Errors.Disp_Node (Get_Source_Scope (Inst)));

      for I in Inst.Objects'Range loop
         if Inst.Objects (I).Kind = Obj_Instance then
            Debug_Elab_Tree_1 (Inst.Objects (I).I_Inst, Level + 1);
         end if;
      end loop;
   end Debug_Elab_Tree_1;

   procedure Debug_Elab_Tree (Inst : Synth_Instance_Acc) is
   begin
      Debug_Elab_Tree_1 (Inst, 0);
   end Debug_Elab_Tree;
end Elab.Vhdl_Context.Debug;
