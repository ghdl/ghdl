--  Interpreter back-end
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

with Ada.Text_IO;
with Sem;
with Canon;
with Annotations;
with Disp_Tree;
with Errorout; use Errorout;
with Flags;
with Disp_Vhdl;
with Post_Sems;

package body Sim_Be is
   procedure Finish_Compilation (Unit: Iir_Design_Unit; Main: Boolean := False)
   is
      use Ada.Text_IO;
      Lib_Unit : Iir;
   begin
      Lib_Unit := Get_Library_Unit (Unit);
      -- Semantic analysis.
      if Flags.Verbose then
         Put_Line ("semantize " & Disp_Node (Lib_Unit));
      end if;
      Sem.Semantic (Unit);

      if (Main or Flags.Dump_All) and then Flags.Dump_Sem then
         Disp_Tree.Disp_Tree (Unit);
      end if;

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      if (Main or Flags.List_All) and then Flags.List_Sem then
         Disp_Vhdl.Disp_Vhdl (Unit);
      end if;

      --  Post checks
      ----------------

      Post_Sems.Post_Sem_Checks (Unit);

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;


      -- Canonicalisation.
      ------------------
      if Flags.Verbose then
         Put_Line ("canonicalize " & Disp_Node (Lib_Unit));
      end if;

      Canon.Canonicalize (Unit);

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      if (Main or Flags.List_All) and then Flags.List_Canon then
         Disp_Vhdl.Disp_Vhdl (Unit);
      end if;

      if Flags.Flag_Elaborate then
         if Get_Kind (Lib_Unit) = Iir_Kind_Architecture_Body then
            declare
               Config : Iir_Design_Unit;
            begin
               Config := Canon.Create_Default_Configuration_Declaration
                 (Lib_Unit);
               Set_Default_Configuration_Declaration (Lib_Unit, Config);
               if (Main or Flags.Dump_All) and then Flags.Dump_Canon then
                  Disp_Tree.Disp_Tree (Config);
               end if;
               if (Main or Flags.List_All) and then Flags.List_Canon then
                  Disp_Vhdl.Disp_Vhdl (Config);
               end if;
            end;
         end if;
      end if;

      -- Annotation.
      -------------
      if Flags.Verbose then
         Put_Line ("annotate " & Disp_Node (Lib_Unit));
      end if;

      Annotations.Annotate (Unit);

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      if (Main or Flags.List_All) and then Flags.List_Annotate then
         Disp_Vhdl.Disp_Vhdl (Unit);
      end if;
      if (Main or Flags.Dump_All) and then Flags.Dump_Annotate then
         Disp_Tree.Disp_Tree (Unit);
      end if;
   end Finish_Compilation;
end Sim_Be;
