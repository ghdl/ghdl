--  Back-end for translation.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Disp_Tree;
with Disp_Vhdl;
with Sem;
with Canon;
with Translation;
with Errorout; use Errorout;
with Post_Sems;
with Flags;
with Ada.Text_IO;

package body Trans_Be is
   procedure Finish_Compilation
     (Unit : Iir_Design_Unit; Main : Boolean := False)
   is
      use Ada.Text_IO;
      Lib : Iir;
   begin
      --  No need to semantize during elaboration.
      --if Flags.Will_Elaborate then
      --   return;
      --end if;

      Lib := Get_Library_Unit (Unit);

      if (Main or Flags.Dump_All) and then Flags.Dump_Parse then
         Disp_Tree.Disp_Tree (Unit);
      end if;

      --  Semantic analysis.
      if Flags.Verbose then
         Put_Line ("semantize " & Disp_Node (Lib));
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

      --  Canonalisation.
      ------------------
      if Flags.Verbose then
         Put_Line ("canonicalize " & Disp_Node (Lib));
      end if;

      Canon.Canonicalize (Unit);

      if (Main or Flags.Dump_All) and then Flags.Dump_Canon then
         Disp_Tree.Disp_Tree (Unit);
      end if;

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      if (Main or Flags.List_All) and then Flags.List_Canon then
         Disp_Vhdl.Disp_Vhdl (Unit);
      end if;

      if Flags.Flag_Elaborate then
         if Get_Kind (Lib) = Iir_Kind_Architecture_Declaration then
            declare
               Config : Iir_Design_Unit;
            begin
               Config := Canon.Create_Default_Configuration_Declaration (Lib);
               Set_Default_Configuration_Declaration (Lib, Config);
               if (Main or Flags.Dump_All) and then Flags.Dump_Canon then
                  Disp_Tree.Disp_Tree (Config);
               end if;
               if (Main or Flags.List_All) and then Flags.List_Canon then
                  Disp_Vhdl.Disp_Vhdl (Config);
               end if;
            end;
         end if;

         --  Do not translate during elaboration.
         --  This is done directly in Translation.Chap12.
         return;
      end if;

      --  Translation
      ---------------
      if not Main then
         --  Main units (those from the analyzed design file) are translated
         --  directly by ortho_front.
         if Flags.Verbose then
            Put_Line ("translate " & Disp_Node (Lib));
         end if;

         Translation.Translate (Unit, Main);

         if Errorout.Nbr_Errors > 0 then
            raise Compilation_Error;
         end if;
      end if;

   end Finish_Compilation;

   procedure Sem_Foreign (Decl : Iir)
   is
      use Translation;
      Fi : Foreign_Info_Type;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Design_Unit =>
            Error_Msg_Sem ("FOREIGN architectures are not yet handled", Decl);
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            null;
         when others =>
            Error_Kind ("sem_foreign", Decl);
      end case;
      --  Let is generate error messages.
      Fi := Translate_Foreign_Id (Decl, False);
   end Sem_Foreign;
end Trans_Be;
