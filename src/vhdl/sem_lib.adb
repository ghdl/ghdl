--  VHDL libraries handling.
--  Copyright (C) 2018 Tristan Gingold
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
with Flags;
with Name_Table;
with Files_Map;
with Iirs_Utils; use Iirs_Utils;
with Errorout; use Errorout;
with Libraries; use Libraries;
with Scanner;
with Parse;
with Disp_Tree;
with Disp_Vhdl;
with Sem;
with Post_Sems;
with Canon;
with Nodes_GC;

package body Sem_Lib is
   procedure Error_Lib_Msg (Msg : String; Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Library, No_Location, Msg, (1 => Arg1));
   end Error_Lib_Msg;

   function Load_File (File : Source_File_Entry) return Iir_Design_File
   is
      Res : Iir_Design_File;
   begin
      Scanner.Set_File (File);
      if Scanner.Detect_Encoding_Errors then
         --  Don't even try to parse such a file.  The BOM will be interpreted
         --  as an identifier, which is not valid at the beginning of a file.
         Res := Null_Iir;
      else
         Res := Parse.Parse_Design_File;
      end if;
      Scanner.Close_File;

      if Res /= Null_Iir then
         Set_Parent (Res, Work_Library);
         Set_Design_File_Filename (Res, Files_Map.Get_File_Name (File));
         Set_Design_File_Source (Res, File);
      end if;
      return Res;
   end Load_File;

   -- parse a file.
   -- Return a design_file without putting it into the library
   -- (because it was not analyzed).
   function Load_File_Name (File_Name: Name_Id) return Iir_Design_File
   is
      Fe : Source_File_Entry;
   begin
      Fe := Files_Map.Read_Source_File (Local_Directory, File_Name);
      if Fe = No_Source_File_Entry then
         Error_Msg_Option ("cannot open " & Name_Table.Image (File_Name));
         return Null_Iir;
      end if;
      return Load_File (Fe);
   end Load_File_Name;

   procedure Finish_Compilation
     (Unit : Iir_Design_Unit; Main : Boolean := False)
   is
      Lib_Unit : Iir;
   begin
      Lib_Unit := Get_Library_Unit (Unit);
      if (Main or Flags.Dump_All) and then Flags.Dump_Parse then
         Disp_Tree.Disp_Tree (Unit);
      end if;

      if Flags.Check_Ast_Level > 0 then
         Nodes_GC.Check_Tree (Unit);
      end if;

      if Flags.Verbose then
         Report_Msg (Msgid_Note, Semantic, +Lib_Unit,
                     "analyze %n", (1 => +Lib_Unit));
      end if;

      Sem.Semantic (Unit);

      if (Main or Flags.Dump_All) and then Flags.Dump_Sem then
         Disp_Tree.Disp_Tree (Unit);
      end if;

      if Errorout.Nbr_Errors > 0 then
         return;
      end if;

      if (Main or Flags.List_All) and then Flags.List_Sem then
         Disp_Vhdl.Disp_Vhdl (Unit);
      end if;

      if Flags.Check_Ast_Level > 0 then
         Nodes_GC.Check_Tree (Unit);
      end if;

      --  Post checks
      ----------------

      Post_Sems.Post_Sem_Checks (Unit);

      if Errorout.Nbr_Errors > 0 then
         return;
      end if;

      --  Canonalisation.
      ------------------

      if Flags.Verbose then
         Report_Msg (Msgid_Note, Semantic, +Lib_Unit,
                     "canonicalize %n", (1 => +Lib_Unit));
      end if;

      Canon.Canonicalize (Unit);

      if (Main or Flags.Dump_All) and then Flags.Dump_Canon then
         Disp_Tree.Disp_Tree (Unit);
      end if;

      if Errorout.Nbr_Errors > 0 then
         return;
      end if;

      if (Main or Flags.List_All) and then Flags.List_Canon then
         Disp_Vhdl.Disp_Vhdl (Unit);
      end if;

      if Flags.Check_Ast_Level > 0 then
         Nodes_GC.Check_Tree (Unit);
      end if;
   end Finish_Compilation;

   procedure Free_Dependence_List (Design : Iir_Design_Unit)
   is
      List : Iir_List;
   begin
      List := Get_Dependence_List (Design);
      if List /= Null_Iir_List then
         Free_Recursive_List (List);
         Destroy_Iir_List (List);
      end if;
   end Free_Dependence_List;

   procedure Load_Parse_Design_Unit (Design_Unit: Iir_Design_Unit; Loc : Iir)
   is
      use Scanner;
      Design_File : constant Iir_Design_File := Get_Design_File (Design_Unit);
      Fe : Source_File_Entry;
      Line, Off: Natural;
      Pos: Source_Ptr;
      Res: Iir;
   begin
      --  The unit must not be loaded.
      pragma Assert (Get_Date_State (Design_Unit) = Date_Disk);

      Fe := Get_Design_File_Source (Design_File);
      if Fe = No_Source_File_Entry then
         --  Load the file in memory.
         Fe := Files_Map.Read_Source_File
           (Get_Design_File_Directory (Design_File),
            Get_Design_File_Filename (Design_File));
         if Fe = No_Source_File_Entry then
            Error_Lib_Msg ("cannot load %n", +Get_Library_Unit (Design_Unit));
            raise Compilation_Error;
         end if;
         Set_Design_File_Source (Design_File, Fe);

         --  Check if the file has changed.
         if not Files_Map.Is_Eq
           (Files_Map.Get_File_Checksum (Fe), Get_File_Checksum (Design_File))
         then
            Error_Msg_Sem (+Loc, "file %i has changed and must be reanalysed",
                           +Get_Design_File_Filename (Design_File));
            raise Compilation_Error;
         end if;
      end if;

      if Get_Date (Design_Unit) = Date_Obsolete then
         Error_Msg_Sem (+Loc, "%n has been obsoleted",
                        +Get_Library_Unit (Design_Unit));
         raise Compilation_Error;
      end if;

      --  Set the position of the lexer
      Set_File (Fe);
      Pos := Get_Design_Unit_Source_Pos (Design_Unit);
      Line := Natural (Get_Design_Unit_Source_Line (Design_Unit));
      Off := Natural (Get_Design_Unit_Source_Col (Design_Unit));
      Files_Map.File_Add_Line_Number (Get_Current_Source_File, Line, Pos);
      Set_Current_Position (Pos + Source_Ptr (Off));

      --  Parse
      Scan;
      Res := Parse.Parse_Design_Unit;
      Close_File;
      if Res = Null_Iir then
         raise Compilation_Error;
      end if;

      Set_Date_State (Design_Unit, Date_Parse);

      --  FIXME: check the library unit read is the one expected.

      --  Move the unit in the library: keep the design_unit of the library,
      --  but replace the library_unit by the one that has been parsed.  Do
      --  not forget to relocate parents.
      Iirs_Utils.Free_Recursive (Get_Library_Unit (Design_Unit));
      Set_Library_Unit (Design_Unit, Get_Library_Unit (Res));
      Set_Design_Unit (Get_Library_Unit (Res), Design_Unit);
      Set_Parent (Get_Library_Unit (Res), Design_Unit);
      declare
         Item : Iir;
      begin
         Item := Get_Context_Items (Res);
         Set_Context_Items (Design_Unit, Item);
         while Is_Valid (Item) loop
            Set_Parent (Item, Design_Unit);
            Item := Get_Chain (Item);
         end loop;
      end;
      Location_Copy (Design_Unit, Res);
      Free_Dependence_List (Design_Unit);
      Set_Dependence_List (Design_Unit, Get_Dependence_List (Res));
      Set_Dependence_List (Res, Null_Iir_List);
      Free_Iir (Res);
   end Load_Parse_Design_Unit;

   procedure Error_Obsolete (Loc : Iir; Msg : String; Args : Earg_Arr) is
   begin
      if not Flags.Flag_Elaborate_With_Outdated then
         if Loc = Null_Iir then
            Error_Msg_Sem (Command_Line_Location, Msg, Args);
         else
            Error_Msg_Sem (+Loc, Msg, Args);
         end if;
      end if;
   end Error_Obsolete;

   --  Check if one of its dependency makes this unit obsolete.
   function Check_Obsolete_Dependence (Design_Unit : Iir; Loc : Iir)
                                      return Boolean
   is
      List : constant Iir_List := Get_Dependence_List (Design_Unit);
      Du_Ts : constant Time_Stamp_Id :=
        Get_Analysis_Time_Stamp (Get_Design_File (Design_Unit));
      U_Ts : Time_Stamp_Id;
      El : Iir;
      It : List_Iterator;
   begin
      if List = Null_Iir_List then
         return False;
      end if;

      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         if Get_Kind (El) = Iir_Kind_Design_Unit then
            U_Ts := Get_Analysis_Time_Stamp (Get_Design_File (El));
            if Files_Map.Is_Gt (U_Ts, Du_Ts) then
               Error_Obsolete
                 (Loc, "%n is obsoleted by %n", (+Design_Unit, +El));
               return True;
            end if;
         end if;
         Next (It);
      end loop;

      return False;
   end Check_Obsolete_Dependence;

   procedure Explain_Obsolete (Design_Unit : Iir_Design_Unit; Loc : Iir)
   is
      List : Iir_List;
      It : List_Iterator;
      El : Iir;
   begin
      pragma Assert (Get_Date_State (Design_Unit) = Date_Analyze);
      pragma Assert (Get_Date (Design_Unit) = Date_Obsolete);

      List := Get_Dependence_List (Design_Unit);
      if List = Null_Iir_List then
         --  Argh, we don't know why.
         Error_Obsolete (Loc, "%n is obsolete", (1 => +Design_Unit));
         return;
      end if;

      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         if Get_Date (El) = Date_Obsolete then
            Error_Obsolete (Loc, "%n is obsoleted by %n", (+Design_Unit, +El));
            return;
         end if;
         Next (It);
      end loop;
   end Explain_Obsolete;

   -- Load, parse, analyze, back-end a design_unit if necessary.
   procedure Load_Design_Unit (Design_Unit : Iir_Design_Unit; Loc : Iir)
   is
      Warnings : Warnings_Setting;
   begin
      if Get_Date (Design_Unit) = Date_Replacing then
         Error_Msg_Sem (+Loc, "circular reference of %n", +Design_Unit);
         return;
      end if;

      if Get_Date_State (Design_Unit) = Date_Disk then
         Load_Parse_Design_Unit (Design_Unit, Loc);
      end if;

      if Get_Date_State (Design_Unit) = Date_Parse then
         --  Analyze the design unit.

         if Get_Date (Design_Unit) = Date_Analyzed then
            --  Work-around for an internal check in sem.
            --  FIXME: to be removed ?
            Set_Date (Design_Unit, Date_Parsed);
         end if;

         --  Avoid infinite recursion, if the unit is self-referenced.
         Set_Date_State (Design_Unit, Date_Analyze);

         --  Disable all warnings.  Warnings are emitted only when the unit
         --  is analyzed.
         Save_Warnings_Setting (Warnings);
         Disable_All_Warnings;

         --  Analyze unit.
         Finish_Compilation (Design_Unit);

         --  Restore warnings.
         Restore_Warnings_Setting (Warnings);

         --  Check if one of its dependency makes this unit obsolete.
         --  FIXME: to do when the dependency is added ?
         if not Flags.Flag_Elaborate_With_Outdated
           and then Check_Obsolete_Dependence (Design_Unit, Loc)
         then
            Set_Date (Design_Unit, Date_Obsolete);
            return;
         end if;
      end if;

      case Get_Date (Design_Unit) is
         when Date_Parsed =>
            raise Internal_Error;
         when Date_Analyzing =>
            --  Self-referenced unit.
            return;
         when Date_Analyzed =>
            --  FIXME: Accept it silently ?
            --  Note: this is used when Flag_Elaborate_With_Outdated is set.
            --  This is also used by anonymous configuration declaration.
            null;
         when Date_Uptodate =>
            return;
         when Date_Valid =>
            null;
         when Date_Obsolete =>
            if not Flags.Flag_Elaborate_With_Outdated then
               Explain_Obsolete (Design_Unit, Loc);
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Load_Design_Unit;

   function Load_Primary_Unit
     (Library: Iir_Library_Declaration; Name: Name_Id; Loc : Iir)
      return Iir_Design_Unit
   is
      Design_Unit: Iir_Design_Unit;
   begin
      Design_Unit := Find_Primary_Unit (Library, Name);
      if Design_Unit /= Null_Iir then
         Load_Design_Unit (Design_Unit, Loc);
      end if;
      return Design_Unit;
   end Load_Primary_Unit;

   -- Load an secondary unit and analyse it.
   function Load_Secondary_Unit
     (Primary: Iir_Design_Unit; Name: Name_Id; Loc : Iir)
     return Iir_Design_Unit
   is
      Design_Unit: Iir_Design_Unit;
   begin
      Design_Unit := Find_Secondary_Unit (Primary, Name);
      if Design_Unit /= Null_Iir then
         Load_Design_Unit (Design_Unit, Loc);
      end if;
      return Design_Unit;
   end Load_Secondary_Unit;
end Sem_Lib;
