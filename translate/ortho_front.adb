--  Ortho entry point for translation.
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
with Types; use Types;
with Name_Table;
with Std_Package;
with Back_End;
with Flags;
with Translation;
with Iirs; use Iirs;
with Libraries; use Libraries;
with Sem;
with Errorout; use Errorout;
with GNAT.OS_Lib;
with Canon;
with Disp_Vhdl;
with Bug;
with Trans_Be;
with Options;

package body Ortho_Front is
   --  The action to be performed by the compiler.
   type Action_Type is
     (
      --  Normal mode: compile a design file.
      Action_Compile,

      --  Elaborate a design unit.
      Action_Elaborate,

      --  Analyze files and elaborate unit.
      Action_Anaelab,

      --  Generate code for std.package.
      Action_Compile_Std_Package
      );
   Action : Action_Type := Action_Compile;

   --  Name of the entity to elaborate.
   Elab_Entity : String_Acc;
   --  Name of the architecture to elaborate.
   Elab_Architecture : String_Acc;
   --  Filename for the list of files to link.
   Elab_Filelist : String_Acc;

   Flag_Expect_Failure : Boolean;

   type Id_Link;
   type Id_Link_Acc is access Id_Link;
   type Id_Link is record
      Id : Name_Id;
      Link : Id_Link_Acc;
   end record;
   Anaelab_Files : Id_Link_Acc := null;
   Anaelab_Files_Last : Id_Link_Acc := null;

   procedure Init is
   begin
      -- Initialize.
      Trans_Be.Register_Translation_Back_End;

      Options.Initialize;

      Elab_Filelist := null;
      Elab_Entity := null;
      Elab_Architecture := null;
      Flag_Expect_Failure := False;
   end Init;

   function Decode_Elab_Option (Arg : String_Acc) return Natural
   is
   begin
      Elab_Architecture := null;
      --  Entity (+ architecture) to elaborate
      if Arg = null then
         Error_Msg_Option
           ("entity or configuration name required after --elab");
         return 0;
      end if;
      if Arg (Arg.all'Last) = ')' then
         --  Name is ENTITY(ARCH).
         --  Split.
         declare
            P : Natural;
            Len : Natural;
            Is_Ext : Boolean;
         begin
            P := Arg.all'Last - 1;
            Len := P - Arg.all'First + 1;
            --  Must be at least 'e(a)'.
            if Len < 4 then
               Error_Msg_Option ("ill-formed name after --elab");
               return 0;
            end if;
            --  Handle extended name.
            if Arg (P) = '\' then
               P := P - 1;
               Is_Ext := True;
            else
               Is_Ext := False;
            end if;
            loop
               if P = Arg.all'First then
                  Error_Msg_Option ("ill-formed name after --elab");
                  return 0;
               end if;
               exit when Arg (P) = '(' and Is_Ext = False;
               if Arg (P) = '\' then
                  if Arg (P - 1) = '\' then
                     P := P - 2;
                  elsif Arg (P - 1) = '(' then
                     P := P - 1;
                     exit;
                  else
                     Error_Msg_Option ("ill-formed name after --elab");
                     return 0;
                  end if;
               else
                  P := P - 1;
               end if;
            end loop;
            Elab_Architecture := new String'(Arg (P + 1 .. Arg'Last - 1));
            Elab_Entity := new String'(Arg (Arg'First .. P - 1));
         end;
      else
         Elab_Entity := new String'(Arg.all);
         Elab_Architecture := new String'("");
      end if;
      return 2;
   end Decode_Elab_Option;

   function Decode_Option (Opt : String_Acc; Arg: String_Acc) return Natural
   is
   begin
      if Opt.all = "--compile-standard" then
         Action := Action_Compile_Std_Package;
         Flags.Bootstrap := True;
         return 1;
      elsif Opt.all = "--elab" then
         if Action /= Action_Compile then
            Error_Msg_Option ("several --elab options");
            return 0;
         end if;
         Action := Action_Elaborate;
         return Decode_Elab_Option (Arg);
      elsif Opt.all = "--anaelab" then
         if Action /= Action_Compile then
            Error_Msg_Option ("several --anaelab options");
            return 0;
         end if;
         Action := Action_Anaelab;
         return Decode_Elab_Option (Arg);
      elsif Opt'Length > 14
        and then Opt (Opt'First .. Opt'First + 13) = "--ghdl-source="
      then
         if Action /= Action_Anaelab then
            Error_Msg_Option
              ("--ghdl-source option allowed only after --anaelab options");
            return 0;
         end if;
         if Arg /= null then
            Error_Msg_Option ("no argument allowed after --ghdl-source");
            return 0;
         end if;
         declare
            L : Id_Link_Acc;
         begin
            L := new Id_Link'(Id => Name_Table.Get_Identifier
                                (Opt (Opt'First + 14 .. Opt'Last)),
                              Link => null);
            if Anaelab_Files = null then
               Anaelab_Files := L;
            else
               Anaelab_Files_Last.Link := L;
            end if;
            Anaelab_Files_Last := L;
         end;
         return 2;
      elsif Opt.all = "-l" then
         if Arg = null then
            Error_Msg_Option ("filename required after -l");
         end if;
         if Elab_Filelist /= null then
            Error_Msg_Option ("several -l options");
         else
            Elab_Filelist := new String'(Arg.all);
         end if;
         return 2;
      elsif Opt.all = "--help" then
         Options.Disp_Options_Help;
         return 1;
      elsif Opt.all = "--expect-failure" then
         Flag_Expect_Failure := True;
         return 1;
      elsif Opt'Length > 7 and then Opt (1 .. 7) = "--ghdl-" then
         if Options.Parse_Option (Opt (7 .. Opt'Last)) then
            return 1;
         else
            return 0;
         end if;
      elsif Options.Parse_Option (Opt.all) then
         return 1;
      else
         return 0;
      end if;
   end Decode_Option;


   --  Lighter version of libraries.is_obselete, since DESIGN_UNIT must be in
   --  the currently analyzed design file.
   function Is_Obsolete (Design_Unit : Iir_Design_Unit)
     return Boolean
   is
      List : Iir_List;
      El : Iir;
   begin
      if Get_Date (Design_Unit) = Date_Obsolete then
         return True;
      end if;
      List := Get_Dependence_List (Design_Unit);
      if Is_Null_List (List) then
         return False;
      end if;
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when Is_Null (El);
         --  FIXME: there may be entity_aspect_entity...
         if Get_Kind (El) = Iir_Kind_Design_Unit
           and then Get_Date (El) = Date_Obsolete
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Obsolete;

   Nbr_Parse : Natural := 0;

   function Parse (Filename : String_Acc) return Boolean
   is
      Res : Iir_Design_File;
      New_Design_File : Iir_Design_File;
      Design : Iir_Design_Unit;
      Next_Design : Iir_Design_Unit;

      --  The vhdl filename to compile.
      Vhdl_File : Name_Id;
   begin
      if Nbr_Parse = 0 then
         --  Initialize only once...
         Libraries.Load_Std_Library;

         -- Here, time_base can be set.
         Translation.Initialize;
         Canon.Canon_Flag_Add_Labels := True;

         if Flags.List_All and then Flags.List_Annotate then
            Disp_Vhdl.Disp_Vhdl (Std_Package.Std_Standard_Unit);
         end if;

         if Action = Action_Anaelab and then Anaelab_Files /= null
         then
            Libraries.Load_Work_Library (True);
         else
            Libraries.Load_Work_Library (False);
         end if;
      end if;
      Nbr_Parse := Nbr_Parse + 1;

      case Action is
         when Action_Elaborate =>
            Flags.Flag_Elaborate := True;
            Flags.Flag_Only_Elab_Warnings := True;
            Translation.Chap12.Elaborate
              (Elab_Entity.all, Elab_Architecture.all,
               Elab_Filelist.all, False);

            if Errorout.Nbr_Errors > 0 then
               --  This may happen (bad entity for example).
               raise Compilation_Error;
            end if;
         when Action_Anaelab =>
            --  Parse files.
            if Anaelab_Files = null then
               Flags.Flag_Elaborate_With_Outdated := False;
            else
               Flags.Flag_Elaborate_With_Outdated := True;
               declare
                  L : Id_Link_Acc;
               begin
                  L := Anaelab_Files;
                  while L /= null loop
                     Res := Libraries.Load_File (L.Id);
                     if Errorout.Nbr_Errors > 0 then
                        raise Compilation_Error;
                     end if;

                     --  Put units into library.
                     Design := Get_First_Design_Unit (Res);
                     while not Is_Null (Design) loop
                        Next_Design := Get_Chain (Design);
                        Set_Chain (Design, Null_Iir);
                        Libraries.Add_Design_Unit_Into_Library (Design);
                        Design := Next_Design;
                     end loop;
                     L := L.Link;
                  end loop;
               end;
            end if;

            Flags.Flag_Elaborate := True;
            Flags.Flag_Only_Elab_Warnings := False;
            Translation.Chap12.Elaborate
              (Elab_Entity.all, Elab_Architecture.all, "", True);

            if Errorout.Nbr_Errors > 0 then
               --  This may happen (bad entity for example).
               raise Compilation_Error;
            end if;
         when Action_Compile_Std_Package =>
            if Filename /= null then
               Error_Msg_Option
                 ("--compile-standard is not compatible with a filename");
               return False;
            end if;
            Translation.Translate_Standard (True);

         when Action_Compile =>
            if Filename = null then
               Error_Msg_Option ("no input file");
               return False;
            end if;
            if Nbr_Parse > 1 then
               Error_Msg_Option ("can compile only one file (file """ &
                                 Filename.all & """ ignored)");
               return False;
            end if;
            Vhdl_File := Name_Table.Get_Identifier (Filename.all);

            Translation.Translate_Standard (False);

            Flags.Flag_Elaborate := False;
            Res := Libraries.Load_File (Vhdl_File);
            if Errorout.Nbr_Errors > 0 then
               raise Compilation_Error;
            end if;

            -- Semantize all design units.
            --  FIXME: outdate the design file?
            New_Design_File := Null_Iir;
            Design := Get_First_Design_Unit (Res);
            while not Is_Null (Design) loop
               -- Sem, canon, annotate a design unit.
               Back_End.Finish_Compilation (Design, True);

               Next_Design := Get_Chain (Design);
               if Errorout.Nbr_Errors = 0 then
                  Set_Chain (Design, Null_Iir);
                  Libraries.Add_Design_Unit_Into_Library (Design);
                  New_Design_File := Get_Design_File (Design);
               end if;

               Design := Next_Design;
            end loop;

            if Errorout.Nbr_Errors > 0 then
               raise Compilation_Error;
            end if;

            --  Do late analysis checks.
            Design := Get_First_Design_Unit (New_Design_File);
            while not Is_Null (Design) loop
               Sem.Sem_Analysis_Checks_List
                 (Design, Flags.Warn_Delayed_Checks);
               Design := Get_Chain (Design);
            end loop;

            --  Compile only now.
            if not Is_Null (New_Design_File) then
               --  Note: the order of design unit is kept.
               Design := Get_First_Design_Unit (New_Design_File);
               while not Is_Null (Design) loop
                  if not Is_Obsolete (Design) then

                     if Get_Kind (Get_Library_Unit (Design))
                       = Iir_Kind_Configuration_Declaration
                     then
                        --  Defer code generation of configuration declaration.
                        --  (default binding may change between analysis and
                        --   elaboration).
                        Translation.Translate (Design, False);
                     else
                        Translation.Translate (Design, True);
                     end if;

                     if Errorout.Nbr_Errors > 0 then
                        --  This can happen (foreign attribute).
                        raise Compilation_Error;
                     end if;
                  end if;

                  Design := Get_Chain (Design);
               end loop;
            end if;

            -- Save the working library.
            Libraries.Save_Work_Library;
      end case;
      if Flag_Expect_Failure then
         return False;
      else
         return True;
      end if;
   exception
      --when File_Error =>
      --   Error_Msg_Option ("cannot open file '" & Filename.all & "'");
      --   return False;
      when Compilation_Error
        | Parse_Error
        | Elaboration_Error =>
         if Flag_Expect_Failure then
            --  Very brutal...
            GNAT.OS_Lib.OS_Exit (0);
         end if;
         return False;
      when Option_Error =>
         return False;
      when E: others =>
         Bug.Disp_Bug_Box (E);
         raise;
   end Parse;
end Ortho_Front;
