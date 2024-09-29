--  Command line options.
--  Copyright (C) 2008 Tristan Gingold
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

with Simple_IO;
with Outputs;
with Errorout; use Errorout;
with Types; use Types;
with Std_Names;
with Name_Table;
with Str_Table;
with Libraries;
with PSL.Nodes;
with PSL.Dump_Tree;
with Flags; use Flags;
with Files_Map;
with File_Comments;

with Vhdl.Nodes;
with Vhdl.Lists;
with Vhdl.Disp_Tree;
with Vhdl.Scanner;
with Vhdl.Parse;
with Vhdl.Errors;
with Vhdl.Back_End; use Vhdl.Back_End;

with Elab.Vhdl_Objtypes;

package body Options is
   procedure Initialize
   is
      Res : Boolean;
   begin
      Name_Table.Initialize;
      Std_Names.Std_Names_Initialize;
      Str_Table.Initialize;
      Vhdl.Lists.Initialize;
      Vhdl.Nodes.Initialize;
      Elab.Vhdl_Objtypes.Initialize;
      Files_Map.Initialize;
      File_Comments.Initialize;
      Libraries.Initialize;
      PSL.Nodes.Init (Libraries.Library_Location);
      PSL.Dump_Tree.Dump_Hdl_Node := Vhdl.Disp_Tree.Disp_Tree_For_Psl'Access;
      Vhdl.Errors.Initialize;

      --  Just in case of debug.
      Res := Outputs.Open_File (null);
      pragma Assert (Res);
   end Initialize;

   procedure Finalize is
   begin
      Name_Table.Finalize;
      Str_Table.Finalize;
      Vhdl.Lists.Finalize;
      Vhdl.Nodes.Finalize;
      Elab.Vhdl_Objtypes.Finalize;
      Files_Map.Finalize;
      File_Comments.Finalize;
      Libraries.Finalize;
      --  TODO: finalize errors (reset counters, handlers...)
      --  TODO: PSL
      --  TODO: backend
   end Finalize;

   function Option_Warning (Opt: String; Val : Boolean) return Option_State
   is
      Id : Msgid_All_Warnings;
   begin
      --  Handle -Werror and -Wno-error
      if Opt = "error" then
         Warning_Error (Msgid_Warning, Val);
         for I in Msgid_Warnings loop
            Warning_Error (I, Val);
         end loop;
         return Option_Ok;
      end if;

      --  Handle -Werror=xxx and -Wno-error=xxx
      if Opt'Length >= 6
        and then Opt (Opt'First .. Opt'First + 5) = "error="
      then
         Id := Warning_Value (Opt (Opt'First + 6 .. Opt'Last));
         if Id = Msgid_Warning then
            Error_Msg_Option ("unknown warning identifier: "
                                & Opt (Opt'First + 6 .. Opt'Last));
            return Option_Err;
         else
            Enable_Warning (Id, True);
            Warning_Error (Id, Val);
            return Option_Ok;
         end if;
      end if;

      -- Handle -Wall
      if Opt = "all" then
         for I in Msgid_Warnings loop
            Enable_Warning(I, True);
         end loop;
         return Option_Ok;
      end if;

      --  Normal warnings.
      Id := Warning_Value (Opt);
      if Id = Msgid_Warning then
         --  Unknown warning.
         Error_Msg_Option ("unknown warning identifier: " & Opt);
         return Option_Err;
      else
         Enable_Warning (Id, Val);
         return Option_Ok;
      end if;
   end Option_Warning;

   function Parse_Option (Opt : String) return Option_State
   is
      pragma Assert (Opt'First = 1);
   begin
      if Opt'Last > 5 and then Opt (1 .. 6) = "--std=" then
         Flag_Relaxed_Rules := False;
         Flag_Relaxed_Files87 := False;
         if Opt'Length = 8 then
            if Opt (7 .. 8) = "87" then
               Vhdl_Std := Vhdl_87;
            elsif Opt (7 .. 8) = "93" then
               Vhdl_Std := Vhdl_93;
            elsif Opt (7 .. 8) = "00" then
               Vhdl_Std := Vhdl_00;
            elsif Opt (7 .. 8) = "02" then
               Vhdl_Std := Vhdl_02;
            elsif Opt (7 .. 8) = "08" then
               Vhdl_Std := Vhdl_08;
            elsif Opt (7 .. 8) = "19" then
               Vhdl_Std := Vhdl_19;
            else
               Error_Msg_Option ("unknown language standard: " & Opt (7 ..8) &
                                 ". Should be one of: 87, 93, 02, 08, 19");
               return Option_Err;
            end if;
         elsif Opt'Length = 9 and then Opt (7 .. 9) = "93c" then
            Vhdl_Std := Vhdl_93;
            Flag_Relaxed_Rules := True;
            Flag_Relaxed_Files87 := True;
         else
            Error_Msg_Option ("unknown language standard. " &
                              "Should be one of: 87, 93, 02, 08, 19");
            return Option_Err;
         end if;
      elsif Opt'Length = 5 and then Opt (1 .. 5) = "--ams" then
         AMS_Vhdl := True;
      elsif Opt'Length >= 2 and then Opt (1 .. 2) = "-P" then
         if Opt'Last = 2 then
            Error_Msg_Option ("missing directory after -P");
            return Option_Err;
         end if;
         if Opt (3) = '=' then
            if Opt'Last = 3 then
               Error_Msg_Option ("missing directory after -P=");
               return Option_Err;
            end if;
            Libraries.Add_Library_Path (Opt (4 .. Opt'Last));
         else
            Libraries.Add_Library_Path (Opt (3 .. Opt'Last));
         end if;
      elsif Opt'Length > 10 and then Opt (1 .. 10) = "--workdir=" then
         Libraries.Set_Work_Library_Path (Opt (11 .. Opt'Last));
      elsif Opt'Length > 10 and then Opt (1 .. 10) = "--warn-no-" then
         --  Handle --warn-no before -warn-!
         return Option_Warning (Opt (11 .. Opt'Last), False);
      elsif Opt'Length > 7 and then Opt (1 .. 7) = "--warn-" then
         return Option_Warning (Opt (8 .. Opt'Last), True);
      elsif Opt'Length > 5 and then Opt (1 .. 5) = "-Wno-" then
         --  Handle -Wno-xxx before -Wxxx
         return Option_Warning (Opt (6 .. Opt'Last), False);
      elsif Opt'Length > 2 and then Opt (1 .. 2) = "-W" then
         return Option_Warning (Opt (3 .. Opt'Last), True);
      elsif Opt'Length > 7 and then Opt (1 .. 7) = "--work=" then
         Libraries.Work_Library_Name := Libraries.Decode_Work_Option (Opt);
         if Libraries.Work_Library_Name = Null_Identifier then
            return Option_Err;
         end if;
      elsif Opt = "-C" or else Opt = "--mb-comments" then
         Mb_Comment := True;
      elsif Opt = "--force-analysis" then
         Flag_Force_Analysis := True;
         Vhdl.Parse.Flag_Parse_Parenthesis := True;
      elsif Opt = "-fcaret-diagnostics" then
         Flag_Caret_Diagnostics := True;
      elsif Opt = "-fno-caret-diagnostics" then
         Flag_Caret_Diagnostics := False;
      elsif Opt = "-fcolor-diagnostics" then
         Flag_Color_Diagnostics := On;
      elsif Opt = "-fno-color-diagnostics" then
         Flag_Color_Diagnostics := Off;
      elsif Opt = "-fdiagnostics-show-option" then
         Flag_Diagnostics_Show_Option := True;
      elsif Opt = "-fno-diagnostics-show-option" then
         Flag_Diagnostics_Show_Option := False;
      elsif Opt'Length > 10 and then Opt (1 .. 10) = "-ftabstop=" then
         declare
            use Files_Map;
            V : Natural;
         begin
            V := Natural'Value (Opt (11 .. Opt'Last));
            if V not in Tab_Stop_Range then
               Error_Msg_Option ("incorrect value for -ftabstop");
               return Option_Err;
            end if;
            Tab_Stop := V;
         exception
            when Constraint_Error =>
               Error_Msg_Option ("numeric value expected after -ftabstop=");
               return Option_Err;
         end;
      elsif Opt'Length > 13 and then Opt (1 .. 13) = "-fmax-errors=" then
         begin
            Max_Nbr_Errors := Natural'Value (Opt (14 .. Opt'Last));
         exception
            when Constraint_Error =>
               Error_Msg_Option ("numeric value expected after -fmax-errors=");
               return Option_Err;
         end;
      elsif Opt = "--bootstrap" then
         Bootstrap := True;
      elsif Opt = "-fexplicit" then
         Flag_Explicit := True;
      elsif Opt = "-frelaxed-rules" or else Opt = "-frelaxed" then
         Flag_Relaxed_Rules := True;
      elsif Opt = "-fsynopsys" then
         Flag_Synopsys := True;
      elsif Opt = "--syn-binding" then
         Flag_Syn_Binding := True;
      elsif Opt = "--no-vital-checks" then
         Flag_Vital_Checks := False;
      elsif Opt = "--vital-checks" then
         Flag_Vital_Checks := True;
      elsif Opt = "-fpsl" then
         Vhdl.Scanner.Flag_Psl_Comment := True;
         Vhdl.Scanner.Flag_Comment_Keyword := True;
      elsif Opt = "-fpragma-translate" then
         Vhdl.Scanner.Flag_Comment_Keyword := True;
         Vhdl.Scanner.Flag_Pragma_Comment := True;
      elsif Opt = "-dp" then
         Dump_Parse := True;
      elsif Opt = "-ds" then
         Dump_Sem := True;
      elsif Opt = "-dc" then
         Dump_Canon := True;
      elsif Opt = "-da" then
         Dump_Annotate := True;
      elsif Opt = "-do" then
         Dump_Origin_Flag := False;
      elsif Opt = "--dall" then
         Dump_All := True;
      elsif Opt = "-dstats" then
         Dump_Stats := True;
      elsif Opt = "--lall" then
         List_All := True;
      elsif Opt = "-lv" then
         List_Verbose := True;
      elsif Opt = "-ls" then
         List_Sem := True;
      elsif Opt = "-lc" then
         List_Canon := True;
      elsif Opt = "-la" then
         List_Annotate := True;
      elsif Opt = "-v" then
         Verbose := True;
      elsif Opt = "--finteger64" then
         Flag_Integer_64 := True;
      elsif Opt = "--ftime32" then
         Flag_Time_64 := False;
      elsif Vhdl.Back_End.Parse_Option /= null
        and then Vhdl.Back_End.Parse_Option.all (Opt)
      then
         null;
      else
         return Option_Unknown;
      end if;
      return Option_Ok;
   end Parse_Option;

   -- Disp help about these options.
   procedure Disp_Help_Options
   is
      procedure P (S : String) renames Simple_IO.Put_Line;
   begin
      P ("Main options:");
      P ("  --work=LIB         use LIB as work library");
      P ("  --workdir=DIR      use DIR for the file library");
      P ("  -PPATH             add PATH in the library path list");
      P ("  --std=87/93/00/02/08  select vhdl 87/93/00/02/08 standard");
      P ("  --std=93c          select vhdl 93 standard and allow 87 syntax");
      P ("  --[no-]vital-checks  do [not] check VITAL restrictions");
      P ("  -Wx or --warn-x    enable a warning (see help-warnings");
      P ("  -Wall              enables all warnings.");
      P ("  -Werror            turns warnings into errors");
--    P ("Simulation option:");
--    P ("  --assert-level=LEVEL     set the level which stop the");
--    P ("           simulation.  LEVEL is note, warning, error,");
--    P ("           failure or none");
      P ("Extensions:");
      P ("  -fexplicit         give priority to explicitly declared operator");
      P ("  -frelaxed-rules    relax some LRM rules");
      P ("  -C  --mb-comments  allow multi-bytes chars in a comment");
      P ("  --bootstrap        allow --work=std");
      P ("  --syn-binding      use synthesis default binding rule");
      P ("  -fpsl              parse psl in comments");
      P ("Compilation list:");
      P ("  -l[sca]            after semantics, canon or annotation");
      P ("  --lall             -lX options apply to all files");
      P ("  -lv                verbose list");
      P ("  -v                 disp compilation stages");
      P ("Compilation dump:");
      P ("  -d[psa]            dump tree after parse, semantics or annotate");
      P ("  --dall             -dX options apply to all files");
      if Vhdl.Back_End.Disp_Option /= null then
         Vhdl.Back_End.Disp_Option.all;
      end if;
   end Disp_Help_Options;

   procedure Disp_Help_Warnings
   is
      use Simple_IO;
      procedure P (S : String) renames Put;
   begin
      P ("Warnings ('*' means on by default):");
      New_Line;
      for I in Msgid_Warnings loop
         P ("  -W");
         declare
            S : constant String := Warning_Image (I);
         begin
            P (S);
            if Is_Warning_Enabled (I) then
               Put ('*');
            else
               Put (' ');
            end if;
            P ((S'Length .. 18 => ' '));
            Put (' ');
         end;

         case I is
            when Warnid_Library =>
               P ("redefinition of a design unit");
            when Warnid_Deprecated_Option =>
               P ("option is deprecated");
            when Warnid_Unexpected_Option =>
               P ("unexpected place of option in the command line");
            when Warnid_Missing_Xref =>
               P ("cross-referenced not found");
            when Warnid_Default_Binding =>
               P ("no default binding");
            when Warnid_Binding =>
               P ("component not bound");
            when Warnid_Port =>
               P ("invalid port association");
            when Warnid_Reserved_Word =>
               P ("use of 93 reserved words in vhdl87");
            when Warnid_Pragma =>
               P ("incorrect pragma directive");
            when Warnid_Nested_Comment =>
               P ("nested comment");
            when Warnid_Parenthesis =>
               P ("suspicious parenthesis");
            when Warnid_Vital_Generic =>
               P ("non-vital generic names");
            when Warnid_Delayed_Checks =>
               P ("checks performed at elaboration");
            when Warnid_Sensitivity =>
               P ("incomplete sensitivity list");
            when Warnid_Body =>
               P ("unnecessary package body");
            when Warnid_Specs =>
               P ("an all/others spec does not apply");
            when Warnid_Universal =>
               P ("invalid universal integer");
            when Warnid_Port_Bounds =>
               P ("mismatch bounds in port association");
            when Warnid_Runtime_Error =>
               P ("error at runtime for a construct");
            when Warnid_Delta_Cycle =>
               P ("delta cycle in postponed process");
            when Warnid_Missing_Wait =>
               P ("infinite loop in process");
            when Warnid_Shared =>
               P ("shared variable is not of protected type");
            when Warnid_Hide =>
               P ("hidden identifier");
            when Warnid_Unused =>
               P ("subprogram is never used");
            when Warnid_Nowrite =>
               P ("signal not written (in synthesis)");
            when Warnid_Logic_Loop =>
               P ("combinatorial loop (in synthesis)");
            when Warnid_Others =>
               P ("useless 'others' choice");
            when Warnid_Pure =>
               P ("violation of pure rules");
            when Warnid_Analyze_Assert =>
               P ("assertion during analysis");
            when Warnid_Attribute =>
               P ("incorrect attribute");
            when Warnid_Useless =>
               P ("useless construct");
            when Warnid_Missing_Assoc =>
               P ("missing association");
            when Warnid_Open_Assoc =>
               P ("open individual association");
            when Warnid_Conformance =>
               P ("violation of conformance rules");
            when Warnid_Unkept_Attribute =>
               P ("attribute is discarded (in synthesis)");
            when Warnid_Unhandled_Attribute =>
               P ("attribute is not handled (in synthesis)");
            when Warnid_Static =>
               P ("'others' choice is not static");
            when Warnid_Elaboration =>
               P ("warning during elaboration");
               null;
         end case;
         Simple_IO.New_Line;
      end loop;
      Put_Line ("Special warning option:");
      Put_Line ("  -Wall              enables all warnings.");
      Put_Line ("  --warn-xxx         same as -Wxxx");
      Put_Line ("  -Wno-xxx           disable warning xxx");
      Put_Line ("  --warn-no-xxx      same as -Wno-xxx");
      Put_Line ("  -Werror            turns warnings into errors");
      Put_Line ("  -Werror=xxx        turn warning xxx into an error");
      Put_Line ("  -Wno-error=xxx     warning xxx is not an error");
   end Disp_Help_Warnings;
end Options;
