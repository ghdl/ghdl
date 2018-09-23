--  Command line options.
--  Copyright (C) 2008 Tristan Gingold
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
with Name_Table;
with Errorout; use Errorout;
with Libraries;
with Std_Names;
with PSL.Nodes;
with PSL.Dump_Tree;
with Disp_Tree;
with Scanner;
with Back_End; use Back_End;
with Flags; use Flags;
with Files_Map;

package body Options is
   procedure Initialize is
   begin
      Std_Names.Std_Names_Initialize;
      Libraries.Init_Paths;
      PSL.Nodes.Init;
      PSL.Dump_Tree.Dump_Hdl_Node := Disp_Tree.Disp_Tree_For_Psl'Access;
   end Initialize;

   function Option_Warning (Opt: String; Val : Boolean) return Boolean is
   begin
      --  Handle -Werror.
      if Opt = "error" then
         Warn_Error := Val;
         return True;
      end if;

      --  Normal warnings.
      for I in Msgid_Warnings loop
         if Warning_Image (I) = Opt then
            Enable_Warning (I, Val);
            return True;
         end if;
      end loop;

      --  -Wreserved is an alias for -Wreserved-word.
      if Opt = "reserved" then
         Enable_Warning (Warnid_Reserved_Word, Val);
         return True;
      end if;

      --  Unknown warning.
      return False;
   end Option_Warning;

   function Parse_Option (Option : String) return Boolean
   is
      subtype Option_String is String (1 .. Option'Length);
      Opt : Option_String renames Option;
   begin
      if Opt'Last > 5 and then Opt (1 .. 6) = "--std=" then
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
            else
               return False;
            end if;
         elsif Opt'Length = 9 and then Opt (7 .. 9) = "93c" then
            Vhdl_Std := Vhdl_93c;
         else
            return False;
         end if;
      elsif Opt'Length = 5 and then Opt (1 .. 5) = "--ams" then
         AMS_Vhdl := True;
      elsif Opt'Length >= 2 and then Opt (1 .. 2) = "-P" then
         if Opt'Last = 2 then
            Error_Msg_Option ("missing directory after -P");
            return True;
         end if;
         if Opt (3) = '=' then
            if Opt'Last = 3 then
               Error_Msg_Option ("missing directory after -P=");
               return True;
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
         declare
            use Name_Table;
            Name : String (1 .. Opt'Last - 8 + 1);
         begin
            Name := Opt (8 .. Opt'Last);
            Scanner.Convert_Identifier (Name);
            Libraries.Work_Library_Name := Get_Identifier (Name);
         end;
      elsif Opt = "-C" or else Opt = "--mb-comments" then
         Mb_Comment := True;
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
               return True;
            end if;
            Tab_Stop := V;
         exception
            when Constraint_Error =>
               Error_Msg_Option ("numeric value expected after -ftabstop=");
               return True;
         end;
      elsif Opt = "--bootstrap" then
         Bootstrap := True;
      elsif Opt = "-fexplicit" then
         Flag_Explicit := True;
      elsif Opt = "-frelaxed-rules" then
         Flag_Relaxed_Rules := True;
      elsif Opt = "--syn-binding" then
         Flag_Syn_Binding := True;
      elsif Opt = "--no-vital-checks" then
         Flag_Vital_Checks := False;
      elsif Opt = "--vital-checks" then
         Flag_Vital_Checks := True;
      elsif Opt = "-fpsl" then
         Scanner.Flag_Psl_Comment := True;
         Scanner.Flag_Comment_Keyword := True;
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
      elsif Back_End.Parse_Option /= null
        and then Back_End.Parse_Option.all (Opt)
      then
         null;
      else
         return False;
      end if;
      return True;
   end Parse_Option;

   -- Disp help about these options.
   procedure Disp_Options_Help
   is
      procedure P (S : String) renames Ada.Text_IO.Put_Line;
   begin
      P ("Main options:");
      P ("  --work=LIB         use LIB as work library");
      P ("  --workdir=DIR      use DIR for the file library");
      P ("  -PPATH             add PATH in the library path list");
      P ("  --std=87/93/00/02/08  select vhdl 87/93/00/02/08 standard");
      P ("  --std=93c          select vhdl 93 standard and allow 87 syntax");
      P ("  --[no-]vital-checks  do [not] check VITAL restrictions");
      P ("Warnings:");
--    P ("  --warn-undriven    disp undriven signals");
      P ("  -Wbinding          warns for component not bound");
      P ("  -Wreserved         warns use of 93 reserved words in vhdl87");
      P ("  -Wlibrary          warns for redefinition of a design unit");
      P ("  -Wvital-generic    warns of non-vital generic names");
      P ("  -Wdelayed-checks   warns for checks performed at elaboration");
      P ("  -Wbody             warns for not necessary package body");
      P ("  -Wspecs            warns if a all/others spec does not apply");
      P ("  -Wunused           warns if a subprogram is never used");
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
      if Back_End.Disp_Option /= null then
         Back_End.Disp_Option.all;
      end if;
   end Disp_Options_Help;

end Options;
