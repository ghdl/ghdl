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
with Ada.Text_IO; use Ada.Text_IO;
with Name_Table;
with Libraries;
with Std_Names;
with PSL.Nodes;
with PSL.Dump_Tree;
with Disp_Tree;
with Scan;
with Back_End; use Back_End;
with Flags; use Flags;

package body Options is
   procedure Initialize is
   begin
      Std_Names.Std_Names_Initialize;
      Libraries.Init_Pathes;
      PSL.Nodes.Init;
      PSL.Dump_Tree.Dump_Hdl_Node := Disp_Tree.Disp_Tree_For_Psl'Access;
   end Initialize;

   function Option_Warning (Opt: String; Val : Boolean) return Boolean is
   begin
--      if Opt = "undriven" then
--         Warn_Undriven := True;
      if Opt = "library" then
         Warn_Library := Val;
      elsif Opt = "default-binding" then
         Warn_Default_Binding := Val;
      elsif Opt = "binding" then
         Warn_Binding := Val;
      elsif Opt = "reserved" then
         Warn_Reserved_Word := Val;
      elsif Opt = "vital-generic" then
         Warn_Vital_Generic := Val;
      elsif Opt = "delayed-checks" then
         Warn_Delayed_Checks := Val;
      elsif Opt = "body" then
         Warn_Body := Val;
      elsif Opt = "specs" then
         Warn_Specs := Val;
      elsif Opt = "unused" then
         Warn_Unused := Val;
      elsif Opt = "error" then
         Warn_Error := Val;
      else
         return False;
      end if;
      return True;
   end Option_Warning;

   function Parse_Option (Opt: String) return Boolean
   is
      Beg: constant Integer := Opt'First;
   begin
      if Opt'Length > 5 and then Opt (Beg .. Beg + 5) = "--std=" then
         if Opt'Length = 8 then
            if Opt (Beg + 6 .. Beg + 7) = "87" then
               Vhdl_Std := Vhdl_87;
            elsif Opt (Beg + 6 .. Beg + 7) = "93" then
               Vhdl_Std := Vhdl_93;
            elsif Opt (Beg + 6 .. Beg + 7) = "00" then
               Vhdl_Std := Vhdl_00;
            elsif Opt (Beg + 6 .. Beg + 7) = "02" then
               Vhdl_Std := Vhdl_02;
            elsif Opt (Beg + 6 .. Beg + 7) = "08" then
               Vhdl_Std := Vhdl_08;
            else
               return False;
            end if;
         elsif Opt'Length = 9 and then Opt (Beg + 6 .. Beg + 8) = "93c" then
            Vhdl_Std := Vhdl_93c;
         else
            return False;
         end if;
      elsif Opt'Length > 2 and then Opt (Beg .. Beg + 1) = "-P" then
         Libraries.Add_Library_Path (Opt (Beg + 2 .. Opt'Last));
      elsif Opt'Length > 10 and then Opt (Beg .. Beg + 9) = "--workdir=" then
         Libraries.Set_Work_Library_Path (Opt (Beg + 10 .. Opt'Last));
      elsif Opt'Length > 10 and then Opt (Beg .. Beg + 9) = "--warn-no-" then
         return Option_Warning (Opt (Beg + 10 .. Opt'Last), False);
      elsif Opt'Length > 7 and then Opt (Beg .. Beg + 6) = "--warn-" then
         return Option_Warning (Opt (Beg + 7 .. Opt'Last), True);
      elsif Opt'Length > 7 and then Opt (Beg .. Beg + 6) = "--work=" then
         declare
            use Name_Table;
         begin
            Name_Length := Opt'Last - (Beg + 7) + 1;
            Name_Buffer (1 .. Name_Length) := Opt (Beg + 7 .. Opt'Last);
            Scan.Convert_Identifier;
            Libraries.Work_Library_Name := Get_Identifier;
         end;
      elsif Opt = "-C" or else Opt = "--mb-comments" then
         Mb_Comment := True;
      elsif Opt = "--bootstrap" then
         Bootstrap := True;
      elsif Opt = "-fexplicit" then
         Flag_Explicit := True;
      elsif Opt = "--syn-binding" then
         Flag_Syn_Binding := True;
      elsif Opt = "--no-vital-checks" then
         Flag_Vital_Checks := False;
      elsif Opt = "--vital-checks" then
         Flag_Vital_Checks := True;
      elsif Opt = "-fpsl" then
         Scan.Flag_Psl_Comment := True;
         Scan.Flag_Comment_Keyword := True;
      elsif Opt = "-dp" then
         Dump_Parse := True;
      elsif Opt = "-ds" then
         Dump_Sem := True;
      elsif Opt = "-dc" then
         Dump_Canon := True;
      elsif Opt = "-da" then
         Dump_Annotate := True;
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
--       elsif Opt'Length > 17
--         and then Opt (Beg .. Beg + 17) = "--time-resolution="
--       then
--          Beg := Beg + 18;
--          if Opt (Beg .. Beg + 1) = "fs" then
--             Time_Resolution := 'f';
--          elsif Opt (Beg .. Beg + 1) = "ps" then
--             Time_Resolution := 'p';
--          elsif Opt (Beg .. Beg + 1) = "ns" then
--             Time_Resolution := 'n';
--          elsif Opt (Beg .. Beg + 1) = "us" then
--             Time_Resolution := 'u';
--          elsif Opt (Beg .. Beg + 1) = "ms" then
--             Time_Resolution := 'm';
--          elsif Opt (Beg .. Beg + 2) = "sec" then
--             Time_Resolution := 's';
--          elsif Opt (Beg .. Beg + 2) = "min" then
--             Time_Resolution := 'M';
--          elsif Opt (Beg .. Beg + 1) = "hr" then
--             Time_Resolution := 'h';
--          else
--             return False;
--          end if;
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
      procedure P (S : String) renames Put_Line;
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
      P ("  --warn-binding     warns for component not bound");
      P ("  --warn-reserved    warns use of 93 reserved words in vhdl87");
      P ("  --warn-library     warns for redefinition of a design unit");
      P ("  --warn-vital-generic  warns of non-vital generic names");
      P ("  --warn-delayed-checks warns for checks performed at elaboration");
      P ("  --warn-body        warns for not necessary package body");
      P ("  --warn-specs       warns if a all/others spec does not apply");
      P ("  --warn-unused      warns if a subprogram is never used");
      P ("  --warn-error       turns warnings into errors");
--    P ("Simulation option:");
--    P ("  --time-resolution=UNIT   set the resolution of type time");
--    P ("            UNIT can be fs, ps, ns, us, ms, sec, min or hr");
--    P ("  --assert-level=LEVEL     set the level which stop the");
--    P ("           simulation.  LEVEL is note, warning, error,");
--    P ("           failure or none");
      P ("Extensions:");
      P ("  -fexplicit         give priority to explicitly declared operator");
      P ("  -C  --mb-comments  allow multi-bytes chars in a comment");
      P ("  --bootstrap        allow --work=std");
      P ("  --syn-binding      use synthesis default binding rule");
      P ("  -fpsl              parse psl in comments");
      P ("Compilation list:");
      P ("  -ls                after semantics");
      P ("  -lc                after canon");
      P ("  -la                after annotation");
      P ("  --lall             -lX options apply to all files");
      P ("  -lv                verbose list");
      P ("  -v                 disp compilation stages");
      P ("Compilation dump:");
      P ("  -dp                dump tree after parsing");
      P ("  -ds                dump tree after semantics");
      P ("  -da                dump tree after annotate");
      P ("  --dall             -dX options apply to all files");
      if Back_End.Disp_Option /= null then
         Back_End.Disp_Option.all;
      end if;
   end Disp_Options_Help;

end Options;
