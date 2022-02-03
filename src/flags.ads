--  Global flags.
--  Copyright (C) 2002, 2003, 2004, 2005, 2008 Tristan Gingold
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

--  All the variables declared in this package are set by Parse_Option function
--  and can by read as soon as the command line is parsed.
--
--  Since the names are not prefixed, this package is expected to be with'ed
--  but not to be use'd.

package Flags is
   --  List of vhdl standards.
   --  VHDL_93c is vhdl_93 with backward compatibility with 87 (file).
   type Vhdl_Std_Type is
     (Vhdl_87, Vhdl_93, Vhdl_00, Vhdl_02, Vhdl_08, Vhdl_19);

   --  Standard accepted.
   Vhdl_Std: Vhdl_Std_Type := Vhdl_93;

   --  Enable AMS-VHDL extensions.
   AMS_Vhdl : Boolean := False;

   --  Some flags (such as vhdl version) must be the same for every design
   --  units of a hierarchy.
   --  The Flag_String is a signature of all these flags.
   --  Note: Flag_String (5) (time resolution) is directly overwritten in
   --   ghdlrun.
   Flag_String : String (1 .. 5);
   procedure Create_Flag_String;

   -- If set, a multi-bytes sequence can appear in a comment, ie, all
   -- characters except VT, CR, LF and FF are allowed in a comment.
   -- Set by -C and --mb-comments
   Mb_Comment: Boolean := False;

   -- If set, relax rules about std library: working library can be std.
   Bootstrap : Boolean := False;

   --  Options -dX
   --  -dp: disp tree after parsing
   Dump_Parse: Boolean := False;

   --  -ds: disp tree after semantic
   Dump_Sem: Boolean := False;

   --  -dc: disp tree after canon
   Dump_Canon : Boolean := False;

   --  -da: disp tree after annotation
   Dump_Annotate: Boolean := False;

   --  -do: don't dump origin expression but evaluated expressions.
   Dump_Origin_Flag : Boolean := True;

   --  --dall: makes -dX options to apply to all files.
   Dump_All: Boolean := False;

   --  -dstats: disp statistics.
   Dump_Stats : Boolean := False;

   --  If not 0, do internal consistency and leaks check on the AST after
   --  analysis.
   Check_Ast_Level : Natural := 0;

   -- -lX options: list tree as a vhdl file.

   -- --lall option: makes -lX options to apply to all files
   List_All: Boolean := False;

   -- -lv: list verbose
   List_Verbose: Boolean := False;

   -- -ls: list tree after semantic.
   List_Sem: Boolean := False;

   -- -lc: list tree after canon.
   List_Canon: Boolean := False;

   -- -la: list tree after back-end annotation.
   List_Annotate: Boolean := False;

   -- -v: disp phase of compilation.
   Verbose : Boolean := False;

   --  If set to true, the parser builds extended locations (defined in
   --  package elocations).  This saves possibly many locations per node, so
   --  it uses more memory.  Useful when a tool (like a style checker) wants
   --  to know the precise layout.  Not used to report errors.
   Flag_Elocations : Boolean := False;

   --  If set to true, it means that analyze is done for elaboration.
   --  The purpose is to avoid spurious warning "will be checked
   --  at elaboration".  It will also create default binding.
   Flag_Elaborate : Boolean := False;

   --  If set, a default aspect entity aspect might be an outdated unit.
   --  Used by ghdldrv.
   Flag_Elaborate_With_Outdated : Boolean := False;

   --  Do not display parse and sem warnings.  Used during elaboration.
   Flag_Only_Elab_Warnings : Boolean := False;

   --  If set, explicit subprogram declarations take precedence over
   --  implicit declarations, even through use clauses.
   Flag_Explicit : Boolean := False;

   --  If set, use 'L.C' rule from VHDL02 to do default component binding.
   Flag_Syn_Binding : Boolean := False;

   --  If set, performs VITAL checks.
   Flag_Vital_Checks : Boolean := True;

   --  Set if analysis is done even after parsing errors.  The analysis code
   --  that handles and tolerates incorrect parse tree should check that this
   --  flag is set.
   Flag_Force_Analysis : Boolean := False;

   --  Integer and time types can be either 32 bits or 64 bits values.
   --  The default is 32 bits for Integer and 64 bits for Time.
   --  Be very careful: if you don't use the default sizes, you may have to
   --  change other parts of your systems (such as GRT).
   Flag_Integer_64 : Boolean := False;
   Flag_Time_64 : Boolean := True;

   --  If set, generate cross-references during sem.
   Flag_Xref : Boolean := False;

   --  If set, all the design units are analyzed in whole to do the simulation.
   Flag_Whole_Analyze : Boolean := False;

   --  If true, relax some rules:
   --  * the scope of an object declaration names start after the declaration,
   --    so that it is possible to use the old name in the default expression:
   --    constant x : xtype := x;
   Flag_Relaxed_Rules : Boolean := True;

   --  If true allows vhdl-87 file style.  Enabled with --std=93c
   Flag_Relaxed_Files87 : Boolean := True;

   --  If true, allow to use synopsys packages (std_logic_arith & co).
   Flag_Synopsys : Boolean := False;

   --  If True, disp original source line and a caret indicating the column.
   Flag_Caret_Diagnostics : Boolean := True;

   type On_Off_Auto_Type is (On, Off, Auto);

   --  -fcolor-diagnostics
   --  -fno-color-diagnostics
   --  Enable colors in diagnostic messages.  The default is auto, which turns
   --  on when a terminal is detected on the standard error.
   Flag_Color_Diagnostics : On_Off_Auto_Type := Auto;

   --  -fdiagnostics-show-option
   Flag_Diagnostics_Show_Option : Boolean := True;
end Flags;
