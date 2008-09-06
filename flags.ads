--  Global flags.
--  Copyright (C) 2002, 2003, 2004, 2005, 2008 Tristan Gingold
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

--  All the variables declared in this package are set by Parse_Option function
--  and can by read as soon as the command line is parsed.
--
--  Since the names are not prefixed, this package is expected to be with'ed
--  but not to be use'd.

package Flags is
   --  List of vhdl standards.
   --  VHDL_93c is vhdl_93 with backward compatibility with 87 (file).
   type Vhdl_Std_Type is
     (Vhdl_87, Vhdl_93c, Vhdl_93, Vhdl_00, Vhdl_02, Vhdl_08);

   -- Standard accepted.
   Vhdl_Std: Vhdl_Std_Type := Vhdl_93c;

   --  Some flags (such as vhdl version) must be the same for every design
   --  units of a hierarchy.
   --  The Flag_String is a signature of all these flags.
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

   --  --dall: makes -dX options to apply to all files.
   Dump_All: Boolean := False;

   --  -dstats: disp statistics.
   Dump_Stats : Boolean := False;

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

   --  If set to true, it means that analyze is done for elaboration.
   --  The purpose is to avoid spurious warning "will be checked
   --  at elaboration"
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

   -- --time-resolution=X
   -- Where X corresponds to:
   -- fs => 'f'
   -- ps => 'p'
   -- ns => 'n'
   -- us => 'u'
   -- ms => 'm'
   -- sec => 's'
   -- min => 'M'
   -- hr => 'h'
   Time_Resolution: Character := 'f';

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

   -- --warn-undriven
   --Warn_Undriven : Boolean := False;

   --  --warn-default-binding
   --  Should emit a warning when there is no default binding for a component
   --  instantiation.
   Warn_Default_Binding : Boolean := False;

   --  --warn-binding
   --  Emit a warning at elaboration for unbound component.
   Warn_Binding : Boolean := True;

   --  --warn-reserved
   --  Emit a warning when a vhdl93 reserved word is used as a
   --  vhdl87 identifier.
   Warn_Reserved_Word : Boolean := False;

   --  --warn-library
   --  Emit a warning when a design unit redefines another design unit.
   Warn_Library : Boolean := False;

   --  --warn-vital-generic
   --  Emit a warning when a generic of a vital entity is not a vital name.
   Warn_Vital_Generic : Boolean := True;

   --  --warn-delayed-checks
   --  Emit warnings about delayed checks (checks performed at elaboration
   --   time).
   Warn_Delayed_Checks : Boolean := False;

   --  --warn-body
   --  Emit a warning when a package body is not required but is analyzed.
   Warn_Body : Boolean := True;

   --  --warn-specs
   --  Emit a warning when an all/others specification does not apply, because
   --  there is no such named entities.
   Warn_Specs : Boolean := True;

   --  --warn-unused
   --  Emit a warning when a declaration is never used.
   --  FIXME: currently only subprograms are handled.
   Warn_Unused : Boolean := True;

   --  --warn-error
   --  Turns warnings into errors.
   Warn_Error : Boolean := False;
end Flags;
