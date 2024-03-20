--  Iir to ortho translator.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Back_End;
with Ortho_Nodes;

package Translation is
   --  Initialize the package: create internal nodes.
   procedure Initialize;

   --  Translate (generate code) for library unit LIB_UNIT.
   --  If MAIN is true, the unit is really the unit being compiled (not an
   --  external unit).  Code shouldn't be generated for external units.
   procedure Translate (Lib_Unit : Iir; Main : Boolean);

   --  Translate std.standard.
   procedure Translate_Standard (Main : Boolean);

   --  Get the ortho node for subprogram declaration DECL.
   function Get_Ortho_Decl (Subprg : Iir) return Ortho_Nodes.O_Dnode;

   --  Get the internal _RESOLV function for FUNC.
   function Get_Resolv_Ortho_Decl (Func : Iir) return Ortho_Nodes.O_Dnode;

   procedure Finalize;

   procedure Gen_Filename (Design_File : Iir);

   --  Generate elaboration code for CONFIG.  Also use units from Configure
   --  package.
   procedure Elaborate (Config : Iir; Whole : Boolean);

   --  If set, generate Run-Time Information nodes.
   Flag_Rti : Boolean := True;

   --  If set, do not generate code for unused subprograms.
   --  Be careful: unless you are in whole compilation mode, this
   --  flag shouldn't be set for packages and entities.
   Flag_Discard_Unused : Boolean := False;

   --  If set, do not generate code for unused implicit subprograms.
   Flag_Discard_Unused_Implicit : Boolean := False;

   --  If set, generate statement body not marked used are discarded.
   --  (can only be used with pre-elaboration).
   Flag_Discard_Unused_Generate : Boolean := False;

   --  If set, dump drivers per process during compilation.
   Flag_Dump_Drivers : Boolean := False;

   --  If set, try to create direct drivers.
   Flag_Direct_Drivers : Boolean := True;

   --  If set, checks ranges (subtype ranges).
   Flag_Range_Checks : Boolean := True;

   --  If set, checks indexes (arrays index and slice).
   Flag_Index_Checks : Boolean := True;

   --  If set, do not create identifiers (for in memory compilation).
   Flag_Discard_Identifiers : Boolean := False;

   --  If true, do not create nested subprograms.
   --  This flag is forced during initialization if the code generated doesn't
   --  support nested subprograms.
   Flag_Unnest_Subprograms : Boolean := False;

   --  If > 0, emit a call for large dynamic allocation on the stack.  Large
   --  defined by the value.
   Flag_Check_Stack_Allocation : Natural := 32 * 1024;

   --  If true, generate subprograms for design elaboration.
   Flag_Elaboration : Boolean := True;

   procedure Register_Translation_Back_End;

   --  If not null, this procedure is called when a foreign subprogram is
   --  created.
   type Foreign_Hook_Access is access
     procedure (Decl : Iir;
                Info : Vhdl.Back_End.Foreign_Info_Type;
                Ortho : Ortho_Nodes.O_Dnode);
   Foreign_Hook : Foreign_Hook_Access := null;
end Translation;
