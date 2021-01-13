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
with Ortho_Nodes;

package Translation is
   --  Initialize the package: create internal nodes.
   procedure Initialize;

   --  Translate (generate code) for design unit UNIT.
   --  If MAIN is true, the unit is really the unit being compiled (not an
   --  external unit).  Code shouldn't be generated for external units.
   procedure Translate (Unit : Iir_Design_Unit; Main : Boolean);

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

   type Foreign_Kind_Type is (Foreign_Unknown,
                              Foreign_Vhpidirect,
                              Foreign_Intrinsic);

   type Foreign_Info_Type (Kind : Foreign_Kind_Type := Foreign_Unknown)
   is record
      case Kind is
         when Foreign_Unknown =>
            null;
         when Foreign_Vhpidirect =>
            Lib_Name : String (1 .. 32);
            Lib_Len : Natural;
            Subprg_Name : String (1 .. 64);
            Subprg_Len : Natural;
         when Foreign_Intrinsic =>
            null;
      end case;
   end record;

   Foreign_Bad : constant Foreign_Info_Type := (Kind => Foreign_Unknown);

   --  Return a foreign_info for DECL.
   --  Can generate error messages, if the attribute expression is ill-formed.
   --  If EXTRACT_NAME is set, internal fields of foreign_info are set.
   --  Otherwise, only KIND discriminent is set.
   --  EXTRACT_NAME should be set only inside translation itself, since the
   --  name can be based on the prefix.
   function Translate_Foreign_Id (Decl : Iir) return Foreign_Info_Type;

   --  If not null, this procedure is called when a foreign subprogram is
   --  created.
   type Foreign_Hook_Access is access procedure (Decl : Iir;
                                                 Info : Foreign_Info_Type;
                                                 Ortho : Ortho_Nodes.O_Dnode);
   Foreign_Hook : Foreign_Hook_Access := null;
end Translation;
