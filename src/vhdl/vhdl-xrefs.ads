--  Cross references.
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
with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Xrefs is
   type Xref_Kind is
     (
      --  Declaration of an identifier.
      Xref_Decl,

      --  Use of a named entity.
      Xref_Ref,

      --  Identifier after the 'end' keyword.
      Xref_End,

      --  Body of a declaration (for package, subprograms or protected type).
      Xref_Body,

      --  A PSL keyword that would be scanned as an identifier
      Xref_Keyword
     );

   --  Initialize the xref table.
   --  Must be called once.
   procedure Init;

   --  Low level xref addition.
   --  An entity at LOC references REF with the KIND way.
   procedure Add_Xref (Loc : Location_Type; Ref : Iir; Kind : Xref_Kind);

   --  Add a declaration of an identifier.
   --  This is somewhat a self-reference.
   procedure Xref_Decl (Decl : Iir);
   pragma Inline (Xref_Decl);

   --  NAME refers to DECL.
   procedure Xref_Ref (Name : Iir; Decl : Iir);
   pragma Inline (Xref_Ref);

   --  BODy refers to SPEC.
   procedure Xref_Body (Bod : Iir; Spec : Iir);
   pragma Inline (Xref_Body);

   --  Just resolved NAME refers to its named entity.
   procedure Xref_Name (Name : Iir);
   pragma Inline (Xref_Name);

   --  LOC is the location of the simple_name after 'end' for DECL.
   procedure Xref_End (Loc : Location_Type; Decl : Iir);
   pragma Inline (Xref_End);

   --  LOC is the location of a PSL keyword.
   procedure Xref_Keyword (Loc : Location_Type);
   pragma Inline (Xref_Keyword);

   --  Sort the xref table by location.  This is required before searching with
   --  Find.
   procedure Sort_By_Location;

   --  Sort the xref table by location of the nodes.
   procedure Sort_By_Node_Location;

   subtype Xref is Natural;

   --  A bad xref.
   --  May be returned by Find.
   Bad_Xref : constant Xref := 0;

   --  First xref.
   --  May be used to size a table.
   First_Xref : constant Xref := 1;

   --  Find a reference by location.
   --  The table must already be sorted with Sort_By_Location.
   --  Returns BAD_REF is does not exist.
   function Find (Loc : Location_Type) return Xref;

   --  End_Xrefs are added by parse and points to the subprogram_body.
   --  This procedure make them points to the subprogram_decl node.
   --  This is done so that every node has a name.
   procedure Fix_End_Xrefs;

   --  Get the last possible xref available.
   --  May be used to size tables.
   function Get_Last_Xref return Xref;

   --  Get the location of N, ie where a name (or operator) appears.
   function Get_Xref_Location (N : Xref) return Location_Type;
   pragma Inline (Get_Xref_Location);

   --  Get the kind of cross-reference.
   function Get_Xref_Kind (N : Xref) return Xref_Kind;
   pragma Inline (Get_Xref_Kind);

   --  Get the node referenced by the name.
   function Get_Xref_Node (N : Xref) return Iir;
   pragma Inline (Get_Xref_Node);
end Vhdl.Xrefs;
