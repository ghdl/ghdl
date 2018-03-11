--  VHDL libraries handling.
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Types; use Types;
with Iirs; use Iirs;
with Std_Names;

package Libraries is
   -- This package defines the library manager.
   -- The purpose of the library manager is to associate library logical names
   -- with host-dependent library.
   --
   -- In this implementation a host-dependent library is a file, whose name
   -- is logical name of the library with the extension '.cf'.  This file
   -- contains the name and the position (filename, line, column and offset)
   -- of all library unit of the library.
   --
   -- The working library WORK can be aliased with a ressource library,
   -- they share the same host-dependenet library whose name is the name
   -- of the ressource library.  This is done by load_work_library.

   --  Location for a command line.
   Command_Line_Location : Location_Type;

   --  A location for library declarations (such as library WORK).
   Library_Location: Location_Type;

   --  Library declaration for the std library.
   --  This is also the first library of the libraries chain.
   Std_Library : Iir_Library_Declaration := Null_Iir;

   --  Library declaration for the work library.
   --  Note: the identifier of the work_library is work_library_name, which
   --  may be different from 'WORK'.
   Work_Library: Iir_Library_Declaration;

   --  Name of the WORK library.
   Work_Library_Name : Name_Id := Std_Names.Name_Work;

   --  Directory of the work library.
   --  Set by default by INIT_PATHS to the local directory.
   Work_Directory : Name_Id;

   --  Local (current) directory.
   Local_Directory : Name_Id;

   --  Correspond to "" (empty identifier).  Used to denote current directory
   --  for library directories.
   Name_Nil : Name_Id;

   --  Chain of obsoleted design units.
   Obsoleted_Design_Units : Iir := Null_Iir;

   --  Initialize library paths table.
   --  Set the local path.
   procedure Init_Paths;

   --  Add PATH in the search path.
   procedure Add_Library_Path (Path : String);

   --  Get the number of path in the search paths.
   function Get_Nbr_Paths return Natural;

   --  Get path N.
   function Get_Path (N : Natural) return Name_Id;

   --  Set PATH as the path of the work library.
   procedure Set_Work_Library_Path (Path : String);

   -- Transform a library identifier into a file name.
   -- Very simple mechanism: just add '-objVV.cf' extension, where VV
   -- is the version.
   function Library_To_File_Name (Library: Iir_Library_Declaration)
                                 return String;

   --  Set the name of the work library, load the work library.
   --  Note: the scanner shouldn't be in use, since this function uses it.
   --  If EMPTY is set, the work library is just created and not loaded.
   procedure Load_Work_Library (Empty : Boolean := False);

   --  Initialize the library manager and load the STD library.
   --  If BUILD_STANDARD is false, the std.standard library is not created.
   procedure Load_Std_Library (Build_Standard : Boolean := True);

   -- Save the work library as a host-dependent library.
   procedure Save_Work_Library;

   --  Start the analyse a file (ie load and parse it).
   --  The file is read from the current directory (unless FILE_NAME is an
   --    absolute path).
   --  Emit an error if the file cannot be opened.
   --  Return NULL_IIR in case of parse error.
   function Load_File (File_Name: Name_Id) return Iir_Design_File;
   function Load_File (File : Source_File_Entry) return Iir_Design_File;

   --  Load, parse, analyze, back-end a design_unit if necessary.
   --  Check Design_Unit is not obsolete.
   --  LOC is the location where the design unit was needed, in case of error.
   procedure Load_Design_Unit (Design_Unit: Iir_Design_Unit; Loc : Iir);

   --  Load and parse DESIGN_UNIT.
   --  Contrary to Load_Design_Unit, the design_unit is not analyzed.
   --  Also, the design_unit must not have been already loaded.
   --  Used almost only by Load_Design_Unit.
   procedure Load_Parse_Design_Unit (Design_Unit: Iir_Design_Unit; Loc : Iir);

   --  Remove the same file as DESIGN_FILE from work library and all of its
   --  units.
   procedure Purge_Design_File (Design_File : Iir_Design_File);

   -- Just return the design_unit for NAME, or NULL if not found.
   function Find_Primary_Unit
     (Library: Iir_Library_Declaration; Name: Name_Id)
     return Iir_Design_Unit;

   -- Load an already analyzed primary unit NAME from library LIBRARY
   -- and compile it.
   -- Return NULL_IIR if not found (ie, NAME does not correspond to a
   --   library unit identifier).
   function Load_Primary_Unit
     (Library: Iir_Library_Declaration; Name: Name_Id; Loc : Iir)
      return Iir_Design_Unit;

   -- Find the secondary unit of PRIMARY.
   -- If PRIMARY is a package declaration, returns the package body,
   -- If PRIMARY is an entity declaration, returns the architecture NAME.
   -- Return NULL_IIR if not found.
   function Find_Secondary_Unit (Primary: Iir_Design_Unit; Name: Name_Id)
      return Iir_Design_Unit;

   -- Load an secondary unit of primary unit PRIMARY and analyse it.
   -- NAME must be set only for an architecture.
   function Load_Secondary_Unit
     (Primary: Iir_Design_Unit; Name: Name_Id; Loc : Iir)
     return Iir_Design_Unit;

   --  Analyze UNIT.
   procedure Finish_Compilation
     (Unit : Iir_Design_Unit; Main : Boolean := False);

   --  Get or create a library from an identifier.
   --  LOC is used only to report errors.
   function Get_Library (Ident : Name_Id; Loc : Location_Type)
                        return Iir_Library_Declaration;

   --  Add or replace an design unit in the work library.
   --  DECL must not have a chain (because it may be modified).
   --
   --  If the design_file of UNIT is not already in the library, a new one
   --  is created.
   --
   --  Units are always appended to the design_file.  Therefore, the order is
   --  kept.
   --
   --  If KEEP_OBSOLETE is True, obsoleted units are kept in the library.
   --  This is used when a whole design file has to be added in the library and
   --  then processed (without that feature, redefined units would disappear).
   procedure Add_Design_Unit_Into_Library
     (Unit : in Iir_Design_Unit; Keep_Obsolete : Boolean := False);

   --  Put all design_units of FILE into the work library, by calling
   --  Add_Design_Unit_Into_Library.
   --  FILE is updated since it may changed (FILE is never put in the library,
   --  a new one is created).
   procedure Add_Design_File_Into_Library (File : in out Iir_Design_File);

   -- Return the latest architecture analysed for entity ENTITY.
   function Get_Latest_Architecture (Entity: Iir_Entity_Declaration)
                                    return Iir_Architecture_Body;

   --  Return the design unit (stubed if not loaded) from UNIT.
   --  UNIT may be either a design unit, in this case UNIT is returned,
   --     or a selected name, in this case the prefix is a library name and
   --        the suffix a primary design unit name,
   --     or an entity_aspect_entity to designate an architectrure.
   --  Return null_iir if the design unit is not found.
   function Find_Design_Unit (Unit : Iir) return Iir_Design_Unit;

   --  Search design file NAME in library LIB.  This is not very efficient as
   --  this is a simple linear search.  NAME must correspond exactely to the
   --  design file name.
   function Find_Design_File (Lib : Iir_Library_Declaration; Name : Name_Id)
                             return Iir;

   --  Find an entity whose name is NAME in any library.
   --  If there is no such entity, return NULL_IIR.
   --  If there are severals entities, return NULL_IIR;
   function Find_Entity_For_Component (Name: Name_Id) return Iir_Design_Unit;

   --  Get the chain of libraries.  Can be used only to read (it musn't be
   --  modified).
   function Get_Libraries_Chain return Iir_Library_Declaration;
end Libraries;
