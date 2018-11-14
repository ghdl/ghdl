with Types; use Types;
with Iirs; use Iirs;

package Sem_Lib is
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

      -- Load an already analyzed primary unit NAME from library LIBRARY
   -- and compile it.
   -- Return NULL_IIR if not found (ie, NAME does not correspond to a
   --   library unit identifier).
   function Load_Primary_Unit
     (Library: Iir_Library_Declaration; Name: Name_Id; Loc : Iir)
      return Iir_Design_Unit;

   -- Load an secondary unit of primary unit PRIMARY and analyse it.
   -- NAME must be set only for an architecture.
   function Load_Secondary_Unit
     (Primary: Iir_Design_Unit; Name: Name_Id; Loc : Iir)
     return Iir_Design_Unit;

   --  Analyze UNIT.
   procedure Finish_Compilation
     (Unit : Iir_Design_Unit; Main : Boolean := False);
end Sem_Lib;
