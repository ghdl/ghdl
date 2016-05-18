with Types; use Types;
with Options;
with Name_Table;
with Iirs; use Iirs;
with Libraries;
with Errorout;
with Ghdlcomp;
with Ghdlsimul;

package body Mhdlsim is
   --  Top unit to elaborate or simulate.  Might not be a VHDL unit.
   Top_Name : Name_Id;
   Top_Unit : Iir;

   function Process_Param (Opt : Ghdl_C_String; Len : Natural)
                          return Integer is
   begin
      if Len > 3 and then Opt (1 .. 3) = "-e " then
         --  Unit to elaborate
         Top_Name := Name_Table.Get_Identifier (Opt (4 .. Len));
         return 0;
      elsif Options.Parse_Option (Opt (1 .. Len)) then
         --  Ok.
         return 0;
      else
         --  Error.
         return 1;
      end if;
   end Process_Param;

   procedure Analyze_Init is
   begin
      --  Load libraries...
      Ghdlcomp.Compile_Analyze_Init (False);
   end Analyze_Init;

   function Analyze_File (File : Ghdl_C_String; Len : Natural)
                         return Integer is
   begin
      Ghdlcomp.Compile_Analyze_File (File (1 .. Len));
      if Errorout.Nbr_Errors > 0 then
         return 1;
      else
         return 0;
      end if;
   end Analyze_File;

   function Known_Top_Unit return Integer
   is
      use Libraries;
   begin
      Top_Unit := Find_Primary_Unit (Work_Library, Top_Name);
      return Boolean'Pos (Top_Unit /= Null_Iir);
   end Known_Top_Unit;

   procedure Elaborate is
   begin
      Ghdlcomp.Compile_Elaborate (new String'(Name_Table.Image (Top_Name)));
   end Elaborate;

   procedure Run is
   begin
      Ghdlcomp.Compile_Run;
   end Run;

   Gnat_Version : constant String := "unknown compiler version" & ASCII.NUL;
   pragma Export (C, Gnat_Version, "__gnat_version");
begin
   --  TODO: set program name.
   Ghdlsimul.Compile_Init;
end Mhdlsim;
