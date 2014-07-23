--  GHDL driver - local commands.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ghdlmain; use Ghdlmain;
with Iirs; use Iirs;

package Ghdllocal is
   type Command_Lib is abstract new Command_Type with null record;

   --  Setup GHDL.
   procedure Init (Cmd : in out Command_Lib);

   --  Handle:
   --  --std=xx, --work=xx, -Pxxx, --workdir=x, --ieee=x, -Px, and -v
   procedure Decode_Option (Cmd : in out Command_Lib;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   --  Disp detailled help.
   procedure Disp_Long_Help (Cmd : Command_Lib);

   --  Set with -v option.
   Flag_Verbose : Boolean := False;

   --  Suffix for asm files.
   Asm_Suffix : constant String := ".s";

   --  Suffix for llvm byte-code files.
   Llvm_Suffix : constant String := ".bc";

   --  Suffix for post files.
   Post_Suffix : constant String := ".on";

   --  Suffix for list files.
   List_Suffix : constant String := ".lst";

   --  Prefix for elab files.
   Elab_Prefix : constant String := "e~";

   --  Path prefix for libraries.
   Prefix_Path : String_Access := null;

   --  getenv ("GHDL_PREFIX").  Set by Setup_Libraries.
   Prefix_Env : String_Access := null;

   Nul : constant Character := Character'Val (0);

   --  Return FILENAME without the extension.
   function Get_Base_Name (Filename : String; Remove_Dir : Boolean := True)
                           return String;

   function Append_Suffix (File : String; Suffix : String)
                          return String_Access;

   --  Return TRUE is UNIT can be at the apex of a design hierarchy.
   function Is_Top_Entity (Unit : Iir) return Boolean;

   --  Display the name of library unit UNIT.
   procedure Disp_Library_Unit (Unit : Iir);

   --  Translate vhdl version into a path element.
   --  Used to search Std and IEEE libraries.
   function Get_Version_Path return String;

   -- Get Prefix_Path, but with 32 added if -m32 is requested
   function Get_Machine_Path_Prefix return String;

   --  Setup standard libaries path.  If LOAD is true, then load them now.
   procedure Setup_Libraries (Load : Boolean);

   --  Setup library, analyze FILES, and if SAVE_LIBRARY is set save the
   --  work library only
   procedure Analyze_Files (Files : Argument_List; Save_Library : Boolean);

   --  Load and parse all libraries and files, starting from the work library.
   --  The work library must already be loaded.
   --  Raise errorout.compilation_error in case of error (parse error).
   procedure Load_All_Libraries_And_Files;

   function Build_Dependence (Prim : String_Access; Sec : String_Access)
     return Iir_List;

   Prim_Name : String_Access;
   Sec_Name : String_Access;

   --  Set PRIM_NAME and SEC_NAME.
   procedure Extract_Elab_Unit
     (Cmd_Name : String; Args : Argument_List; Next_Arg : out Natural);

   procedure Register_Commands;
end Ghdllocal;
