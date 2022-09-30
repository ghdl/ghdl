--  VHDL libraries handling.
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
with System;
with Interfaces.C_Streams;
with GNAT.OS_Lib;

with Logging; use Logging;
with Tables;
with Errorout; use Errorout;
with Options; use Options;
with Name_Table; use Name_Table;
with Str_Table;
with Files_Map;
with Flags;
with Std_Names;

with Vhdl.Tokens;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Scanner;
with Vhdl.Utils; use Vhdl.Utils;

package body Libraries is
   --  Chain of known libraries.  This is also the top node of all iir node.
   Libraries_Chain : Iir_Library_Declaration := Null_Iir;
   Libraries_Chain_Last : Iir_Library_Declaration := Null_Iir;

   --  Last design_file used.  Kept to speed-up operations.
   Last_Design_File : Iir_Design_File := Null_Iir;

   --  Table of library paths.
   package Paths is new Tables
     (Table_Index_Type => Integer,
      Table_Component_Type => Name_Id,
      Table_Low_Bound => 1,
      Table_Initial => 4);

   --  Report an error message.
   procedure Error_Lib_Msg (Msg : String) is
   begin
      Report_Msg (Msgid_Error, Library, No_Source_Coord, Msg);
   end Error_Lib_Msg;

   procedure Create_Virtual_Locations
   is
      use Files_Map;
      Library_Source_File : Source_File_Entry;
      Command_Source_File : Source_File_Entry;
   begin
      Library_Source_File := Create_Virtual_Source_File
        (Get_Identifier ("*libraries*"));
      Command_Source_File := Create_Virtual_Source_File
        (Get_Identifier ("*command line*"));
      Command_Line_Location := File_To_Location (Command_Source_File);
      Library_Location := File_To_Location (Library_Source_File);
   end Create_Virtual_Locations;

   --  Initialize paths table.
   --  Set the local path.
   procedure Initialize is
   begin
      --  Always look in current directory first.
      Paths.Init;
      Name_Nil := Get_Identifier ("");
      Paths.Append (Name_Nil);

      Local_Directory := Name_Nil;
      Work_Directory := Name_Nil;

      Libraries_Chain := Null_Iir;
      Std_Library := Null_Iir;
      Work_Library_Name := Std_Names.Name_Work;

      Create_Virtual_Locations;
   end Initialize;

   procedure Finalize is
   begin
      Paths.Free;
   end Finalize;

   function Path_To_Id (Path : String) return Name_Id is
   begin
      if Path (Path'Last) /= GNAT.OS_Lib.Directory_Separator then
         return Get_Identifier (Path & GNAT.OS_Lib.Directory_Separator);
      else
         return Get_Identifier (Path);
      end if;
   end Path_To_Id;

   procedure Add_Library_Path (Path : String)
   is
   begin
      if Path'Length = 0 then
         return;
      end if;
      Paths.Append (Path_To_Id (Path));
   end Add_Library_Path;

   function Get_Nbr_Paths return Natural is
   begin
      return Paths.Last;
   end Get_Nbr_Paths;

   function Get_Path (N : Natural) return Name_Id is
   begin
      if N not in Paths.First .. Paths.Last then
         raise Constraint_Error;
      end if;

      return Paths.Table (N);
   end Get_Path;

   -- Transform a library identifier into a file name.
   -- Very simple mechanism: just add '-objVV.cf' extension, where VV
   -- is the version.
   function Library_To_File_Name (Library: Iir_Library_Declaration)
                                 return String
   is
      use Flags;
   begin
      case Vhdl_Std is
         when Vhdl_87 =>
            return Image_Identifier (Library) & "-obj87.cf";
         when Vhdl_93 | Vhdl_00 | Vhdl_02 =>
            return Image_Identifier (Library) & "-obj93.cf";
         when Vhdl_08 =>
            return Image_Identifier (Library) & "-obj08.cf";
         when Vhdl_19 =>
            return Image_Identifier (Library) & "-obj19.cf";
      end case;
   end Library_To_File_Name;

   --  Search LIBRARY in the library path.
   procedure Search_Library_In_Path (Library : Iir)
   is
      use Flags;
      File_Name : constant String := Library_To_File_Name (Library);
      Library_Id : constant Name_Id := Get_Identifier (Library);
      Id_Len : constant Natural := Get_Name_Length (Library_Id);
   begin
      for I in Paths.First .. Paths.Last loop
         --  Try PATH/LIBxxx.cf
         declare
            Path : constant String :=
              Image (Paths.Table (I)) & File_Name & ASCII.NUL;
         begin
            if GNAT.OS_Lib.Is_Regular_File (Path'Address) then
               Set_Library_Directory (Library, Paths.Table (I));
               exit;
            end if;
         end;

         --  Try PATH/LIB/vNN/LIBxxx.cf
         declare
            Pfx : constant String := Image (Paths.Table (I));
            Pfx_Len : constant Natural := Pfx'Length;
            L : Natural;
            Path : String (1 .. Pfx_Len + Id_Len + 5 + File_Name'Length + 1);
         begin
            L := Pfx_Len;
            Path (1 .. L) := Pfx;
            Path (L + 1 .. L + Id_Len) := Image (Library_Id);
            L := L + Id_Len;
            Path (L + 1) := GNAT.OS_Lib.Directory_Separator;
            case Vhdl_Std is
               when Vhdl_87 =>
                  Path (L + 2 .. L + 4) := "v87";
               when Vhdl_93 | Vhdl_00 | Vhdl_02 =>
                  Path (L + 2 .. L + 4) := "v93";
               when Vhdl_08 =>
                  Path (L + 2 .. L + 4) := "v08";
               when Vhdl_19 =>
                  Path (L + 2 .. L + 4) := "v19";
            end case;
            L := L + 5;
            Path (L) := GNAT.OS_Lib.Directory_Separator;
            Path (L + 1 .. L + File_Name'Length) := File_Name;
            Path (L + File_Name'Length + 1) := Character'Val (0);
            if GNAT.OS_Lib.Is_Regular_File (Path'Address) then
               --  For Get_Identifier: keep only the path part (including the
               --  trailing path separator).
               Set_Library_Directory (Library, Get_Identifier (Path (1 .. L)));
               exit;
            end if;
         end;
      end loop;
   end Search_Library_In_Path;

   --  Set PATH as the path of the work library.
   procedure Set_Work_Library_Path (Path : String) is
   begin
      Work_Directory := Path_To_Id (Path);
      if not GNAT.OS_Lib.Is_Directory (Get_Address (Work_Directory))
        and then Is_Warning_Enabled (Warnid_Library)
      then
         --  This is a warning, since 'clean' action should not fail in
         --  this cases.
         Warning_Msg_Option
           (Warnid_Library,
            "directory '" & Path & "' set by --workdir= does not exist");
         --  raise Option_Error;
      end if;
   end Set_Work_Library_Path;

   --  Every design unit is put in this hash table to be quickly found by
   --  its (primary) identifier.
   Unit_Hash_Length : constant Name_Id := 127;
   subtype Hash_Id is Name_Id range 0 .. Unit_Hash_Length - 1;
   Unit_Hash_Table : array (Hash_Id) of Iir := (others => Null_Iir);

   --  Get the hash value for DESIGN_UNIT.
   --  Architectures use the entity name.
   function Get_Hash_Id_For_Unit (Design_Unit : Iir_Design_Unit) return Hash_Id
   is
      Lib_Unit : Iir;
      Id : Name_Id;
   begin
      if Get_Kind (Design_Unit) = Iir_Kind_Foreign_Module then
         Id := Get_Identifier (Design_Unit);
      else
         Lib_Unit := Get_Library_Unit (Design_Unit);
         case Iir_Kinds_Library_Unit (Get_Kind (Lib_Unit)) is
            when Iir_Kinds_Primary_Unit
               | Iir_Kind_Package_Body
               | Iir_Kind_Foreign_Module =>
               Id := Get_Identifier (Lib_Unit);
            when Iir_Kind_Architecture_Body =>
               --  Architectures are put with the entity identifier.
               Id := Get_Entity_Identifier_Of_Architecture (Lib_Unit);
         end case;
      end if;
      return Id mod Unit_Hash_Length;
   end Get_Hash_Id_For_Unit;

   --  Put DESIGN_UNIT into the unit hash table.
   procedure Add_Unit_Hash (Design_Unit : Iir)
   is
      Id : Hash_Id;
   begin
      Id := Get_Hash_Id_For_Unit (Design_Unit);
      Set_Hash_Chain (Design_Unit, Unit_Hash_Table (Id));
      Unit_Hash_Table (Id) := Design_Unit;
   end Add_Unit_Hash;

   --  Remove DESIGN_UNIT from the unit hash table.
   procedure Remove_Unit_Hash (Design_Unit : Iir)
   is
      Id : Hash_Id;
      Unit, Prev, Next : Iir_Design_Unit;
   begin
      Id := Get_Hash_Id_For_Unit (Design_Unit);
      Unit := Unit_Hash_Table (Id);
      Prev := Null_Iir;
      while Unit /= Null_Iir loop
         Next := Get_Hash_Chain (Unit);
         if Unit = Design_Unit then
            if Prev = Null_Iir then
               Unit_Hash_Table (Id) := Next;
            else
               Set_Hash_Chain (Prev, Next);
            end if;
            return;
         end if;
         Prev := Unit;
         Unit := Next;
      end loop;
      --  Not found.
      raise Internal_Error;
   end Remove_Unit_Hash;

   procedure Purge_Design_File (Design_File : Iir_Design_File)
   is
      Prev, File, Next : Iir_Design_File;
      Unit : Iir_Design_Unit;

      File_Name : constant Name_Id := Get_Design_File_Filename (Design_File);
      Dir_Name : constant Name_Id := Get_Design_File_Directory (Design_File);
   begin
      File := Get_Design_File_Chain (Work_Library);
      Prev := Null_Iir;
      loop
         if File = Null_Iir then
            --  Not found ???
            return;
         end if;

         Next := Get_Chain (File);
         exit when Get_Design_File_Filename (File) = File_Name
           and then Get_Design_File_Directory (File) = Dir_Name;

         Prev := File;
         File := Next;
      end loop;

      --  Remove from library.
      if Prev = Null_Iir then
         Set_Design_File_Chain (Work_Library, Next);
      else
         Set_Chain (Prev, Next);
      end if;

      --  Remove all units from unit hash table.
      Unit := Get_First_Design_Unit (File);
      while Unit /= Null_Iir loop
         Remove_Unit_Hash (Unit);
         Unit := Get_Chain (Unit);
      end loop;

      --  Clear the Last_Design_File cache.
      if Last_Design_File = Design_File then
         Last_Design_File := Null_Iir;
      end if;
   end Purge_Design_File;

   -- Load the contents of a library from a map file.
   -- The format of this file, used by save_library and load_library is
   -- as follow:
   --
   -- file_format ::= header { design_file_format }
   -- header ::= v 3
   -- design_file_format ::=
   --      filename_format { design_unit_format  }
   -- filename_format ::=
   --      FILE directory "filename" "file_time_stamp" "analyze_time_stamp":
   -- design_unit_format ::= entity_format
   --                        | architecture_format
   --                        | package_format
   --                        | package_body_format
   --                        | configuration_format
   --                        | context_format
   -- position_format ::= LINE(POS) + OFF on DATE
   -- entity_format ::=
   --      ENTITY identifier AT position_format ;
   -- architecture_format ::=
   --      ARCHITECTURE identifier of name AT position_format ;
   -- package_format ::=
   --      PACKAGE identifier AT position_format [BODY] ;
   -- package_body_format ::=
   --      PACKAGE BODY identifier AT position_format ;
   -- configuration_format ::=
   --      CONFIGURATION identifier AT position_format ;
   -- context_format ::=
   --      CONTEXT identifier AT position_format ;
   --
   -- The position_format meaning is:
   --       LINE is the line number (first line is number 1),
   --       POS is the offset of this line number, as a source_ptr value,
   --       OFF is the offset in the line, starting with 0.
   --       DATE is the symbolic date of analysis (order).
   --
   -- Return TRUE if the library was found.
   function Load_Library (Library: Iir_Library_Declaration) return Boolean
   is
      use Vhdl.Scanner;
      use Vhdl.Tokens;

      File : Source_File_Entry;

      --  Report an error message and abort.
      procedure Bad_Library_Format;
      pragma No_Return (Bad_Library_Format);

      procedure Bad_Library_Format is
      begin
         Error_Lib_Msg (Image (Files_Map.Get_File_Name (File)) &
                          ": bad library format");
         raise Compilation_Error;
      end Bad_Library_Format;

      procedure Scan_Expect (Tok: Token_Type) is
      begin
         Scan;
         if Current_Token /= Tok then
            Bad_Library_Format;
         end if;
      end Scan_Expect;

      function Current_Time_Stamp return Time_Stamp_Id is
      begin
         if Current_String_Length /= Time_Stamp_String'Length then
            Bad_Library_Format;
         end if;
         return Time_Stamp_Id (Current_String_Id);
      end Current_Time_Stamp;

      function String_To_Name_Id return Name_Id
      is
         Len : constant Nat32 := Current_String_Length;
         Str_Id : constant String8_Id := Current_String_Id;
         Buf : String (1 .. Natural (Len));
      begin
         for I in 1 .. Len loop
            Buf (Natural (I)) := Str_Table.Char_String8 (Str_Id, I);
         end loop;
         --  FIXME: should remove last string.
         return Get_Identifier (Buf);
      end String_To_Name_Id;

      Trace_Library_Load : constant Boolean := False;

      Design_Unit, Last_Design_Unit : Iir_Design_Unit;
      Lib_Ident : constant Name_Id := Get_Identifier (Library);

      Design_File: Iir_Design_File;
      Library_Unit: Iir;
      Line, Col: Int32;
      File_Id : Name_Id;
      File_Dir : Name_Id;
      Pos: Source_Ptr;
      Date: Date_Type;
      Max_Date: Date_Type := Date_Valid'First;
      Dir : Name_Id;
   begin
      -- Check the library was not already loaded.
      pragma Assert (Get_Design_File_Chain (Library) = Null_Iir);

      if Trace_Library_Load then
         Log_Line ("Load library " & Image (Lib_Ident));
      end if;

      -- Try to open the library file map.
      Dir := Get_Library_Directory (Library);
      if Dir = Null_Identifier then
         Search_Library_In_Path (Library);
         Dir := Get_Library_Directory (Library);
      end if;
      if Dir = Null_Identifier then
         --  Not found.
         Set_Date (Library, Date_Valid'First);
         return False;
      end if;

      File_Id := Get_Identifier (Library_To_File_Name (Library));
      if Trace_Library_Load then
         Log_Line ("  from " & Image (Dir) & Image (File_Id));
      end if;

      File := Files_Map.Read_Source_File (Dir, File_Id);
      if File = No_Source_File_Entry then
         --  Not found.
         Set_Date (Library, Date_Valid'First);
         return False;
      end if;

      Vhdl.Scanner.Set_File (File);

      --  Parse header.
      Scan;
      if Current_Token /= Tok_Identifier
        or else Current_Identifier /= Std_Names.Name_V
      then
         Bad_Library_Format;
      end if;
      Scan_Expect (Tok_Integer);
      if Current_Iir_Int64 /= 4 then
         Bad_Library_Format;
      end if;
      Scan;

      Last_Design_Unit := Null_Iir;
      while Current_Token /= Tok_Eof loop
         if Current_Token = Tok_File then
            -- This is a new design file.
            Design_File := Create_Iir (Iir_Kind_Design_File);

            Scan;
            if Current_Token = Tok_Dot then
               --  The filename is local, use the directory of the library.
               if Dir = Name_Nil then
                  File_Dir := Files_Map.Get_Home_Directory;
               else
                  File_Dir := Dir;
               end if;
            elsif Current_Token = Tok_Slash then
               --  The filename is an absolute file.
               File_Dir := Null_Identifier;
            elsif Current_Token = Tok_String then
               File_Dir := String_To_Name_Id;
            else
               Bad_Library_Format;
            end if;

            Set_Design_File_Directory (Design_File, File_Dir);

            Scan_Expect (Tok_String);
            Set_Design_File_Filename (Design_File, String_To_Name_Id);

            -- FIXME: check the file name is uniq.

            Set_Parent (Design_File, Library);

            --  Prepend.
            Set_Chain (Design_File, Get_Design_File_Chain (Library));
            Set_Design_File_Chain (Library, Design_File);

            Scan_Expect (Tok_String);
            if Current_String_Length /= File_Checksum_String'Length then
               Bad_Library_Format;
            end if;
            Set_File_Checksum
              (Design_File, File_Checksum_Id (Current_String_Id));

            Scan_Expect (Tok_String);
            Set_Analysis_Time_Stamp (Design_File, Current_Time_Stamp);

            Scan_Expect (Tok_Colon);
            Scan;
            Last_Design_Unit := Null_Iir;
         else
            -- This is a new design unit.
            Design_Unit := Create_Iir (Iir_Kind_Design_Unit);
            Set_Design_File (Design_Unit, Design_File);
            case Current_Token is
               when Tok_Entity =>
                  Library_Unit := Create_Iir (Iir_Kind_Entity_Declaration);
                  Scan;
               when Tok_Architecture =>
                  Library_Unit := Create_Iir (Iir_Kind_Architecture_Body);
                  Scan;
               when Tok_Configuration =>
                  Library_Unit :=
                    Create_Iir (Iir_Kind_Configuration_Declaration);
                  Scan;
               when Tok_Package =>
                  Scan;
                  if Current_Token = Tok_Body then
                     Library_Unit := Create_Iir (Iir_Kind_Package_Body);
                     Scan;
                  else
                     Library_Unit := Create_Iir (Iir_Kind_Package_Declaration);
                  end if;
               when Tok_Context =>
                  Library_Unit := Create_Iir (Iir_Kind_Context_Declaration);
                  Scan;
               when Tok_Vunit =>
                  Library_Unit := Create_Iir (Iir_Kind_Vunit_Declaration);
                  Scan;
               when Tok_Vmode =>
                  Library_Unit := Create_Iir (Iir_Kind_Vmode_Declaration);
                  Scan;
               when Tok_Vprop =>
                  Library_Unit := Create_Iir (Iir_Kind_Vprop_Declaration);
                  Scan;
               when others =>
                  Log_Line
                    ("load_library: line must start with " &
                     "'architecture', 'entity', 'package' or 'configuration'");
                  raise Internal_Error;
            end case;

            if Current_Token /= Tok_Identifier then
               raise Internal_Error;
            end if;
            Set_Identifier (Library_Unit, Current_Identifier);
            Set_Identifier (Design_Unit, Current_Identifier);

            if Get_Kind (Library_Unit) = Iir_Kind_Architecture_Body then
               declare
                  Ent : Iir;
               begin
                  Scan_Expect (Tok_Of);
                  Scan_Expect (Tok_Identifier);
                  Ent := Create_Iir (Iir_Kind_Simple_Name);
                  Set_Identifier (Ent, Current_Identifier);
                  Set_Entity_Name (Library_Unit, Ent);
               end;
            end if;

            -- Scan position.
            Scan_Expect (Tok_Identifier); -- at
            Scan_Expect (Tok_Integer);
            Line := Int32 (Current_Iir_Int64);
            Scan_Expect (Tok_Left_Paren);
            Scan_Expect (Tok_Integer);
            Pos := Source_Ptr (Current_Iir_Int64);
            Scan_Expect (Tok_Right_Paren);
            Scan_Expect (Tok_Plus);
            Scan_Expect (Tok_Integer);
            Col := Int32 (Current_Iir_Int64);
            Scan_Expect (Tok_On);
            Scan_Expect (Tok_Integer);
            Date := Date_Type (Current_Iir_Int64);

            Scan;
            if Get_Kind (Library_Unit) = Iir_Kind_Package_Declaration
              and then Current_Token = Tok_Body
            then
               Set_Need_Body (Library_Unit, True);
               Scan;
            end if;
            if Current_Token /= Tok_Semi_Colon then
               raise Internal_Error;
            end if;
            Scan;

            if False then
               Log_Line ("line:" & Int32'Image (Line)
                           & ", pos:" & Source_Ptr'Image (Pos));
            end if;

            -- Keep the position of the design unit.
            --Set_Location (Design_Unit, Location_Type (File));
            --Set_Location (Library_Unit, Location_Type (File));
            Set_Design_Unit_Source_Pos (Design_Unit, Pos);
            Set_Design_Unit_Source_Line (Design_Unit, Line);
            Set_Design_Unit_Source_Col (Design_Unit, Col);
            Set_Date (Design_Unit, Date);
            if Date > Max_Date then
               Max_Date := Date;
            end if;
            Set_Date_State (Design_Unit, Date_Disk);
            Set_Library_Unit (Design_Unit, Library_Unit);
            Set_Design_Unit (Library_Unit, Design_Unit);

            --  Add in the unit hash table.
            Add_Unit_Hash (Design_Unit);

            if Last_Design_Unit = Null_Iir then
               Set_First_Design_Unit (Design_File, Design_Unit);
            else
               Set_Chain (Last_Design_Unit, Design_Unit);
            end if;
            Last_Design_Unit := Design_Unit;
            Set_Last_Design_Unit (Design_File, Design_Unit);
         end if;
      end loop;
      Set_Date (Library, Max_Date);

      Vhdl.Scanner.Close_File;

      --  Don't need the library file anymore.
      Files_Map.Unload_Last_Source_File (File);

      return True;
   end Load_Library;

   -- Note: the scanner shouldn't be in use, since this procedure uses it.
   function Load_Std_Library (Build_Standard : Boolean := True) return Boolean
   is
      use Vhdl.Std_Package;
      Dir : Name_Id;
   begin
      --  This procedure must not be called twice.
      pragma Assert (Libraries_Chain = Null_Iir);

      Flags.Create_Flag_String;

      Vhdl.Std_Package.Create_First_Nodes;

      --  Create the library.
      Std_Library := Create_Iir (Iir_Kind_Library_Declaration);
      Set_Identifier (Std_Library, Std_Names.Name_Std);
      Set_Location (Std_Library, Library_Location);
      Libraries_Chain := Std_Library;
      Libraries_Chain_Last := Std_Library;

      if Build_Standard then
         Create_Std_Standard_Package (Std_Library);
         Add_Unit_Hash (Std_Standard_Unit);
      end if;

      if Flags.Bootstrap
        and then Work_Library_Name = Std_Names.Name_Std
      then
         Dir := Work_Directory;
      else
         Dir := Null_Identifier;
      end if;
      Set_Library_Directory (Std_Library, Dir);
      if Load_Library (Std_Library) = False
        and then not Flags.Bootstrap
      then
         Error_Msg_Option ("cannot find ""std"" library");
         return False;
      end if;

      if Build_Standard then
         --  Add the standard_file into the library.
         --  This is done after Load_Library, because it checks there is no
         --  previous files in the library.
         Set_Location (Std_Library, Get_Location (Standard_Package));
         Set_Parent (Std_Standard_File, Std_Library);
         Set_Chain (Std_Standard_File, Get_Design_File_Chain (Std_Library));
         Set_Design_File_Chain (Std_Library, Std_Standard_File);
      end if;

      Set_Visible_Flag (Std_Library, True);
      return True;
   end Load_Std_Library;

   procedure Load_Work_Library (Empty : Boolean := False)
   is
      use Std_Names;
   begin
      if Work_Library_Name = Name_Std then
         if not Flags.Bootstrap then
            Error_Msg_Option ("the WORK library cannot be STD");
            raise Option_Error;
         end if;
         Work_Library := Std_Library;
      else
         --  If the library is already known, just switch to it.  This is used
         --  for --work= option in the middle of files.
         Work_Library := Vhdl.Utils.Find_Name_In_Chain
           (Libraries_Chain, Work_Library_Name);
         if Work_Library /= Null_Iir then
            return;
         end if;

         Work_Library := Create_Iir (Iir_Kind_Library_Declaration);
         Set_Location (Work_Library, Library_Location);
         Set_Library_Directory (Work_Library, Work_Directory);

         Set_Identifier (Work_Library, Work_Library_Name);

         if not Empty then
            if Load_Library (Work_Library) = False then
               null;
            end if;
         else
            Set_Date (Work_Library, Date_Valid'First);
         end if;

         --  Add it to the list of libraries.
         Set_Chain (Libraries_Chain_Last, Work_Library);
         Libraries_Chain_Last := Work_Library;
      end if;
      Set_Visible_Flag (Work_Library, True);
   end Load_Work_Library;

   function Get_Library_No_Create (Ident : Name_Id)
                                  return Iir_Library_Declaration is
   begin
      --  The library work is a little bit special.
      if Ident = Std_Names.Name_Work or else Ident = Work_Library_Name then
         --  load_work_library must have been called before.
         pragma Assert (Work_Library /= Null_Iir);
         return Work_Library;
      end if;

      --  Check if the library has already been loaded.
      return Vhdl.Utils.Find_Name_In_Chain (Libraries_Chain, Ident);
   end Get_Library_No_Create;

   -- Get or create a library from an identifier.
   function Get_Library (Ident: Name_Id; Loc : Location_Type)
                        return Iir_Library_Declaration
   is
      Library: Iir_Library_Declaration;
   begin
      Library := Get_Library_No_Create (Ident);
      if Library /= Null_Iir then
         return Library;
      end if;

      --  This is a new library.
      --  Load_std_library must have been called before.
      pragma Assert (Ident /= Std_Names.Name_Std);

      Library := Create_Iir (Iir_Kind_Library_Declaration);
      Set_Location (Library, Library_Location);
      Set_Library_Directory (Library, Null_Identifier);
      Set_Identifier (Library, Ident);
      if Load_Library (Library) = False then
         Error_Msg_Sem (+Loc, "cannot find resource library %i", +Ident);
      end if;
      Set_Visible_Flag (Library, True);

      Set_Chain (Libraries_Chain_Last, Library);
      Libraries_Chain_Last := Library;

      return Library;
   end Get_Library;

   --  Return TRUE if UNIT1 and UNIT2 have identifiers for the same
   --  design unit identifier.
   --  eg: 'entity A' and 'package A' returns TRUE.
   function Is_Same_Library_Unit (Unit1, Unit2 : Iir) return Boolean
   is
      Entity_Name1, Entity_Name2: Name_Id;
      Unit1_Kind, Unit2_Kind : Iir_Kind;
   begin
      if Get_Identifier (Unit1) /= Get_Identifier (Unit2) then
         return False;
      end if;

      Unit1_Kind := Get_Kind (Unit1);
      Unit2_Kind := Get_Kind (Unit2);

      --  Package and package body are never the same library unit.
      if Unit1_Kind = Iir_Kind_Package_Declaration
        and then Unit2_Kind = Iir_Kind_Package_Body
      then
         return False;
      end if;
      if Unit2_Kind = Iir_Kind_Package_Declaration
        and then Unit1_Kind = Iir_Kind_Package_Body
      then
         return False;
      end if;

      --  Two architecture declarations are identical only if they also have
      --  the same entity name.
      if Unit1_Kind = Iir_Kind_Architecture_Body
        and then Unit2_Kind = Iir_Kind_Architecture_Body
      then
         Entity_Name1 := Get_Entity_Identifier_Of_Architecture (Unit1);
         Entity_Name2 := Get_Entity_Identifier_Of_Architecture (Unit2);
         if Entity_Name1 /= Entity_Name2 then
            return False;
         end if;
      end if;

      --  An architecture declaration never conflits with a library unit that
      --  is not an architecture declaration.
      if (Unit1_Kind = Iir_Kind_Architecture_Body
          and then Unit2_Kind /= Iir_Kind_Architecture_Body)
        or else
        (Unit1_Kind /= Iir_Kind_Architecture_Body
         and then Unit2_Kind = Iir_Kind_Architecture_Body)
      then
         return False;
      end if;

      return True;
   end Is_Same_Library_Unit;

   --  Return true iff DEP (an element of a dependence list) is design unit
   --  UNIT.
   function Is_Design_Unit (Dep : Iir; Unit : Iir) return Boolean
   is
      Lib_Unit : Iir;
   begin
      case Get_Kind (Dep) is
         when Iir_Kind_Design_Unit =>
            return Dep = Unit;
         when Iir_Kind_Selected_Name =>
            declare
               Lib : constant Iir := Get_Library (Get_Design_File (Unit));
            begin
               if Get_Identifier (Get_Prefix (Dep)) /= Get_Identifier (Lib)
               then
                  return False;
               end if;
            end;
            Lib_Unit := Get_Library_Unit (Unit);
            case Iir_Kinds_Library_Unit (Get_Kind (Lib_Unit)) is
               when Iir_Kinds_Primary_Unit
                  | Iir_Kind_Package_Body
                  | Iir_Kind_Foreign_Module =>
                  return Get_Identifier (Dep) = Get_Identifier (Lib_Unit);
               when Iir_Kind_Architecture_Body =>
                  return False;
            end case;
         when Iir_Kind_Entity_Aspect_Entity =>
            Lib_Unit := Get_Library_Unit (Unit);
            if Get_Kind (Lib_Unit) /= Iir_Kind_Architecture_Body then
               return False;
            end if;
            if Get_Identifier (Get_Architecture (Dep))
              /= Get_Identifier (Lib_Unit)
            then
               return False;
            end if;

            if Get_Entity (Dep) /= Get_Entity (Lib_Unit) then
               return False;
            end if;

            return True;

         when others =>
            Error_Kind ("is_design_unit", Dep);
      end case;
   end Is_Design_Unit;

   function Find_Design_Unit (Unit : Iir) return Iir_Design_Unit is
   begin
      case Get_Kind (Unit) is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Foreign_Module =>
            return Unit;
         when Iir_Kind_Selected_Name =>
            declare
               Lib : Iir_Library_Declaration;
            begin
               Lib := Get_Library (Get_Identifier (Get_Prefix (Unit)),
                                   Get_Location (Unit));
               return Find_Primary_Unit (Lib, Get_Identifier (Unit));
            end;
         when Iir_Kind_Entity_Aspect_Entity =>
            return Find_Secondary_Unit
              (Get_Design_Unit (Get_Entity (Unit)),
               Get_Identifier (Get_Architecture (Unit)));
         when others =>
            Error_Kind ("find_design_unit", Unit);
      end case;
   end Find_Design_Unit;

   function Find_Design_File (Lib : Iir_Library_Declaration; Name : Name_Id)
                             return Iir
   is
      File : Iir;
   begin
      File := Get_Design_File_Chain (Lib);
      while Is_Valid (File) loop
         if Get_Design_File_Filename (File) = Name then
            return File;
         end if;
         File := Get_Chain (File);
      end loop;
      return Null_Iir;
   end Find_Design_File;

   --  Mark UNIT as obsolete.  Mark all units that depends on UNIT as
   --  obsolete.
   procedure Mark_Unit_Obsolete (Unit : Iir_Design_Unit)
   is
      Lib, File, Un : Iir;
      List : Iir_List;
      It : List_Iterator;
      El : Iir;
   begin
      Set_Date (Unit, Date_Obsolete);

      Lib := Libraries_Chain;
      while Is_Valid (Lib) loop
         File := Get_Design_File_Chain (Lib);
         while Is_Valid (File) loop
            Un := Get_First_Design_Unit (File);
            while Is_Valid (Un) loop
               if Get_Kind (Un) /= Iir_Kind_Foreign_Module then
                  List := Get_Dependence_List (Un);
               else
                  List := Null_Iir_List;
               end if;

               if List /= Null_Iir_List
                 and then Get_Date (Un) /= Date_Obsolete
               then
                  pragma Assert (Get_Date_State (Un) = Date_Analyze);

                  It := List_Iterate (List);
                  while Is_Valid (It) loop
                     El := Get_Element (It);

                     if Is_Design_Unit (El, Unit) then

                        --  Keep direct reference (for speed-up).
                        if Get_Kind (El) /= Iir_Kind_Design_Unit then
                           Vhdl.Utils.Free_Recursive (El);
                           Set_Element (It, Unit);
                        end if;

                        --  Recurse.
                        Mark_Unit_Obsolete (Un);
                     end if;
                     Next (It);
                  end loop;
               end if;

               Un := Get_Chain (Un);
            end loop;
            File := Get_Chain (File);
         end loop;
         Lib := Get_Chain (Lib);
      end loop;
   end Mark_Unit_Obsolete;

   --  This procedure is called when the DESIGN_UNIT (either the stub created
   --  when a library is read or created from a previous unit in a source
   --  file) has been replaced by a new unit.  Free everything but DESIGN_UNIT,
   --  because it may be referenced in other units (dependence...)
   --  FIXME: Isn't the library unit also referenced too ?
   procedure Free_Design_Unit (Design_Unit : Iir_Design_Unit)
   is
      Lib : Iir;
      Unit : Iir_Design_Unit;
      Dep_List : Iir_List;
   begin
      --  Free dependence list.
      Dep_List := Get_Dependence_List (Design_Unit);
      Destroy_Iir_List (Dep_List);
      Set_Dependence_List (Design_Unit, Null_Iir_List);

      --  Free default configuration of architecture (if any).
      Lib := Get_Library_Unit (Design_Unit);
      if Lib /= Null_Iir
        and then Get_Kind (Lib) = Iir_Kind_Architecture_Body
      then
         Free_Iir (Get_Entity_Name (Lib));
         Unit := Get_Default_Configuration_Declaration (Lib);
         if Unit /= Null_Iir then
            Free_Design_Unit (Unit);
         end if;
      end if;

      --  Free library unit.
      Free_Iir (Lib);
      Set_Library_Unit (Design_Unit, Null_Iir);
   end Free_Design_Unit;

   procedure Remove_Unit_From_File
     (Unit_Ref : Iir_Design_Unit; File : Iir_Design_File)
   is
      Prev : Iir_Design_Unit;
      Unit, Next : Iir_Design_Unit;
   begin
      Prev := Null_Iir;
      Unit := Get_First_Design_Unit (File);
      while Unit /= Null_Iir loop
         Next := Get_Chain (Unit);
         if Unit = Unit_Ref then
            if Prev = Null_Iir then
               Set_First_Design_Unit (File, Next);
            else
               Set_Chain (Prev, Next);
            end if;
            if Next = Null_Iir then
               Set_Last_Design_Unit (File, Prev);
            end if;
            return;
         end if;
         Prev := Unit;
         Unit := Next;
      end loop;
      --  Not found.
      raise Internal_Error;
   end Remove_Unit_From_File;

   -- Add or replace a design unit in the working library.
   procedure Add_Design_Unit_Into_Library
     (Unit : in Iir_Design_Unit; Keep_Obsolete : Boolean := False)
   is
      Design_File: Iir_Design_File;
      Design_Unit, Prev_Design_Unit : Iir_Design_Unit;
      Last_Unit : Iir_Design_Unit;
      Library_Unit: Iir;
      New_Library_Unit: Iir;
      Unit_Id : Name_Id;
      Date: Date_Type;
      New_Lib_Checksum : File_Checksum_Id;
      Id : Hash_Id;

      --  File name and dir name of DECL.
      File_Name : Name_Id;
      Dir_Name : Name_Id;
   begin
      --  As specified, the Chain must be not set.
      pragma Assert (Get_Chain (Unit) = Null_Iir);

      --  The unit must not be in the library.
      pragma Assert (Get_Date_State (Unit) = Date_Extern);

      --  Mark this design unit as being loaded.
      case Get_Kind (Unit) is
         when Iir_Kind_Design_Unit =>
            New_Library_Unit := Get_Library_Unit (Unit);
         when Iir_Kind_Foreign_Module =>
            New_Library_Unit := Unit;
         when others =>
            raise Internal_Error;
      end case;
      Unit_Id := Get_Identifier (New_Library_Unit);

      --  Set the date of the design unit as the most recently analyzed
      --  design unit.
      case Get_Date (Unit) is
         when Date_Parsed =>
            Set_Date_State (Unit, Date_Parse);
         when Date_Analyzed =>
            Date := Get_Date (Work_Library) + 1;
            Set_Date (Unit, Date);
            Set_Date (Work_Library, Date);
            Set_Date_State (Unit, Date_Analyze);
         when Date_Valid =>
            raise Internal_Error;
         when others =>
            raise Internal_Error;
      end case;

      --  Set file time stamp.
      declare
         File : constant Source_File_Entry :=
           Get_Design_File_Source (Get_Design_File (Unit));
      begin
         New_Lib_Checksum := Files_Map.Get_File_Checksum (File);
         File_Name := Files_Map.Get_File_Name (File);
         if GNAT.OS_Lib.Is_Absolute_Path (Image (File_Name)) then
            Dir_Name := Null_Identifier;
         else
            Dir_Name := Files_Map.Get_Home_Directory;
         end if;
      end;

      if Unit_Id = Null_Identifier then
         pragma Assert (Flags.Flag_Force_Analysis);
         return;
      end if;

      --  Try to find a design unit with the same name in the work library.
      Id := Get_Hash_Id_For_Unit (Unit);
      declare
         Design_Unit, Prev_Design_Unit : Iir_Design_Unit;
         Next_Design_Unit : Iir_Design_Unit;
      begin
         Design_Unit := Unit_Hash_Table (Id);
         Prev_Design_Unit := Null_Iir;
         while Design_Unit /= Null_Iir loop
            Next_Design_Unit := Get_Hash_Chain (Design_Unit);
            Design_File := Get_Design_File (Design_Unit);
            case Get_Kind (Design_Unit) is
               when Iir_Kind_Foreign_Module =>
                  Library_Unit := Design_Unit;
               when Iir_Kind_Design_Unit =>
                  Library_Unit := Get_Library_Unit (Design_Unit);
               when others =>
                  raise Internal_Error;
            end case;
            if Get_Identifier (Design_Unit) = Unit_Id
              and then Get_Library (Design_File) = Work_Library
              and then Is_Same_Library_Unit (New_Library_Unit, Library_Unit)
            then
               --  LIBRARY_UNIT and UNIT designate the same design unit.
               Mark_Unit_Obsolete (Design_Unit);

               --  Remove the old one from the hash table.
               --  Remove DESIGN_UNIT from the unit_hash.
               if Prev_Design_Unit = Null_Iir then
                  Unit_Hash_Table (Id) := Next_Design_Unit;
               else
                  Set_Hash_Chain (Prev_Design_Unit, Next_Design_Unit);
               end if;

               --  Remove DESIGN_UNIT from the design_file.
               --  If KEEP_OBSOLETE is True, units that are obsoleted by units
               --  in the same design file are kept.  This allows to process
               --  (pretty print, xrefs, ...) all units of a design file.
               --  But still remove units that are replaced (if a file was
               --  already in the library).
               if not Keep_Obsolete
                 or else Get_Date_State (Design_Unit) = Date_Disk
               then
                  Remove_Unit_From_File (Design_Unit, Design_File);

                  --  Put removed units in a list so that they are still
                  --  referenced.
                  Set_Chain (Design_Unit, Obsoleted_Design_Units);
                  Obsoleted_Design_Units := Design_Unit;
               end if;

               --  UNIT *must* replace library_unit if they don't belong
               --  to the same file.
               if Get_Design_File_Filename (Design_File) = File_Name
                 and then Get_Design_File_Directory (Design_File) = Dir_Name
               then
                  --  In the same file.
                  if Get_Date_State (Design_Unit) = Date_Analyze then
                     --  Warns only if we are not re-analyzing the file.
                     if Is_Warning_Enabled (Warnid_Library) then
                        Warning_Msg_Sem
                          (Warnid_Library, +Unit,
                           "redefinition of a library unit in "
                             & "same design file:");
                        Warning_Msg_Sem
                          (Warnid_Library, +Unit, "%n defined at %l is now %n",
                           (+Library_Unit, +Library_Unit, +New_Library_Unit));
                     end if;
                  else
                     --  Free the stub corresponding to the unit.  This is the
                     --  common case when a unit is reanalyzed after a change.
                     if not Keep_Obsolete then
                        Free_Design_Unit (Design_Unit);
                     end if;
                  end if;

                  --  Note: the current design unit should not be freed if
                  --  in use; unfortunatly, this is not obvious to check.
               else
                  if Is_Warning_Enabled (Warnid_Library)
                    and then Get_Kind (Library_Unit) in Iir_Kinds_Primary_Unit
                  then
                     if Get_Kind (Library_Unit) /= Get_Kind (New_Library_Unit)
                     then
                        Warning_Msg_Sem
                          (Warnid_Library, +Unit,
                           "changing definition of a library unit:");
                        Warning_Msg_Sem
                          (Warnid_Library, +Unit,
                           "%n is now %n", (+Library_Unit, +New_Library_Unit));
                     end if;
                     Warning_Msg_Sem
                       (Warnid_Library, +Unit,
                        "%n was also defined in file %i",
                        (+Library_Unit,
                         +Get_Design_File_Filename (Design_File)));
                  end if;
               end if;
               --  Continue to search as there can be several units with the
               --  same name (like package and package body).
            end if;

            Prev_Design_Unit := Design_Unit;
            Design_Unit := Next_Design_Unit;
         end loop;
      end;

      --  Try to find the design file in the library.
      --  First try the last one found.
      if Last_Design_File /= Null_Iir
        and then Get_Library (Last_Design_File) = Work_Library
        and then Get_Design_File_Filename (Last_Design_File) = File_Name
        and then Get_Design_File_Directory (Last_Design_File) = Dir_Name
      then
         Design_File := Last_Design_File;
      else
         --  Search.
         Design_File := Get_Design_File_Chain (Work_Library);
         while Design_File /= Null_Iir loop
            if Get_Design_File_Filename (Design_File) = File_Name
              and then Get_Design_File_Directory (Design_File) = Dir_Name
            then
               exit;
            end if;
            Design_File := Get_Chain (Design_File);
         end loop;
         Last_Design_File := Design_File;
      end if;

      if Design_File /= Null_Iir
        and then New_Lib_Checksum /= No_File_Checksum_Id
        and then not Files_Map.Is_Eq (New_Lib_Checksum,
                                      Get_File_Checksum (Design_File))
      then
         --  FIXME: this test is not enough: what about reanalyzing
         --   unmodified files (this works only because the order is not
         --   changed).
         --  Design file is updated.
         --  Outdate all other units, overwrite the design_file.
         Set_File_Checksum (Design_File, New_Lib_Checksum);
         Design_Unit := Get_First_Design_Unit (Design_File);
         while Design_Unit /= Null_Iir loop
            if Design_Unit /= Unit then
               --  Mark other design unit as obsolete.
               Mark_Unit_Obsolete (Design_Unit);
               Remove_Unit_Hash (Design_Unit);
            else
               raise Internal_Error;
            end if;
            Prev_Design_Unit := Design_Unit;
            Design_Unit := Get_Chain (Design_Unit);

            --  Put it on the obsolete list so that it is always referenced.
            Set_Chain (Prev_Design_Unit, Obsoleted_Design_Units);
            Obsoleted_Design_Units := Prev_Design_Unit;
         end loop;
         Set_First_Design_Unit (Design_File, Null_Iir);
         Set_Last_Design_Unit (Design_File, Null_Iir);
      end if;

      if Design_File = Null_Iir then
         -- This is the first apparition of the design file.
         Design_File := Create_Iir (Iir_Kind_Design_File);
         Location_Copy (Design_File, Unit);

         Set_Design_File_Filename (Design_File, File_Name);
         Set_Design_File_Directory (Design_File, Dir_Name);

         Set_File_Checksum (Design_File, New_Lib_Checksum);
         Set_Parent (Design_File, Work_Library);
         Set_Chain (Design_File, Get_Design_File_Chain (Work_Library));
         Set_Design_File_Chain (Work_Library, Design_File);
      end if;

      --  Add DECL to DESIGN_FILE.
      Last_Unit := Get_Last_Design_Unit (Design_File);
      if Last_Unit = Null_Iir then
         pragma Assert (Get_First_Design_Unit (Design_File) = Null_Iir);
         Set_First_Design_Unit (Design_File, Unit);
      else
         pragma Assert (Get_First_Design_Unit (Design_File) /= Null_Iir);
         Set_Chain (Last_Unit, Unit);
      end if;
      Set_Last_Design_Unit (Design_File, Unit);
      Set_Design_File (Unit, Design_File);

      --  Add DECL in unit hash table.
      Set_Hash_Chain (Unit, Unit_Hash_Table (Id));
      Unit_Hash_Table (Id) := Unit;

      --  Update the analyzed time stamp.
      Set_Analysis_Time_Stamp (Design_File, Files_Map.Get_Os_Time_Stamp);
   end Add_Design_Unit_Into_Library;

   procedure Add_Design_File_Into_Library (File : in out Iir_Design_File)
   is
      Unit : Iir_Design_Unit;
      Next_Unit : Iir_Design_Unit;
      First_Unit : Iir_Design_Unit;
   begin
      Unit := Get_First_Design_Unit (File);
      First_Unit := Unit;
      Set_First_Design_Unit (File, Null_Iir);
      Set_Last_Design_Unit (File, Null_Iir);
      while Unit /= Null_Iir loop
         Next_Unit := Get_Chain (Unit);
         Set_Chain (Unit, Null_Iir);
         Libraries.Add_Design_Unit_Into_Library (Unit, True);
         Unit := Next_Unit;
      end loop;
      if First_Unit /= Null_Iir then
         File := Get_Design_File (First_Unit);
      end if;
   end Add_Design_File_Into_Library;

   -- Save the file map of library LIBRARY.
   procedure Save_Library (Library: Iir_Library_Declaration)
   is
      use System;
      use Interfaces.C_Streams;
      use GNAT.OS_Lib;
      Temp_Name: constant String := Image (Work_Directory)
        & '_' & Library_To_File_Name (Library) & ASCII.NUL;
      Mode : constant String := 'w' & ASCII.NUL;
      Stream : FILEs;
      Success : Boolean;

      --  Write a string to the temporary file.
      procedure WR (S : String)
      is
         Close_Res : int;
         pragma Unreferenced (Close_Res);
      begin
         if Integer (fwrite (S'Address, S'Length, 1, Stream)) /= 1 then
            Error_Lib_Msg
              ("cannot write library file for " & Image_Identifier (Library));
            Close_Res := fclose (Stream);
            Delete_File (Temp_Name'Address, Success);
            --  Ignore failure to delete the file.
            raise Option_Error;
         end if;
      end WR;

      --  Write a line terminator in the temporary file.
      procedure WR_LF is
      begin
         WR (String'(1 => ASCII.LF));
      end WR_LF;

      Design_File: Iir_Design_File;
      Design_Unit: Iir_Design_Unit;
      Library_Unit: Iir;
      Dir : Name_Id;

      Off, Line: Natural;
      Pos: Source_Ptr;
      Source_File : Source_File_Entry;
   begin
      --  Create a temporary file so that the real library is atomically
      --  updated, and won't be corrupted in case of Control-C, or concurrent
      --  writes.
      Stream := fopen (Temp_Name'Address, Mode'Address);

      if Stream = NULL_Stream then
         Error_Lib_Msg
           ("cannot create library file for " & Image_Identifier (Library));
         raise Option_Error;
      end if;

      --  Header: version.
      WR ("v 4");
      WR_LF;

      Design_File := Get_Design_File_Chain (Library);
      while Design_File /= Null_Iir loop
         --  Ignore std.standard as there is no corresponding file.
         if Design_File = Vhdl.Std_Package.Std_Standard_File then
            goto Continue;
         end if;
         Design_Unit := Get_First_Design_Unit (Design_File);

         if Design_Unit /= Null_Iir then
            WR ("file ");
            Dir := Get_Design_File_Directory (Design_File);
            if Dir = Null_Identifier then
               --  Absolute filenames.
               WR ("/");
            elsif Work_Directory = Name_Nil
              and then Dir = Files_Map.Get_Home_Directory
            then
               --  If the library is in the current directory, do not write
               --  it.  This allows to move the library file.
               WR (".");
            else
               WR ("""");
               WR (Image (Dir));
               WR ("""");
            end if;
            WR (" """);
            WR (Image (Get_Design_File_Filename (Design_File)));
            WR (""" """);
            WR (Files_Map.Get_File_Checksum_String
                  (Get_File_Checksum (Design_File)));
            WR (""" """);
            WR (Files_Map.Get_Time_Stamp_String
                  (Get_Analysis_Time_Stamp (Design_File)));
            WR (""":");
            WR_LF;
         end if;

         while Design_Unit /= Null_Iir loop
            Library_Unit := Get_Library_Unit (Design_Unit);

            WR ("  ");
            case Get_Kind (Library_Unit) is
               when Iir_Kind_Entity_Declaration =>
                  WR ("entity ");
                  WR (Image_Identifier (Library_Unit));
               when Iir_Kind_Architecture_Body =>
                  WR ("architecture ");
                  WR (Image_Identifier (Library_Unit));
                  WR (" of ");
                  WR (Image (Get_Entity_Identifier_Of_Architecture
                               (Library_Unit)));
               when Iir_Kind_Package_Declaration
                 | Iir_Kind_Package_Instantiation_Declaration =>
                  WR ("package ");
                  WR (Image_Identifier (Library_Unit));
               when Iir_Kind_Package_Body =>
                  WR ("package body ");
                  WR (Image_Identifier (Library_Unit));
               when Iir_Kind_Configuration_Declaration =>
                  WR ("configuration ");
                  WR (Image_Identifier (Library_Unit));
               when Iir_Kind_Context_Declaration =>
                  WR ("context ");
                  WR (Image_Identifier (Library_Unit));
               when Iir_Kind_Vunit_Declaration =>
                  WR ("vunit ");
                  WR (Image_Identifier (Library_Unit));
               when Iir_Kind_Vprop_Declaration =>
                  WR ("vprop ");
                  WR (Image_Identifier (Library_Unit));
               when Iir_Kind_Vmode_Declaration =>
                  WR ("vmode ");
                  WR (Image_Identifier (Library_Unit));
               when others =>
                  Error_Kind ("save_library", Library_Unit);
            end case;

            if Get_Date_State (Design_Unit) = Date_Disk then
               Pos := Get_Design_Unit_Source_Pos (Design_Unit);
               Line := Natural (Get_Design_Unit_Source_Line (Design_Unit));
               Off := Natural (Get_Design_Unit_Source_Col (Design_Unit));
            else
               Files_Map.Location_To_Coord (Get_Location (Design_Unit),
                                            Source_File, Pos, Line, Off);
            end if;

            WR (" at");
            WR (Natural'Image (Line));
            WR ("(");
            WR (Source_Ptr'Image (Pos));
            WR (") +");
            WR (Natural'Image (Off));
            WR (" on");
            WR (Date_Type'Image (Get_Date (Design_Unit)));
            case Get_Date (Design_Unit) is
               when Date_Valid
                 | Date_Obsolete
                 | Date_Analyzed
                 | Date_Parsed =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;
            if Get_Kind (Library_Unit) = Iir_Kind_Package_Declaration
              and then Get_Need_Body (Library_Unit)
            then
               WR (" body");
            end if;
            WR (";");
            WR_LF;

            Design_Unit := Get_Chain (Design_Unit);
         end loop;
         << Continue >> null;
         Design_File := Get_Chain (Design_File);
      end loop;

      declare
         Fclose_Res : int;
         pragma Unreferenced (Fclose_Res);
      begin
         Fclose_Res := fclose (Stream);
      end;

      --  Rename the temporary file to the library file.
      --  FIXME: It may fail if they aren't on the same filesystem, but we
      --  could assume it doesn't happen (humm...)
      declare
         File_Name: constant String := Image (Work_Directory)
           & Library_To_File_Name (Library) & ASCII.NUL;
         Delete_Success : Boolean;
      begin
         --  For windows: renames doesn't overwrite destination; so first
         --  delete it. This can create races condition on Unix: if the
         --  program is killed between delete and rename, the library is lost.
         Delete_File (File_Name'Address, Delete_Success);
         Rename_File (Temp_Name'Address, File_Name'Address, Success);
         if not Success then
            --  Renaming may fail if the new filename is in a non-existant
            --  directory.
            Error_Lib_Msg
              ("cannot update library file """
                 & File_Name (File_Name'First .. File_Name'Last - 1)
                 & """");
            Delete_File (Temp_Name'Address, Success);
            raise Option_Error;
         end if;
      end;
   end Save_Library;

   -- Save the map of the work library.
   procedure Save_Work_Library is
   begin
      Save_Library (Work_Library);
   end Save_Work_Library;

   -- Return the name of the latest architecture analysed for an entity.
   function Get_Latest_Architecture (Entity: Iir_Entity_Declaration)
                                    return Iir_Architecture_Body
   is
      Entity_Id : Name_Id;
      Lib : Iir_Library_Declaration;
      Design_File: Iir_Design_File;
      Design_Unit: Iir_Design_Unit;
      Library_Unit: Iir;
      Res: Iir_Design_Unit;
   begin
      --  FIXME: use hash
      Entity_Id := Get_Identifier (Entity);
      Lib := Get_Library (Get_Design_File (Get_Design_Unit (Entity)));
      Design_File := Get_Design_File_Chain (Lib);
      Res := Null_Iir;
      while Design_File /= Null_Iir loop
         Design_Unit := Get_First_Design_Unit (Design_File);
         while Design_Unit /= Null_Iir loop
            if Get_Kind (Design_Unit) = Iir_Kind_Design_Unit then
               Library_Unit := Get_Library_Unit (Design_Unit);

               if Get_Kind (Library_Unit) = Iir_Kind_Architecture_Body
                 and then (Get_Entity_Identifier_Of_Architecture (Library_Unit)
                             = Entity_Id)
               then
                  if Res = Null_Iir then
                     Res := Design_Unit;
                  elsif Get_Date (Design_Unit) > Get_Date (Res) then
                     Res := Design_Unit;
                  end if;
               end if;
            end if;
            Design_Unit := Get_Chain (Design_Unit);
         end loop;
         Design_File := Get_Chain (Design_File);
      end loop;
      if Res = Null_Iir then
         return Null_Iir;
      else
         return Get_Library_Unit (Res);
      end if;
   end Get_Latest_Architecture;

   --  Return the declaration of primary unit NAME of LIBRARY.
   function Find_Primary_Unit
     (Library: Iir_Library_Declaration; Name: Name_Id) return Iir_Design_Unit
   is
      Unit : Iir_Design_Unit;
      Lib_Unit : Iir;
   begin
      Unit := Unit_Hash_Table (Name mod Unit_Hash_Length);
      while Unit /= Null_Iir loop
         if Get_Identifier (Unit) = Name
           and then Get_Library (Get_Design_File (Unit)) = Library
         then
            Lib_Unit := Get_Library_Unit (Unit);
            case Iir_Kinds_Library_Unit (Get_Kind (Lib_Unit)) is
               when Iir_Kinds_Primary_Unit
                  | Iir_Kind_Foreign_Module =>
                  --  Only return a primary unit.
                  return Unit;
               when Iir_Kinds_Secondary_Unit =>
                  null;
            end case;
         end if;
         Unit := Get_Hash_Chain (Unit);
      end loop;

      -- The primary unit is not in the library, return null.
      return Null_Iir;
   end Find_Primary_Unit;

   -- Return the declaration of secondary unit NAME for PRIMARY, or null if
   -- not found.
   function Find_Secondary_Unit (Primary: Iir_Design_Unit; Name: Name_Id)
      return Iir_Design_Unit
   is
      Lib_Prim : constant Iir := Get_Library (Get_Design_File (Primary));
      Primary_Ident : constant Name_Id :=
        Get_Identifier (Get_Library_Unit (Primary));
      Design_Unit: Iir_Design_Unit;
      Library_Unit: Iir;
   begin
      Design_Unit := Unit_Hash_Table (Primary_Ident mod Unit_Hash_Length);
      while Design_Unit /= Null_Iir loop

         --  The secondary is always in the same library as the primary.
         if Get_Kind (Design_Unit) /= Iir_Kind_Foreign_Module
           and then Get_Library (Get_Design_File (Design_Unit)) = Lib_Prim
         then
            Library_Unit := Get_Library_Unit (Design_Unit);
            -- Set design_unit to null iff this is not the correct
            -- design unit.
            case Get_Kind (Library_Unit) is
               when Iir_Kind_Architecture_Body =>
                  -- The entity field can be either an identifier (if the
                  -- library unit was not loaded) or an access to the entity
                  -- unit.
                  if (Get_Entity_Identifier_Of_Architecture (Library_Unit)
                        = Primary_Ident)
                    and then Get_Identifier (Library_Unit) = Name
                  then
                     return Design_Unit;
                  end if;
               when Iir_Kind_Package_Body =>
                  if Name = Null_Identifier
                    and then Get_Identifier (Library_Unit) = Primary_Ident
                  then
                     return Design_Unit;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         Design_Unit := Get_Hash_Chain (Design_Unit);
      end loop;

      -- The architecture or the body is not in the library, return null.
      return Null_Iir;
   end Find_Secondary_Unit;

   function Find_Entity_For_Component (Name: Name_Id) return Iir_Design_Unit
   is
      Res : Iir_Design_Unit := Null_Iir;
      Unit : Iir_Design_Unit;
   begin
      Res := Null_Iir;
      Unit := Unit_Hash_Table (Name mod Unit_Hash_Length);
      while Unit /= Null_Iir loop
         if Get_Identifier (Unit) = Name then
            case Get_Kind (Get_Library_Unit (Unit)) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Foreign_Module =>
                  if Res /= Null_Iir then
                     --  Many entities.
                     return Null_Iir;
                  else
                     Res := Unit;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         Unit := Get_Hash_Chain (Unit);
      end loop;

      return Res;
   end Find_Entity_For_Component;

   function Get_Libraries_Chain return Iir_Library_Declaration is
   begin
      return Libraries_Chain;
   end Get_Libraries_Chain;

   function Decode_Work_Option (Opt : String) return Name_Id
   is
      Name : String (Opt'First + 7 .. Opt'Last);
      Err : Boolean;
   begin
      Name := Opt (Opt'First + 7 .. Opt'Last);
      Vhdl.Scanner.Convert_Identifier (Name, Err);
      if Err then
         return Null_Identifier;
      end if;
      return Get_Identifier (Name);
      -- Libraries.Work_Library_Name :=
   end Decode_Work_Option;
end Libraries;
