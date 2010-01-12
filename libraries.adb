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
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Table;
with GNAT.OS_Lib;
with Errorout; use Errorout;
with Scan;
with Iirs_Utils;
with Parse;
with Back_End;
with Name_Table; use Name_Table;
with Str_Table;
with Sem_Scopes;
with Tokens;
with Files_Map;
with Flags;
with Std_Package;

package body Libraries is
   --  Chain of known libraries.  This is also the top node of all iir node.
   Libraries_Chain : Iir_Library_Declaration := Null_Iir;
   Libraries_Chain_Last : Iir_Library_Declaration := Null_Iir;

   --  A location for any implicit declarations (such as library WORK).
   Implicit_Location: Location_Type;

   --  Table of library pathes.
   package Pathes is new GNAT.Table
     (Table_Index_Type => Integer,
      Table_Component_Type => Name_Id,
      Table_Low_Bound => 1,
      Table_Initial => 4,
      Table_Increment => 100);

   --  Initialize pathes table.
   --  Set the local path.
   procedure Init_Pathes
   is
   begin
      Name_Nil := Get_Identifier ("");
      Pathes.Append (Name_Nil);
      Local_Directory := Name_Nil;
      Work_Directory := Name_Nil;
   end Init_Pathes;

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
      --  Nice message instead of constraint_error.
      if Path'Length + 2 >= Name_Buffer'Length then
         Error_Msg ("argument of -P is too long");
         return;
      end if;
      Pathes.Append (Path_To_Id (Path));
   end Add_Library_Path;

   function Get_Nbr_Pathes return Natural is
   begin
      return Pathes.Last;
   end Get_Nbr_Pathes;

   function Get_Path (N : Natural) return Name_Id is
   begin
      if N > Pathes.Last or N < Pathes.First then
         raise Constraint_Error;
      end if;
      return Pathes.Table (N);
   end Get_Path;

   --  Set PATH as the path of the work library.
   procedure Set_Work_Library_Path (Path : String) is
   begin
      Work_Directory := Path_To_Id (Path);
      if not GNAT.OS_Lib.Is_Directory (Get_Address (Work_Directory)) then
         --  This is a warning, since 'clean' action should not fail in
         --  this cases.
         Warning_Msg
           ("directory '" & Path & "' set by --workdir= does not exist");
         --  raise Option_Error;
      end if;
   end Set_Work_Library_Path;

   --  Open LIBRARY map file, return TRUE if successful.
   function Set_Library_File_Name (Dir : Name_Id;
                                   Library: Iir_Library_Declaration)
     return Boolean
   is
      File_Name : constant String := Back_End.Library_To_File_Name (Library);
      Fe : Source_File_Entry;
   begin
      Fe := Files_Map.Load_Source_File (Dir, Get_Identifier (File_Name));
      if Fe = No_Source_File_Entry then
         return False;
      end if;
      Scan.Set_File (Fe);
      return True;
   end Set_Library_File_Name;

   --  Every design unit is put in this hash table to be quickly found by
   --  its (primary) identifier.
   Unit_Hash_Length : constant Name_Id := 127;
   subtype Hash_Id is Name_Id range 0 .. Unit_Hash_Length - 1;
   Unit_Hash_Table : array (Hash_Id) of Iir := (others => Null_Iir);

   --  Get the hash value for DESIGN_UNIT.
   --  Architectures use the entity name.
   function Get_Hash_Id_For_Unit (Design_Unit : Iir_Design_Unit)
                                 return Hash_Id
   is
      Lib_Unit : Iir;
      Id : Name_Id;
   begin
      Lib_Unit := Get_Library_Unit (Design_Unit);
      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body =>
            Id := Get_Identifier (Lib_Unit);
         when Iir_Kind_Architecture_Declaration =>
            --  Architectures are put with the entity identifier.
            Id := Get_Identifier (Get_Entity (Lib_Unit));
         when others =>
            Error_Kind ("get_id_for_unit_hash", Lib_Unit);
      end case;
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

      File_Name : Name_Id;
      Dir_Name : Name_Id;
   begin
      File_Name := Get_Design_File_Filename (Design_File);
      Dir_Name := Get_Design_File_Directory (Design_File);

      File := Get_Design_File_Chain (Work_Library);
      Prev := Null_Iir;
      while File /= Null_Iir loop
         Next := Get_Chain (File);
         if Get_Design_File_Filename (File) = File_Name
           and then Get_Design_File_Directory (File) = Dir_Name
         then
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

            return;
         end if;
         Prev := File;
         File := Next;
      end loop;
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
   --      FILE directory "FILENAME" file_time_stamp analyze_time_stamp:
   -- design_unit_format ::= entity_format
   --                        | architecture_format
   --                        | package_format
   --                        | package_body_format
   --                        | configuration_format
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
   --
   -- The position_format meaning is:
   --       LINE is the line number (first line is number 1),
   --       POS is the offset of this line number, as a source_ptr value,
   --       OFF is the offset in the line, starting with 0.
   --       DATE is the symbolic date of analysis (order).
   --
   -- Return TRUE if the library was found.
   function Load_Library (Library: Iir_Library_Declaration)
     return Boolean
   is
      use Scan;
      use Tokens;
      use Iirs_Utils;

      File : Source_File_Entry;

      procedure Bad_Library_Format is
      begin
         Error_Msg (Image (Files_Map.Get_File_Name (File)) &
                    ": bad library format");
      end Bad_Library_Format;

      procedure Scan_Expect (Tok: Token_Type) is
      begin
         Scan.Scan;
         if Current_Token /= Tok then
            Bad_Library_Format;
            raise Compilation_Error;
         end if;
      end Scan_Expect;

      function Current_Time_Stamp return Time_Stamp_Id is
      begin
         if Current_String_Length /= Time_Stamp_String'Length then
            Bad_Library_Format;
            raise Compilation_Error;
         end if;
         return Time_Stamp_Id (Current_String_Id);
      end Current_Time_Stamp;

      function String_To_Name_Id return Name_Id
      is
         Len : Int32;
         Ptr : String_Fat_Acc;
      begin
         Len := Current_String_Length;
         Ptr := Str_Table.Get_String_Fat_Acc (Current_String_Id);
         for I in 1 .. Len loop
            Name_Table.Name_Buffer (Natural (I)) := Ptr (I);
         end loop;
         Name_Table.Name_Length := Natural (Len);
         --  FIXME: should remove last string.
         return Get_Identifier;
      end String_To_Name_Id;

      Design_Unit, Last_Design_Unit : Iir_Design_Unit;
      Lib_Ident : Name_Id;

      function Scan_Unit_List return Iir_List is
      begin
         if Current_Token = Tok_Left_Paren then
            Scan_Expect (Tok_Identifier);
            loop
               Scan_Expect (Tok_Dot);
               Scan_Expect (Tok_Identifier);
               Scan.Scan;
               if Current_Token = Tok_Left_Paren then
                  --  This is an architecture.
                  Scan_Expect (Tok_Identifier);
                  Scan_Expect (Tok_Right_Paren);
                  Scan.Scan;
               end if;
               exit when Current_Token /= Tok_Comma;
               Scan.Scan;
            end loop;
            Scan.Scan;
         end if;
         return Null_Iir_List;
      end Scan_Unit_List;

      Design_File: Iir_Design_File;
      Library_Unit: Iir;
      Line, Col: Natural;
      File_Dir : Name_Id;
      Pos: Source_Ptr;
      Date: Date_Type;
      Max_Date: Date_Type := Date_Valid'First;
      Dir : Name_Id;
   begin
      Lib_Ident := Get_Identifier (Library);

      if False then
         Ada.Text_IO.Put_Line ("Load library " & Image (Lib_Ident));
      end if;

      -- Check the library was not already loaded.
      if Get_Design_File_Chain (Library) /= Null_Iir then
         raise Internal_Error;
      end if;

      -- Try to open the library file map.
      Dir := Get_Library_Directory (Library);
      if Dir = Null_Identifier then
         --  Search in the library path.
         declare
            File_Name : constant String :=
              Back_End.Library_To_File_Name (Library);
            L : Natural;
         begin
            for I in Pathes.First .. Pathes.Last loop
               Image (Pathes.Table (I));
               L := Name_Length + File_Name'Length;
               Name_Buffer (Name_Length + 1 .. L) := File_Name;
               Name_Buffer (L + 1) := Character'Val (0);
               if GNAT.OS_Lib.Is_Regular_File (Name_Buffer'Address) then
                  Dir := Pathes.Table (I);
                  Set_Library_Directory (Library, Dir);
                  exit;
               end if;
            end loop;
         end;
      end if;
      if Dir = Null_Identifier
        or else not Set_Library_File_Name (Dir, Library)
      then
         --  Not found.
         Set_Date (Library, Date_Valid'First);
         return False;
      end if;
      File := Get_Current_Source_File;

      --  Parse header.
      Scan.Scan;
      if Current_Token /= Tok_Identifier
        or else Name_Length /= 1 or else Name_Buffer (1) /= 'v'
      then
         Bad_Library_Format;
         raise Compilation_Error;
      end if;
      Scan_Expect (Tok_Integer);
      if Current_Iir_Int64 not in 1 .. 3 then
         Bad_Library_Format;
         raise Compilation_Error;
      end if;
      Scan.Scan;

      Last_Design_Unit := Null_Iir;
      while Current_Token /= Tok_Eof loop
         if Current_Token = Tok_File then
            -- This is a new design file.
            Design_File := Create_Iir (Iir_Kind_Design_File);

            Scan.Scan;
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
               --  Be compatible with version 1: an empty directory for
               --  an absolute filename.
               if Current_String_Length = 0 then
                  File_Dir := Null_Identifier;
               else
                  File_Dir := String_To_Name_Id;
               end if;
            else
               Bad_Library_Format;
               raise Compilation_Error;
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
            Set_File_Time_Stamp (Design_File, Current_Time_Stamp);

            Scan_Expect (Tok_String);
            Set_Analysis_Time_Stamp (Design_File, Current_Time_Stamp);

            Scan_Expect (Tok_Colon);
            Scan.Scan;
            Last_Design_Unit := Null_Iir;
         else
            -- This is a new design unit.
            Design_Unit := Create_Iir (Iir_Kind_Design_Unit);
            Set_Design_File (Design_Unit, Design_File);
            case Current_Token is
               when Tok_Entity =>
                  Library_Unit := Create_Iir (Iir_Kind_Entity_Declaration);
                  Scan.Scan;
               when Tok_Architecture =>
                  Library_Unit :=
                    Create_Iir (Iir_Kind_Architecture_Declaration);
                  Scan.Scan;
               when Tok_Configuration =>
                  Library_Unit :=
                    Create_Iir (Iir_Kind_Configuration_Declaration);
                  Scan.Scan;
               when Tok_Package =>
                  Scan.Scan;
                  if Current_Token = Tok_Body then
                     Library_Unit := Create_Iir (Iir_Kind_Package_Body);
                     Scan.Scan;
                  else
                     Library_Unit := Create_Iir (Iir_Kind_Package_Declaration);
                  end if;
               when Tok_With =>
                  if Library_Unit = Null_Iir
                    or else
                    Get_Kind (Library_Unit)
                    /= Iir_Kind_Architecture_Declaration
                  then
                     Put_Line ("load_library: invalid use of 'with'");
                     raise Internal_Error;
                  end if;
                  Scan_Expect (Tok_Configuration);
                  Scan_Expect (Tok_Colon);
                  Scan.Scan;
                  Set_Dependence_List (Design_Unit, Scan_Unit_List);
                  goto Next_Line;
               when others =>
                  Put_Line
                    ("load_library: line must start with " &
                     "'architecture', 'entity', 'package' or 'configuration'");
                  raise Internal_Error;
            end case;

            if Current_Token /= Tok_Identifier then
               raise Internal_Error;
            end if;
            Set_Identifier (Library_Unit, Current_Identifier);
            Set_Identifier (Design_Unit, Current_Identifier);
            Set_Visible_Flag (Design_Unit, True);

            if Get_Kind (Library_Unit) = Iir_Kind_Architecture_Declaration then
               Scan_Expect (Tok_Of);
               Scan_Expect (Tok_Identifier);
               Set_Entity (Library_Unit, Current_Text);
            end if;

            -- Scan position.
            Scan_Expect (Tok_Identifier); -- at
            Scan_Expect (Tok_Integer);
            Line := Natural (Current_Iir_Int64);
            Scan_Expect (Tok_Left_Paren);
            Scan_Expect (Tok_Integer);
            Pos := Source_Ptr (Current_Iir_Int64);
            Scan_Expect (Tok_Right_Paren);
            Scan_Expect (Tok_Plus);
            Scan_Expect (Tok_Integer);
            Col := Natural (Current_Iir_Int64);
            Scan_Expect (Tok_On);
            Scan_Expect (Tok_Integer);
            Date := Date_Type (Current_Iir_Int64);

            Scan.Scan;
            if Get_Kind (Library_Unit) = Iir_Kind_Package_Declaration
              and then Current_Token = Tok_Body
            then
               Set_Need_Body (Library_Unit, True);
               Scan.Scan;
            end if;
            if Current_Token /= Tok_Semi_Colon then
               raise Internal_Error;
            end if;
            Scan.Scan;

            if False then
               Put_Line ("line:" & Natural'Image (Line)
                         & ", pos:" & Source_Ptr'Image (Pos));
            end if;

            -- Scan dependence list.
            Set_Dependence_List (Design_Unit, Scan_Unit_List);

            -- Keep the position of the design unit.
            --Set_Location (Design_Unit, Location_Type (File));
            --Set_Location (Library_Unit, Location_Type (File));
            Set_Pos_Line_Off (Design_Unit, Pos, Line, Col);
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
         << Next_Line >> null;
      end loop;
      Set_Date (Library, Max_Date);
      Close_File;
      return True;
   end Load_Library;

   procedure Create_Virtual_Locations
   is
      use Files_Map;
      Implicit_Source_File : Source_File_Entry;
      Command_Source_File : Source_File_Entry;
   begin
      Implicit_Source_File := Create_Virtual_Source_File
        (Get_Identifier ("*implicit*"));
      Command_Source_File := Create_Virtual_Source_File
        (Get_Identifier ("*command line*"));
      Command_Line_Location := Source_File_To_Location (Command_Source_File);
      Implicit_Location := Source_File_To_Location (Implicit_Source_File);
   end Create_Virtual_Locations;

   -- Note: the scanner shouldn't be in use, since this procedure uses it.
   procedure Load_Std_Library (Build_Standard : Boolean := True)
   is
      use Std_Package;
      Dir : Name_Id;
   begin
      if Libraries_Chain /= Null_Iir then
         --  This procedure must not be called twice.
         raise Internal_Error;
      end if;

      Flags.Create_Flag_String;
      Create_Virtual_Locations;

      Std_Package.Create_First_Nodes;

      --  Create the library.
      Std_Library := Create_Iir (Iir_Kind_Library_Declaration);
      Set_Identifier (Std_Library, Std_Names.Name_Std);
      Set_Location (Std_Library, Implicit_Location);
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
      end if;

      if Build_Standard then
         --  Add the standard_file into the library.
         --  This is done after Load_Library, because it checks there is no
         --  previous files in the library.
         Set_Parent (Std_Standard_File, Std_Library);
         Set_Chain (Std_Standard_File, Get_Design_File_Chain (Std_Library));
         Set_Design_File_Chain (Std_Library, Std_Standard_File);
      end if;

      Set_Visible_Flag (Std_Library, True);
   end Load_Std_Library;

   procedure Load_Work_Library (Empty : Boolean := False)
   is
      use Std_Names;
   begin
      if Work_Library_Name = Name_Std then
         if not Flags.Bootstrap then
            Error_Msg_Option ("the WORK library cannot be STD");
            return;
         end if;
         Work_Library := Std_Library;
      else
         Work_Library := Create_Iir (Iir_Kind_Library_Declaration);
         Set_Location (Work_Library, Implicit_Location);
         --Set_Visible_Flag (Work_Library, True);
         Set_Library_Directory (Work_Library, Work_Directory);

         Set_Identifier (Work_Library, Work_Library_Name);

         if not Empty then
            if Load_Library (Work_Library) = False then
               null;
            end if;
         end if;

         --  Add it to the list of libraries.
         Set_Chain (Libraries_Chain_Last, Work_Library);
         Libraries_Chain_Last := Work_Library;
      end if;
      Set_Visible_Flag (Work_Library, True);
   end Load_Work_Library;

--    procedure Unload_Library (Library : Iir_Library_Declaration)
--    is
--       File : Iir_Design_File;
--       Unit : Iir_Design_Unit;
--    begin
--       loop
--          File := Get_Design_File_Chain (Library);
--          exit when File = Null_Iir;
--          Set_Design_File_Chain (Library, Get_Chain (File));

--          loop
--             Unit := Get_Design_Unit_Chain (File);
--             exit when Unit = Null_Iir;
--             Set_Design_Unit_Chain (File, Get_Chain (Unit));

--             --  Units should not be loaded.
--             if Get_Loaded_Flag (Unit) then
--                raise Internal_Error;
--             end if;

--             --  Free dependences list.
--          end loop;
--       end loop;
--    end Unload_Library;

--    procedure Unload_All_Libraries
--    is
--       Library : Iir_Library_Declaration;
--    begin
--       if Get_Identifier (Std_Library) /= Name_Std then
--          raise Internal_Error;
--       end if;
--       if Std_Library /= Libraries_Chain then
--          raise Internal_Error;
--       end if;
--       loop
--          Library := Get_Chain (Libraries_Chain);
--          exit when Library = Null_Iir;
--          Set_Chain (Libraries_Chain, Get_Chain (Libraries_Chain));
--          Unload_Library (Library);
--       end loop;
--    end Unload_All_Libraries;

   -- Get or create a library from an identifier.
   function Get_Library (Ident: Name_Id; Loc : Location_Type)
                        return Iir_Library_Declaration
   is
      Library: Iir_Library_Declaration;
   begin
      -- library work is a little bit special.
      if Ident = Std_Names.Name_Work or else Ident = Work_Library_Name then
         if Work_Library = Null_Iir then
            --  load_work_library must have been called before.
            raise Internal_Error;
         end if;
         return Work_Library;
      end if;

      --  Check if the library has already been loaded.
      Library := Iirs_Utils.Find_Name_In_Chain (Libraries_Chain, Ident);
      if Library /= Null_Iir then
         return Library;
      end if;

      --  This is a new library.
      if Ident = Std_Names.Name_Std then
         --  Load_std_library must have been called before.
         raise Internal_Error;
      end if;

      Library := Create_Iir (Iir_Kind_Library_Declaration);
      Set_Location (Library, Scan.Get_Token_Location);
      Set_Library_Directory (Library, Null_Identifier);
      Set_Identifier (Library, Ident);
      if Load_Library (Library) = False then
         Error_Msg_Sem ("cannot find resource library """
                        & Name_Table.Image (Ident) & """", Loc);
      end if;
      Set_Visible_Flag (Library, True);

      Set_Chain (Libraries_Chain_Last, Library);
      Libraries_Chain_Last := Library;

      return Library;
   end Get_Library;

   -- Return TRUE if LIBRARY_UNIT and UNIT have identifiers for the same
   -- design unit identifier.
   -- eg: 'entity A' and 'package A' returns TRUE.
   function Is_Same_Library_Unit (Library_Unit, Unit: Iir) return Boolean
   is
      Entity_Name1, Entity_Name2: Name_Id;
      Library_Unit_Kind, Unit_Kind : Iir_Kind;
   begin
      if Get_Identifier (Unit) /= Get_Identifier (Library_Unit) then
         return False;
      end if;

      Library_Unit_Kind := Get_Kind (Library_Unit);
      Unit_Kind := Get_Kind (Unit);

      --  Package and package body are never the same library unit.
      if Library_Unit_Kind = Iir_Kind_Package_Declaration
        and then Unit_Kind = Iir_Kind_Package_Body
      then
         return False;
      end if;
      if Unit_Kind = Iir_Kind_Package_Declaration
        and then Library_Unit_Kind = Iir_Kind_Package_Body
      then
         return False;
      end if;

      --  Two architecture declarations are identical only if they also have
      --  the same entity name.
      if Unit_Kind = Iir_Kind_Architecture_Declaration
        and then Library_Unit_Kind = Iir_Kind_Architecture_Declaration
      then
         Entity_Name1 := Get_Identifier (Get_Entity (Unit));
         Entity_Name2 := Get_Identifier (Get_Entity (Library_Unit));
         if Entity_Name1 /= Entity_Name2 then
            return False;
         end if;
      end if;

      --  An architecture declaration never conflits with a library unit that
      --  is not an architecture declaration.
      if (Unit_Kind = Iir_Kind_Architecture_Declaration
          and then Library_Unit_Kind /= Iir_Kind_Architecture_Declaration)
        or else
        (Unit_Kind /= Iir_Kind_Architecture_Declaration
         and then Library_Unit_Kind = Iir_Kind_Architecture_Declaration)
      then
         return False;
      end if;

      return True;
   end Is_Same_Library_Unit;

   procedure Free_Dependence_List (Design : Iir_Design_Unit)
   is
      List : Iir_List;
      El : Iir;
   begin
      List := Get_Dependence_List (Design);
      if List /= Null_Iir_List then
         for I in Natural loop
            El := Get_Nth_Element (List, I);
            exit when El = Null_Iir;
            Iirs_Utils.Free_Recursive (El);
         end loop;
         Destroy_Iir_List (List);
      end if;
   end Free_Dependence_List;

   procedure Free_Design_Unit (Design_Unit : Iir_Design_Unit)
   is
      Lib : Iir;
      Unit : Iir_Design_Unit;
      Dep_List : Iir_List;
   begin
      Dep_List := Get_Dependence_List (Design_Unit);
      Destroy_Iir_List (Dep_List);
      Lib := Get_Library_Unit (Design_Unit);
      if Lib /= Null_Iir
        and then Get_Kind (Lib) = Iir_Kind_Architecture_Declaration
      then
         Unit := Get_Default_Configuration_Declaration (Lib);
         if Unit /= Null_Iir then
            Free_Design_Unit (Unit);
         end if;
      end if;
      Iirs_Utils.Free_Old_Iir (Lib);
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

   --  Last design_file used.  Kept to speed-up operations.
   Last_Design_File : Iir_Design_File := Null_Iir;

   -- Add or replace a design unit in the working library.
   procedure Add_Design_Unit_Into_Library (Unit : Iir_Design_Unit)
   is
      Design_File: Iir_Design_File;
      Design_Unit, Prev_Design_Unit : Iir_Design_Unit;
      Last_Unit : Iir_Design_Unit;
      Library_Unit: Iir;
      New_Library_Unit: Iir;
      Unit_Id : Name_Id;
      Date: Date_Type;
      New_Lib_Time_Stamp : Time_Stamp_Id;
      Id : Hash_Id;

      --  File name and dir name of DECL.
      File_Name : Name_Id;
      Dir_Name : Name_Id;
   begin
      pragma Assert (Get_Chain (Unit) = Null_Iir);

      if Get_Date_State (Unit) /= Date_Extern then
         raise Internal_Error;
      end if;

      --  Mark this design unit as being loaded.
      New_Library_Unit := Get_Library_Unit (Unit);
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
         File : Source_File_Entry;
         Pos : Source_Ptr;
      begin
         Files_Map.Location_To_File_Pos (Get_Location (New_Library_Unit),
                                         File, Pos);
         New_Lib_Time_Stamp := Files_Map.Get_File_Time_Stamp (File);
         File_Name := Files_Map.Get_File_Name (File);
         Image (File_Name);
         if Files_Map.Is_Absolute_Pathname (Name_Buffer (1 .. Name_Length))
         then
            Dir_Name := Null_Identifier;
         else
            Dir_Name := Files_Map.Get_Home_Directory;
         end if;
      end;

      --  Try to find a design unit with the same name in the work library.
      Id := Get_Hash_Id_For_Unit (Unit);
      Design_Unit := Unit_Hash_Table (Id);
      Prev_Design_Unit := Null_Iir;
      while Design_Unit /= Null_Iir loop
         Design_File := Get_Design_File (Design_Unit);
         Library_Unit := Get_Library_Unit (Design_Unit);
         if Get_Identifier (Design_Unit) = Unit_Id
           and then Get_Library (Design_File) = Work_Library
           and then Is_Same_Library_Unit (New_Library_Unit, Library_Unit)
         then
            --  LIBRARY_UNIT and UNIT designate the same design unit.
            --  Remove the old one.
            Set_Date (Design_Unit, Date_Obsolete);
            declare
               Next_Design : Iir;
            begin
               --  Remove DESIGN_UNIT from the unit_hash.
               Next_Design := Get_Hash_Chain (Design_Unit);
               if Prev_Design_Unit = Null_Iir then
                  Unit_Hash_Table (Id) := Next_Design;
               else
                  Set_Hash_Chain (Prev_Design_Unit, Next_Design);
               end if;

               --  Remove DESIGN_UNIT from the design_file.
               Remove_Unit_From_File (Design_Unit, Design_File);
            end;

            -- UNIT *must* replace library_unit if they don't belong
            -- to the same file.
            if Get_Design_File_Filename (Design_File) = File_Name
              and then Get_Design_File_Directory (Design_File) = Dir_Name
            then
               --  In the same file.
               if Get_Date_State (Design_Unit) = Date_Analyze then
                  --  Warns only if we are not re-analyzing the file.
                  if Flags.Warn_Library then
                     Warning_Msg_Sem
                       ("redefinition of a library unit in "
                        & "same design file:", Unit);
                     Warning_Msg_Sem
                       (Disp_Node (Library_Unit) & " defined at "
                        & Disp_Location (Library_Unit) & " is now "
                        & Disp_Node (New_Library_Unit), Unit);
                  end if;
               else
                  --  Free the stub.
                  Free_Design_Unit (Design_Unit);
               end if;

               --  Note: the current design unit should not be freed if
               --  in use; unfortunatly, this is not obvious to check.
            else
               if Flags.Warn_Library then
                  if Get_Kind (Library_Unit) /= Get_Kind (New_Library_Unit)
                  then
                     Warning_Msg ("changing definition of a library unit:");
                     Warning_Msg (Disp_Node (Library_Unit) & " is now "
                                  & Disp_Node (New_Library_Unit));
                  end if;
                  Warning_Msg
                    ("library unit '"
                     & Iirs_Utils.Image_Identifier (Library_Unit)
                     & "' was also defined in file '"
                     & Image (Get_Design_File_Filename (Design_File))
                     & ''');
               end if;
            end if;
            exit;
         end if;
         Prev_Design_Unit := Design_Unit;
         Design_Unit := Get_Hash_Chain (Design_Unit);
      end loop;

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
        and then not Files_Map.Is_Eq (New_Lib_Time_Stamp,
                                      Get_File_Time_Stamp (Design_File))
      then
         -- FIXME: this test is not enough: what about reanalyzing
         --  unmodified files (this works only because the order is not
         --  changed).
         -- Design file is updated.
         -- Outdate all other units, overwrite the design_file.
         Set_File_Time_Stamp (Design_File, New_Lib_Time_Stamp);
         Design_Unit := Get_First_Design_Unit (Design_File);
         while Design_Unit /= Null_Iir loop
            if Design_Unit /= Unit then
               --  Mark other design unit as obsolete.
               Set_Date (Design_Unit, Date_Obsolete);
               Remove_Unit_Hash (Design_Unit);
            else
               raise Internal_Error;
            end if;
            Design_Unit := Get_Chain (Design_Unit);
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

         Set_File_Time_Stamp (Design_File, New_Lib_Time_Stamp);
         Set_Parent (Design_File, Work_Library);
         Set_Chain (Design_File, Get_Design_File_Chain (Work_Library));
         Set_Design_File_Chain (Work_Library, Design_File);
      end if;

      --  Add DECL to DESIGN_FILE.
      Last_Unit := Get_Last_Design_Unit (Design_File);
      if Last_Unit = Null_Iir then
         if Get_First_Design_Unit (Design_File) /= Null_Iir then
            raise Internal_Error;
         end if;
         Set_First_Design_Unit (Design_File, Unit);
      else
         if Get_First_Design_Unit (Design_File) = Null_Iir then
            raise Internal_Error;
         end if;
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
         Libraries.Add_Design_Unit_Into_Library (Unit);
         Unit := Next_Unit;
      end loop;
      if First_Unit /= Null_Iir then
         File := Get_Design_File (First_Unit);
      end if;
   end Add_Design_File_Into_Library;

   -- Save the file map of library LIBRARY.
   procedure Save_Library (Library: Iir_Library_Declaration) is
      File: File_Type;

      Design_File: Iir_Design_File;
      Design_Unit: Iir_Design_Unit;
      Library_Unit: Iir;
      Dir : Name_Id;

      Off, Line: Natural;
      Pos: Source_Ptr;
      Source_File : Source_File_Entry;
   begin
      -- FIXME: directory
      declare
         use Files_Map;
         File_Name: constant String := Image (Work_Directory)
           & Back_End.Library_To_File_Name (Library);
      begin
         Create (File, Out_File, File_Name);
      exception
         when Use_Error =>
            Open (File, Out_File, File_Name);
         when Name_Error =>
            Error_Msg ("cannot create library file """ & File_Name & """");
            raise Option_Error;
      end;

      --  Header: version.
      Put_Line (File, "v 3");

      Design_File := Get_Design_File_Chain (Library);
      while Design_File /= Null_Iir loop
         if Design_File = Std_Package.Std_Standard_File then
            goto Continue;
         end if;
         Design_Unit := Get_First_Design_Unit (Design_File);

         if Design_Unit /= Null_Iir then
            Put (File, "file ");
            Dir := Get_Design_File_Directory (Design_File);
            if Dir = Null_Identifier then
               --  Absolute filenames.
               Put (File, "/");
            elsif Work_Directory = Name_Nil
              and then Dir = Files_Map.Get_Home_Directory
            then
               --  If the library is in the current directory, do not write
               --  it.  This allows to move the library file.
               Put (File, ".");
            else
               Image (Dir);
               Put (File, """");
               Put (File, Name_Buffer (1 .. Name_Length));
               Put (File, """");
            end if;
            Put (File, " """);
            Image (Get_Design_File_Filename (Design_File));
            Put (File, Name_Buffer (1 .. Name_Length));
            Put (File, """ """);
            Put (File, Files_Map.Get_Time_Stamp_String
                 (Get_File_Time_Stamp (Design_File)));
            Put (File, """ """);
            Put (File, Files_Map.Get_Time_Stamp_String
                 (Get_Analysis_Time_Stamp (Design_File)));
            Put_Line (File, """:");
         end if;

         while Design_Unit /= Null_Iir loop
            Library_Unit := Get_Library_Unit (Design_Unit);

            Put (File, "  ");
            case Get_Kind (Library_Unit) is
               when Iir_Kind_Entity_Declaration =>
                  Put (File, "entity ");
                  Put (File, Iirs_Utils.Image_Identifier (Library_Unit));
               when Iir_Kind_Architecture_Declaration =>
                  Put (File, "architecture ");
                  Put (File, Iirs_Utils.Image_Identifier (Library_Unit));
                  Put (File, " of ");
                  Put (File, Iirs_Utils.Image_Identifier
                       (Get_Entity (Library_Unit)));
               when Iir_Kind_Package_Declaration =>
                  Put (File, "package ");
                  Put (File, Iirs_Utils.Image_Identifier (Library_Unit));
               when Iir_Kind_Package_Body =>
                  Put (File, "package body ");
                  Put (File, Iirs_Utils.Image_Identifier (Library_Unit));
               when Iir_Kind_Configuration_Declaration =>
                  Put (File, "configuration ");
                  Put (File, Iirs_Utils.Image_Identifier (Library_Unit));
               when others =>
                  Error_Kind ("save_library", Library_Unit);
            end case;

            if Get_Date_State (Design_Unit) = Date_Disk then
               Get_Pos_Line_Off (Design_Unit, Pos, Line, Off);
            else
               Files_Map.Location_To_Coord (Get_Location (Design_Unit),
                                            Source_File, Pos, Line, Off);
            end if;

            Put (File, " at");
            Put (File, Natural'Image (Line));
            Put (File, "(");
            Put (File, Source_Ptr'Image (Pos));
            Put (File, ") +");
            Put (File, Natural'Image (Off));
            Put (File, " on");
            case Get_Date (Design_Unit) is
               when Date_Valid
                 | Date_Analyzed
                 | Date_Parsed =>
                  Put (File, Date_Type'Image (Get_Date (Design_Unit)));
               when others =>
                  Put_Line (Date_Type'Image (Get_Date (Design_Unit)));
                  raise Internal_Error;
            end case;
            if Get_Kind (Library_Unit) = Iir_Kind_Package_Declaration
              and then Get_Need_Body (Library_Unit)
            then
               Put (File, " body");
            end if;
            Put_Line (File, ";");

            Design_Unit := Get_Chain (Design_Unit);
         end loop;
         << Continue >> null;
         Design_File := Get_Chain (Design_File);
      end loop;

      Close (File);
   end Save_Library;

   -- Save the map of the work library.
   procedure Save_Work_Library is
   begin
      Save_Library (Work_Library);
   end Save_Work_Library;

   -- Return the name of the latest architecture analysed for an entity.
   function Get_Latest_Architecture (Entity: Iir_Entity_Declaration)
     return Iir_Architecture_Declaration
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
            Library_Unit := Get_Library_Unit (Design_Unit);

            if Get_Kind (Library_Unit) = Iir_Kind_Architecture_Declaration
              and then Get_Identifier (Get_Entity (Library_Unit)) = Entity_Id
            then
               if Res = Null_Iir then
                  Res := Design_Unit;
               elsif Get_Date (Design_Unit) > Get_Date (Res) then
                  Res := Design_Unit;
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

   function Load_File (File : Source_File_Entry) return Iir_Design_File
   is
      Res : Iir_Design_File;
   begin
      Scan.Set_File (File);
      Res := Parse.Parse_Design_File;
      Scan.Close_File;
      if Res /= Null_Iir then
         Set_Parent (Res, Work_Library);
         Set_Design_File_Filename (Res, Files_Map.Get_File_Name (File));
      end if;
      return Res;
   end Load_File;

   -- parse a file.
   -- Return a design_file without putting it into the library
   -- (because it was not semantized).
   function Load_File (File_Name: Name_Id) return Iir_Design_File
   is
      Fe : Source_File_Entry;
   begin
      Fe := Files_Map.Load_Source_File (Local_Directory, File_Name);
      if Fe = No_Source_File_Entry then
         Error_Msg_Option ("cannot open " & Image (File_Name));
         return Null_Iir;
      end if;
      return Load_File (Fe);
   end Load_File;

   function Find_Design_Unit (Unit : Iir) return Iir_Design_Unit is
   begin
      case Get_Kind (Unit) is
         when Iir_Kind_Design_Unit =>
            return Unit;
         when Iir_Kind_Selected_Name =>
            declare
               Lib : Iir_Library_Declaration;
            begin
               Lib := Get_Library (Get_Identifier (Get_Prefix (Unit)),
                                   Get_Location (Unit));
               return Find_Primary_Unit (Lib, Get_Suffix_Identifier (Unit));
            end;
         when Iir_Kind_Entity_Aspect_Entity =>
            declare
               Prim : Iir_Design_Unit;
            begin
               Prim := Find_Design_Unit (Get_Entity (Unit));
               if Prim = Null_Iir then
                  return Null_Iir;
               end if;
               return Find_Secondary_Unit
                 (Prim, Get_Identifier (Get_Architecture (Unit)));
            end;
         when others =>
            Error_Kind ("find_design_unit", Unit);
      end case;
   end Find_Design_Unit;

   function Is_Obsolete (Design_Unit : Iir_Design_Unit; Loc : Iir)
     return Boolean
   is
      procedure Error_Obsolete (Msg : String) is
      begin
         if not Flags.Flag_Elaborate_With_Outdated then
            Error_Msg_Sem (Msg, Loc);
         end if;
      end Error_Obsolete;

      List : Iir_List;
      El : Iir;
      Unit : Iir_Design_Unit;
      U_Ts : Time_Stamp_Id;
      Du_Ts : Time_Stamp_Id;
   begin
      if Get_Date (Design_Unit) = Date_Obsolete then
         Error_Obsolete (Disp_Node (Design_Unit) & " is obsolete");
         return True;
      end if;
      List := Get_Dependence_List (Design_Unit);
      if List = Null_Iir_List then
         return False;
      end if;
      Du_Ts := Get_Analysis_Time_Stamp (Get_Design_File (Design_Unit));
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         Unit := Find_Design_Unit (El);
         if Unit /= Null_Iir then
            U_Ts := Get_Analysis_Time_Stamp (Get_Design_File (Unit));
            if Files_Map.Is_Gt (U_Ts, Du_Ts) then
               Error_Obsolete
                 (Disp_Node (Design_Unit) & " is obsoleted by " &
                  Disp_Node (Unit));
               return True;
            elsif Is_Obsolete (Unit, Loc) then
               Error_Obsolete
                 (Disp_Node (Design_Unit) & " depends on obsolete unit");
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Is_Obsolete;

   procedure Load_Parse_Design_Unit (Design_Unit: Iir_Design_Unit; Loc : Iir)
   is
      use Scan;
      Line, Off: Natural;
      Pos: Source_Ptr;
      Res: Iir;
      Design_File : Iir_Design_File;
      Fe : Source_File_Entry;
   begin
      if Get_Date_State (Design_Unit) /= Date_Disk then
         raise Internal_Error;
      end if;

      --  Load and parse the unit.
      Design_File := Get_Design_File (Design_Unit);
      Fe := Files_Map.Load_Source_File
        (Get_Design_File_Directory (Design_File),
         Get_Design_File_Filename (Design_File));
      if Fe = No_Source_File_Entry then
         Error_Msg
           ("cannot load " & Disp_Node (Get_Library_Unit (Design_Unit)));
         raise Compilation_Error;
      end if;
      Set_File (Fe);

      if not Files_Map.Is_Eq
        (Files_Map.Get_File_Time_Stamp (Get_Current_Source_File),
         Get_File_Time_Stamp (Design_File))
      then
         Error_Msg_Sem
           ("file " & Image (Get_Design_File_Filename (Design_File))
            & " has changed and must be reanalysed", Loc);
         raise Compilation_Error;
      elsif Get_Date (Design_Unit) = Date_Obsolete then
         Error_Msg_Sem
           (''' & Disp_Node (Get_Library_Unit (Design_Unit))
            & "' is not anymore in the file",
            Design_Unit);
         raise Compilation_Error;
      end if;
      Get_Pos_Line_Off (Design_Unit, Pos, Line, Off);
      Files_Map.File_Add_Line_Number (Get_Current_Source_File, Line, Pos);
      Set_Current_Position (Pos + Source_Ptr (Off));
      Res := Parse.Parse_Design_Unit;
      Close_File;
      if Res = Null_Iir then
         raise Compilation_Error;
      end if;
      Set_Date_State (Design_Unit, Date_Parse);
      --  FIXME: check the library unit read is the one expected.
      --  Copy node.
      Iirs_Utils.Free_Recursive (Get_Library_Unit (Design_Unit));
      Set_Library_Unit (Design_Unit, Get_Library_Unit (Res));
      Set_Design_Unit (Get_Library_Unit (Res), Design_Unit);
      Set_Parent (Get_Library_Unit (Res), Design_Unit);
      Set_Context_Items (Design_Unit, Get_Context_Items (Res));
      Location_Copy (Design_Unit, Res);
      Free_Dependence_List (Design_Unit);
      Set_Dependence_List (Design_Unit, Get_Dependence_List (Res));
      Set_Dependence_List (Res, Null_Iir_List);
      Free_Iir (Res);
   end Load_Parse_Design_Unit;

   -- Load, parse, semantize, back-end a design_unit if necessary.
   procedure Load_Design_Unit (Design_Unit: Iir_Design_Unit; Loc : Iir) is
   begin
      if Get_Date_State (Design_Unit) = Date_Disk then
         Load_Parse_Design_Unit (Design_Unit, Loc);
      end if;

      if Get_Date_State (Design_Unit) = Date_Parse then
         --  Analyze the design unit.

         if Get_Date (Design_Unit) = Date_Analyzed then
            --  Work-around for an internal check in sem.
            --  FIXME: to be removed ?
            Set_Date (Design_Unit, Date_Parsed);
         end if;

         --  Avoid infinite recursion, if the unit is self-referenced.
         Set_Date_State (Design_Unit, Date_Analyze);

         Sem_Scopes.Push_Interpretations;
         Back_End.Finish_Compilation (Design_Unit);
         Sem_Scopes.Pop_Interpretations;

      end if;

      case Get_Date (Design_Unit) is
         when Date_Parsed =>
            raise Internal_Error;
         when Date_Analyzing =>
            --  Self-referenced unit.
            return;
         when Date_Analyzed =>
            --  FIXME: Accept it silently ?
            --  Note: this is used when Flag_Elaborate_With_Outdated is set.
            --  This is also used by anonymous configuration declaration.
            null;
         when Date_Uptodate =>
            return;
         when Date_Valid =>
            null;
         when Date_Obsolete =>
            if not Flags.Flag_Elaborate_With_Outdated then
               Error_Msg_Sem (Disp_Node (Design_Unit) & " is obsolete", Loc);
               return;
            end if;
         when others =>
            raise Internal_Error;
      end case;

      if not Flags.Flag_Elaborate_With_Outdated
        and then Is_Obsolete (Design_Unit, Loc)
      then
         Set_Date (Design_Unit, Date_Obsolete);
      end if;
   end Load_Design_Unit;

   --  Return the declaration of primary unit NAME of LIBRARY.
   function Find_Primary_Unit
     (Library: Iir_Library_Declaration; Name: Name_Id)
      return Iir_Design_Unit
   is
      Unit : Iir_Design_Unit;
   begin
      Unit := Unit_Hash_Table (Name mod Unit_Hash_Length);
      while Unit /= Null_Iir loop
         if Get_Identifier (Unit) = Name
           and then Get_Library (Get_Design_File (Unit)) = Library
         then
            case Get_Kind (Get_Library_Unit (Unit)) is
               when Iir_Kind_Package_Declaration
                 | Iir_Kind_Entity_Declaration
                 | Iir_Kind_Configuration_Declaration =>
                  --  Only return a primary unit.
                  return Unit;
               when others =>
                  null;
            end case;
         end if;
         Unit := Get_Hash_Chain (Unit);
      end loop;

      -- The primary unit is not in the library, return null.
      return Null_Iir;
   end Find_Primary_Unit;

   function Load_Primary_Unit
     (Library: Iir_Library_Declaration; Name: Name_Id; Loc : Iir)
      return Iir_Design_Unit
   is
      Design_Unit: Iir_Design_Unit;
   begin
      Design_Unit := Find_Primary_Unit (Library, Name);
      if Design_Unit /= Null_Iir then
         Load_Design_Unit (Design_Unit, Loc);
      end if;
      return Design_Unit;
   end Load_Primary_Unit;

   -- Return the declaration of secondary unit NAME for PRIMARY, or null if
   -- not found.
   function Find_Secondary_Unit (Primary: Iir_Design_Unit; Name: Name_Id)
      return Iir_Design_Unit
   is
      Design_Unit: Iir_Design_Unit;
      Library_Unit: Iir;
      Primary_Ident: Name_Id;
      Ident: Name_Id;
      Lib_Prim : Iir;
   begin
      Lib_Prim := Get_Library (Get_Design_File (Primary));
      Primary_Ident := Get_Identifier (Get_Library_Unit (Primary));
      Design_Unit := Unit_Hash_Table (Primary_Ident mod Unit_Hash_Length);
      while Design_Unit /= Null_Iir loop
         Library_Unit := Get_Library_Unit (Design_Unit);

         --  The secondary is always in the same library as the primary.
         if Get_Library (Get_Design_File (Design_Unit)) = Lib_Prim then
            -- Set design_unit to null iff this is not the correct
            -- design unit.
            case Get_Kind (Library_Unit) is
               when Iir_Kind_Architecture_Declaration =>
                  -- The entity field can be either an identifier (if the
                  -- library unit was not loaded) or an access to the entity
                  -- unit.
                  Ident := Get_Identifier (Get_Entity (Library_Unit));
                  if Ident = Primary_Ident
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

   -- Load an secondary unit and analyse it.
   function Load_Secondary_Unit
     (Primary: Iir_Design_Unit; Name: Name_Id; Loc : Iir)
      return Iir_Design_Unit
   is
      Design_Unit: Iir_Design_Unit;
   begin
      Design_Unit := Find_Secondary_Unit (Primary, Name);
      if Design_Unit /= Null_Iir then
         Load_Design_Unit (Design_Unit, Loc);
      end if;
      return Design_Unit;
   end Load_Secondary_Unit;

   function Find_Entity_For_Component (Name: Name_Id) return Iir_Design_Unit
   is
      Res : Iir_Design_Unit := Null_Iir;
      Unit : Iir_Design_Unit;
   begin
      Unit := Unit_Hash_Table (Name mod Unit_Hash_Length);
      while Unit /= Null_Iir loop
         if Get_Identifier (Unit) = Name
           and then (Get_Kind (Get_Library_Unit (Unit))
                     = Iir_Kind_Entity_Declaration)
         then
            if Res = Null_Iir then
               Res := Unit;
            else
               --  Many entities.
               return Null_Iir;
            end if;
         end if;
         Unit := Get_Hash_Chain (Unit);
      end loop;

      return Res;
   end Find_Entity_For_Component;

end Libraries;
