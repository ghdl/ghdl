--  GHDL driver - print commands.
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
with Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Tables;
with Types; use Types;
with Flags;
with Name_Table; use Name_Table;
with Files_Map;
with Libraries;
with Options; use Options;
with Errorout; use Errorout;
with Version;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Tokens;
with Vhdl.Scanner;
with Vhdl.Parse;
with Vhdl.Canon;
with Vhdl.Xrefs;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Vhdl.Prints;
with Vhdl.Formatters; use Vhdl.Formatters;
with Vhdl.Elocations;

with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;

package body Ghdlprint is
   type Html_Format_Type is (Html_2, Html_Css);
   Html_Format : Html_Format_Type := Html_2;

   procedure Put_Html (C : Character) is
   begin
      case C is
         when '>' =>
            Put ("&gt;");
         when '<' =>
            Put ("&lt;");
         when '&' =>
            Put ("&amp;");
         when others =>
            Put (C);
      end case;
   end Put_Html;

   procedure Put_Html (S : String) is
   begin
      for I in S'Range loop
         Put_Html (S (I));
      end loop;
   end Put_Html;

   package Nat_IO is new Ada.Text_IO.Integer_IO (Num => Natural);
   procedure Put_Nat (N : Natural) is
   begin
      Nat_IO.Put (N, Width => 0);
   end Put_Nat;

   type Filexref_Info_Type is record
      Output : String_Acc;
      Referenced : Boolean;
   end record;
   type Filexref_Info_Arr is array (Source_File_Entry range <>)
     of Filexref_Info_Type;
   type Filexref_Info_Arr_Acc is access Filexref_Info_Arr;
   Filexref_Info : Filexref_Info_Arr_Acc := null;

   --  If True, at least one xref is missing.
   Missing_Xref : Boolean := False;

   procedure PP_Html_File (File : Source_File_Entry)
   is
      use Flags;
      use Vhdl.Scanner;
      use Vhdl.Tokens;
      use Files_Map;
      use Ada.Characters.Latin_1;

      Line : Natural;
      Buf : File_Buffer_Acc;
      Prev_Tok : Token_Type;

      --  Current logical column number.  Used to expand TABs.
      Col : Natural;

      --  Position just after the last token.
      Last_Tok : Source_Ptr;

      --  Position just before the current token.
      Bef_Tok : Source_Ptr;

      --  Position just after the current token.
      Aft_Tok : Source_Ptr;

      procedure Disp_Ln
      is
         N : Natural;
         Str : String (1 .. 5);
      begin
         case Html_Format is
            when Html_2 =>
               Put ("<font size=-1>");
            when Html_Css =>
               Put ("<i>");
         end case;
         N := Line;
         for I in reverse Str'Range loop
            if N = 0 then
               Str (I) := ' ';
            else
               Str (I) := Character'Val (48 + N mod 10);
               N := N / 10;
            end if;
         end loop;
         Put (Str);
         case Html_Format is
            when Html_2 =>
               Put ("</font>");
            when Html_Css =>
               Put ("</i>");
         end case;
         Put (" ");
         Col := 0;
      end Disp_Ln;

      procedure Disp_Spaces
      is
         C : Character;
         P : Source_Ptr;
         N_Col : Natural;
      begin
         P := Last_Tok;
         while P < Bef_Tok loop
            C := Buf (P);
            if C = HT then
               --  Expand TABS.
               N_Col := Col + Tab_Stop;
               N_Col := N_Col - N_Col mod Tab_Stop;
               while Col < N_Col loop
                  Put (' ');
                  Col := Col + 1;
               end loop;
            else
               Put (' ');
               Col := Col + 1;
            end if;
            P := P + 1;
         end loop;
      end Disp_Spaces;

      procedure Disp_Text
      is
         P : Source_Ptr;
      begin
         P := Bef_Tok;
         while P < Aft_Tok loop
            Put_Html (Buf (P));
            Col := Col + 1;
            P := P + 1;
         end loop;
      end Disp_Text;

      procedure Disp_Reserved is
      begin
         Disp_Spaces;
         case Html_Format is
            when Html_2 =>
               Put ("<font color=red>");
               Disp_Text;
               Put ("</font>");
            when Html_Css =>
               Put ("<em>");
               Disp_Text;
               Put ("</em>");
         end case;
      end Disp_Reserved;

      procedure Disp_Href (Loc : Location_Type)
      is
         L_File : Source_File_Entry;
         L_Pos : Source_Ptr;
      begin
         Location_To_File_Pos (Loc, L_File, L_Pos);
         Put (" href=""");
         if L_File /= File then
            --  External reference.
            if Filexref_Info (L_File).Output /= null then
               Put (Filexref_Info (L_File).Output.all);
               Put ("#");
               Put_Nat (Natural (L_Pos));
            else
               --  Reference to an unused file.
               Put ("index.html#f");
               Put_Nat (Natural (L_File));
               Filexref_Info (L_File).Referenced := True;
            end if;
         else
            --  Local reference.
            Put ("#");
            Put_Nat (Natural (L_Pos));
         end if;
         Put ("""");
      end Disp_Href;

      procedure Disp_Anchor (Loc : Location_Type)
      is
         L_File : Source_File_Entry;
         L_Pos : Source_Ptr;
      begin
         Put (" name=""");
         Location_To_File_Pos (Loc, L_File, L_Pos);
         Put_Nat (Natural (L_Pos));
         Put ("""");
      end Disp_Anchor;

      procedure Disp_Identifier
      is
         use Vhdl.Xrefs;
         Ref : Xref;
         Decl : Iir;
         Bod : Iir;
         Loc : Location_Type;
      begin
         if Flags.Flag_Xref then
            Loc := File_Pos_To_Location (File, Bef_Tok);
            Ref := Find (Loc);
            if Ref = Bad_Xref then
               Disp_Spaces;
               Disp_Text;
               Warning_Msg_Sem (Warnid_Missing_Xref, Loc, "cannot find xref");
               Missing_Xref := True;
               return;
            end if;
         else
            Disp_Spaces;
            Disp_Text;
            return;
         end if;
         case Get_Xref_Kind (Ref) is
            when Xref_Keyword =>
               Disp_Reserved;
            when Xref_Decl =>
               Disp_Spaces;
               Put ("<a");
               Disp_Anchor (Loc);
               Decl := Get_Xref_Node (Ref);
               case Get_Kind (Decl) is
                  when Iir_Kind_Function_Declaration
                    | Iir_Kind_Procedure_Declaration =>
                     Bod := Get_Subprogram_Body (Decl);
                  when Iir_Kind_Package_Declaration =>
                     Bod := Get_Package_Body (Decl);
                  when Iir_Kind_Type_Declaration =>
                     Decl := Get_Type (Decl);
                     case Get_Kind (Decl) is
                        when Iir_Kind_Protected_Type_Declaration =>
                           Bod := Get_Protected_Type_Body (Decl);
                        when Iir_Kind_Incomplete_Type_Definition =>
                           Bod := Get_Type_Declarator (Decl);
                        when others =>
                           Bod := Null_Iir;
                     end case;
                  when others =>
                     Bod := Null_Iir;
               end case;
               if Bod /= Null_Iir then
                  Disp_Href (Get_Location (Bod));
               end if;
               Put (">");
               Disp_Text;
               Put ("</a>");
            when Xref_Ref
              | Xref_End =>
               Disp_Spaces;
               Decl := Get_Xref_Node (Ref);
               Loc := Get_Location (Decl);
               if Loc /= Location_Nil then
                  Put ("<a");
                  Disp_Href (Loc);
                  Put (">");
                  Disp_Text;
                  Put ("</a>");
               else
                  --  This may happen for overload list, in use clauses.
                  Disp_Text;
               end if;
            when Xref_Body =>
               Disp_Spaces;
               Put ("<a");
               Disp_Anchor (Loc);
               Disp_Href (Get_Location (Get_Xref_Node (Ref)));
               Put (">");
               Disp_Text;
               Put ("</a>");
         end case;
      end Disp_Identifier;

      procedure Disp_Attribute
      is
         use Vhdl.Xrefs;
         Ref : Xref;
         Decl : Iir;
         Loc : Location_Type;
      begin
         Disp_Spaces;
         if Flags.Flag_Xref then
            Loc := File_Pos_To_Location (File, Bef_Tok);
            Ref := Find (Loc);
         else
            Ref := Bad_Xref;
         end if;
         if Ref = Bad_Xref then
            case Html_Format is
               when Html_2 =>
                  Put ("<font color=orange>");
                  Disp_Text;
                  Put ("</font>");
               when Html_Css =>
                  Put ("<var>");
                  Disp_Text;
                  Put ("</var>");
            end case;
         else
            Decl := Get_Xref_Node (Ref);
            Loc := Get_Location (Decl);
            Put ("<a");
            Disp_Href (Loc);
            Put (">");
            Disp_Text;
            Put ("</a>");
         end if;
      end Disp_Attribute;
   begin
      Vhdl.Scanner.Flag_Comment := True;
      Vhdl.Scanner.Flag_Newline := True;

      Set_File (File);
      Buf := Get_File_Source (File);

      Put_Line ("<pre>");
      Line := 1;
      Disp_Ln;
      Last_Tok := Source_Ptr_Org;
      Prev_Tok := Tok_Invalid;
      loop
         Scan;
         Bef_Tok := Get_Token_Position;
         Aft_Tok := Get_Position;
         case Current_Token is
            when Tok_Eof =>
               exit;
            when Tok_Newline =>
               New_Line;
               Line := Line + 1;
               Disp_Ln;
            when Tok_Line_Comment
              | Tok_Block_Comment_Start =>
               Disp_Spaces;
               case Html_Format is
                  when Html_2 =>
                     Put ("<font color=green>");
                  when Html_Css =>
                     Put ("<tt>");
               end case;
               Disp_Text;
               if Current_Token = Tok_Block_Comment_Start then
                  loop
                     Scan_Block_Comment;
                     Bef_Tok := Get_Token_Position;
                     Aft_Tok := Get_Position;
                     case Current_Token is
                        when Tok_Newline =>
                           New_Line;
                           Line := Line + 1;
                           Disp_Ln;
                        when Tok_Eof =>
                           exit;
                        when Tok_Block_Comment_Text =>
                           Disp_Text;
                        when Tok_Block_Comment_End =>
                           Disp_Text;
                           exit;
                        when others =>
                           raise Internal_Error;
                     end case;
                  end loop;
               end if;
               case Html_Format is
                  when Html_2 =>
                     Put ("</font>");
                  when Html_Css =>
                     Put ("</tt>");
               end case;
            when Tok_Mod .. Tok_Vunit =>
               Disp_Reserved;
            when Tok_Semi_Colon =>
               Disp_Spaces;
               Disp_Text;
            when Tok_Across .. Tok_Tolerance =>
               Disp_Reserved;
            when Tok_Psl_Clock
               | Tok_Psl_Endpoint
               | Tok_Psl_Boolean
               | Tok_Psl_Const
               | Tok_Inf
               | Tok_Within
               | Tok_Abort
               | Tok_Async_Abort
               | Tok_Sync_Abort
               | Tok_Before
               | Tok_Before_Em
               | Tok_Before_Un
               | Tok_Before_Em_Un
               | Tok_Until_Em
               | Tok_Until_Un
               | Tok_Until_Em_Un
               | Tok_Always
               | Tok_Never
               | Tok_Eventually_Em
               | Tok_Next_Em
               | Tok_Next_A
               | Tok_Next_A_Em
               | Tok_Next_E
               | Tok_Next_E_Em
               | Tok_Next_Event
               | Tok_Next_Event_Em
               | Tok_Next_Event_A
               | Tok_Next_Event_A_Em
               | Tok_Next_Event_E_Em
               | Tok_Next_Event_E
               | Tok_Prev
               | Tok_Stable
               | Tok_Rose
               | Tok_Fell
               | Tok_Onehot
               | Tok_Onehot0 =>
               Disp_Spaces;
               Disp_Text;
            when Tok_String
               | Tok_Bit_String
               | Tok_Character =>
               Disp_Spaces;
               case Html_Format is
                  when Html_2 =>
                     Put ("<font color=blue>");
                     Disp_Text;
                     Put ("</font>");
                  when Html_Css =>
                     Put ("<kbd>");
                     Disp_Text;
                     Put ("</kbd>");
               end case;
            when Tok_Identifier =>
               if Prev_Tok = Tok_Tick then
                  Disp_Attribute;
               else
                  Disp_Identifier;
               end if;
            when Tok_Left_Paren .. Tok_Colon
              | Tok_Comma .. Tok_Dot
              | Tok_Equal_Equal
              | Tok_Integer
              | Tok_Integer_Letter
              | Tok_Real
              | Tok_Equal .. Tok_Slash =>
               Disp_Spaces;
               Disp_Text;
            when Tok_Invalid
              | Tok_Block_Comment_Text
              | Tok_Block_Comment_End =>
               raise Internal_Error;
         end case;
         Last_Tok := Aft_Tok;
         Prev_Tok := Current_Token;
      end loop;
      Close_File;
      New_Line;
      Put_Line ("</pre>");
      Put_Line ("<hr/>");
   end PP_Html_File;

   procedure Put_Html_Header
   is
   begin
      Put ("<html>");
      Put_Line (" <head>");
      case Html_Format is
         when Html_2 =>
            null;
         when Html_Css =>
            Put_Line (" <link rel=stylesheet type=""text/css""");
            Put_Line ("  href=""ghdl.css"" title=""default""/>");
      end case;
      --Put_Line ("<?xml version=""1.0"" encoding=""utf-8"" ?>");
      --Put_Line("<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Strict//EN""");
      --Put_Line ("""https://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"">");
      --Put_Line ("<html xmlns=""https://www.w3.org/1999/xhtml"""
      --         & " xml:lang=""en"">");
      --Put_Line ("<head>");
   end Put_Html_Header;

   procedure Put_Css is
   begin
      Put_Line ("/* EM is used for reserved words */");
      Put_Line ("EM { color : red; font-style: normal }");
      New_Line;
      Put_Line ("/* TT is used for comments */");
      Put_Line ("TT { color : green; font-style: normal }");
      New_Line;
      Put_Line ("/* KBD is used for literals and strings */");
      Put_Line ("KBD { color : blue; font-style: normal }");
      New_Line;
      Put_Line ("/* I is used for line numbers */");
      Put_Line ("I { color : gray; font-size: 50% }");
      New_Line;
      Put_Line ("/* VAR is used for attributes name */");
      Put_Line ("VAR { color : orange; font-style: normal }");
      New_Line;
      Put_Line ("/* A is used for identifiers.  */");
      Put_Line ("A { color: blue; font-style: normal;");
      Put_Line ("    text-decoration: none }");
   end Put_Css;

   procedure Put_Html_Foot
   is
   begin
      Put_Line ("<p>");
      Put ("<small>This page was generated using ");
      Put ("<a href=""http://ghdl.free.fr"">GHDL ");
      Put (Version.Ghdl_Ver);
      Put (' ');
      Put (Version.Ghdl_Release);
      Put ("</a>, a program written by");
      Put (" Tristan Gingold");
      New_Line;
      Put_Line ("</p>");
      Put_Line ("</body>");
      Put_Line ("</html>");
   end Put_Html_Foot;

   function Create_Output_Filename (Name : String; Num : Natural)
                                   return String_Acc
   is
      --  Position of the extension.  0 if none.
      Ext_Pos : Natural;

      Num_Str : String := Natural'Image (Num);
   begin
      --  Search for the extension.
      Ext_Pos := 0;
      for I in reverse Name'Range loop
         exit when Name (I) = Directory_Separator;
         if Name (I) = '.' then
            Ext_Pos := I - 1;
            exit;
         end if;
      end loop;
      if Ext_Pos = 0 then
         Ext_Pos := Name'Last;
      end if;
      Num_Str (1) := '.';
      return new String'(Name (Name'First .. Ext_Pos) & Num_Str & ".html");
   end Create_Output_Filename;

   --  Command --chop.
   type Command_Chop is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Chop; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Chop) return String;
   procedure Perform_Action (Cmd : in out Command_Chop;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Chop; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "chop"
        or else Name = "--chop";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Chop) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "chop [OPTS] FILEs"
        & ASCII.LF & "  Chop FILEs"
        & ASCII.LF & "  alias: --chop";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Chop; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Ada.Characters.Latin_1;

      Files : Iir_Array (Args'Range);

      function Build_File_Name_Length (Lib : Iir) return Natural
      is
         Id : constant Name_Id := Get_Identifier (Lib);
         Len : Natural;
         Id1 : Name_Id;
      begin
         Len := Get_Name_Length (Id);
         case Get_Kind (Lib) is
            when Iir_Kind_Configuration_Declaration
              | Iir_Kind_Entity_Declaration
              | Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Instantiation_Declaration =>
               null;
            when Iir_Kind_Package_Body =>
               Len := Len + 1 + 4; -- add -body
            when Iir_Kind_Architecture_Body =>
               Id1 := Get_Entity_Identifier_Of_Architecture (Lib);
               Len := Len + 1 + Get_Name_Length (Id1);
            when others =>
               Error_Kind ("build_file_name", Lib);
         end case;
         Len := Len + 1 + 4; --  add .vhdl
         return Len;
      end Build_File_Name_Length;

      procedure Build_File_Name (Lib : Iir; Res : out String)
      is
         Id : constant Name_Id := Get_Identifier (Lib);
         P : Natural;

         procedure Append (Str : String) is
         begin
            Res (P + 1 .. P + Str'Length) := Str;
            P := P + Str'Length;
         end Append;
      begin
         P := Res'First - 1;
         case Get_Kind (Lib) is
            when Iir_Kind_Configuration_Declaration
              | Iir_Kind_Entity_Declaration
              | Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Instantiation_Declaration =>
               Append (Image (Id));
            when Iir_Kind_Package_Body =>
               Append (Image (Id));
               Append ("-body");
            when Iir_Kind_Architecture_Body =>
               Append (Image (Get_Entity_Identifier_Of_Architecture (Lib)));
               Append ("-");
               Append (Image (Id));
            when others =>
               raise Internal_Error;
         end case;
         Append (".vhdl");
      end Build_File_Name;

      --  Scan source file BUF+START until end of line.
      --  Return line kind to KIND and position of next line to NEXT.
      type Line_Type is (Line_Blank, Line_Comment, Line_Text);
      procedure Find_Eol (Buf : File_Buffer_Acc;
                          Start : Source_Ptr;
                          Next : out Source_Ptr;
                          Kind : out Line_Type)
      is
         P : Source_Ptr;
      begin
         P := Start;

         Kind := Line_Blank;

         --  Skip blanks.
         while Buf (P) = ' ' or Buf (P) = HT loop
            P := P + 1;
         end loop;

         --  Skip comment if any.
         if Buf (P) = '-' and Buf (P + 1) = '-' then
            Kind := Line_Comment;
            P := P + 2;
         elsif Buf (P) /= CR and Buf (P) /= LF and Buf (P) /= EOT then
            Kind := Line_Text;
         end if;

         --  Skip until end of line.
         while Buf (P) /= CR and Buf (P) /= LF and Buf (P) /= EOT loop
            P := P + 1;
         end loop;

         if Buf (P) = CR then
            P := P + 1;
            if Buf (P) = LF then
               P := P + 1;
            end if;
         elsif Buf (P) = LF then
            P := P + 1;
            if Buf (P) = CR then
               P := P + 1;
            end if;
         end if;

         Next := P;
      end Find_Eol;

      Id : Name_Id;
      Design_File : Iir_Design_File;
      Unit : Iir;
      Lib : Iir;
      Len : Natural;
   begin
      Flags.Bootstrap := True;
      Flags.Flag_Elocations := True;
      --  Load word library.
      if not Libraries.Load_Std_Library then
         raise Option_Error;
      end if;
      Libraries.Load_Work_Library;

      --  First loop: parse source file, check destination file does not
      --  exist.
      for I in Args'Range loop
         Id := Get_Identifier (Args (I).all);
         Design_File := Load_File_Name (Id);
         if Design_File = Null_Iir then
            raise Compile_Error;
         end if;
         Files (I) := Design_File;
         Unit := Get_First_Design_Unit (Design_File);
         while Unit /= Null_Iir loop
            Lib := Get_Library_Unit (Unit);
            Len := Build_File_Name_Length (Lib);
            declare
               Filename : String (1 .. Len + 1);
            begin
               Build_File_Name (Lib, Filename);
               Filename (Len + 1) := Ghdllocal.Nul;
               if Is_Regular_File (Filename) then
                  Error ("file '" & Filename (1 .. Len) & "' already exists");
                  raise Compile_Error;
               end if;
               Put (Filename (1 .. Len));
               Put ("  (for ");
               Disp_Library_Unit (Lib);
               Put (")");
               New_Line;
            end;
            Unit := Get_Chain (Unit);
         end loop;
      end loop;

      --  Second loop: do the real work.
      for I in Args'Range loop
         Design_File := Files (I);
         Unit := Get_First_Design_Unit (Design_File);
         declare
            use Files_Map;

            File_Entry : Source_File_Entry;
            Buffer : File_Buffer_Acc;

            Start : Source_Ptr;
            Lend : Source_Ptr;
            First : Source_Ptr;
            Next : Source_Ptr;
            Kind : Line_Type;
         begin
            --  A design_file must have at least one design unit.
            if Unit = Null_Iir then
               raise Compile_Error;
            end if;

            Location_To_File_Pos
              (Get_Location (Unit), File_Entry, Start);
            Buffer := Get_File_Source (File_Entry);

            First := Source_Ptr_Org;
            if Get_Chain (Unit) /= Null_Iir then
               --  If there is only one unit, then the whole file is written.
               --  First last blank line.
               Next := Source_Ptr_Org;
               loop
                  Start := Next;
                  Find_Eol (Buffer, Start, Next, Kind);
                  exit when Kind = Line_Text;
                  if Kind = Line_Blank then
                     First := Next;
                  end if;
               end loop;

               --  FIXME: write header.
            end if;

            while Unit /= Null_Iir loop
               Lib := Get_Library_Unit (Unit);

               Location_To_File_Pos
                 (Vhdl.Elocations.Get_End_Location (Lib), File_Entry, Lend);
               if Lend < First then
                  raise Internal_Error;
               end if;

               --  Find the ';'.
               while Buffer (Lend) /= ';' loop
                  Lend := Lend + 1;
               end loop;
               Lend := Lend + 1;
               --  Find end of line.
               Find_Eol (Buffer, Lend, Next, Kind);
               if Kind = Line_Text then
                  --  There is another unit on the same line.
                  Next := Lend;
                  --  Skip blanks.
                  while Buffer (Next) = ' ' or Buffer (Next) = HT loop
                     Next := Next + 1;
                  end loop;
               else
                  --  Find first blank line.
                  loop
                     Start := Next;
                     Find_Eol (Buffer, Start, Next, Kind);
                     exit when Kind /= Line_Comment;
                  end loop;
                  if Kind = Line_Text then
                     --  There is not blank lines.
                     --  All the comments are supposed to belong to the next
                     --  unit.
                     Find_Eol (Buffer, Lend, Next, Kind);
                     Lend := Next;
                  else
                     Lend := Start;
                  end if;
               end if;

               if Get_Chain (Unit) = Null_Iir then
                  --  Last unit.
                  --  Put the end of the file in it.
                  Lend := Get_File_Length (File_Entry);
               end if;

               --  FIXME: file with only one unit.
               --  FIXME: set extension.
               Len := Build_File_Name_Length (Lib);
               declare
                  Filename : String (1 .. Len + 1);
                  Fd : File_Descriptor;

                  Wlen : Integer;
               begin
                  Build_File_Name (Lib, Filename);
                  Filename (Len + 1) := Character'Val (0);
                  Fd := Create_File (Filename, Binary);
                  if Fd = Invalid_FD then
                     Error
                       ("cannot create file '" & Filename (1 .. Len) & "'");
                     raise Compile_Error;
                  end if;
                  Wlen := Integer (Lend - First);
                  if Write (Fd, Buffer (First)'Address, Wlen) /= Wlen then
                     Error ("cannot write to '" & Filename (1 .. Len) & "'");
                     raise Compile_Error;
                  end if;
                  Close (Fd);
               end;
               First := Next;

               Unit := Get_Chain (Unit);
            end loop;
         end;
      end loop;
   end Perform_Action;

   --  Command --lines.
   type Command_Lines is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Lines; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Lines) return String;
   procedure Perform_Action (Cmd : in out Command_Lines;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Lines; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "lines"
        or else Name = "--lines";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Lines) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "lines FILEs"
        & ASCII.LF & "  Precede line with its number"
        & ASCII.LF & "  alias: --lines";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Lines; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Vhdl.Scanner;
      use Vhdl.Tokens;
      use Files_Map;
      use Ada.Characters.Latin_1;

      Id : Name_Id;
      Fe : Source_File_Entry;
      Local_Id : Name_Id;
      Line : Natural;
      File : Source_File_Entry;
      Buf : File_Buffer_Acc;
      Ptr : Source_Ptr;
      Eptr : Source_Ptr;
      C : Character;
      N : Natural;
      Log : Natural;
      Str : String (1 .. 10);
   begin
      Local_Id := Get_Identifier ("");
      for I in Args'Range loop
         --  Load the file.
         Id := Get_Identifier (Args (I).all);
         Fe := Files_Map.Read_Source_File (Local_Id, Id);
         if Fe = No_Source_File_Entry then
            Error ("cannot open file " & Args (I).all);
            raise Compile_Error;
         end if;
         Set_File (Fe);

         --  Scan the content, to compute the number of lines.
         loop
            Scan;
            exit when Current_Token = Tok_Eof;
         end loop;
         File := Get_Current_Source_File;
         Line := Get_Current_Line;
         Close_File;

         --  Compute log10 of line.
         N := Line;
         Log := 0;
         loop
            N := N / 10;
            Log := Log + 1;
            exit when N = 0;
         end loop;

         --  Disp file name.
         Put (Args (I).all);
         Put (':');
         New_Line;

         Buf := Get_File_Source (File);
         for J in 1 .. Line loop
            Ptr := File_Line_To_Position (File, J);
            exit when Ptr = Source_Ptr_Bad;
            exit when Buf (Ptr) = Files_Map.EOT;

            --  Disp line number.
            N := J;
            for K in reverse 1 .. Log loop
               if N = 0 then
                  Str (K) := ' ';
               else
                  Str (K) := Character'Val (48 + N mod 10);
                  N := N / 10;
               end if;
            end loop;
            Put (Str (1 .. Log));
            Put (": ");

            --  Search for end of line (or end of file).
            Eptr := Ptr;
            loop
               C := Buf (Eptr);
               exit when C = Files_Map.EOT or C = LF or C = CR;
               Eptr := Eptr + 1;
            end loop;

            --  Disp line.
            if Eptr > Ptr then
               --  Avoid constraint error on conversion of nul array.
               declare
                  subtype Conv_Subtype is String (1 .. Natural (Eptr - Ptr));
               begin
                  Put (Conv_Subtype (Buf (Ptr .. Eptr - 1)));
               end;
            end if;
            New_Line;
         end loop;
      end loop;
   end Perform_Action;

   --  Command Reprint.
   type Command_Reprint is new Command_Lib with record
      --  Do a semantic analysis.
      Flag_Sem : Boolean := True;

      --  Reprint even in case of errors.
      Flag_Force : Boolean := False;

      --  Format the outputs, using LEVEL and REALIGN.
      Flag_Format : Boolean := False;
      Level : Format_Level := Format_Indent;
      Flag_Realign : Boolean := False;

      --  Output only lines within this range.
      First_Line : Positive := 1;
      Last_Line : Positive := Positive'Last;

      --  Collect and display comments.
      Flag_Comments : Boolean := True;
   end record;
   function Decode_Command (Cmd : Command_Reprint; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Reprint) return String;
   procedure Decode_Option (Cmd : in out Command_Reprint;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Perform_Action (Cmd : in out Command_Reprint;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Reprint; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "reprint"
        or else Name = "--reprint";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Reprint) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "reprint [OPTS] FILEs"
        & ASCII.LF & "  Redisplay FILEs"
        & ASCII.LF & "  alias: --reprint";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Reprint;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
      pragma Assert (Option'First = 1);
   begin
      if Option = "--no-sem" then
         Cmd.Flag_Sem := False;
         Res := Option_Ok;
      elsif Option = "--force" then
         Cmd.Flag_Force := True;
         Res := Option_Ok;
      elsif Option = "--realign" then
         Cmd.Flag_Realign := True;
         Res := Option_Ok;
      elsif Option'Length > 8 and then Option (1 .. 8) = "--range=" then
         declare
            F : constant Natural := 9;
            L : constant Natural := Option'Last;
            Colon : constant Natural := Index (Option (F .. L), ':');
         begin
            if Colon = 0 then
               Cmd.First_Line := Positive'Value (Option (F .. L));
               Cmd.Last_Line := Cmd.First_Line;
            else
               if Colon > 9 then
                  Cmd.First_Line := Positive'Value (Option (F .. Colon - 1));
               end if;
               if Colon < Option'Last then
                  Cmd.Last_Line := Positive'Value (Option (Colon + 1 .. L));
               end if;
            end if;
            Res := Option_Ok;
         exception
            when Constraint_Error =>
               Res := Option_Err;
         end;
      elsif Option = "--comments" then
         Cmd.Flag_Comments := True;
         Res := Option_Ok;
      else
         Decode_Option (Command_Lib (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Perform_Action (Cmd : in out Command_Reprint;
                             Args : Argument_List)
   is
      Design_File : Iir_Design_File;

      Unit : Iir;
      Next_Unit : Iir;

      Id : Name_Id;
   begin
      if Cmd.Flag_Sem then
         -- Libraries are required for semantic analysis.
         if not Setup_Libraries (True) then
            return;
         end if;
      end if;

      --  Keep parenthesis during parse.
      Vhdl.Parse.Flag_Parse_Parenthesis := True;

      Vhdl.Canon.Canon_Flag_Concurrent_Stmts := False;
      Vhdl.Canon.Canon_Flag_Configurations := False;
      Vhdl.Canon.Canon_Flag_Specification_Lists := False;
      Vhdl.Canon.Canon_Flag_Associations := False;

      Flags.Flag_Gather_Comments := Cmd.Flag_Comments;

      --  Parse all files.
      for I in Args'Range loop
         Id := Name_Table.Get_Identifier (Args (I).all);
         Design_File := Load_File_Name (Id);
         if Design_File = Null_Iir
           or else (Errorout.Nbr_Errors > 0 and not Cmd.Flag_Force)
         then
            raise Errorout.Compilation_Error;
         end if;

         Unit := Get_First_Design_Unit (Design_File);
         if Cmd.Flag_Sem then
            Design_File := Null_Iir;
         end if;
         while Unit /= Null_Iir loop
            if Cmd.Flag_Sem then
               --  Analyze the design unit.
               Vhdl.Sem_Lib.Finish_Compilation (Unit, True);
               if Cmd.Flag_Sem and then Design_File = Null_Iir then
                  Design_File := Get_Design_File (Unit);
               end if;
            end if;

            Next_Unit := Get_Chain (Unit);
            if not Cmd.Flag_Format
              and then (Errorout.Nbr_Errors = 0 or Cmd.Flag_Force)
            then
               Vhdl.Prints.Disp_Vhdl (Unit);
            end if;
            if Errorout.Nbr_Errors = 0 then
               if Cmd.Flag_Sem then
                  Set_Chain (Unit, Null_Iir);
                  Libraries.Add_Design_Unit_Into_Library (Unit);
               end if;
            end if;

            Unit := Next_Unit;
         end loop;

         if Errorout.Nbr_Errors > 0 then
            raise Errorout.Compilation_Error;
         end if;

         if Cmd.Flag_Format then
            Vhdl.Formatters.Format (Design_File,
                                    Cmd.Level,
                                    Cmd.Flag_Realign,
                                    Cmd.First_Line, Cmd.Last_Line);
         end if;
      end loop;
   end Perform_Action;

   --  Command Format
   type Command_Format is new Command_Reprint with null record;
   function Decode_Command (Cmd : Command_Format; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Format) return String;
   procedure Decode_Option (Cmd : in out Command_Format;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Perform_Action (Cmd : in out Command_Format;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Format; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "fmt"
        or else Name = "--format";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Format) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "fmt [OPTS] FILEs"
        & ASCII.LF & "  Format FILEs"
        & ASCII.LF & "  alias: --format";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Format;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
      pragma Assert (Option'First = 1);
   begin
      if Option = "--level=indent" then
         Cmd.Level := Format_Indent;
         Res := Option_Ok;
      elsif Option = "--level=none" then
         Cmd.Level := Format_None;
         Res := Option_Ok;
      elsif Option = "--level=space" then
         Cmd.Level := Format_Space;
         Res := Option_Ok;
      else
         Decode_Option (Command_Reprint (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Perform_Action (Cmd : in out Command_Format;
                             Args : Argument_List) is
   begin
      Cmd.Flag_Format := True;
      Perform_Action (Command_Reprint (Cmd), Args);
   end Perform_Action;

   --  Command compare tokens.
   type Command_Compare_Tokens is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Compare_Tokens; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Compare_Tokens) return String;
   procedure Perform_Action (Cmd : in out Command_Compare_Tokens;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Compare_Tokens; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "compare-tokens"
        or else Name = "--compare-tokens";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Compare_Tokens) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "compare-tokens [OPTS] REF FILEs"
        & ASCII.LF & "  Compare FILEs with REF"
        & ASCII.LF & "  alias: --compare-tokens";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Compare_Tokens;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Vhdl.Tokens;
      use Vhdl.Scanner;

      package Ref_Tokens is new Tables
        (Table_Component_Type => Token_Type,
         Table_Index_Type => Integer,
         Table_Low_Bound => 0,
         Table_Initial => 1024);

      Id : Name_Id;
      Fe : Source_File_Entry;
      Local_Id : Name_Id;
      Tok_Idx : Natural;
   begin
      if Args'Length < 1 then
         Error ("missing ref file");
         raise Compile_Error;
      end if;

      Local_Id := Get_Identifier ("");

      for I in Args'Range loop
         --  Load the file.
         Id := Get_Identifier (Args (I).all);
         Fe := Files_Map.Read_Source_File (Local_Id, Id);
         if Fe = No_Source_File_Entry then
            Error ("cannot open file " & Args (I).all);
            raise Compile_Error;
         end if;
         Set_File (Fe);

         if I = Args'First then
            --  Scan ref file
            loop
               Scan;
               Ref_Tokens.Append (Current_Token);
               exit when Current_Token = Tok_Eof;
            end loop;
         else
            --  Scan file
            Tok_Idx := Ref_Tokens.First;
            loop
               Scan;
               if Ref_Tokens.Table (Tok_Idx) /= Current_Token then
                  Report_Msg (Msgid_Error, Errorout.Parse, Get_Token_Coord,
                              "token mismatch");
                  exit;
               end if;
               case Current_Token is
                  when Tok_Eof =>
                     exit;
                  when others =>
                     null;
               end case;
               Tok_Idx := Tok_Idx + 1;
            end loop;
         end if;
         Close_File;
      end loop;

      Ref_Tokens.Free;

      if Nbr_Errors /= 0 then
         raise Compilation_Error;
      end if;
   end Perform_Action;

   --  Command html.
   type Command_Html is abstract new Command_Lib with null record;

   procedure Decode_Option (Cmd : in out Command_Html;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);

   procedure Disp_Long_Help (Cmd : Command_Html);

   procedure Decode_Option (Cmd : in out Command_Html;
                            Option : String;
                            Arg : String;
                            Res : out Option_State) is
   begin
      if Option = "--format=css" then
         Html_Format := Html_Css;
         Res := Option_Ok;
      elsif Option = "--format=html2" then
         Html_Format := Html_2;
         Res := Option_Ok;
      else
         Decode_Option (Command_Lib (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Disp_Long_Help (Cmd : Command_Html) is
   begin
      Disp_Long_Help (Command_Lib (Cmd));
      Put_Line ("--format=html2  Use FONT attributes");
      Put_Line ("--format=css    Use ghdl.css file");
   end Disp_Long_Help;

   --  Command --pp-html.
   type Command_PP_Html is new Command_Html with null record;
   function Decode_Command (Cmd : Command_PP_Html; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_PP_Html) return String;
   procedure Perform_Action (Cmd : in out Command_PP_Html;
                             Files : Argument_List);

   function Decode_Command (Cmd : Command_PP_Html; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "pp-html"
        or else Name = "--pp-html";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_PP_Html) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "pp-html FILEs"
        & ASCII.LF & "  Pretty-print FILEs in HTML"
        & ASCII.LF & "  alias: --pp-html";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_PP_Html;
                             Files : Argument_List)
   is
      pragma Unreferenced (Cmd);

      Id : Name_Id;
      Fe : Source_File_Entry;
      Local_Id : Name_Id;
   begin
      Local_Id := Get_Identifier ("");
      Put_Html_Header;
      Put_Line ("  <title>");
      for I in Files'Range loop
         Put ("    ");
         Put_Line (Files (I).all);
      end loop;
      Put_Line ("  </title>");
      Put_Line ("</head>");
      New_Line;
      Put_Line ("<body>");

      for I in Files'Range loop
         Id := Get_Identifier (Files (I).all);
         Fe := Files_Map.Read_Source_File (Local_Id, Id);
         if Fe = No_Source_File_Entry then
            Error ("cannot open file " & Files (I).all);
            raise Compile_Error;
         end if;
         Put ("  <h1>");
         Put (Files (I).all);
         Put ("</h1>");
         New_Line;

         PP_Html_File (Fe);
      end loop;
      Put_Html_Foot;
   end Perform_Action;

   --  Command --xref-html.
   type Command_Xref_Html is new Command_Html with record
      Output_Dir : String_Access := null;
      Check_Missing : Boolean := False;
   end record;

   function Decode_Command (Cmd : Command_Xref_Html; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Xref_Html) return String;
   procedure Decode_Option (Cmd : in out Command_Xref_Html;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Disp_Long_Help (Cmd : Command_Xref_Html);

   procedure Perform_Action (Cmd : in out Command_Xref_Html;
                             Files_Name : Argument_List);

   function Decode_Command (Cmd : Command_Xref_Html; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "xref-html"
        or else Name = "--xref-html";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Xref_Html) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "xref-html FILEs"
        & ASCII.LF & "  Display FILEs in HTML with xrefs"
        & ASCII.LF & "  alias: --xref-html";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Xref_Html;
                            Option : String;
                            Arg : String;
                            Res : out Option_State) is
   begin
      if Option = "-o" then
         if Arg = "" then
            Res := Option_Arg_Req;
         else
            Cmd.Output_Dir := new String'(Arg);
            Res := Option_Arg;
         end if;
      elsif Option = "--check-missing" then
         Cmd.Check_Missing := True;
         Enable_Warning (Warnid_Missing_Xref, True);
         Res := Option_Ok;
      else
         Decode_Option (Command_Html (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Disp_Long_Help (Cmd : Command_Xref_Html) is
   begin
      Disp_Long_Help (Command_Html (Cmd));
      Put_Line ("-o DIR          Put generated files into DIR (def: html/)");
      Put_Line ("--check-missing Fail if a reference is missing");
      New_Line;
      Put_Line ("When format is css, the CSS file 'ghdl.css' "
                & "is never overwritten.");
   end Disp_Long_Help;

   procedure Analyze_Design_File_Units (File : Iir_Design_File)
   is
      Unit : Iir_Design_Unit;
   begin
      Unit := Get_First_Design_Unit (File);
      while Unit /= Null_Iir loop
         case Get_Date_State (Unit) is
            when Date_Extern
              | Date_Disk =>
               raise Internal_Error;
            when Date_Parse =>
               Vhdl.Sem_Lib.Load_Design_Unit (Unit, Get_Location (Unit));
               if Errorout.Nbr_Errors /= 0 then
                  raise Compilation_Error;
               end if;
            when Date_Analyze =>
               null;
         end case;
         Unit := Get_Chain (Unit);
      end loop;
   end Analyze_Design_File_Units;

   procedure Perform_Action
     (Cmd : in out Command_Xref_Html; Files_Name : Argument_List)
   is
      use GNAT.Directory_Operations;

      Id : Name_Id;
      File : Source_File_Entry;

      type File_Data is record
         Fe : Source_File_Entry;
         Design_File : Iir;
         Output : String_Acc;
      end record;
      type File_Data_Array is array (Files_Name'Range) of File_Data;

      Output_Dir : String_Access;

      Files : File_Data_Array;
      Output : File_Type;
   begin
      Vhdl.Xrefs.Init;
      Flags.Flag_Xref := True;

      --  Load work library.
      if not Setup_Libraries (True) then
         return;
      end if;

      Output_Dir := Cmd.Output_Dir;
      if Output_Dir = null then
         Output_Dir := new String'("html");
      elsif Output_Dir.all = "-" then
         Output_Dir := null;
      end if;

      --  Try to create the directory.
      if Output_Dir /= null
        and then not Is_Directory (Output_Dir.all)
      then
         begin
            Make_Dir (Output_Dir.all);
         exception
            when Directory_Error =>
               Error ("cannot create directory " & Output_Dir.all);
               return;
         end;
      end if;

      --  Parse all files.
      for I in Files'Range loop
         Id := Get_Identifier (Files_Name (I).all);
         File := Files_Map.Read_Source_File (Libraries.Local_Directory, Id);
         if File = No_Source_File_Entry then
            Error ("cannot open " & Image (Id));
            return;
         end if;
         Files (I).Fe := File;
         Files (I).Design_File := Load_File (File);
         if Files (I).Design_File = Null_Iir then
            return;
         end if;
         Files (I).Output := Create_Output_Filename
           (Base_Name (Files_Name (I).all), I);
         if Is_Regular_File (Files (I).Output.all) then
            --  Prevent overwrite.
            null;
         end if;
         --  Put units in library.
         Libraries.Add_Design_File_Into_Library (Files (I).Design_File);
      end loop;

      --  Analyze all files.
      for I in Files'Range loop
         Analyze_Design_File_Units (Files (I).Design_File);
      end loop;

      Vhdl.Xrefs.Sort_By_Location;

      if False then
         --  Dump locations
         for I in 1 .. Vhdl.Xrefs.Get_Last_Xref loop
            declare
               use Vhdl.Xrefs;

               procedure Put_Loc (L : Location_Type)
               is
                  use Files_Map;

                  L_File : Source_File_Entry;
                  L_Pos : Source_Ptr;
               begin
                  Files_Map.Location_To_File_Pos (L, L_File, L_Pos);
                  Put_Nat (Natural (L_File));
                  --Image (Get_File_Name (L_File));
                  --Put (Name_Buffer (1 .. Name_Length));
                  Put (":");
                  Put_Nat (Natural (L_Pos));
               end Put_Loc;
            begin
               Put_Loc (Get_Xref_Location (I));
               case Get_Xref_Kind (I) is
                  when Xref_Decl =>
                     Put (" decl ");
                     Put (Image (Get_Identifier (Get_Xref_Node (I))));
                  when Xref_Ref =>
                     Put (" use ");
                     Put_Loc (Get_Location (Get_Xref_Node (I)));
                  when Xref_End =>
                     Put (" end ");
                  when Xref_Body =>
                     Put (" body ");
                  when Xref_Keyword =>
                     Put (" keyword ");
               end case;
               New_Line;
            end;
         end loop;
      end if;

      --  Create filexref_info.
      Filexref_Info := new Filexref_Info_Arr
        (No_Source_File_Entry .. Files_Map.Get_Last_Source_File_Entry);
      Filexref_Info.all := (others => (Output => null,
                                       Referenced => False));
      for I in Files'Range loop
         Filexref_Info (Files (I).Fe).Output := Files (I).Output;
      end loop;

      for I in Files'Range loop
         if Output_Dir /= null then
            Create (Output, Out_File,
                    Output_Dir.all & Directory_Separator
                    & Files (I).Output.all);

            Set_Output (Output);
         end if;

         Put_Html_Header;
         Put_Line ("  <title>");
         Put_Html (Files_Name (I).all);
         Put ("</title>");
         Put_Line ("</head>");
         New_Line;
         Put_Line ("<body>");

         Put ("<h1>");
         Put_Html (Files_Name (I).all);
         Put ("</h1>");
         New_Line;

         PP_Html_File (Files (I).Fe);
         Put_Html_Foot;

         if Output_Dir /= null then
            Close (Output);
         end if;
      end loop;

      --  Create indexes.
      if Output_Dir /= null then
         Create (Output, Out_File,
                 Output_Dir.all & Directory_Separator & "index.html");
         Set_Output (Output);

         Put_Html_Header;
         Put_Line ("  <title>Xrefs indexes</title>");
         Put_Line ("</head>");
         New_Line;
         Put_Line ("<body>");
         Put_Line ("<p>list of files:");
         Put_Line ("<ul>");
         for I in Files'Range loop
            Put ("<li>");
            Put ("<a href=""");
            Put (Files (I).Output.all);
            Put (""">");
            Put_Html (Files_Name (I).all);
            Put ("</a>");
            Put ("</li>");
            New_Line;
         end loop;
         Put_Line ("</ul></p>");
         Put_Line ("<hr>");

         --  TODO: list of design units.

         Put_Line ("<p>list of files referenced but not available:");
         Put_Line ("<ul>");
         for I in No_Source_File_Entry + 1 .. Filexref_Info'Last loop
            if Filexref_Info (I).Output = null
              and then Filexref_Info (I).Referenced
            then
               Put ("<li><a name=""f");
               Put_Nat (Natural (I));
               Put (""">");
               Put_Html (Image (Files_Map.Get_File_Name (I)));
               Put ("</a></li>");
               New_Line;
            end if;
         end loop;
         Put_Line ("</ul></p><hr>");
         Put_Html_Foot;

         Close (Output);
      end if;

      if Html_Format = Html_Css
        and then Output_Dir /= null
      then
         declare
            Css_Filename : constant String :=
              Output_Dir.all & Directory_Separator & "ghdl.css";
         begin
            if not Is_Regular_File (Css_Filename & Nul) then
               Create (Output, Out_File, Css_Filename);
               Set_Output (Output);
               Put_Css;
               Close (Output);
            end if;
         end;
      end if;

      if Missing_Xref and Cmd.Check_Missing then
         Error ("missing xrefs");
         raise Compile_Error;
      end if;
   exception
      when Compilation_Error =>
         Error ("xrefs has failed due to compilation error");
   end Perform_Action;


   --  Command --xref
   type Command_Xref is new Command_Lib with null record;

   function Decode_Command (Cmd : Command_Xref; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Xref) return String;

   procedure Perform_Action (Cmd : in out Command_Xref;
                             Files_Name : Argument_List);

   function Decode_Command (Cmd : Command_Xref; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "xref"
        or else Name = "--xref";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Xref) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "xref FILEs"
        & ASCII.LF & "  Generate xrefs"
        & ASCII.LF & "  alias: --xref";
   end Get_Short_Help;

   procedure Perform_Action
     (Cmd : in out Command_Xref; Files_Name : Argument_List)
   is
      pragma Unreferenced (Cmd);

      use Files_Map;

      Id : Name_Id;
      File : Source_File_Entry;

      type File_Data is record
         Fe : Source_File_Entry;
         Design_File : Iir;
      end record;
      type File_Data_Array is array (Files_Name'Range) of File_Data;

      Files : File_Data_Array;
   begin
      --  Load work library.
      if not Setup_Libraries (True) then
         return;
      end if;

      Vhdl.Xrefs.Init;
      Flags.Flag_Xref := True;

      --  Parse all files.
      for I in Files'Range loop
         Id := Get_Identifier (Files_Name (I).all);
         File := Read_Source_File (Libraries.Local_Directory, Id);
         if File = No_Source_File_Entry then
            Error ("cannot open " & Image (Id));
            return;
         end if;
         Files (I).Fe := File;
         Files (I).Design_File := Load_File (File);
         if Files (I).Design_File = Null_Iir then
            return;
         end if;
         --  Put units in library.
         --  Note: design_units stay while design_file get empty.
         Libraries.Add_Design_File_Into_Library (Files (I).Design_File);
      end loop;

      --  Analyze all files.
      for I in Files'Range loop
         Analyze_Design_File_Units (Files (I).Design_File);
      end loop;

      Vhdl.Xrefs.Fix_End_Xrefs;
      Vhdl.Xrefs.Sort_By_Node_Location;

      for F in Files'Range loop

         Put ("GHDL-XREF V0");

         declare
            use Vhdl.Xrefs;

            Cur_Decl : Iir;
            Cur_File : Source_File_Entry;

            procedure Emit_Loc (Loc : Location_Type; C : Character)
            is
               L_File : Source_File_Entry;
               L_Pos : Source_Ptr;
               L_Line : Natural;
               L_Off : Natural;
            begin
               Location_To_Coord (Loc, L_File, L_Pos, L_Line, L_Off);
               --Put_Nat (Natural (L_File));
               --Put (':');
               Put_Nat (L_Line);
               Put (C);
               Put_Nat (L_Off);
            end Emit_Loc;

            procedure Emit_Decl (N : Iir)
            is
               Loc : Location_Type;
               Loc_File : Source_File_Entry;
               Loc_Pos : Source_Ptr;
               C : Character;
               Dir : Name_Id;
            begin
               New_Line;
               Cur_Decl := N;
               Loc := Get_Location (N);
               Location_To_File_Pos (Loc, Loc_File, Loc_Pos);
               if Loc_File /= Cur_File then
                  Cur_File := Loc_File;
                  Put ("XFILE: ");
                  Dir := Get_Directory_Name (Cur_File);
                  if Dir /= Null_Identifier then
                     Put (Image (Dir));
                  end if;
                  Put (Image (Get_File_Name (Cur_File)));
                  New_Line;
               end if;

               --  Unused letters:
               --   b d fgh jk  no qr  uvwxyz
               --     D   H JK MNO QR  U WXYZ
               case Get_Kind (N) is
                  when Iir_Kind_Type_Declaration =>
                     C := 'T';
                  when Iir_Kind_Subtype_Declaration =>
                     C := 't';
                  when Iir_Kind_Entity_Declaration =>
                     C := 'E';
                  when Iir_Kind_Architecture_Body =>
                     C := 'A';
                  when Iir_Kind_Library_Declaration =>
                     C := 'L';
                  when Iir_Kind_Package_Declaration =>
                     C := 'P';
                  when Iir_Kind_Package_Body =>
                     C := 'B';
                  when Iir_Kind_Function_Declaration =>
                     C := 'F';
                  when Iir_Kind_Procedure_Declaration =>
                     C := 'p';
                  when Iir_Kind_Interface_Signal_Declaration =>
                     C := 's';
                  when Iir_Kind_Signal_Declaration =>
                     C := 'S';
                  when Iir_Kind_Interface_Constant_Declaration =>
                     C := 'c';
                  when Iir_Kind_Constant_Declaration =>
                     C := 'C';
                  when Iir_Kind_Variable_Declaration =>
                     C := 'V';
                  when Iir_Kind_Element_Declaration =>
                     C := 'e';
                  when Iir_Kind_Iterator_Declaration =>
                     C := 'i';
                  when Iir_Kind_Attribute_Declaration =>
                     C := 'a';
                  when Iir_Kind_Enumeration_Literal =>
                     C := 'l';
                  when Iir_Kind_Component_Declaration =>
                     C := 'm';
                  when Iir_Kind_Component_Instantiation_Statement =>
                     C := 'I';
                  when Iir_Kind_If_Generate_Statement
                     | Iir_Kind_For_Generate_Statement =>
                     C := 'G';
                  when others =>
                     C := '?';
               end case;
               Emit_Loc (Loc, C);
               --Disp_Tree.Disp_Iir_Address (N);
               Put (' ');
               case Get_Kind (N) is
                  when Iir_Kind_Function_Body
                    | Iir_Kind_Procedure_Body =>
                     null;
                  when others =>
                     Put (Image (Get_Identifier (N)));
               end case;
            end Emit_Decl;

            procedure Emit_Ref (R : Xref; T : Character)
            is
               N : Iir;
            begin
               N := Get_Xref_Node (R);
               if N /= Cur_Decl then
                  Emit_Decl (N);
               end if;
               Put (' ');
               Emit_Loc (Get_Xref_Location (R), T);
            end Emit_Ref;

            Loc : Location_Type;
            Loc_File : Source_File_Entry;
            Loc_Pos : Source_Ptr;
         begin
            Cur_Decl := Null_Iir;
            Cur_File := No_Source_File_Entry;

            for I in First_Xref .. Get_Last_Xref loop
               Loc := Get_Xref_Location (I);
               Location_To_File_Pos (Loc, Loc_File, Loc_Pos);
               if Loc_File = Files (F).Fe then
                  --  This is a local location.
                  case Get_Xref_Kind (I) is
                     when Xref_Decl =>
                        Emit_Decl (Get_Xref_Node (I));
                     when Xref_End =>
                        Emit_Ref (I, 'e');
                     when Xref_Ref =>
                        Emit_Ref (I, 'r');
                     when Xref_Body =>
                        Emit_Ref (I, 'b');
                     when Xref_Keyword =>
                        --  Keywords location are not written.
                        null;
                  end case;
               end if;
            end loop;
            New_Line;
         end;
      end loop;
   exception
      when Compilation_Error =>
         Error ("xrefs has failed due to compilation error");
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Chop);
      Register_Command (new Command_Lines);
      Register_Command (new Command_Reprint);
      Register_Command (new Command_Format);
      Register_Command (new Command_Compare_Tokens);
      Register_Command (new Command_PP_Html);
      Register_Command (new Command_Xref_Html);
      Register_Command (new Command_Xref);
   end Register_Commands;
end Ghdlprint;
