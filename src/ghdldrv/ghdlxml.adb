--  GHDL driver - xml commands
--  Copyright (C) 2016 Tristan Gingold
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Simple_IO; use Simple_IO;

with Types; use Types;
with Name_Table; use Name_Table;
with Vhdl.Nodes_Meta; use Vhdl.Nodes_Meta;
with Files_Map;
with Vhdl.Disp_Tree; use Vhdl.Disp_Tree;
with Ghdlprint; use Ghdlprint;
with Libraries;
with Errorout; use Errorout;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;

package body Ghdlxml is

   procedure Disp_Iir (Id : String; N : Iir);

   --  Try to keep line length below that number.
   Max_Line_Len : constant Natural := 80;

   --  Number of space by indentation level.
   Indent_Size : constant Natural := 2;

   --  Current indentation level.
   Indent : Natural;

   --  Current column.
   Col : Natural;

   --  Low-level display routines.  Consider line length, and handle
   --  indentation.  Only use these routines to display content.

   --  Display indentation.
   procedure Put_Indent is
      Blanks : constant String (1 .. Indent_Size * Indent) := (others => ' ');
   begin
      pragma Assert (Col = 0);
      Put (Blanks);
      Col := Indent_Size * Indent;
   end Put_Indent;

   procedure Put_Stag (Name : String) is
   begin
      Put_Indent;
      Put ('<');
      Put (Name);
      Col := Col + 1 + Name'Length;
   end Put_Stag;

   procedure Put_Stag_End is
   begin
      Put ('>');
      New_Line;
      Col := 0;
      Indent := Indent + 1;
   end Put_Stag_End;

   procedure Put_Empty_Stag_End is
   begin
      Put ("/>");
      New_Line;
      Col := 0;
   end Put_Empty_Stag_End;

   procedure Put_Etag (Name : String) is
   begin
      Indent := Indent - 1;
      Put_Indent;
      Put ("</");
      Put (Name);
      Put (">");
      New_Line;
      Col := 0;
   end Put_Etag;

   procedure Put_Attribute (Attr : String; Value : String)
   is
      --  Number of characters to be displayed.
      Len : constant Natural := 4 + Attr'Length + Value'Length;
   begin
      if Col + Len >= Max_Line_Len
        and then Indent * Indent_Size + Len < Max_Line_Len
      then
         New_Line;
         Col := 0;
         Put_Indent;
      end if;
      Put (' ');
      Put (Attr);
      Put ("=""");
      Put (Value);
      Put ('"');
      Col := Col + Len;
   end Put_Attribute;

   --  Espace special characters for XML.
   --
   --  According to: http://www.w3.org/TR/REC-xml/#NT-AttValue
   --
   --  [10] AttValue ::= '"' ([^<&"] | Reference)* '"'
   --                  | "'" ([^<&'] | Reference)* "'"
   --  [67] Reference ::= EntityRef | CharRef
   --  [66] CharRef ::= '&#' [0-9]+ ';'
   --                 | '&#x' [0-9a-fA-F]+ ';'
   function To_XML (Str : String) return String
   is
      To_Hex : constant array (0 .. 15) of Character := "0123456789abcdef";
      --  The escape sequence uses 6 characters.
      Res : String (1 ..  6 * Str'Length);
      Idx : Positive;
      C : Character;
      C_Pos : Natural;
   begin
      Idx := Res'First;
      for I in Str'Range loop
         C := Str (I);
         case C is
            when '<' | '&' | '"'
              | Character'Val (128) .. Character'Val (255) =>
               Res (Idx + 0) := '&';
               Res (Idx + 1) := '#';
               Res (Idx + 2) := 'x';
               C_Pos := Character'Pos (C);
               Res (Idx + 3) := To_Hex (C_Pos / 16);
               Res (Idx + 4) := To_Hex (C_Pos mod 16);
               Res (Idx + 5) := ';';
               Idx := Idx + 6;
            when others =>
               Res (Idx) := C;
               Idx := Idx + 1;
         end case;
      end loop;
      return Res (1 .. Idx - 1);
   end To_XML;

   function XML_Image (Id : Name_Id) return String is
   begin
      return To_XML (Image (Id));
   end XML_Image;

   --  Strip leading blank if any.
   function Strip (S : String) return String
   is
      F : constant Natural := S'First;
   begin
      if F > S'Last then
         return "";
      elsif S (F) = ' ' then
         return S (F + 1 .. S'Last);
      else
         return S;
      end if;
   end Strip;

   procedure Put_Field (F : Fields_Enum; Value : String) is
   begin
      Put_Attribute (Get_Field_Image (F), Value);
   end Put_Field;

   procedure Disp_Iir_Ref (Id : String; N : Iir) is
   begin
      if N = Null_Iir then
         return;
      end if;

      Put_Stag (Id);
      Put_Attribute ("ref", Strip (Iir'Image (N)));
      Put_Empty_Stag_End;
   end Disp_Iir_Ref;

   procedure Disp_Iir_List_Ref (Id : String; L : Iir_List) is
   begin
      if L = Null_Iir_List then
         return;
      end if;

      Put_Stag (Id);
      Put_Attribute ("list-ref", Strip (Iir_List'Image (L)));
      Put_Empty_Stag_End;
   end Disp_Iir_List_Ref;

   procedure Disp_Iir_Flist_Ref (Id : String; L : Iir_Flist) is
   begin
      if L = Null_Iir_Flist then
         return;
      end if;

      Put_Stag (Id);
      Put_Attribute ("flist-ref", Strip (Iir_Flist'Image (L)));
      Put_Empty_Stag_End;
   end Disp_Iir_Flist_Ref;

   procedure Disp_Iir_Chain_Elements (Chain : Iir)
   is
      El : Iir;
   begin
      El := Chain;
      while Is_Valid (El) loop
         Disp_Iir ("el", El);
         El := Get_Chain (El);
      end loop;
   end Disp_Iir_Chain_Elements;

   procedure Disp_Iir_Chain (Id : String; N : Iir) is
   begin
      if N = Null_Iir then
         return;
      end if;

      Put_Stag (Id);
      Put_Stag_End;
      Disp_Iir_Chain_Elements (N);
      Put_Etag (Id);
   end Disp_Iir_Chain;

   procedure Disp_Iir_List (Id : String; L : Iir_List; Ref : Boolean)
   is
      El : Iir;
      It : List_Iterator;
   begin
      case L is
         when  Null_Iir_List =>
            null;

         when Iir_List_All =>
            Put_Stag (Id);
            Put_Attribute ("list-id", "all");
            Put_Empty_Stag_End;

         when others =>
            Put_Stag (Id);
            Put_Attribute ("list-id", Strip (Iir_List'Image (L)));
            Put_Stag_End;

            It := List_Iterate (L);
            while Is_Valid (It) loop
               El := Get_Element (It);
               if Ref then
                  Disp_Iir_Ref ("el", El);
               else
                  Disp_Iir ("el", El);
               end if;
               Next (It);
            end loop;

            Put_Etag (Id);
      end case;
   end Disp_Iir_List;

   procedure Disp_Iir_Flist (Id : String; L : Iir_Flist; Ref : Boolean)
   is
      El : Iir;
   begin
      if L = Null_Iir_Flist then
         return;
      end if;

      Put_Stag (Id);
      case L is
         when Iir_Flist_All =>
            Put_Attribute ("flist-id", "all");
            Put_Empty_Stag_End;
            return;
         when Iir_Flist_Others =>
            Put_Attribute ("flist-id", "others");
            Put_Empty_Stag_End;
            return;
         when others =>
            Put_Attribute ("flist-id", Strip (Iir_Flist'Image (L)));
            Put_Stag_End;
      end case;

      for I in Flist_First .. Flist_Last (L) loop
         El := Get_Nth_Element (L, I);
         if Ref then
            Disp_Iir_Ref ("el", El);
         else
            Disp_Iir ("el", El);
         end if;
      end loop;

      Put_Etag (Id);
   end Disp_Iir_Flist;

   procedure Disp_Iir (Id : String; N : Iir) is
   begin
      if N = Null_Iir then
         return;
      end if;

      Put_Stag (Id);

      Put_Attribute ("id", Strip (Iir'Image (N)));
      Put_Attribute ("kind", Get_Iir_Image (Get_Kind (N)));

      declare
         Loc : constant Location_Type := Get_Location (N);
         File : Name_Id;
         Line : Natural;
         Col : Natural;
      begin
         if Loc /= No_Location then
            Files_Map.Location_To_Position (Loc, File, Line, Col);
            Put_Attribute ("file", Image (File));
            Put_Attribute ("line", Strip (Natural'Image (Line)));
            Put_Attribute ("col", Strip (Natural'Image (Col)));
         end if;
      end;

      declare
         Fields : constant Fields_Array := Get_Fields (Get_Kind (N));
         F : Fields_Enum;
      begin
         --  First attributes
         for I in Fields'Range loop
            F := Fields (I);
            case Get_Field_Type (F) is
               when Type_Iir =>
                  null;
               when Type_Iir_List =>
                  null;
               when Type_Iir_Flist =>
                  null;
               when Type_String8_Id =>
                  null;
               when Type_PSL_NFA =>
                  Put_Field (F, "PSL-NFA");
                  --  Disp_PSL_NFA (Get_PSL_NFA (N, F), Sub_Indent);
               when Type_PSL_Node =>
                  Put_Field (F, "PSL-NODE");
               when Type_Source_Ptr =>
                  null;
               when Type_Date_Type =>
                  Put_Field
                    (F, Strip (Date_Type'Image (Get_Date_Type (N, F))));
               when Type_Number_Base_Type =>
                  Put_Field
                    (F, Number_Base_Type'Image (Get_Number_Base_Type (N, F)));
               when Type_Iir_Constraint =>
                  Put_Field
                    (F, Image_Iir_Constraint (Get_Iir_Constraint (N, F)));
               when Type_Iir_Mode =>
                  Put_Field (F, Image_Iir_Mode (Get_Iir_Mode (N, F)));
               when Type_Iir_Force_Mode =>
                  Put_Field (F, Image_Iir_Force_Mode
                               (Get_Iir_Force_Mode (N, F)));
               when Type_Iir_Index32 =>
                  Put_Field (F, Iir_Index32'Image (Get_Iir_Index32 (N, F)));
               when Type_Int64 =>
                  Put_Field (F, Int64'Image (Get_Int64 (N, F)));
               when Type_Boolean =>
                  Put_Field (F, Image_Boolean (Get_Boolean (N, F)));
               when Type_Iir_Staticness =>
                  Put_Field (F, Image_Iir_Staticness
                               (Get_Iir_Staticness (N, F)));
               when Type_Scalar_Size =>
                  Put_Field (F, Image_Scalar_Size (Get_Scalar_Size (N, F)));
               when Type_Date_State_Type =>
                  Put_Field (F, Image_Date_State_Type
                               (Get_Date_State_Type (N, F)));
               when Type_Iir_All_Sensitized =>
                  Put_Field (F, Image_Iir_All_Sensitized
                               (Get_Iir_All_Sensitized (N, F)));
               when Type_Iir_Signal_Kind =>
                  Put_Field (F, Image_Iir_Signal_Kind
                               (Get_Iir_Signal_Kind (N, F)));
               when Type_Tri_State_Type =>
                  Put_Field (F, Image_Tri_State_Type
                               (Get_Tri_State_Type (N, F)));
               when Type_Iir_Pure_State =>
                  Put_Field (F, Image_Iir_Pure_State
                               (Get_Iir_Pure_State (N, F)));
               when Type_Iir_Delay_Mechanism =>
                  Put_Field (F, Image_Iir_Delay_Mechanism
                               (Get_Iir_Delay_Mechanism (N, F)));
               when Type_Iir_Predefined_Functions =>
                  Put_Field (F, Image_Iir_Predefined_Functions
                               (Get_Iir_Predefined_Functions (N, F)));
               when Type_Direction_Type =>
                  Put_Field (F, Image_Direction_Type
                               (Get_Direction_Type (N, F)));
               when Type_Iir_Int32 =>
                  Put_Field
                    (F, Strip (Iir_Int32'Image (Get_Iir_Int32 (N, F))));
               when Type_Int32 =>
                  Put_Field (F, Strip (Int32'Image (Get_Int32 (N, F))));
               when Type_Fp64 =>
                  Put_Field (F, Fp64'Image (Get_Fp64 (N, F)));
               when Type_Time_Stamp_Id =>
                  Put_Field (F, Image_Time_Stamp_Id
                               (Get_Time_Stamp_Id (N, F)));
               when Type_File_Checksum_Id =>
                  Put_Field (F, Image_File_Checksum_Id
                               (Get_File_Checksum_Id (N, F)));
               when Type_Token_Type =>
                  Put_Field (F, Image_Token_Type (Get_Token_Type (N, F)));
               when Type_Name_Id =>
                  Put_Field (F, XML_Image (Get_Name_Id (N, F)));
               when Type_Source_File_Entry =>
                  null;
            end case;
         end loop;

         Put_Stag_End;

         for I in Fields'Range loop
            F := Fields (I);
            case Get_Field_Type (F) is
               when Type_Iir =>
                  declare
                     V : constant Iir := Get_Iir (N, F);
                     Img : constant String := Get_Field_Image (F);
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Disp_Iir (Img, V);
                        when Attr_Ref
                          | Attr_Forward_Ref
                          | Attr_Maybe_Forward_Ref =>
                           Disp_Iir_Ref (Img, V);
                        when Attr_Maybe_Ref =>
                           if Get_Is_Ref (N) then
                              Disp_Iir_Ref (Img, V);
                           else
                              Disp_Iir (Img, V);
                           end if;
                        when Attr_Chain =>
                           Disp_Iir_Chain (Img, V);
                        when Attr_Chain_Next =>
                           null;
                        when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                           raise Internal_Error;
                     end case;
                  end;
               when Type_Iir_List =>
                  declare
                     L : constant Iir_List := Get_Iir_List (N, F);
                     Img : constant String := Get_Field_Image (F);
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Disp_Iir_List (Img, L, False);
                        when Attr_Of_Ref =>
                           Disp_Iir_List (Img, L, True);
                        when Attr_Of_Maybe_Ref =>
                           Disp_Iir_List (Img, L, Get_Is_Ref (N));
                        when Attr_Ref =>
                           Disp_Iir_List_Ref (Img, L);
                        when others =>
                           raise Internal_Error;
                     end case;
                  end;
               when Type_Iir_Flist =>
                  declare
                     L : constant Iir_Flist := Get_Iir_Flist (N, F);
                     Img : constant String := Get_Field_Image (F);
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Disp_Iir_Flist (Img, L, False);
                        when Attr_Of_Ref =>
                           Disp_Iir_Flist (Img, L, True);
                        when Attr_Of_Maybe_Ref =>
                           Disp_Iir_Flist (Img, L, Get_Is_Ref (N));
                        when Attr_Ref =>
                           Disp_Iir_Flist_Ref (Img, L);
                        when others =>
                           raise Internal_Error;
                     end case;
                  end;
               when Type_String8_Id =>
                  --  Special handling for strings
                  declare
                     Len : constant Int32 := Get_String_Length (N);
                  begin
                     Put_Stag (Get_Field_Image (F));
                     Put_Attribute ("length", Strip (Int32'Image (Len)));
                     Put_Attribute ("content", To_XML (Image_String8 (N)));
                     Put_Empty_Stag_End;
                  end;
               when others =>
                  null;
            end case;
         end loop;
      end;

      Put_Etag (Id);
   end Disp_Iir;

   --  Command --file-to-xml
   type Command_File_To_Xml is new Command_Lib with null record;

   function Decode_Command (Cmd : Command_File_To_Xml; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_File_To_Xml) return String;

   procedure Perform_Action (Cmd : in out Command_File_To_Xml;
                             Files_Name : Argument_List);

   function Decode_Command (Cmd : Command_File_To_Xml; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "file-to-xml"
        or else Name = "--file-to-xml";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_File_To_Xml) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "file-to-xml FILEs"
        & ASCII.LF & "  Dump AST in XML"
        & ASCII.LF & "  alias: --file-to-xml";
   end Get_Short_Help;

   procedure Perform_Action
     (Cmd : in out Command_File_To_Xml; Files_Name : Argument_List)
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

      Indent := 0;
      Col := 0;
      Put_Line
        ("<?xml version=""1.0"" encoding=""UTF-8"" standalone=""yes""?>");
      Put_Stag ("root");
      Put_Attribute ("version", "0.13");
      Put_Stag_End;
      Disp_Iir_Chain_Elements (Libraries.Get_Libraries_Chain);
      Put_Etag ("root");
   exception
      when Compilation_Error =>
         Error ("xml dump failed due to compilation error");
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_File_To_Xml);
   end Register_Commands;
end Ghdlxml;
