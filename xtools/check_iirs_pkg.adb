--  Tool to check the coherence of the iirs package.
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
with GNAT.Spitbol; use GNAT.Spitbol;
with GNAT.Spitbol.Patterns; use GNAT.Spitbol.Patterns;
with GNAT.Spitbol.Table_Integer; use GNAT.Spitbol.Table_Integer;
with GNAT.Table;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Command_Line; use Ada.Command_Line;

package body Check_Iirs_Pkg is
   --  Exception raise in case of error.
   Err : exception;

   --  Identifier get by getident_pat.
   Ident : VString := Nul;
   Ident_2 : VString := Nul;
   Ident_3 : VString := Nul;
   Ident_4 : VString := Nul;
   Ident_5 : VString := Nul;

   --  Enumel_Pat set this variable to the position of the comma.
   --  Used to detect the absence of a comma.
   Comma_Pos : aliased Natural;

   --  Patterns
   --  Space.
   Wsp : Pattern := Span (' ');

   --  "type Iir_Kind is".
   Type_Iir_Kind_Pat : Pattern :=
     Wsp & "type" & Wsp & "Iir_Kind" & Wsp & "is" & Rpos (0);

   --  "("
   Lparen_Pat : Pattern := Wsp & '(' & Rpos (0);

   --  Comment.
   Comment_Pat : Pattern := Wsp & "--";

   --  End of ada line
   Eol_Pat : Pattern := Comment_Pat or Rpos (0);

   --  "," followed by EOL.
   Comma_Eol_Pat : Pattern := ',' & Eol_Pat;

   --  A-Za-z
   Basic_Pat : Pattern := Span (Basic_Set);

   --  A-Za-z0-9
   Alnum_Pat : Pattern := Span (Alphanumeric_Set);

   --  Ada identifier.
   Ident_Pat : Pattern := Basic_Pat & Arbno (('_' or "") & Alnum_Pat);
   -- Basic_Pat & Arbno (Alnum_Pat) & Arbno ('_' & Alnum_Pat);

   --  Eat the ada identifier.
   Getident_Pat : Pattern := Ident_Pat * Ident;
   Getident2_Pat : Pattern := Ident_Pat * Ident_2;
   Getident3_Pat : Pattern := Ident_Pat * Ident_3;
   Getident4_Pat : Pattern := Ident_Pat * Ident_4;
   Getident5_Pat : Pattern := Ident_Pat * Ident_5;

   --  Get an enumeration elements.
   Enumel_Pat : Pattern := Wsp & Getident_Pat
     & ((',' & Setcur (Comma_Pos'Access)) or "") & Eol_Pat;

   --  End of an enumeration declaration.
   End_Enum_Pat : Pattern := Wsp & ");" & Eol_Pat;

   Format_Pat : Pattern := "      Format_" & Getident_Pat
     & ((',' & Setcur (Comma_Pos'Access)) or "") & Eol_Pat;

   Fields_Of_Format_Pat : Pattern := "   -- Fields of Format_" & Getident_Pat
     & ":" & Rpos (0);

   --  "subtype XX is Iir_Kind range".
   Iir_Kind_Subtype_Pat : Pattern :=
     Wsp & "subtype" & Wsp & Getident_Pat & Wsp & "is" & Wsp & "Iir_Kind"
     & Wsp & "range" & Eol_Pat;

   --  Pattern for a range.
   Start_Range_Pat : Pattern := Wsp & Getident_Pat & Wsp & ".." & Eol_Pat;
   Comment_Range_Pat : Pattern := Wsp & "--" & Getident_Pat & Rpos (0);
   End_Range_Pat : Pattern := Wsp & Getident_Pat & ";" & Eol_Pat;

   --  End of public package part.
   End_Pat : Pattern := "end Iirs;" & Rpos (0);

   --  Pattern for a function field.
   Func_Decl_Pat : Pattern := "   --  Field: " & Getident_Pat
     & ( "" or (" (" & Getident2_Pat & ")")) & Rpos (0);

   --  function Get_XXX.
   Function_Get_Pat : Pattern := "   function Get_"  & Getident_Pat
     & " (" & Getident2_Pat & " : " & Getident3_Pat & ") return "
     & Getident4_Pat & ";" & Rpos (0);

   --  procedure Set_XXX.
   Procedure_Set_Pat : Pattern := "   procedure Set_"  & Getident_Pat
     & " (" & Getident2_Pat & " : " & Getident3_Pat
     & "; " & Getident4_Pat & " : " & Getident5_Pat & ");" & Rpos (0);

   Field_Decl_Pat : Pattern := "   --   " & Getident_Pat & " : ";
   Field_Type_Pat : Pattern := "   --   " & Ident_Pat & " : "
     & Getident_Pat & ("" or (" (" & Arb & ")")) & Rpos (0);

   --  Formats of nodes.
   type Format_Type is range 0 .. 7;
   No_Format : constant Format_Type := 0;
   Format_Pos : Format_Type := No_Format;

   Format2pos : GNAT.Spitbol.Table_Integer.Table (8);

   type Format_Info is record
      Name : String_Access;
   end record;

   Formats : array (Format_Type) of Format_Info := (others => (Name => null));

   type Format_Mask_Type is array (Format_Type) of Boolean;
   pragma Pack (Format_Mask_Type);

   --  Type of a IIR name.
   type Iir_Type is new Natural range 0 .. 255;
   No_Iir : constant Iir_Type := 0;

   --  Table to convert an Iir name to its position.
   Iir_Kind2pos : GNAT.Spitbol.Table_Integer.Table (256);
   --  Last iir used during table construction.
   Iir_Pos : Iir_Type := No_Iir;

   --  Table of Get_ functions.
   Function2pos : GNAT.Spitbol.Table_Integer.Table (256);

   --  Table of field.
   Field2pos : GNAT.Spitbol.Table_Integer.Table (32);

   type Range_Type is record
     L : Iir_Type;
     H : Iir_Type;
   end record;

   Null_Range : constant Range_Type := (No_Iir, No_Iir);

   function Img (Rng : Range_Type) return String is
   begin
      return "(" & Iir_Type'Image (Rng.L) & ", "
        & Iir_Type'Image (Rng.H) & ")";
   end Img;

   package Table_Range is new GNAT.Spitbol.Table (Range_Type, Null_Range, Img);
   use Table_Range;

   Iir_Kinds2pos : Table_Range.Table (32);

   --  Field type.  They represent a raw field.
   type Field_Type is new Integer range 0 .. 64;
   No_Field : constant Field_Type := 0;
   --  Position of the last field.
   Field_Pos : Field_Type := No_Field;

   type Field_Info is record
      --  Name of the field.
      Name : String_Access;
      --  Type of the field.
      Ftype : String_Access;
      --  Formats in which the field is valid.
      Formats : Format_Mask_Type;
   end record;

   package Field_Table is new GNAT.Table
     (Table_Component_Type => Field_Info,
      Table_Index_Type => Field_Type,
      Table_Low_Bound => 1,
      Table_Initial => 32,
      Table_Increment => 100);

   --  Function type.  They represent a field name.
   type Func_Type is new Natural;
   No_Func : constant Func_Type := 0;
   --  Last function known; used during the construction of the func_table.
   Function_Pos : Func_Type := No_Func;

   type Field2Func_Array is array (Field_Type) of Func_Type;

   --  Information for each Iir node.
   type Iir_Info is record
      --  Name of the Kind.
      Name : String_Access;

      --  If TRUE, the node was described.
      Described : Boolean;

      --  Format used by the node.
      Format : Format_Type;

      --  Function used to get the value of each field.
      Func : Field2Func_Array;
   end record;

   --  Table of IIr.
   package Iir_Table is new GNAT.Table
     (Table_Component_Type => Iir_Info,
      Table_Index_Type => Iir_Type,
      Table_Low_Bound => 1,
      Table_Initial => 256,
      Table_Increment => 100);

   --  Table of functions.
   type Iir_Bool_Array is array (Iir_Type) of Boolean;
   pragma Pack (Iir_Bool_Array);

   type Conversion_Type is (None, Via_Pos_Attr, Via_Unchecked);

   type Func_Info is record
      --  Name of the function.
      Name : Vstring;
      --  Field get/set by the function.
      Field : Field_Type;
      --  If true, the iir use this function.
      Uses : Iir_Bool_Array;
      --  Name of the target.
      Target_Name : String_Access;
      --  Type of the target.
      Target_Type : String_Access;
      --  Name of the value.
      Value_Name : String_Access;
      --  Type of the value.
      Value_Type : String_Access;
      --  Conversion;
      Conv : Conversion_Type;
   end record;

   package Func_Table is new GNAT.Table
     (Table_Component_Type => Func_Info,
      Table_Index_Type => Func_Type,
      Table_Low_Bound => 1,
      Table_Initial => 256,
      Table_Increment => 100);

   --  Get the position of IIR V.
   function Get_Iir_Pos (V : VString) return Iir_Type
   is
      P : Integer;
   begin
      P := Get (Iir_Kind2pos, V);

      if P < 0 then
         --  Identifier unknown.
         raise Err;
      end if;
      return Iir_Type (P);
   end Get_Iir_Pos;

   Disp_Func : Boolean := False;

   Flag_Disp_Format : Boolean := False;
   Flag_Disp_Field : Boolean := False;

   procedure Read_Fields
   is
      In_Node : File_Type;
      Line : VString := Nul;

      Format_Mask : Format_Mask_Type;

      procedure Parse_Field
      is
         P : Integer;
         Name : Vstring := Ident;
      begin
         if not Match (Line, Field_Type_Pat) then
            Put_Line ("**  field declaration without type");
            raise Err;
         end if;

         --  Check if the field is not already known.
         P := Get (Field2pos, Name);
         if P > 0 then
            if Ident /= Field_Table.Table (Field_Type (P)).Ftype.all then
               Put_Line ("*** field type mismatch");
               raise Err;
            end if;
            for I in Format_Mask'Range loop
               if Format_Mask (I) then
                  Field_Table.Table (Field_Type (P)).Formats (I) := True;
               end if;
            end loop;
            return;
         end if;

         Field_Pos := Field_Pos + 1;
         Set (Field2pos, Name, Natural (Field_Pos));
         Field_Table.Set_Last (Field_Pos);
         Field_Table.Table (Field_Pos) :=
           (Name => new String'(To_String (Name)),
            Ftype => new String'(To_String (Ident)),
            Formats => Format_Mask);
         if Flag_Disp_Field then
            Put_Line ("found field '"
                      & Field_Table.Table (Field_Pos).Name.all & "'");
         end if;
      end Parse_Field;
   begin
      Open (In_Node, In_File, "../nodes.ads");

      Anchored_Mode := True;

      --  Read lines until "type format_type is":
      loop
         Line := Get_Line (In_Node);
         exit when Match (Line, "   type Format_Type is" & Rpos (0));
      end loop;
      --  Expect '('.
      Line := Get_Line (In_Node);
      if not Match (Line, "     (" & Rpos (0)) then
         raise Err;
      end if;

      --  Read all formats.
      loop
         Line := Get_Line (In_Node);

         --  Read the identifier.
         Comma_Pos := 0;
         if not Match (Line, Format_Pat) then
            raise Err;
         end if;

         --  Put it into the table.
         Format_Pos := Format_Pos + 1;
         Set (Format2Pos, Ident, Natural (Format_Pos));
         Formats (Format_Pos) := (Name => new String'(To_String (Ident)));
         if Flag_Disp_Format then
            Put_Line ("found format " & S (Ident));
         end if;

         --  If there is no comma, then this is the end of enumeration.
         exit when Comma_Pos = 0;
      end loop;

      --  Read ");"
      Line := Get_Line (In_Node);
      if not Match (Line, "     );" & Rpos (0)) then
         raise Err;
      end if;

      --  Read fields.

      loop
         Line := Get_Line (In_Node);
         exit when Match (Line, "   -- Common fields are:" & Rpos (0));
      end loop;
      Format_Mask := (others => True);
      loop
         Line := Get_Line (In_Node);
         if Match (Line, Field_Decl_Pat) then
            Parse_Field;
         elsif Match (Line, Rpos (0)) then
            Line := Get_Line (In_Node);
            exit when not Match (Line, Fields_Of_Format_Pat);
            declare
               P : Integer;
            begin
               P := Get (Format2pos, Ident);
               if P < 0 then
                  Put_Line ("*** unknown format");
                  raise Err;
               end if;
               Format_Mask := (others => False);
               Format_Mask (Format_Type (P)) := True;
            end;
         else
            Put_Line ("**  bad line in field declarations");
            raise Err;
         end if;
      end loop;
      Close (In_Node);

      if False then
        Put_Line ("Fields:");
        for I in 1 .. Field_Pos loop
           Put (Field_Table.Table (I).Name.all);
           Put (": ");
           Put (Field_Table.Table (I).Ftype.all);
           Put ("  ");
           for J in Format_Mask_Type'Range loop
              if Field_Table.Table (I).Formats (J)
                and then Formats (J).Name /= null
              then
                 Put (" ");
                 Put (Formats (J).Name.all);
              end if;
           end loop;
           New_Line;
        end loop;
      end if;
   end Read_Fields;

   --  Read all Iir_Kind_* names and put them into Iir_Table.
   --  Fill Iir_Kinds2pos
   --  Fill Func_Table.
   procedure Check_Iirs
   is
      --  iirs.ads file.
      In_Iirs : File_Type;

      --  Line read from In_Iirs.
      Line : VString := Nul;
   begin
      --  Open the file.
      Open (In_Iirs, In_File, "../iirs.ads");

      Anchored_Mode := True;

      --  Read lines until "type Iir_Kind is"
      loop
         Line := Get_Line (In_Iirs);
         exit when Match (Line, Type_Iir_Kind_Pat);
      end loop;

      if Flag_Disp_Iir then
         Put_Line ("found iir_kind at line"
                   & Positive_Count'Image (Ada.Text_IO.Line (In_Iirs)));
      end if;

      --Debug_Mode := True;

      --  Read '('
      Line := Get_Line (In_Iirs);
      if not Match (Line, Lparen_Pat) then
         raise Err;
      end if;

      --  Read all kind.
      loop
         Line := Get_Line (In_Iirs);

         --  Skip comments and empty lines.
         if Match (Line, Eol_Pat) then
            goto Continue;
         end if;

         --  Read the identifier.
         Comma_Pos := 0;
         if not Match (Line, Enumel_Pat) then
            raise Err;
         end if;

         --  Put it into the table.
         Iir_Pos := Iir_Pos + 1;
         Set (Iir_Kind2pos, Ident, Natural (Iir_Pos));
         Iir_Table.Set_Last (Iir_Pos);
         Iir_Table.Table (Iir_Pos) := (Name => new String'(To_String (Ident)),
                                       Described => False,
                                       Format => No_Format,
                                       Func => (others => No_Func));
         if Flag_Disp_Iir then
            Put_Line ("found " & S (Ident) & Iir_Type'Image (Iir_Pos));
         end if;

         --  If there is no comma, then this is the end of enumeration.
         exit when Comma_Pos = 0;
         << Continue >> null;
      end loop;

      --  Read ");"
      Line := Get_Line (In_Iirs);
      if not Match (Line, End_Enum_Pat) then
         raise Err;
      end if;

      --  Look for iir_kind subtype.
      loop
         Line := Get_Line (In_Iirs);
         exit when Match (Line, End_Pat);

         Ident_2 := Null_Unbounded_String;

         if Match (Line, Iir_Kind_Subtype_Pat) then
            declare
               Start : Iir_Type;
               Pos : Iir_Type;
               P : Iir_Type;
               Rng_Ident : VString := Ident;
            begin
               Line := Get_Line (In_Iirs);
               if not Match (Line, Start_Range_Pat) then
                  --  Bad pattern for left bound.
                  raise Err;
               end if;
               Start := Get_Iir_Pos (Ident);
               Pos := Start;
               if Flag_Disp_Subtype then
                  Put_Line ("found subtype " & S (Rng_Ident));
                  Put_Line ("     " & S (Ident) & " .."
                            & Iir_Type'Image (Pos));
               end if;

               loop
                  Line := Get_Line (In_Iirs);
                  if Match (Line, End_Range_Pat) then
                     P := Get_Iir_Pos (Ident);
                     if P /= Pos + 1 and then Flag_Disp_Subtype Then
                        Put_Line ("** missing comments");
                        for I in Pos + 1 .. P - 1 loop
                           Put_Line ("   --" & Iir_Table.Table (I).Name.all);
                        end loop;
                     end if;
                     Set (Iir_Kinds2pos, Rng_Ident, Range_Type'(Start, P));
                     if Flag_Disp_Subtype then
                        Put_Line ("     " & S (Ident) & Iir_Type'Image (P));
                     end if;
                     exit;
                  elsif Match (Line, Comment_Range_Pat) then
                     P := Get_Iir_Pos (Ident);
                     if P /= Pos + 1 then
                        --  Bad order.
                        raise Err;
                     else
                        Pos := Pos + 1;
                     end if;
                  else
                     --  Comment (with identifier) or end of range expected.
                     raise Err;
                  end if;
               end loop;
            end;
         elsif Match (Line, Func_Decl_Pat) then
            declare
               Field_Pos : Integer;
               F : Func_Type;
               Conv : Conversion_Type;
            begin
               Field_Pos := Get (Field2pos, Ident);
               if Field_Pos < 0 then
                  Put_Line ("*** field not found: '" & S (Ident) & "'");
                  raise Err;
               end if;

               if Ident_2 /= Null_Unbounded_String then
                  if Ident_2 = "pos" then
                     Conv := Via_Pos_Attr;
                  elsif Ident_2 = "uc" then
                     Conv := Via_Unchecked;
                  else
                     Put_Line ("*** bad conversion");
                     raise Err;
                  end if;
               else
                  Conv := None;
               end if;

               Line := Get_Line (In_Iirs);
               if not Match (Line, Function_Get_Pat) then
                  Put_Line ("*** function expected");
                  raise Err;
               end if;

               if False then
                  Put_Line ("found function " & S (Ident));
               end if;
               Function_Pos := Function_Pos + 1;
               F := Function_Pos;
               Set (Function2pos, Ident, Integer (Function_Pos));
               Func_Table.Set_Last (Function_Pos);
               Func_Table.Table (Function_Pos) :=
                 (Name => Ident,
                  Field => Field_Type (Field_Pos),
                  Uses => (others => False),
                  Target_Name => new String'(To_String (Ident_2)),
                  Target_Type => new String'(To_String (Ident_3)),
                  Value_Name => null,
                  Value_Type => new String'(To_String (Ident_4)),
                  Conv => Conv);

               Line := Get_Line (In_Iirs);
               if Match (Line, Procedure_Set_Pat) then
                  if Func_Table.Table (F).Target_Name.all /= Ident_2 then
                     Put_Line ("*** procedure target name mismatch ("
                               & Func_Table.Table (F).Target_Name.all
                               & " vs " & S (Ident_2) &")");
                     raise Err;
                  end if;
                  if Func_Table.Table (F).Target_Type.all /= Ident_3 then
                     Put_Line ("*** procedure target type name mismatch");
                     raise Err;
                  end if;
                  if Func_Table.Table (F).Value_Type.all /= Ident_5 then
                     Put_Line ("*** procedure target type name mismatch");
                     raise Err;
                  end if;
                  Func_Table.Table (F).Value_Name :=
                    new String'(To_String (Ident_4));
               else
                  if not Match (Line, Rpos (0)) then
                     Put_Line ("*** procedure or empty line expected");
                     raise Err;
                  end if;
               end if;
            end;
         end if;
      end loop;
      Close (In_Iirs);
      Set_Exit_Status (Success);
   exception
      when Err =>
         Put_Line ("*** Fatal error at line"
                   & Positive_Count'Image (Ada.Text_IO.Line (In_Iirs)));
         Set_Exit_Status (Failure);
         raise;
   end Check_Iirs;

   --  Start of node description.
   Start_Of_Iir_Kind_Pat : Pattern := "   -- Start of Iir_Kind." & Rpos (0);
   End_Of_Iir_Kind_Pat : Pattern := "   -- End of Iir_Kind." & Rpos (0);

   --  Box ("----------") delimiters.
   Box_Delim_Pat : Pattern := "   --" & Span ('-') & Rpos (0);

   --  Inside a box ("-- XXX --").
   Box_Inside_Pat : Pattern := "   --" & Arb & "--" & Rpos (0);

   --  Get a iir_kind identifier.
   Desc_Iir_Kind_Pat : Pattern :=
     "   -- " & Getident_Pat
     & ("" or ( " (" & Getident2_Pat & ")"))
     & Rpos (0);

   Subprogram_Pat : Pattern := "   --   Get" & ("_" or "/Set_") & Getident_Pat
     & ((" " & Arb) or "") & Rpos (0);

   Desc_Only_For_Pat : Pattern := "   -- Only for " & Getident_Pat & ":"
     & Rpos (0);
   Desc_Comment_Pat : Pattern := "   -- " & (Alnum_Pat or Any ("*_(.|"));
   Desc_Empty_Pat : Pattern := "   --" & Rpos (0);
   Desc_Subprogram_Pat : Pattern := "   --   " & ("function" or "procedure");

   Field_Pat : Pattern := Arb & "(" & Getident_Pat & ")";
   Alias_Field_Pat : Pattern := Arb & "(Alias " & Getident_Pat & ")";

   Disp_Desc : Boolean := False;

   --  Check descriptions.
   procedure Read_Desc
   is
      --  iirs.ads file.
      In_Iirs : File_Type;

      --  Current line.
      Line : VString;

      --  IIR being described.
      type Iir_Array is array (Natural range <>) of Iir_Type;
      Iir_Desc : Iir_Array (1 .. 32);
      Nbr_Desc : Natural := 0;

      Only_For : Iir_Array (1 .. 16) := (others => No_Iir);
      Nbr_Only_For : Natural := 0;

      --  Just say IIR N is being described.
      procedure Add_Desc (N : Iir_Type; Format : Format_Type) is
      begin
         if Iir_Table.Table (N).Described then
            Put_Line ("*** iir already described");
            raise Err;
         end if;

         Iir_Table.Table (N).Described := True;
         Iir_Table.Table (N).Format := Format;
         Nbr_Desc := Nbr_Desc + 1;
         Iir_Desc (Nbr_Desc) := N;
      end Add_Desc;

   begin
      --  Open the file.
      Open (In_Iirs, In_File, "../iirs.ads");

      Anchored_Mode := True;

      if False then
         --  List of fields.
         Set (Field2pos, "Field1", 1);
         Set (Field2pos, "Field2", 2);
         Set (Field2pos, "Field3", 3);
         Set (Field2pos, "Field4", 4);
         Set (Field2pos, "Field5", 5);
         Set (Field2pos, "Field6", 6);
         Set (Field2pos, "Field7", 7);
         Set (Field2pos, "Nbr2", 6);
         Set (Field2pos, "Nbr3", 7);

         Set (Field2pos, "Ident", 8);
         Set (Field2pos, "Field0", 9);
         Set (Field2pos, "Attr", 10);
         Set (Field2pos, "Chain", 11);

         Set (Field2pos, "Flag1", 12);
         Set (Field2pos, "Flag2", 13);
         Set (Field2pos, "Flag3", 14);
         Set (Field2pos, "Flag4", 15);
         Set (Field2pos, "Flag5", 16);
         Set (Field2pos, "Odigit_1", 17);
         Set (Field2pos, "Odigit_2", 18);
         Set (Field2pos, "State1", 19);
         Set (Field2pos, "Staticness_1", 20);
         Set (Field2pos, "Staticness_2", 21);
      end if;

      --  Read lines until "-- Start of Iir_Kind."
      loop
         Line := Get_Line (In_Iirs);
         exit when Match (Line, Start_Of_Iir_Kind_Pat);
      end loop;

      --Debug_Mode := True;

      --  Read descriptions.
      L1 : loop

         --  Empty lines.
         loop
            Line := Get_Line (In_Iirs);
            exit when not Match (Line, Rpos (0));
         end loop;

         if Match (Line, Box_Delim_Pat) then
            --  A box.
            Line := Get_Line (In_Iirs);
            if not Match (Line, Box_Inside_Pat) then
               raise Err;
            end if;
            Line := Get_Line (In_Iirs);
            if not Match (Line, Box_Delim_Pat) then
               raise Err;
            end if;
         else
            --  A description.
            if not Match (Line, "   -- Iir_Kind") then
               if Match (Line, End_Of_Iir_Kind_Pat) then
                  exit L1;
               elsif Match (Line, "   -- For Iir_Kinds_") then
                  null;
               else
                  raise Err;
               end if;
            end if;

            --  Get iir_kind.
            declare
               P_Num : Integer;
               Rng : Range_Type;
               Format : Format_Type;
            begin
               --  No iir being described.
               Nbr_Desc := 0;
               loop
                  Ident_2 := Nul;
                  exit when not Match (Line, Desc_Iir_Kind_Pat);

                  --  Check format.
                  if Ident_2 = Nul then
                     Put_Line ("*** no format for " & S (Ident));
                     raise Err;
                  end if;
                  P_Num := Get (Format2pos, Ident_2);
                  if P_Num < 0 then
                     Put_Line ("*** unknown format");
                     raise Err;
                  end if;
                  Format := Format_Type (P_Num);

                  --  Handle nodes.
                  P_Num := Get (Iir_Kind2pos, Ident);
                  if P_Num >= 0 then
                     Add_Desc (Iir_Type (P_Num), Format);
                  else
                     Rng := Get (Iir_Kinds2pos, Ident);
                     if Rng = Null_Range then
                        Put_Line ("*** " & S (Ident));
                        raise Err;
                     end if;
                     for I in Rng.L .. Rng.H loop
                        Add_Desc (I, Format);
                     end loop;
                  end if;

                  if Disp_Desc then
                     Put_Line ("desc for " & S (Ident));
                  end if;

                  Line := Get_Line (In_Iirs);
               end loop;
            end;

            --Debug_Mode := True;

            --  Read the functions.
            loop
               if not Match (Line, Comment_Pat) then
                  if Match (Line, Rpos (0)) then
                     exit;
                  else
                     raise Err;
                  end if;
               end if;
               declare
                  Func : Func_Type;
                  Func_Num : Integer;
                  Field : Field_Type;
                  Field_Num : Integer;
                  Is_Alias : Boolean;

                  procedure Add_Field (N : Iir_Type) is
                  begin
                     if not Field_Table.Table (Field).
                       Formats (Iir_Table.Table (N).Format)
                     then
                        Put_Line ("** no field for format");
                        raise Err;
                     end if;
                     if Is_Alias then
                        if Iir_Table.Table (N).Func (Field) = No_Func
                        then
                           Put_Line ("**  aliased field not yet used");
                           raise Err;
                        end if;
                     else
                        if Iir_Table.Table (N).Func (Field) /= No_Func
                          --and then
                          --Iir_Table.Table (N).Func (Field) /= Func
                        then
                           Put_Line ("**  Field already used");
                           raise Err;
                        end if;
                        Iir_Table.Table (N).Func (Field) := Func;
                     end if;
                     Func_Table.Table (Func).Uses (N) := True;
                  end Add_Field;
               begin
                  if Match (Line, Subprogram_Pat) then
                     if Disp_Desc then
                        Put ("subprg: " & S (Ident));
                     end if;
                     Func_Num := Get (Function2pos, Ident);
                     if Func_Num < 0 then
                        Put_Line (Standard_Error,
                                  "*** function not found: " & S (Ident));
                        raise Err;
                     end if;
                     Func := Func_Type (Func_Num);
                     if Match (Line, Field_Pat) then
                        Is_Alias := False;
                     elsif Match (Line, Alias_Field_Pat) then
                        Is_Alias := True;
                     else
                        raise Err;
                     end if;
                     if Disp_Desc then
                        Put_Line ("  (" & S (Ident) & ")");
                     end if;
                     Field_Num := Get (Field2pos, Ident);
                     if Field_Num < 0 then
                        Put_Line ("*** unknown field: " & S (Ident));
                        raise Err;
                     end if;
                     Field := Field_Type (Field_Num);
                     if Func_Table.Table (Func).Field /= Field then
                        if Func_Table.Table (Func).Field = No_Field then
                           Func_Table.Table (Func).Field := Field;
                        else
                           --  Field redefined for the function.
                           Put_Line ("**  field redefined for the function");
                           raise Err;
                        end if;
                     end if;

                     --  Check the field is not already used by another func.
                     if Nbr_Only_For > 0 then
                        for I in 1 .. Nbr_Only_For loop
                           Add_Field (Only_For (I));
                        end loop;
                        Nbr_Only_For := 0;
                     else
                        for I in 1 .. Nbr_Desc loop
                           Add_Field (Iir_Desc (I));
                        end loop;
                     end if;
                  elsif Match (Line, Desc_Only_For_Pat) then
                     declare
                        P_Num : Integer;
                        Rng : Range_Type;

                        procedure Add_Only_For (N : Iir_Type) is
                        begin
                           for I in 1 .. Nbr_Desc loop
                              if Iir_Desc (I) = N then
                                 Nbr_Only_For := Nbr_Only_For + 1;
                                 Only_For (Nbr_Only_For) := N;
                                 return;
                              end if;
                           end loop;
                           Put_Line ("**  not currently described");
                           raise Err;
                        end Add_Only_For;
                     begin
                        P_Num := Get (Iir_Kind2pos, Ident);
                        if P_Num >= 0 then
                           Add_Only_For (Iir_Type (P_Num));
                        else
                           Rng := Get (Iir_Kinds2pos, Ident);
                           if Rng = Null_Range then
                              Put_Line ("*** " & S (Ident));
                              raise Err;
                           end if;
                           for I in Rng.L .. Rng.H loop
                              Add_Only_For (I);
                           end loop;
                        end if;
                     end;
                  elsif Match (Line, "   -- Only") then
                     Put_Line ("**  bad only for line");
                     raise Err;
                  elsif Match (Line, Desc_Comment_Pat) then
                     null;
                  elsif Match (Line, Desc_Empty_Pat) then
                     null;
                  elsif Match (Line, Desc_Subprogram_Pat) then
                     null;
                  else
                     raise Err;
                  end if;
               end;
               Line := Get_Line (In_Iirs);
            end loop;
         end if;
      end loop L1;

      --  Check each Iir was described.
      for I in Iir_Table.First .. Iir_Table.Last loop
         if not Iir_Table.Table (I).Described then
            Put_Line ("*** not described: " & Iir_Table.Table (I).Name.all);
            raise Err;
         end if;
      end loop;

      Close (In_Iirs);
   exception
      when Err =>
         Put_Line ("*** Fatal error at line"
                   & Positive_Count'Image (Ada.Text_IO.Line (In_Iirs) - 1));
         Put_Line ("*** Line is " & S (Line));
         Set_Exit_Status (Failure);
         raise;
   end Read_Desc;

   procedure Gen_Func
   is
      function Is_Used (F : Func_Type) return Boolean
      is
      begin
         for I in Func_Table.Table (F).Uses'Range loop
            if Func_Table.Table (F).Uses (I) then
               return True;
            end if;
         end loop;
         return False;
      end Is_Used;
      Is_First : Boolean;
      Same_Name : Boolean;
   begin
      Put_Line ("   function Get_Format (Kind : Iir_Kind) "
                & "return Format_Type is");
      Put_Line ("   begin");
      Put_Line ("      case Kind is");
      for I in 1 .. Format_Pos loop
         Is_First := True;
         Put      ("         when ");
         for J in Iir_Table.First .. Iir_Table.Last loop
            if Iir_Table.Table (J).Format = I then
               if not Is_First then
                  New_Line;
                  Put ("           | ");
               end if;
               Is_First := False;
               Put (Iir_Table.Table (J).Name.all);
            end if;
         end loop;
         Put_Line (" =>");
         Put      ("            return Format_");
         Put (Formats (I).Name.all);
         Put_Line (";");
      end loop;
      Put_Line ("      end case;");
      Put_Line ("   end Get_Format;");
      New_Line;

      --  Builder.
      Put_Line ("   function Create_Iir (Kind : Iir_Kind) return Iir");
      Put_Line ("   is");
      Put_Line ("      Res : Iir;");
      Put_Line ("      Format : Format_Type;");
      Put_Line ("   begin");
      Put_Line ("      Format := Get_Format (Kind);");
      Put_Line ("      Res := Create_Node (Format);");
      Put_Line ("      Set_Nkind (Res, Iir_Kind'Pos (Kind));");
      Put_Line ("      return Res;");
      Put_Line ("   end Create_Iir;");
      New_Line;

      for I in Func_Table.First .. Func_Table.Last loop
         declare
            F : Func_Info renames Func_Table.Table (I);
         begin
            --  Avoid bug get_parent.
            if Is_Used (I) then
               Same_Name := F.Name = Field_Table.Table (F.Field).Name.all;
               if Flag_Checks then
                  Put ("   procedure Check_Kind_For_");
                  Put (F.Name);
                  Put (" (Target : Iir) is");
                  New_Line;
                  Put_Line ("   begin");
                  Put_Line ("      case Get_Kind (Target) is");
                  Put ("         when ");
                  Is_First := True;
                  for J in F.Uses'Range loop
                     if F.Uses (J) then
                        if not Is_First then
                           New_Line;
                           Put ("           | ");
                        else
                           Is_First := False;
                        end if;
                        Put (Iir_Table.Table (J).Name.all);
                     end if;
                  end loop;
                  Put_Line (" =>");
                  Put_Line ("            null;");
                  Put_Line ("         when others =>");
                  Put ("            Failed (""");
                  Put (F.Name);
                  Put_Line (""", Target);");
                  Put_Line ("      end case;");
                  Put ("   end Check_Kind_For_");
                  Put (F.Name);
                  Put_Line (";");
                  New_Line;
               end if;

               Put ("   function Get_");
               Put (F.Name);
               Put (" (");
               Put (F.Target_Name.all);
               Put (" : ");
               Put (F.Target_Type.all);
               Put (") return ");
               Put (F.Value_Type.all);
               if Col > 76 then
                  New_Line;
                  Put ("     ");
               end if;
               Put (" is");
               New_Line;
               Put_Line ("   begin");
               if Flag_Checks then
                  Put ("      Check_Kind_For_");
                  Put (F.Name);
                  Put (" (");
                  Put (F.Target_Name.all);
                  Put (");");
                  New_Line;
               end if;
               Put ("      return ");
               case F.Conv is
                  when None =>
                     null;
                  when Via_Pos_Attr =>
                     Put (F.Value_Type.all);
                     Put ("'Val (");
                  when Via_Unchecked =>
                     Put (Field_Table.Table (F.Field).Ftype.all);
                     Put ("_To_");
                     Put (F.Value_Type.all);
                     Put (" (");
               end case;
               if Same_Name then
                  Put ("Nodes.");
               end if;
               Put ("Get_");
               Put (Field_Table.Table (F.Field).Name.all);
               Put (" (");
               Put (F.Target_Name.all);
               Put (")");
               case F.Conv is
                  when None =>
                     null;
                  when Via_Pos_Attr
                    | Via_Unchecked =>
                     Put (")");
               end case;
               Put (";");
               New_Line;
               Put ("   end Get_");
               Put (F.Name);
               Put (";");
               New_Line;
               New_Line;

               if F.Value_Name /= null then
                  Put ("   procedure Set_");
                  Put (F.Name);
                  Put (" (");
                  Put (F.Target_Name.all);
                  Put (" : ");
                  Put (F.Target_Type.all);
                  Put ("; ");
                  Put (F.Value_Name.all);
                  Put (" : ");
                  Put (F.Value_Type.all);
                  Put (")");
                  if Col > 76 then
                     New_Line;
                     Put ("     ");
                  end if;
                  Put (" is");
                  New_Line;
                  Put_Line ("   begin");
                  if Flag_Checks then
                     Put ("      Check_Kind_For_");
                     Put (F.Name);
                     Put (" (");
                     Put (F.Target_Name.all);
                     Put (");");
                     New_Line;
                  end if;
                  Put ("      ");
                  if Same_Name then
                     Put ("Nodes.");
                  end if;
                  Put ("Set_");
                  Put (Field_Table.Table (F.Field).Name.all);
                  Put (" (");
                  Put (F.Target_Name.all);
                  Put (", ");
                  case F.Conv is
                     when None =>
                        null;
                     when Via_Pos_Attr =>
                        Put (F.Value_Type.all);
                        Put ("'Pos (");
                     when Via_Unchecked =>
                        Put (F.Value_Type.all);
                        Put ("_To_");
                        Put (Field_Table.Table (F.Field).Ftype.all);
                        Put (" (");
                  end case;
                  Put (F.Value_Name.all);
                  case F.Conv is
                     when None =>
                        null;
                     when Via_Pos_Attr
                       | Via_Unchecked =>
                        Put (")");
                  end case;
                  Put (");");
                  New_Line;
                  Put ("   end Set_");
                  Put (F.Name);
                  Put (";");
                  New_Line;
                  New_Line;
               end if;
            end if;
         end;
      end loop;
   end Gen_Func;

   procedure List_Free_Fields
   is
   begin
      for I in Iir_Table.First .. Iir_Table.Last loop
         declare
            Info : Iir_Info renames Iir_Table.Table (I);
         begin
            Put_Line (Info.Name.all);
            for J in 1 .. Field_Pos loop
               if Info.Func (J) = No_Func
                 and then Field_Table.Table (J).Formats (Info.Format)
               then
                  Put (" ");
                  Put_Line (Field_Table.Table (J).Name.all);
               end if;
            end loop;
         end;
      end loop;
   end List_Free_Fields;
end Check_Iirs_Pkg;

