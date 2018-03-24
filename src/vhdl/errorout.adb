--  Error message handling.
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

with Ada.Text_IO;
with GNAT.OS_Lib;
with Scanner;
with Name_Table;
with Iirs_Utils; use Iirs_Utils;
with Files_Map; use Files_Map;
with Ada.Strings.Unbounded;
with Std_Names;
with Flags; use Flags;
with PSL.Nodes;

package body Errorout is
   --  Name of the program, used to report error message.
   Program_Name : String_Acc := null;

   --  Terminal.

   --  Set Flag_Color_Diagnostics to On or Off if is was Auto.
   procedure Detect_Terminal
   is
      --  Import isatty.
      function isatty (Fd : Integer) return Integer;
      pragma Import (C, isatty);

      --  Awful way to detect if the host is Windows.  Should be replaced by
      --  a host-specific package.
      Is_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';
   begin
      if Flag_Color_Diagnostics = Auto then
         if Is_Windows then
            --  Off by default on Windows, as the consoles may not support
            --  ANSI control sequences.  Should be replaced by calls to the
            --  Win32 API.
            Flag_Color_Diagnostics := Off;
         else
            --  On Linux/Unix/Mac OS X: use color only when the output is to a
            --  tty.
            if isatty (2) /= 0 then
               Flag_Color_Diagnostics := On;
            else
               Flag_Color_Diagnostics := Off;
            end if;
         end if;
      end if;
   end Detect_Terminal;

   --  Color to be used for various part of messages.
   type Color_Type is (Color_Locus,
                       Color_Note, Color_Warning, Color_Error, Color_Fatal,
                       Color_Message,
                       Color_None);

   --  Switch to COLOR.
   procedure Set_Color (Color : Color_Type)
   is
      procedure Put (S : String)
      is
         use Ada.Text_IO;
      begin
         Put (Standard_Error, S);
      end Put;
   begin
      if Flag_Color_Diagnostics = Off then
         return;
      end if;

      --  Use ANSI sequences.
      --  They are also documented on msdn in 'Console Virtual Terminal
      --  sequences'.

      Put (ASCII.ESC & '[');
      case Color is
         when Color_Locus   => Put ("1");    --  Bold
         when Color_Note    => Put ("1;36"); --  Bold, cyan
         when Color_Warning => Put ("1;35"); --  Bold, magenta
         when Color_Error   => Put ("1;31"); --  Bold, red
         when Color_Fatal   => Put ("1;33"); --  Bold, yellow
         when Color_Message => Put ("0;1");  --  Normal, bold
         when Color_None    => Put ("0");    --  Normal
      end case;
      Put ("m");
   end Set_Color;

   --  Warnings.

   Warnings_Control : Warnings_Setting := Default_Warnings;

   procedure Enable_Warning (Id : Msgid_Warnings; Enable : Boolean) is
   begin
      Warnings_Control (Id).Enabled := Enable;
   end Enable_Warning;

   function Is_Warning_Enabled (Id : Msgid_Warnings) return Boolean is
   begin
      return Warnings_Control (Id).Enabled;
   end Is_Warning_Enabled;

   function Warning_Image (Id : Msgid_Warnings) return String
   is
      Img : constant String := Msgid_Warnings'Image (Id);

      --  Prefix to strip.
      Prefix : constant String := "WARNID_";
      pragma Assert (Img'Length > Prefix'Length);
      pragma Assert (Img (1 .. Prefix'Length) = Prefix);
      Res : String (1 .. Img'Last - Prefix'Length);
      C : Character;
   begin
      --  Convert to lower cases, and '_' to '-'.
      for I in Res'Range loop
         C := Img (Prefix'Length + I);
         case C is
            when '_' =>
               C := '-';
            when 'A' .. 'Z' =>
               C := Character'Val (Character'Pos (C) + 32);
            when others =>
               raise Internal_Error;
         end case;
         Res (I) := C;
      end loop;

      return Res;
   end Warning_Image;

   procedure Save_Warnings_Setting (Res : out Warnings_Setting) is
   begin
      Res := Warnings_Control;
   end Save_Warnings_Setting;

   procedure Disable_All_Warnings is
   begin
      Warnings_Control := (others => (Enabled => False, Error => False));
   end Disable_All_Warnings;

   procedure Restore_Warnings_Setting (Res : Warnings_Setting) is
   begin
      Warnings_Control := Res;
   end Restore_Warnings_Setting;

   --  Error arguments

   function "+" (V : Iir) return Earg_Type is
   begin
      return (Kind => Earg_Iir, Val_Iir => V);
   end "+";

   function "+" (V : Location_Type) return Earg_Type is
   begin
      return (Kind => Earg_Location, Val_Loc => V);
   end "+";

   function "+" (V : Name_Id) return Earg_Type is
   begin
      return (Kind => Earg_Id, Val_Id => V);
   end "+";

   function "+" (V : Tokens.Token_Type) return Earg_Type is
   begin
      return (Kind => Earg_Token, Val_Tok => V);
   end "+";

   function "+" (V : Character) return Earg_Type is
   begin
      return (Kind => Earg_Char, Val_Char => V);
   end "+";

   function Get_Location_Safe (N : Iir) return Location_Type is
   begin
      if N = Null_Iir then
         return Location_Nil;
      else
         return Get_Location (N);
      end if;
   end Get_Location_Safe;

   function "+" (L : Iir) return Location_Type renames Get_Location_Safe;

   function "+" (L : PSL_Node) return Location_Type
   is
      use PSL.Nodes;
   begin
      if L = Null_Node then
         return No_Location;
      else
         return PSL.Nodes.Get_Location (L);
      end if;
   end "+";

   Msg_Len : Natural;

   procedure Put (Str : String)
   is
      use Ada.Text_IO;
   begin
      Msg_Len := Msg_Len + Str'Length;
      Put (Standard_Error, Str);
   end Put;

   procedure Put (C : Character)
   is
      use Ada.Text_IO;
   begin
      Msg_Len := Msg_Len + 1;
      Put (Standard_Error, C);
   end Put;

   procedure Put_Line (Str : String := "")
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Str);
      Msg_Len := 0;
   end Put_Line;

   procedure Disp_Natural (Val: Natural)
   is
      Str: constant String := Natural'Image (Val);
   begin
      Put (Str (Str'First + 1 .. Str'Last));
   end Disp_Natural;

   procedure Error_Kind (Msg : String; An_Iir : Iir) is
   begin
      Put_Line (Msg & ": cannot handle "
                & Iir_Kind'Image (Get_Kind (An_Iir))
                & " (" & Disp_Location (An_Iir) & ')');
      raise Internal_Error;
   end Error_Kind;

   procedure Error_Kind (Msg : String; Def : Iir_Predefined_Functions) is
   begin
      Put_Line (Msg & ": cannot handle "
                & Iir_Predefined_Functions'Image (Def));
      raise Internal_Error;
   end Error_Kind;

   procedure Error_Kind (Msg : String; N : PSL_Node) is
   begin
      Put (Msg);
      Put (": cannot handle ");
      Put_Line (PSL.Nodes.Nkind'Image (PSL.Nodes.Get_Kind (N)));
      raise Internal_Error;
   end Error_Kind;

   procedure Disp_Location
     (File: Name_Id; Line: Natural; Col: Natural) is
   begin
      if File = Null_Identifier then
         Put ("??");
      else
         Put (Name_Table.Image (File));
      end if;
      Put (':');
      Disp_Natural (Line);
      Put (':');
      Disp_Natural (Col);
      Put (':');
   end Disp_Location;

   procedure Set_Program_Name (Name : String) is
   begin
      Program_Name := new String'(Name);
   end Set_Program_Name;

   procedure Disp_Program_Name is
   begin
      if Program_Name /= null then
         Put (Program_Name.all);
         Put (':');
      end if;
   end Disp_Program_Name;

   procedure Report_Msg (Id : Msgid_Type;
                         Origin : Report_Origin;
                         Loc : Location_Type;
                         Msg : String;
                         Args : Earg_Arr := No_Eargs;
                         Cont : Boolean := False)
   is
      pragma Unreferenced (Cont);
      procedure Location_To_Position (Location : Location_Type;
                                      File : out Source_File_Entry;
                                      Line : out Natural;
                                      Col : out Natural)
      is
         Name : Name_Id;
         Line_Pos : Source_Ptr;
         Offset : Natural;
      begin
         Location_To_Coord (Location, File, Line_Pos, Line, Offset);
         Coord_To_Position (File, Line_Pos, Offset, Name, Col);
      end Location_To_Position;

      File : Source_File_Entry;
      Line : Natural;
      Col : Natural;
      Progname : Boolean;
   begin
      --  By default, no location.
      File := No_Source_File_Entry;
      Line := 0;
      Col := 0;

      --  And no program name.
      Progname := False;

      Detect_Terminal;

      case Origin is
         when Option
           | Library =>
            Progname := True;
         when Elaboration =>
            if Loc = No_Location then
               Progname := True;
            else
               Location_To_Position (Loc, File, Line, Col);
            end if;
         when Scan =>
            if Loc = No_Location then
               File := Scanner.Get_Current_Source_File;
               Line := Scanner.Get_Current_Line;
               Col := Scanner.Get_Current_Column;
            else
               Location_To_Position (Loc, File, Line, Col);
            end if;
         when Parse =>
            if Loc = No_Location then
               File := Scanner.Get_Current_Source_File;
               Line := Scanner.Get_Current_Line;
               Col := Scanner.Get_Token_Column;
            else
               Location_To_Position (Loc, File, Line, Col);
            end if;
         when Semantic =>
            if Loc = No_Location then
               File := No_Source_File_Entry;
               Line := 0;
               Col := 0;
            else
               Location_To_Position (Loc, File, Line, Col);
            end if;
      end case;

      Msg_Len := 0;

      if Flag_Color_Diagnostics = On then
         Set_Color (Color_Locus);
      end if;

      if Progname then
         Disp_Program_Name;
      elsif File /= No_Source_File_Entry then
         Disp_Location (Get_File_Name (File), Line, Col);
      else
         Disp_Location (Null_Identifier, 0, 0);
      end if;

      --  Display level.
      declare
         Id_Level : Msgid_Type;
      begin
         if Flags.Warn_Error
           and then (Id = Msgid_Warning or Id in Msgid_Warnings)
         then
            Id_Level := Msgid_Error;
         else
            Id_Level := Id;
         end if;

         case Id_Level is
            when Msgid_Note =>
               if Flag_Color_Diagnostics = On then
                  Set_Color (Color_Note);
               end if;
               Put ("note:");
            when Msgid_Warning | Msgid_Warnings =>
               if Flag_Color_Diagnostics = On then
                  Set_Color (Color_Warning);
               end if;
               Put ("warning:");
            when Msgid_Error =>
               Nbr_Errors := Nbr_Errors + 1;
               if Flag_Color_Diagnostics = On then
                  Set_Color (Color_Error);
               end if;
               if Msg_Len = 0
                 or else Flag_Color_Diagnostics = On
               then
                  --  'error:' is displayed only if not location is present, or
                  --  if messages are colored.
                  Put ("error:");
               end if;
            when Msgid_Fatal =>
               if Flag_Color_Diagnostics = On then
                  Set_Color (Color_Fatal);
               end if;
               Put ("fatal:");
         end case;
      end;

      if Flag_Color_Diagnostics = On then
         Set_Color (Color_Message);
      end if;
      Put (' ');

      --  Display message.
      declare
         First, N : Positive;
         Argn : Integer;
      begin
         N := Msg'First;
         First := N;
         Argn := Args'First;
         while N <= Msg'Last loop
            if Msg (N) = '%' then
               Put (Msg (First .. N - 1));
               First := N + 2;
               pragma Assert (N < Msg'Last);
               N := N + 1;
               case Msg (N) is
                  when '%' =>
                     Put ('%');
                     Argn := Argn - 1;
                  when 'i' =>
                     --  Identifier.
                     declare
                        Arg : Earg_Type renames Args (Argn);
                        Id : Name_Id;
                     begin
                        Put ('"');
                        case Arg.Kind is
                           when Earg_Iir =>
                              Id := Get_Identifier (Arg.Val_Iir);
                           when Earg_Id =>
                              Id := Arg.Val_Id;
                           when others =>
                              --  Invalid conversion to identifier.
                              raise Internal_Error;
                        end case;
                        Put (Name_Table.Image (Id));
                        Put ('"');
                     end;
                  when 'c' =>
                     --  Character
                     declare
                        Arg : Earg_Type renames Args (Argn);
                     begin
                        Put (''');
                        case Arg.Kind is
                           when Earg_Char =>
                              Put (Arg.Val_Char);
                           when others =>
                              --  Invalid conversion to character.
                              raise Internal_Error;
                        end case;
                        Put (''');
                     end;
                  when 't' =>
                     --  A token
                     declare
                        use Tokens;
                        Arg : Earg_Type renames Args (Argn);
                        Tok : Token_Type;
                     begin
                        case Arg.Kind is
                           when Earg_Token =>
                              Tok := Arg.Val_Tok;
                           when others =>
                              --  Invalid conversion to character.
                              raise Internal_Error;
                        end case;
                        if Tok = Tok_Identifier then
                           Put ("an identifier");
                        else
                           Put (''');
                           Put (Image (Tok));
                           Put (''');
                        end if;
                     end;
                  when 'l' =>
                     --  Location
                     declare
                        Arg : Earg_Type renames Args (Argn);
                        Arg_Loc : Location_Type;
                        Arg_File : Source_File_Entry;
                        Arg_Line : Natural;
                        Arg_Col : Natural;
                     begin
                        pragma Assert (not Progname);
                        case Arg.Kind is
                           when Earg_Location =>
                              Arg_Loc := Arg.Val_Loc;
                           when Earg_Iir =>
                              Arg_Loc := Get_Location (Arg.Val_Iir);
                           when others =>
                              raise Internal_Error;
                        end case;
                        Location_To_Position
                          (Arg_Loc, Arg_File, Arg_Line, Arg_Col);

                        --  Do not print the filename if in the same file as
                        --  the error location.
                        if Arg_File = File then
                           Put ("line ");
                        else
                           Put (Name_Table.Image (Get_File_Name (Arg_File)));
                           Put (':');
                        end if;
                        Disp_Natural (Arg_Line);
                        Put (':');
                        Disp_Natural (Arg_Col);
                     end;
                  when 'n' =>
                     --  Node
                     declare
                        Arg : Earg_Type renames Args (Argn);
                     begin
                        case Arg.Kind is
                           when Earg_Iir =>
                              Put (Disp_Node (Arg.Val_Iir));
                           when others =>
                              --  Invalid conversion to node.
                              raise Internal_Error;
                        end case;
                     end;
                  when others =>
                     --  Unknown format.
                     raise Internal_Error;
               end case;
               Argn := Argn + 1;
            end if;
            N := N + 1;
         end loop;
         Put (Msg (First .. N - 1));

         --  Are all arguments displayed ?
         pragma Assert (Argn > Args'Last);
      end;

      if Flag_Diagnostics_Show_Option
        and then Id in Msgid_Warnings
      then
         Put (" [-W");
         Put (Warning_Image (Id));
         Put ("]");
      end if;

      if Flag_Color_Diagnostics = On then
         Set_Color (Color_None);
      end if;

      Put_Line;

      if Flag_Caret_Diagnostics
        and then (File /= No_Source_File_Entry and Line /= 0)
      then
         Put_Line (Extract_Expanded_Line (File, Line));
         Put_Line ((1 .. Col - 1 => ' ') & '^');
      end if;
   end Report_Msg;

   procedure Error_Msg_Option_NR (Msg: String) is
   begin
      Report_Msg (Msgid_Error, Option, No_Location, Msg);
   end Error_Msg_Option_NR;

   procedure Error_Msg_Option (Msg: String) is
   begin
      Error_Msg_Option_NR (Msg);
      raise Option_Error;
   end Error_Msg_Option;

   procedure Warning_Msg_Option (Id : Msgid_Warnings; Msg: String) is
   begin
      Report_Msg (Id, Option, No_Location, Msg);
   end Warning_Msg_Option;

   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Args : Earg_Arr := No_Eargs;
                              Cont : Boolean := False) is
   begin
      if Flags.Flag_Only_Elab_Warnings then
         return;
      end if;
      Report_Msg (Id, Semantic, Loc, Msg, Args, Cont);
   end Warning_Msg_Sem;

   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Arg1 : Earg_Type;
                              Cont : Boolean := False) is
   begin
      Warning_Msg_Sem (Id, Loc, Msg, Earg_Arr'(1 => Arg1), Cont);
   end Warning_Msg_Sem;

   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Arg1 : Earg_Type;
                               Cont : Boolean := False) is
   begin
      Report_Msg (Id, Elaboration, +Loc, Msg, Earg_Arr'(1 => Arg1), Cont);
   end Warning_Msg_Elab;

   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Args : Earg_Arr := No_Eargs;
                               Cont : Boolean := False) is
   begin
      Report_Msg (Id, Elaboration, +Loc, Msg, Args, Cont);
   end Warning_Msg_Elab;

   -- Disp a message during scan.
   procedure Error_Msg_Scan (Msg: String) is
   begin
      Report_Msg (Msgid_Error, Scan, No_Location, Msg);
   end Error_Msg_Scan;

   procedure Error_Msg_Scan (Loc : Location_Type; Msg: String) is
   begin
      Report_Msg (Msgid_Error, Scan, Loc, Msg);
   end Error_Msg_Scan;

   procedure Error_Msg_Scan (Msg: String; Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Scan, No_Location, Msg, (1 => Arg1));
   end Error_Msg_Scan;

   -- Disp a message during scan.
   procedure Warning_Msg_Scan (Id : Msgid_Warnings; Msg: String) is
   begin
      Report_Msg (Id, Scan, No_Location, Msg);
   end Warning_Msg_Scan;

   procedure Warning_Msg_Scan (Id : Msgid_Warnings;
                               Msg: String;
                               Arg1 : Earg_Type;
                               Cont : Boolean := False) is
   begin
      Report_Msg (Id, Scan, No_Location, Msg, (1 => Arg1), Cont);
   end Warning_Msg_Scan;

   procedure Error_Msg_Parse (Msg: String; Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Parse, No_Location, Msg, (1 => Arg1));
   end Error_Msg_Parse;

   procedure Error_Msg_Parse
     (Msg: String; Args : Earg_Arr := No_Eargs; Cont : Boolean := False) is
   begin
      Report_Msg (Msgid_Error, Parse, No_Location, Msg, Args, Cont);
   end Error_Msg_Parse;

   procedure Error_Msg_Parse_1 (Msg: String) is
   begin
      Report_Msg (Msgid_Error, Parse, No_Location, Msg);
   end Error_Msg_Parse_1;

   procedure Error_Msg_Parse (Loc : Location_Type; Msg: String) is
   begin
      Report_Msg (Msgid_Error, Parse, Loc, Msg);
   end Error_Msg_Parse;

   -- Disp a message during semantic analysis.
   -- LOC is used for location and current token.
   procedure Error_Msg_Sem (Msg: String; Loc: in Iir) is
   begin
      Report_Msg (Msgid_Error, Semantic, Get_Location_Safe (Loc), Msg);
   end Error_Msg_Sem;

   procedure Error_Msg_Sem (Loc: Location_Type;
                            Msg: String;
                            Args : Earg_Arr := No_Eargs;
                            Cont : Boolean := False) is
   begin
      Report_Msg (Msgid_Error, Semantic, Loc, Msg, Args, Cont);
   end Error_Msg_Sem;

   procedure Error_Msg_Sem
     (Loc: Location_Type; Msg: String; Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Semantic, Loc, Msg, (1 => Arg1));
   end Error_Msg_Sem;

   procedure Error_Msg_Sem_1 (Msg: String; Loc : PSL_Node) is
   begin
      Error_Msg_Sem (+Loc, Msg);
   end Error_Msg_Sem_1;

   procedure Error_Msg_Relaxed (Origin : Report_Origin;
                                Id : Msgid_Warnings;
                                Msg : String;
                                Loc : Iir;
                                Args : Earg_Arr := No_Eargs)
   is
      Level : Msgid_Type;
   begin
      if Flag_Relaxed_Rules or Vhdl_Std = Vhdl_93c then
         if not Is_Warning_Enabled (Id) then
            return;
         end if;
         Level := Id;
      else
         Level := Msgid_Error;
      end if;
      Report_Msg (Level, Origin, Get_Location_Safe (Loc), Msg, Args);
   end Error_Msg_Relaxed;

   procedure Error_Msg_Sem_Relaxed (Loc : Iir;
                                    Id : Msgid_Warnings;
                                    Msg : String;
                                    Args : Earg_Arr := No_Eargs) is
   begin
      Error_Msg_Relaxed (Semantic, Id, Msg, Loc, Args);
   end Error_Msg_Sem_Relaxed;

   -- Disp a message during elaboration.
   procedure Error_Msg_Elab
     (Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Elaboration, No_Location, Msg, Args);
   end Error_Msg_Elab;

   procedure Error_Msg_Elab
     (Msg: String; Arg1 : Earg_Type) is
   begin
      Error_Msg_Elab (Msg, Earg_Arr'(1 => Arg1));
   end Error_Msg_Elab;

   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Elaboration, +Loc, Msg, Args);
   end Error_Msg_Elab;

   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Arg1 : Earg_Type) is
   begin
      Error_Msg_Elab (Loc, Msg, Earg_Arr'(1 => Arg1));
   end Error_Msg_Elab;

   procedure Error_Msg_Elab_Relaxed (Loc : Iir;
                                     Id : Msgid_Warnings;
                                     Msg : String;
                                     Args : Earg_Arr := No_Eargs) is
   begin
      Error_Msg_Relaxed (Elaboration, Id, Msg, Loc, Args);
   end Error_Msg_Elab_Relaxed;

   -- Disp a bug message.
   procedure Error_Internal (Expr: in Iir; Msg: String := "")
   is
      pragma Unreferenced (Expr);
   begin
      Put ("internal error: ");
      Put_Line (Msg);
      raise Internal_Error;
   end Error_Internal;

   function Disp_Label (Node : Iir; Str : String) return String
   is
      Id : Name_Id;
   begin
      Id := Get_Label (Node);
      if Id = Null_Identifier then
         return "(unlabeled) " & Str;
      else
         return Str & " labeled """ & Name_Table.Image (Id) & """";
      end if;
   end Disp_Label;

   -- Disp a node.
   -- Used for output of message.
   function Disp_Node (Node: Iir) return String is
      function Disp_Identifier (Node : Iir; Str : String) return String
      is
         Id : Name_Id;
      begin
         Id := Get_Identifier (Node);
         return Str & " """ & Name_Table.Image (Id) & """";
      end Disp_Identifier;

      function Disp_Type (Node : Iir; Str : String) return String
      is
         Decl: Iir;
      begin
         Decl := Get_Type_Declarator (Node);
         if Decl = Null_Iir then
            return "anonymous " & Str
              & " defined at " & Disp_Location (Node);
         else
            return Disp_Identifier (Decl, Str);
         end if;
      end Disp_Type;

   begin
      case Get_Kind (Node) is
         when Iir_Kind_String_Literal8 =>
            return "string literal";
         when Iir_Kind_Character_Literal =>
            return "character literal " & Image_Identifier (Node);
         when Iir_Kind_Integer_Literal =>
            return "integer literal";
         when Iir_Kind_Floating_Point_Literal =>
            return "floating point literal";
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return "physical literal";
         when Iir_Kind_Enumeration_Literal =>
            return "enumeration literal " & Image_Identifier (Node);
         when Iir_Kind_Element_Declaration =>
            return Disp_Identifier (Node, "element");
         when Iir_Kind_Record_Element_Constraint =>
            return "record element constraint";
         when Iir_Kind_Array_Element_Resolution =>
            return "array element resolution";
         when Iir_Kind_Record_Resolution =>
            return "record resolution";
         when Iir_Kind_Record_Element_Resolution =>
            return "record element resolution";
         when Iir_Kind_Null_Literal =>
            return "null literal";
         when Iir_Kind_Overflow_Literal =>
            return Disp_Node (Get_Literal_Origin (Node));
         when Iir_Kind_Unaffected_Waveform =>
            return "unaffected waveform";
         when Iir_Kind_Aggregate =>
            return "aggregate";
         when Iir_Kind_Unit_Declaration =>
            return Disp_Identifier (Node, "physical unit");
         when Iir_Kind_Simple_Aggregate =>
            return "locally static array literal";

         when Iir_Kind_Operator_Symbol =>
            return "operator name";
         when Iir_Kind_Aggregate_Info =>
            return "aggregate info";
         when Iir_Kind_Signature =>
            return "signature";
         when Iir_Kind_Waveform_Element =>
            return "waveform element";
         when Iir_Kind_Conditional_Waveform =>
            return "conditional waveform";
         when Iir_Kind_Conditional_Expression =>
            return "conditional expression";
         when Iir_Kind_Association_Element_Open =>
            return "open association element";
         when Iir_Kind_Association_Element_By_Individual =>
            return "individual association element";
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
            | Iir_Kind_Association_Element_Subprogram =>
            return "association element";
         when Iir_Kind_Overload_List =>
            return "overloaded name or expression";

         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Wildcard_Type_Definition =>
            return Image_Identifier (Get_Type_Declarator (Node));
         when Iir_Kind_Array_Type_Definition =>
            return Disp_Type (Node, "array type");
         when Iir_Kind_Array_Subtype_Definition =>
            return Disp_Type (Node, "array subtype");
         when Iir_Kind_Record_Type_Definition =>
            return Disp_Type (Node, "record type");
         when Iir_Kind_Record_Subtype_Definition =>
            return Disp_Type (Node, "record subtype");
         when Iir_Kind_Enumeration_Subtype_Definition =>
            return Disp_Type (Node, "enumeration subtype");
         when Iir_Kind_Integer_Subtype_Definition =>
            return Disp_Type (Node, "integer subtype");
         when Iir_Kind_Physical_Type_Definition =>
            return Disp_Type (Node, "physical type");
         when Iir_Kind_Physical_Subtype_Definition =>
            return Disp_Type (Node, "physical subtype");
         when Iir_Kind_File_Type_Definition =>
            return Disp_Type (Node, "file type");
         when Iir_Kind_Access_Type_Definition =>
            return Disp_Type (Node, "access type");
         when Iir_Kind_Access_Subtype_Definition =>
            return Disp_Type (Node, "access subtype");
         when Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Floating_Type_Definition =>
            return Disp_Type (Node, "floating type");
         when Iir_Kind_Incomplete_Type_Definition =>
            return Disp_Type (Node, "incomplete type");
         when Iir_Kind_Interface_Type_Definition =>
            return Disp_Type (Node, "interface type");
         when Iir_Kind_Protected_Type_Declaration =>
            return Disp_Type (Node, "protected type");
         when Iir_Kind_Protected_Type_Body =>
            return Disp_Type (Node, "protected type body");
         when Iir_Kind_Subtype_Definition =>
            return "subtype definition";

         when Iir_Kind_Scalar_Nature_Definition =>
            return Image_Identifier (Get_Nature_Declarator (Node));

         when Iir_Kind_Choice_By_Expression =>
            return "choice by expression";
         when Iir_Kind_Choice_By_Range =>
            return "choice by range";
         when Iir_Kind_Choice_By_Name =>
            return "choice by name";
         when Iir_Kind_Choice_By_Others =>
            return "others choice";
         when Iir_Kind_Choice_By_None =>
            return "positionnal choice";

         when Iir_Kind_Function_Call =>
            return "function call";
         when Iir_Kind_Procedure_Call_Statement =>
            return "procedure call statement";
         when Iir_Kind_Procedure_Call =>
            return "procedure call";
         when Iir_Kind_Selected_Name =>
            return ''' & Name_Table.Image (Get_Identifier (Node)) & ''';
         when Iir_Kind_Simple_Name =>
            return ''' & Name_Table.Image (Get_Identifier (Node)) & ''';
         when Iir_Kind_Reference_Name =>
            --  Shouldn't happen.
            return "name";
         when Iir_Kind_External_Constant_Name =>
            return "external constant name";
         when Iir_Kind_External_Signal_Name =>
            return "external signal name";
         when Iir_Kind_External_Variable_Name =>
            return "external variable name";

         when Iir_Kind_Package_Pathname =>
            return "package pathname";
         when Iir_Kind_Absolute_Pathname =>
            return "absolute pathname";
         when Iir_Kind_Relative_Pathname =>
            return "relative pathname";
         when Iir_Kind_Pathname_Element =>
            return "pathname element";

         when Iir_Kind_Entity_Aspect_Entity =>
            declare
               Arch : constant Iir := Get_Architecture (Node);
               Ent : constant Iir := Get_Entity (Node);
            begin
               if Arch = Null_Iir then
                  return "aspect " & Disp_Node (Ent);
               else
                  return "aspect " & Disp_Node (Ent)
                    & '(' & Image_Identifier (Arch) & ')';
               end if;
            end;
         when Iir_Kind_Entity_Aspect_Configuration =>
            return "configuration entity aspect";
         when Iir_Kind_Entity_Aspect_Open =>
            return "open entity aspect";

         when Iir_Kinds_Monadic_Operator
           | Iir_Kinds_Dyadic_Operator =>
            return "operator """
              & Name_Table.Image (Get_Operator_Name (Node)) & """";
         when Iir_Kind_Parenthesis_Expression =>
            return "expression";
         when Iir_Kind_Qualified_Expression =>
            return "qualified expression";
         when Iir_Kind_Type_Conversion =>
            return "type conversion";
         when Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Allocator_By_Expression =>
            return "allocator";
         when Iir_Kind_Indexed_Name =>
            return "indexed name";
         when Iir_Kind_Range_Expression =>
            return "range expression";
         when Iir_Kind_Implicit_Dereference =>
            return "implicit access dereference";
         when Iir_Kind_Dereference =>
            return "access dereference";
         when Iir_Kind_Selected_Element =>
            return "selected element";
         when Iir_Kind_Selected_By_All_Name =>
            return ".all name";
         when Iir_Kind_Psl_Expression =>
            return "PSL instantiation";

         when Iir_Kind_Interface_Constant_Declaration =>
            if Get_Parent (Node) = Null_Iir then
               --  For constant interface of predefined operator.
               return "anonymous interface";
            end if;
            case Get_Kind (Get_Parent (Node)) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Block_Header =>
                  return Disp_Identifier (Node, "generic");
               when others =>
                  return Disp_Identifier (Node, "constant interface");
            end case;
         when Iir_Kind_Interface_Signal_Declaration =>
            case Get_Kind (Get_Parent (Node)) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Block_Header =>
                  return Disp_Identifier (Node, "port");
               when others =>
                  return Disp_Identifier (Node, "signal interface");
            end case;
         when Iir_Kind_Interface_Variable_Declaration =>
            return Disp_Identifier (Node, "variable interface");
         when Iir_Kind_Interface_File_Declaration =>
            return Disp_Identifier (Node, "file interface");
         when Iir_Kind_Interface_Package_Declaration =>
            return Disp_Identifier (Node, "package interface");
         when Iir_Kind_Interface_Type_Declaration =>
            return Disp_Identifier (Node, "type interface");
         when Iir_Kind_Signal_Declaration =>
            return Disp_Identifier (Node, "signal");
         when Iir_Kind_Variable_Declaration =>
            return Disp_Identifier (Node, "variable");
         when Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Declaration =>
            return Disp_Identifier (Node, "constant");
         when Iir_Kind_File_Declaration =>
            return Disp_Identifier (Node, "file");
         when Iir_Kind_Object_Alias_Declaration =>
            return Disp_Identifier (Node, "alias");
         when Iir_Kind_Non_Object_Alias_Declaration =>
            return Disp_Identifier (Node, "non-object alias");
         when Iir_Kind_Guard_Signal_Declaration =>
            return "GUARD signal";
         when Iir_Kind_Signal_Attribute_Declaration =>
            --  Should not appear.
            return "signal attribute";
         when Iir_Kind_Group_Template_Declaration =>
            return Disp_Identifier (Node, "group template");
         when Iir_Kind_Group_Declaration =>
            return Disp_Identifier (Node, "group");

         when Iir_Kind_Library_Declaration
           | Iir_Kind_Library_Clause =>
            return Disp_Identifier (Node, "library");
         when Iir_Kind_Design_File =>
            return "design file";

         when Iir_Kind_Procedure_Declaration =>
            return Disp_Identifier (Node, "procedure");
         when Iir_Kind_Function_Declaration =>
            return Disp_Identifier (Node, "function");
         when Iir_Kind_Interface_Procedure_Declaration =>
            return Disp_Identifier (Node, "interface procedure");
         when Iir_Kind_Interface_Function_Declaration =>
            return Disp_Identifier (Node, "interface function");
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            return "subprogram body";

         when Iir_Kind_Package_Declaration =>
            return Disp_Identifier (Node, "package");
         when Iir_Kind_Package_Body =>
            return Disp_Identifier (Node, "package body");
         when Iir_Kind_Entity_Declaration =>
            return Disp_Identifier (Node, "entity");
         when Iir_Kind_Architecture_Body =>
            return Disp_Identifier (Node, "architecture") &
              " of" & Disp_Identifier (Get_Entity_Name (Node), "");
         when Iir_Kind_Configuration_Declaration =>
            declare
               Id : Name_Id;
               Ent : Iir;
               Arch : Iir;
            begin
               Id := Get_Identifier (Node);
               if Id /= Null_Identifier then
                  return Disp_Identifier (Node, "configuration");
               else
                  Ent := Get_Entity (Node);
                  Arch := Get_Block_Specification
                    (Get_Block_Configuration (Node));
                  return "default configuration of "
                    & Image_Identifier (Ent)
                    & '(' & Image_Identifier (Arch) & ')';
               end if;
            end;
         when Iir_Kind_Context_Declaration =>
            return Disp_Identifier (Node, "context");
         when Iir_Kind_Package_Instantiation_Declaration =>
            return Disp_Identifier (Node, "instantiation package");

         when Iir_Kind_Package_Header =>
            return "package header";

         when Iir_Kind_Component_Declaration =>
            return Disp_Identifier (Node, "component");

         when Iir_Kind_Design_Unit =>
            return Disp_Node (Get_Library_Unit (Node));
         when Iir_Kind_Use_Clause =>
            return "use clause";
         when Iir_Kind_Context_Reference =>
            return "context reference";
         when Iir_Kind_Disconnection_Specification =>
            return "disconnection specification";

         when Iir_Kind_Slice_Name =>
            return "slice";
         when Iir_Kind_Parenthesis_Name =>
            return "function call, slice or indexed name";
         when Iir_Kind_Type_Declaration =>
            return Disp_Identifier (Node, "type");
         when Iir_Kind_Anonymous_Type_Declaration =>
            return Disp_Identifier (Node, "type");
         when Iir_Kind_Subtype_Declaration =>
            return Disp_Identifier (Node, "subtype");

         when Iir_Kind_Nature_Declaration =>
            return Disp_Identifier (Node, "nature");
         when Iir_Kind_Subnature_Declaration =>
            return Disp_Identifier (Node, "subnature");

         when Iir_Kind_Component_Instantiation_Statement =>
            return Disp_Identifier (Node, "component instance");
         when Iir_Kind_Configuration_Specification =>
            return "configuration specification";
         when Iir_Kind_Component_Configuration =>
            return "component configuration";

         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            return "concurrent procedure call";
         when Iir_Kind_For_Generate_Statement =>
            return "for generate statement";
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_If_Generate_Else_Clause =>
            return "if generate statement";
         when Iir_Kind_Case_Generate_Statement =>
            return "case generate statement";
         when Iir_Kind_Generate_Statement_Body =>
            return "generate statement";

         when Iir_Kind_Simple_Simultaneous_Statement =>
            return "simple simultaneous statement";

         when Iir_Kind_Psl_Declaration =>
            return Disp_Identifier (Node, "PSL declaration");
         when Iir_Kind_Psl_Endpoint_Declaration =>
            return Disp_Identifier (Node, "PSL endpoint declaration");

         when Iir_Kind_Terminal_Declaration =>
            return Disp_Identifier (Node, "terminal declaration");
         when Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            return Disp_Identifier (Node, "quantity declaration");

         when Iir_Kind_Attribute_Declaration =>
            return Disp_Identifier (Node, "attribute");
         when Iir_Kind_Attribute_Specification =>
            return "attribute specification";
         when Iir_Kind_Entity_Class =>
            return "entity class";
         when Iir_Kind_Attribute_Value =>
            return "attribute value";
         when Iir_Kind_Attribute_Name =>
            return "attribute";
         when Iir_Kind_Base_Attribute =>
            return "'base attribute";
         when Iir_Kind_Length_Array_Attribute =>
            return "'length attribute";
         when Iir_Kind_Range_Array_Attribute =>
            return "'range attribute";
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            return "'reverse_range attribute";
         when Iir_Kind_Subtype_Attribute =>
            return "'subtype attribute";
         when Iir_Kind_Element_Attribute =>
            return "'element attribute";
         when Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Ascending_Array_Attribute =>
            return "'ascending attribute";
         when Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Left_Array_Attribute =>
            return "'left attribute";
         when Iir_Kind_Right_Type_Attribute
           | Iir_Kind_Right_Array_Attribute =>
            return "'right attribute";
         when Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Low_Array_Attribute =>
            return "'low attribute";
         when Iir_Kind_Leftof_Attribute =>
            return "'leftof attribute";
         when Iir_Kind_Rightof_Attribute =>
            return "'rightof attribute";
         when Iir_Kind_Pred_Attribute =>
            return "'pred attribute";
         when Iir_Kind_Succ_Attribute =>
            return "'succ attribute";
         when Iir_Kind_Pos_Attribute =>
            return "'pos attribute";
         when Iir_Kind_Val_Attribute =>
            return "'val attribute";
         when Iir_Kind_Image_Attribute =>
            return "'image attribute";
         when Iir_Kind_Value_Attribute =>
            return "'value attribute";
         when Iir_Kind_High_Type_Attribute
           | Iir_Kind_High_Array_Attribute =>
            return "'high attribute";
         when Iir_Kind_Transaction_Attribute =>
            return "'transaction attribute";
         when Iir_Kind_Stable_Attribute =>
            return "'stable attribute";
         when Iir_Kind_Quiet_Attribute =>
            return "'quiet attribute";
         when Iir_Kind_Delayed_Attribute =>
            return "'delayed attribute";
         when Iir_Kind_Driving_Attribute =>
            return "'driving attribute";
         when Iir_Kind_Driving_Value_Attribute =>
            return "'driving_value attribute";
         when Iir_Kind_Event_Attribute =>
            return "'event attribute";
         when Iir_Kind_Active_Attribute =>
            return "'active attribute";
         when Iir_Kind_Last_Event_Attribute =>
            return "'last_event attribute";
         when Iir_Kind_Last_Active_Attribute =>
            return "'last_active attribute";
         when Iir_Kind_Last_Value_Attribute =>
            return "'last_value attribute";
         when Iir_Kind_Behavior_Attribute =>
            return "'behavior attribute";
         when Iir_Kind_Structure_Attribute =>
            return "'structure attribute";

         when Iir_Kind_Path_Name_Attribute =>
            return "'path_name attribute";
         when Iir_Kind_Instance_Name_Attribute =>
            return "'instance_name attribute";
         when Iir_Kind_Simple_Name_Attribute =>
            return "'simple_name attribute";

         when Iir_Kind_For_Loop_Statement =>
            return Disp_Label (Node, "for loop statement");
         when Iir_Kind_While_Loop_Statement =>
            return Disp_Label (Node, "loop statement");
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement =>
            return Disp_Label (Node, "process");
         when Iir_Kind_Block_Statement =>
            return Disp_Label (Node, "block statement");
         when Iir_Kind_Block_Header =>
            return "block header";
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent simple signal assignment");
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent conditional signal assignment");
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent selected signal assignment");
         when Iir_Kind_Concurrent_Assertion_Statement =>
            return Disp_Label (Node, "concurrent assertion");
         when Iir_Kind_Psl_Assert_Statement =>
            return Disp_Label (Node, "PSL assertion");
         when Iir_Kind_Psl_Cover_Statement =>
            return Disp_Label (Node, "PSL cover");
         when Iir_Kind_Psl_Default_Clock =>
            return "PSL default clock";

         when Iir_Kind_If_Statement =>
            return Disp_Label (Node, "if statement");
         when Iir_Kind_Elsif =>
            return Disp_Label (Node, "else/elsif statement");
         when Iir_Kind_Next_Statement =>
            return Disp_Label (Node, "next statement");
         when Iir_Kind_Exit_Statement =>
            return Disp_Label (Node, "exit statement");
         when Iir_Kind_Case_Statement =>
            return Disp_Label (Node, "case statement");
         when Iir_Kind_Return_Statement =>
            return Disp_Label (Node, "return statement");
         when Iir_Kind_Simple_Signal_Assignment_Statement =>
            return Disp_Label (Node, "signal assignment statement");
         when Iir_Kind_Conditional_Signal_Assignment_Statement =>
            return Disp_Label
              (Node, "conditional signal assignment statement");
         when Iir_Kind_Selected_Waveform_Assignment_Statement =>
            return Disp_Label
              (Node, "selected waveform assignment statement");
         when Iir_Kind_Variable_Assignment_Statement =>
            return Disp_Label (Node, "variable assignment statement");
         when Iir_Kind_Conditional_Variable_Assignment_Statement =>
            return Disp_Label
              (Node, "conditional variable assignment statement");
         when Iir_Kind_Null_Statement =>
            return Disp_Label (Node, "null statement");
         when Iir_Kind_Wait_Statement =>
            return Disp_Label (Node, "wait statement");
         when Iir_Kind_Assertion_Statement =>
            return Disp_Label (Node, "assertion statement");
         when Iir_Kind_Report_Statement =>
            return Disp_Label (Node, "report statement");

         when Iir_Kind_Block_Configuration =>
            return "block configuration";
         when Iir_Kind_Binding_Indication =>
            return "binding indication";

         when Iir_Kind_Error =>
            return "error";
         when Iir_Kind_Unused =>
            return "*unused*";
      end case;
   end Disp_Node;

   -- Disp a node location.
   -- Used for output of message.

   function Disp_Location (Node: Iir) return String is
   begin
      return Image (Get_Location (Node));
   end Disp_Location;

   function Disp_Name (Kind : Iir_Kind) return String is
   begin
      case Kind is
         when Iir_Kind_Constant_Declaration =>
            return "constant declaration";
         when Iir_Kind_Signal_Declaration =>
            return "signal declaration";
         when Iir_Kind_Variable_Declaration =>
            return "variable declaration";
         when Iir_Kind_File_Declaration =>
            return "file declaration";
         when others =>
            return "???" & Iir_Kind'Image (Kind);
      end case;
   end Disp_Name;

   function Image (N : Iir_Int64) return String
   is
      Res : constant String := Iir_Int64'Image (N);
   begin
      if Res (1) = ' ' then
         return Res (2 .. Res'Last);
      else
         return Res;
      end if;
   end Image;

   function Disp_Discrete (Dtype : Iir; Pos : Iir_Int64) return String is
   begin
      case Get_Kind (Dtype) is
         when Iir_Kind_Integer_Type_Definition =>
            return Image (Pos);
         when Iir_Kind_Enumeration_Type_Definition =>
            return Name_Table.Image
              (Get_Identifier (Get_Nth_Element
                               (Get_Enumeration_Literal_List (Dtype),
                                Natural (Pos))));
         when others =>
            Error_Kind ("disp_discrete", Dtype);
      end case;
   end Disp_Discrete;

   function Disp_Subprg (Subprg : Iir) return String
   is
      use Ada.Strings.Unbounded;
      Res : Unbounded_String;

      procedure Append_Type (Def : Iir)
      is
         use Name_Table;
         Decl : Iir := Get_Type_Declarator (Def);
      begin
         if Decl = Null_Iir then
            Decl := Get_Type_Declarator (Get_Base_Type (Def));
            if Decl = Null_Iir then
               Append (Res, "*unknown*");
               return;
            end if;
         end if;
         Append (Res, Image (Get_Identifier (Decl)));
      end Append_Type;

   begin
      case Get_Kind (Subprg) is
         when Iir_Kind_Enumeration_Literal =>
            Append (Res, "enumeration literal ");
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            Append (Res, "function ");
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            Append (Res, "procedure ");
         when others =>
            Error_Kind ("disp_subprg", Subprg);
      end case;

      declare
         use Name_Table;

         Id : constant Name_Id := Get_Identifier (Subprg);
      begin
         case Id is
            when Std_Names.Name_Id_Operators
              | Std_Names.Name_Word_Operators
              | Std_Names.Name_Xnor
              | Std_Names.Name_Shift_Operators =>
               Append (Res, """");
               Append (Res, Image (Id));
               Append (Res, """");
            when others =>
               Append (Res, Image (Id));
         end case;
      end;

      Append (Res, " [");

      case Get_Kind (Subprg) is
         when Iir_Kinds_Subprogram_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration =>
            declare
               El : Iir;
            begin
               El := Get_Interface_Declaration_Chain (Subprg);
               while El /= Null_Iir loop
                  Append_Type (Get_Type (El));
                  El := Get_Chain (El);
                  exit when El = Null_Iir;
                  Append (Res, ", ");
               end loop;
            end;
         when others =>
            null;
      end case;

      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Enumeration_Literal =>
            Append (Res, " return ");
            Append_Type (Get_Return_Type (Subprg));
         when others =>
            null;
      end case;

      Append (Res, "]");

      return To_String (Res);
   end Disp_Subprg;

   --  DEF must be any type definition.
   --  Return the type name of DEF, handle anonymous subtypes.
   function Disp_Type_Name (Def : Iir) return String
   is
      Decl : Iir;
   begin
      Decl := Get_Type_Declarator (Def);
      if Decl /= Null_Iir then
         return Image_Identifier (Decl);
      end if;
      Decl := Get_Type_Declarator (Get_Base_Type (Def));
      if Decl /= Null_Iir then
         return "a subtype of " & Image_Identifier (Decl);
      else
         return "an unknown type";
      end if;
   end Disp_Type_Name;

   function Disp_Type_Of (Node : Iir) return String
   is
      A_Type : Iir;
   begin
      A_Type := Get_Type (Node);
      if A_Type = Null_Iir then
         return "unknown";
      elsif Get_Kind (A_Type) = Iir_Kind_Overload_List then
         declare
            use Ada.Strings.Unbounded;
            List : constant Iir_List := Get_Overload_List (A_Type);
            Nbr : constant Natural := Get_Nbr_Elements (List);
            Res : Unbounded_String;
            El : Iir;
            It : List_Iterator;
         begin
            if Nbr = 0 then
               return "unknown";
            elsif Nbr = 1 then
               return Disp_Type_Name (Get_First_Element (List));
            else
               Append (Res, "one of ");
               It := List_Iterate (List);
               for I in 0 .. Nbr - 1 loop
                  pragma Assert (Is_Valid (It));
                  El := Get_Element (It);
                  Append (Res, Disp_Type_Name (El));
                  if I < Nbr - 2 then
                     Append (Res, ", ");
                  elsif I = Nbr - 2 then
                     Append (Res, " or ");
                  end if;
                  Next (It);
               end loop;
               return To_String (Res);
            end if;
         end;
      else
         return Disp_Type_Name (A_Type);
      end if;
   end Disp_Type_Of;

   procedure Error_Pure
     (Origin : Report_Origin; Caller : Iir; Callee : Iir; Loc : Iir)
   is
      L : Iir;
   begin
      if Loc = Null_Iir then
         L := Caller;
      else
         L := Loc;
      end if;
      Error_Msg_Relaxed
        (Origin, Warnid_Pure,
         "pure " & Disp_Node (Caller) & " cannot call (impure) "
         & Disp_Node (Callee), L);
      Error_Msg_Relaxed
        (Origin, Warnid_Pure,
         "(" & Disp_Node (Callee) & " is defined here)", Callee);
   end Error_Pure;

   procedure Error_Not_Match (Expr: Iir; A_Type: Iir) is
   begin
      if Get_Kind (A_Type) = Iir_Kind_Error then
         --  Cascade error message.
         return;
      end if;
      Error_Msg_Sem ("can't match " & Disp_Node (Expr) & " with type "
                     & Disp_Node (A_Type), Expr);
   end Error_Not_Match;

   function Get_Mode_Name (Mode : Iir_Mode) return String is
   begin
      case Mode is
         when Iir_Unknown_Mode =>
            raise Internal_Error;
         when Iir_Linkage_Mode =>
            return "linkage";
         when Iir_Buffer_Mode =>
            return "buffer";
         when Iir_Out_Mode =>
            return "out";
         when Iir_Inout_Mode =>
            return "inout";
         when Iir_In_Mode =>
            return "in";
      end case;
   end Get_Mode_Name;

end Errorout;
