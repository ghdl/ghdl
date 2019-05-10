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

with Vhdl.Scanner;
with Name_Table;
with Files_Map; use Files_Map;
with Flags; use Flags;
with PSL.Nodes;
with Str_Table;

with Vhdl.Errors; use Vhdl.Errors;

package body Errorout is
   function Natural_Image (Val: Natural) return String
   is
      Str: constant String := Natural'Image (Val);
   begin
      return Str (Str'First + 1 .. Str'Last);
   end Natural_Image;

   function Get_Error_Col (E : Error_Record) return Natural
   is
      Line_Pos : Source_Ptr;
   begin
      Line_Pos := File_Line_To_Position (E.File, E.Line);
      return Coord_To_Col (E.File, Line_Pos, E.Offset);
   end Get_Error_Col;

   Report_Handler : Report_Msg_Handler;

   procedure Set_Report_Handler (Handler : Report_Msg_Handler) is
   begin
      Report_Handler := Handler;
   end Set_Report_Handler;

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

   function "+" (V : Location_Type) return Earg_Type is
   begin
      return (Kind => Earg_Location, Val_Loc => V);
   end "+";

   function "+" (V : Name_Id) return Earg_Type is
   begin
      return (Kind => Earg_Id, Val_Id => V);
   end "+";

   function "+" (V : Character) return Earg_Type is
   begin
      return (Kind => Earg_Char, Val_Char => V);
   end "+";

   function "+" (V : String8_Len_Type) return Earg_Type is
   begin
      return (Kind => Earg_String8, Val_Str8 => V);
   end "+";

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

   procedure Report_Msg (Id : Msgid_Type;
                         Origin : Report_Origin;
                         Loc : Location_Type;
                         Msg : String;
                         Args : Earg_Arr := No_Eargs;
                         Cont : Boolean := False)
   is
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
      New_Id : Msgid_Type;
      Offset : Natural;
      Loc_Length : Natural;
      Line_Pos : Source_Ptr;
      pragma Unreferenced (Line_Pos);
   begin
      --  Discard warnings that aren't enabled.
      if Id in Msgid_Warnings and then not Is_Warning_Enabled (Id) then
         return;
      end if;

      --  Reclassify warnings to errors if -Werror.
      if Flags.Warn_Error
        and then (Id = Msgid_Warning or Id in Msgid_Warnings)
      then
         New_Id := Msgid_Error;
      else
         New_Id := Id;
      end if;
      pragma Unreferenced (Id);

      --  Limit the number of errors.
      if not Cont and then New_Id = Msgid_Error then
         Nbr_Errors := Nbr_Errors + 1;
         if Nbr_Errors > Max_Nbr_Errors then
            return;
         end if;
      end if;

      --  Set error location.
      File := No_Source_File_Entry;
      Line := 0;
      Offset := 0;
      Loc_Length := 0;

      case Origin is
         when Option
           | Library =>
            pragma Assert (Loc = No_Location);
            null;
         when others =>
            if Loc /= No_Location then
               Location_To_Coord (Loc, File, Line_Pos, Line, Offset);
            else
               case Origin is
                  when Option
                    | Library =>
                     raise Program_Error;
                  when Elaboration =>
                     null;
                  when Scan =>
                     File := Vhdl.Scanner.Get_Current_Source_File;
                     Line := Vhdl.Scanner.Get_Current_Line;
                     Offset := Vhdl.Scanner.Get_Current_Offset;
                     Loc_Length := 1;
                  when Parse =>
                     File := Vhdl.Scanner.Get_Current_Source_File;
                     Line := Vhdl.Scanner.Get_Current_Line;
                     Offset := Vhdl.Scanner.Get_Token_Offset;
                     Loc_Length := Vhdl.Scanner.Get_Current_Offset - Offset;
                  when Semantic =>
                     null;
               end case;
            end if;
      end case;

      Report_Handler.Error_Start
        (Err => (Origin, New_Id, Cont, File, Line, Offset, Loc_Length));

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
               Report_Handler.Message (Msg (First .. N - 1));
               First := N + 2;
               pragma Assert (N < Msg'Last);
               N := N + 1;
               case Msg (N) is
                  when '%' =>
                     Report_Handler.Message ("%");
                     Argn := Argn - 1;
                  when 'i' =>
                     --  Identifier.
                     declare
                        Arg : Earg_Type renames Args (Argn);
                        Id : Name_Id;
                     begin
                        Report_Handler.Message ("""");
                        case Arg.Kind is
                           when Earg_Iir =>
                              Id := Get_Identifier (Arg.Val_Iir);
                           when Earg_Id =>
                              Id := Arg.Val_Id;
                           when others =>
                              --  Invalid conversion to identifier.
                              raise Internal_Error;
                        end case;
                        Report_Handler.Message (Name_Table.Image (Id));
                        Report_Handler.Message ("""");
                     end;
                  when 'c' =>
                     --  Character
                     declare
                        Arg : Earg_Type renames Args (Argn);
                     begin
                        Report_Handler.Message ("'");
                        case Arg.Kind is
                           when Earg_Char =>
                              Report_Handler.Message ((1 => Arg.Val_Char));
                           when others =>
                              --  Invalid conversion to character.
                              raise Internal_Error;
                        end case;
                        Report_Handler.Message ("'");
                     end;
                  when 't' =>
                     --  A token
                     declare
                        use Vhdl.Tokens;
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
                        case Tok is
                           when Tok_Identifier =>
                              Report_Handler.Message ("an identifier");
                           when Tok_Eof =>
                              Report_Handler.Message ("end of file");
                           when others =>
                              Report_Handler.Message ("'");
                              Report_Handler.Message (Image (Tok));
                              Report_Handler.Message ("'");
                        end case;
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
                           Report_Handler.Message ("line ");
                        else
                           Report_Handler.Message
                             (Name_Table.Image (Get_File_Name (Arg_File)));
                           Report_Handler.Message (":");
                        end if;
                        Report_Handler.Message (Natural_Image (Arg_Line));
                        Report_Handler.Message (":");
                        Report_Handler.Message (Natural_Image (Arg_Col));
                     end;
                  when 'n' =>
                     --  Node
                     declare
                        Arg : Earg_Type renames Args (Argn);
                     begin
                        case Arg.Kind is
                           when Earg_Iir =>
                              Report_Handler.Message (Disp_Node (Arg.Val_Iir));
                           when others =>
                              --  Invalid conversion to node.
                              raise Internal_Error;
                        end case;
                     end;
                  when 's' =>
                     --  String
                     declare
                        Arg : Earg_Type renames Args (Argn);
                     begin
                        Report_Handler.Message ("""");
                        case Arg.Kind is
                           when Earg_String8 =>
                              Report_Handler.Message
                                (Str_Table.String_String8
                                   (Arg.Val_Str8.Str, Arg.Val_Str8.Len));
                           when others =>
                              --  Invalid conversion to character.
                              raise Internal_Error;
                        end case;
                        Report_Handler.Message ("""");
                     end;
                  when others =>
                     --  Unknown format.
                     raise Internal_Error;
               end case;
               Argn := Argn + 1;
            end if;
            N := N + 1;
         end loop;
         Report_Handler.Message (Msg (First .. N - 1));

         --  Are all arguments displayed ?
         pragma Assert (Argn > Args'Last);
      end;

      Report_Handler.Message_End.all;

      if not Cont
        and then New_Id = Msgid_Error
        and then Nbr_Errors = Max_Nbr_Errors
      then
         --  Limit reached.  Emit a message.
         Report_Handler.Error_Start (Err => (Option, Msgid_Error, False,
                                             No_Source_File_Entry, 0, 0, 0));
         Report_Handler.Message ("error limit reached");
         Report_Handler.Message_End.all;
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

   function Make_Earg_Vhdl_Node (V : Iir) return Earg_Type is
   begin
      return (Kind => Earg_Iir, Val_Iir => V);
   end Make_Earg_Vhdl_Node;

   function Make_Earg_Vhdl_Token (V : Vhdl.Tokens.Token_Type)
                                 return Earg_Type is
   begin
      return (Kind => Earg_Token, Val_Tok => V);
   end Make_Earg_Vhdl_Token;


end Errorout;
