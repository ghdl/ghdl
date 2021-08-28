--  Error message handling.
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

with Name_Table;
with Files_Map; use Files_Map;
with Str_Table;

package body Errorout is
   --  Messages in a group.
   --  Set to 0 for individual messages,
   --  Set to 1 .. n for messages in a group.
   In_Group : Natural := 0;

   Report_Handler : Report_Msg_Handler;

   procedure Set_Report_Handler (Handler : Report_Msg_Handler) is
   begin
      Report_Handler := Handler;
   end Set_Report_Handler;

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

   procedure Warning_Error (Id : Msgid_All_Warnings; As_Error : Boolean) is
   begin
      Warnings_Control (Id).Error := As_Error;
   end Warning_Error;

   function Is_Warning_Error (Id : Msgid_All_Warnings) return Boolean is
   begin
      return Warnings_Control (Id).Error;
   end Is_Warning_Error;

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

   function "+" (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Uns32, Val_Uns32 => V);
   end "+";

   function "+" (V : Int32) return Earg_Type is
   begin
      return (Kind => Earg_Int32, Val_Int32 => V);
   end "+";

   function "+" (V : String8_Len_Type) return Earg_Type is
   begin
      return (Kind => Earg_String8, Val_Str8 => V);
   end "+";

   function "+" (L : Location_Type) return Source_Coord_Type
   is
      Res : Source_Coord_Type;
   begin
      Files_Map.Location_To_Coord
        (L, Res.File, Res.Line_Pos, Res.Line, Res.Offset);
      return Res;
   end "+";

   procedure Output_Identifier (Id : Name_Id) is
   begin
      Report_Handler.Message (Name_Table.Image (Id));
   end Output_Identifier;

   procedure Output_Quoted_Identifier (Id : Name_Id) is
   begin
      Report_Handler.Message ("""");
      Output_Identifier (Id);
      Report_Handler.Message ("""");
   end Output_Quoted_Identifier;

   procedure Output_Quoted_Character (C : Character) is
   begin
      Report_Handler.Message ("'");
      Report_Handler.Message ((1 => C));
      Report_Handler.Message ("'");
   end Output_Quoted_Character;

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

   procedure Output_Location (Err : Error_Record; Loc : Location_Type)
   is
      Arg_File : Source_File_Entry;
      Arg_Line : Natural;
      Arg_Col : Natural;
   begin
      Location_To_Position (Loc, Arg_File, Arg_Line, Arg_Col);

      --  Do not print the filename if in the same file as
      --  the error location.
      if Arg_File = Err.File then
         Report_Handler.Message ("line ");
      else
         Report_Handler.Message (Name_Table.Image (Get_File_Name (Arg_File)));
         Report_Handler.Message (":");
      end if;
      Report_Handler.Message (Natural_Image (Arg_Line));
      Report_Handler.Message (":");
      Report_Handler.Message (Natural_Image (Arg_Col));
   end Output_Location;

   procedure Output_Uns32 (V : Uns32)
   is
      S : constant String := Uns32'Image (V);
   begin
      Report_Handler.Message (S (2 .. S'Last));
   end Output_Uns32;

   procedure Output_String8 (Str : String8_Len_Type) is
   begin
      Report_Handler.Message ("""");
      Report_Handler.Message (Str_Table.String_String8 (Str.Str, Str.Len));
      Report_Handler.Message ("""");
   end Output_String8;

   procedure Output_Message (S : String) is
   begin
      Report_Handler.Message (S);
   end Output_Message;

   type Handlers_Array is array (Earg_Lang_Kind) of Earg_Handler;
   Lang_Handlers : Handlers_Array := (others => null);

   procedure Register_Earg_Handler
     (Kind : Earg_Kind; Handler : Earg_Handler) is
   begin
      if Lang_Handlers (Kind) /= null
        and then Lang_Handlers (Kind) /= Handler
      then
         --  Cannot change handler.
         raise Internal_Error;
      end if;
      Lang_Handlers (Kind) := Handler;
   end Register_Earg_Handler;

   procedure Report_Msg (Id : Msgid_Type;
                         Origin : Report_Origin;
                         Loc : Source_Coord_Type;
                         Msg : String;
                         Args : Earg_Arr := No_Eargs)
   is
      New_Id : Msgid_Type;
      Err : Error_Record;
   begin
      --  Discard warnings that aren't enabled.
      if Id in Msgid_Warnings and then not Is_Warning_Enabled (Id) then
         return;
      end if;

      --  Reclassify warnings to errors if -Werror.
      if Id in Msgid_All_Warnings and then Is_Warning_Error (Id) then
         New_Id := Msgid_Error;
      else
         New_Id := Id;
      end if;
      pragma Unreferenced (Id);

      if In_Group <= 1
        and then New_Id = Msgid_Error
      then
         if Nbr_Errors = Max_Nbr_Errors then
            --  Limit reached.  Emit a message on the first message.
            Report_Handler.Error_Start
              (Err => (Option, Msgid_Error,
                       No_Source_File_Entry, 0, 0, 0));
            Report_Handler.Message ("error limit reached");
            Report_Handler.Message_End.all;
         end if;

         Nbr_Errors := Nbr_Errors + 1;
      end if;

      --  Limit the number of errors.
      if New_Id = Msgid_Error and then Nbr_Errors > Max_Nbr_Errors then
         return;
      end if;

      Err := (Origin, New_Id, Loc.File, Loc.Line, Loc.Offset, 0);
      Report_Handler.Error_Start (Err);

      if In_Group > 0 then
         In_Group := In_Group + 1;
      end if;

      --  Display message.
      declare
         First, N : Positive;
         Argn : Integer;
         Format : Character;
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
               Format := Msg (N);
               if Format = '%' then
                  --  Special case because there is no argument for the
                  --  escape format.
                  Report_Handler.Message ("%");
               else
                  declare
                     Arg : Earg_Type renames Args (Argn);
                  begin
                     case Arg.Kind is
                        when Earg_None =>
                           raise Internal_Error;
                        when Earg_Location =>
                           if Format = 'l' then
                              Output_Location (Err, Arg.Val_Loc);
                           else
                              raise Internal_Error;
                           end if;
                        when Earg_Id =>
                           if Format = 'i' then
                              Output_Quoted_Identifier (Arg.Val_Id);
                           else
                              raise Internal_Error;
                           end if;
                        when Earg_Char =>
                           if Format = 'c' then
                              Output_Quoted_Character (Arg.Val_Char);
                           else
                              raise Internal_Error;
                           end if;
                        when Earg_String8 =>
                           if Format = 's' then
                              Output_String8 (Arg.Val_Str8);
                           else
                              raise Internal_Error;
                           end if;
                        when Earg_Uns32 =>
                           if Format = 'v' then
                              Output_Uns32 (Arg.Val_Uns32);
                           else
                              raise Internal_Error;
                           end if;
                        when Earg_Int32 =>
                           raise Internal_Error;
                        when Earg_Lang_Kind =>
                           if Lang_Handlers (Arg.Kind) = null then
                              raise Internal_Error;
                           end if;
                           Lang_Handlers (Arg.Kind)
                             (Format, Err, Arg.Val_Lang);
                     end case;
                  end;
                  Argn := Argn + 1;
               end if;
            end if;
            N := N + 1;
         end loop;
         Report_Handler.Message (Msg (First .. N - 1));

         --  Are all arguments displayed ?
         pragma Assert (Argn > Args'Last);
      end;

      Report_Handler.Message_End.all;
   end Report_Msg;

   procedure Report_Start_Group is
   begin
      pragma Assert (In_Group = 0);
      In_Group := 1;
      Report_Handler.Message_Group.all (True);
   end Report_Start_Group;

   procedure Report_End_Group is
   begin
      pragma Assert (In_Group > 0);
      In_Group := 0;
      Report_Handler.Message_Group.all (False);
   end Report_End_Group;

   procedure Error_Msg_Option (Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Option, No_Source_Coord, Msg, Args);
   end Error_Msg_Option;

   procedure Warning_Msg_Option (Id : Msgid_Warnings; Msg: String) is
   begin
      Report_Msg (Id, Option, No_Source_Coord, Msg);
   end Warning_Msg_Option;

   function Make_Earg_Vhdl_Node (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Vhdl_Node, Val_Lang => V);
   end Make_Earg_Vhdl_Node;

   function Make_Earg_Vhdl_Token (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Vhdl_Token, Val_Lang => V);
   end Make_Earg_Vhdl_Token;

   function Make_Earg_Synth_Instance (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Synth_Instance, Val_Lang => V);
   end Make_Earg_Synth_Instance;

   function Make_Earg_Synth_Net (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Synth_Net, Val_Lang => V);
   end Make_Earg_Synth_Net;

   function Make_Earg_Synth_Name (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Synth_Name, Val_Lang => V);
   end Make_Earg_Synth_Name;

   function Make_Earg_Verilog_Node (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Verilog_Node, Val_Lang => V);
   end Make_Earg_Verilog_Node;

   function Make_Earg_Verilog_Token (V : Uns32) return Earg_Type is
   begin
      return (Kind => Earg_Verilog_Token, Val_Lang => V);
   end Make_Earg_Verilog_Token;
end Errorout;
