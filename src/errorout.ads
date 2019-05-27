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
with Types; use Types;
with Vhdl.Nodes;
with Vhdl.Tokens;

package Errorout is
   Option_Error: exception;
   Compilation_Error: exception;

   --  The number of errors (ie, number of calls to error_msg*).
   Nbr_Errors : Natural := 0;

   --  Maximum number of errors, before silent them.
   Max_Nbr_Errors : constant Natural := 100;

   type Msgid_Type is
     (
      --  Any note
      Msgid_Note,

      --  Specific warnings

      --  Design unit redefines another design unit.
      Warnid_Library,

      --  Missing Xref in pretty print.
      Warnid_Missing_Xref,

      --  No default binding for a component instantiation.
      Warnid_Default_Binding,

      --  Unbound component.
      Warnid_Binding,

      --  Unconnected IN port without defaults (in relaxed mode).
      Warnid_Port,

      --  Vhdl93 reserved word is used as a vhdl87 identifier.
      Warnid_Reserved_Word,

      --  Start of block comment ('/*') appears in a block comment.
      Warnid_Nested_Comment,

      --  Use of a tool directive.
      Warnid_Directive,

      --  Weird use of parenthesis.
      Warnid_Parenthesis,

      --  Generic of a vital entity is not a vital name.
      Warnid_Vital_Generic,

      --  Delayed checks (checks performed at elaboration time).
      Warnid_Delayed_Checks,

      --  Package body is not required but is analyzed.
      Warnid_Body,

      --  An all/others specification does not apply, because there is no such
      --  named entities.
      Warnid_Specs,

      --  Incorrect use of universal value.
      Warnid_Universal,

      --  Mismatch of bounds between actual and formal in a scalar port
      --  association
      Warnid_Port_Bounds,

      --  Runtime error detected at analysis time.
      Warnid_Runtime_Error,

      --  Signal assignment creates a delta cycle in a postponed process.
      Warnid_Delta_Cycle,

      --  Declaration of a shared variable with a non-protected type.
      Warnid_Shared,

      --  A declaration hides a previous one.
      Warnid_Hide,

      --  Emit a warning when a declaration is never used.
      --  FIXME: currently only subprograms are handled.
      Warnid_Unused,

      --  Others choice is not needed, all values are already covered.
      Warnid_Others,

      --  Violation of pure rules.
      Warnid_Pure,

      --  Violation of staticness rules
      Warnid_Static,

      --  Any warning
      Msgid_Warning,

      --  Any error
      Msgid_Error,

      --  Any fatal error
      Msgid_Fatal
     );

   --  All specific warning messages.
   subtype Msgid_Warnings is Msgid_Type
     range Warnid_Library .. Warnid_Static;

   subtype Msgid_All_Warnings is Msgid_Type
     range Msgid_Warnings'First .. Msgid_Warning;

   --  Get the image of a warning.  This correspond the the identifier of ID,
   --  in lower case, without the Msgid_Warn_ prefix and with '_' replaced
   --  by '-'.
   function Warning_Image (Id : Msgid_Warnings) return String;

   --  Enable or disable a warning.
   procedure Enable_Warning (Id : Msgid_Warnings; Enable : Boolean);

   --  Get enable status of a warning.
   function Is_Warning_Enabled (Id : Msgid_Warnings) return Boolean;

   --  Consider a warning as an error.
   procedure Warning_Error (Id : Msgid_All_Warnings; As_Error : Boolean);

   --  State of warnings.
   type Warnings_Setting is private;

   --  Global control of warnings.
   --  Used to disable warnings while a referenced unit is analyzed.
   procedure Save_Warnings_Setting (Res : out Warnings_Setting);
   procedure Disable_All_Warnings;
   procedure Restore_Warnings_Setting (Res : Warnings_Setting);

   type Earg_Type is private;
   type Earg_Arr is array (Natural range <>) of Earg_Type;

   --  An empty array (for no arguments).
   No_Eargs : constant Earg_Arr;

   --  Report display:
   --  %%: %
   --  %i: identifier
   --  %c: character
   --  %t: token
   --  %l: location
   --  %n: node name
   --  %s: a string
   --  TODO: %m: mode, %y: type of
   function "+" (V : Location_Type) return Earg_Type;
   function "+" (V : Name_Id) return Earg_Type;
   function "+" (V : Character) return Earg_Type;
   function "+" (V : String8_Len_Type) return Earg_Type;

   --  Convert location.
   function "+" (L : Location_Type) return Source_Coord_Type;

   --  Pass that detected the error.
   type Report_Origin is
     (Option, Library, Scan, Parse, Semantic, Elaboration);

   type Error_Record is record
      Origin : Report_Origin;
      Id : Msgid_Type;

      --  Error soure file.
      File : Source_File_Entry;

      --  The first line is line 1, 0 can be used when line number is not
      --  relevant.
      Line : Natural;

      --  Offset in the line.  The first character is at offset 0.
      Offset : Natural;

      --  Length of the location (for a range).  It is assumed to be on the
      --  same line; use 0 when unknown.
      Length : Natural;
   end record;

   type Error_Start_Handler is access procedure (Err : Error_Record);
   type Message_Str_Handler is access procedure (Str : String);
   type Message_End_Handler is access procedure;
   type Message_Group_Handler is access procedure (Start : Boolean);

   type Report_Msg_Handler is record
      Error_Start : Error_Start_Handler;
      Message : Message_Str_Handler;
      Message_End : Message_End_Handler;
      Message_Group : Message_Group_Handler;
   end record;

   procedure Set_Report_Handler (Handler : Report_Msg_Handler);

   --  Generic report message.
   --  If ORIGIN is Option or Library, LOC must be No_Source_Coord and the
   --  program name is displayed.
   procedure Report_Msg (Id : Msgid_Type;
                         Origin : Report_Origin;
                         Loc : Source_Coord_Type;
                         Msg : String;
                         Args : Earg_Arr := No_Eargs);

   --  Group several messages (for multi-lines messages).
   --  Report_Start_Group must be called before the first Report_Msg call,
   --  and Report_End_Group after the last one.
   procedure Report_Start_Group;
   procedure Report_End_Group;

   --  Disp an error, prepended with program name, and raise option_error.
   --  This is used for errors before initialisation, such as bad option or
   --  bad filename.
   procedure Error_Msg_Option (Msg: String; Args : Earg_Arr := No_Eargs);
   pragma No_Return (Error_Msg_Option);

   --  Same as Error_Msg_Option but do not raise Option_Error.
   procedure Error_Msg_Option_NR (Msg: String);

   --  Warn about an option.
   procedure Warning_Msg_Option (Id : Msgid_Warnings; Msg: String);

   function Make_Earg_Vhdl_Node (V : Vhdl.Nodes.Iir) return Earg_Type;
   function Make_Earg_Vhdl_Token (V : Vhdl.Tokens.Token_Type) return Earg_Type;
private
   type Earg_Kind is
     (Earg_None,
      Earg_Location, Earg_Id, Earg_Char, Earg_String8,
      Earg_Vhdl_Node, Earg_Vhdl_Token);

   type Earg_Type (Kind : Earg_Kind := Earg_None) is record
      case Kind is
         when Earg_None =>
            null;
         when Earg_Location =>
            Val_Loc : Location_Type;
         when Earg_Id =>
            Val_Id : Name_Id;
         when Earg_Char =>
            Val_Char : Character;
         when Earg_String8 =>
            Val_Str8 : String8_Len_Type;
         when Earg_Vhdl_Node =>
            Val_Vhdl_Node : Vhdl.Nodes.Iir;
         when Earg_Vhdl_Token =>
            Val_Vhdl_Tok : Vhdl.Tokens.Token_Type;
      end case;
   end record;

   No_Eargs : constant Earg_Arr := (1 .. 0 => (Kind => Earg_None));

   type Warning_Control_Type is record
      Enabled : Boolean;
      Error : Boolean;
   end record;

   type Warnings_Setting is array (Msgid_All_Warnings) of Warning_Control_Type;

   Default_Warnings : constant Warnings_Setting :=
     (Warnid_Library | Warnid_Binding | Warnid_Port | Warnid_Shared
        | Warnid_Runtime_Error | Warnid_Pure | Warnid_Specs | Warnid_Hide
        | Msgid_Warning  => (Enabled => True, Error => False),
      others             => (Enabled => False, Error => False));

   --  Compute the column from Error_Record E.
   function Get_Error_Col (E : Error_Record) return Natural;

   --  Image of VAL, without the leading space.
   function Natural_Image (Val: Natural) return String;
end Errorout;
