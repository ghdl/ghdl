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
with Iirs; use Iirs;
with Tokens;

package Errorout is
   Option_Error: exception;
   Parse_Error: exception;
   Compilation_Error: exception;

   --  Set the program name, used in error messages for options.  Not displayed
   --  if not initialized.
   procedure Set_Program_Name (Name : String);

   -- This kind can't be handled.
   --procedure Error_Kind (Msg: String; Kind: Iir_Kind);
   procedure Error_Kind (Msg: String; An_Iir: in Iir);
   procedure Error_Kind (Msg: String; Def : Iir_Predefined_Functions);
   procedure Error_Kind (Msg : String; N : PSL_Node);
   pragma No_Return (Error_Kind);

   -- The number of errors (ie, number of calls to error_msg*).
   Nbr_Errors: Natural := 0;

   type Msgid_Type is
     (--  Any note
      Msgid_Note,

      --  Any warning
      Msgid_Warning,

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

      --  Any error
      Msgid_Error,

      --  Any fatal error
      Msgid_Fatal);

   --  All specific warning messages.
   subtype Msgid_Warnings is Msgid_Type
     range Warnid_Library .. Warnid_Static;

   --  Get the image of a warning.  This correspond the the identifier of ID,
   --  in lower case, without the Msgid_Warn_ prefix and with '_' replaced
   --  by '-'.
   function Warning_Image (Id : Msgid_Warnings) return String;

   --  Enable or disable a warning.
   procedure Enable_Warning (Id : Msgid_Warnings; Enable : Boolean);

   --  Get enable status of a warning.
   function Is_Warning_Enabled (Id : Msgid_Warnings) return Boolean;

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
   --  TODO: %m: mode, %y: type of, %s: disp_subprg
   function "+" (V : Iir) return Earg_Type;
   function "+" (V : Location_Type) return Earg_Type;
   function "+" (V : Name_Id) return Earg_Type;
   function "+" (V : Tokens.Token_Type) return Earg_Type;
   function "+" (V : Character) return Earg_Type;

   --  Convert location.
   function "+" (L : Iir) return Location_Type;
   function "+" (L : PSL_Node) return Location_Type;

   --  Pass that detected the error.
   type Report_Origin is
     (Option, Library, Scan, Parse, Semantic, Elaboration);

   --  Generic report message.  LOC maybe No_Location.
   --  If ORIGIN is Option or Library, LOC must be No_Location and the program
   --  name is displayed.
   procedure Report_Msg (Id : Msgid_Type;
                         Origin : Report_Origin;
                         Loc : Location_Type;
                         Msg : String;
                         Args : Earg_Arr := No_Eargs;
                         Cont : Boolean := False);

   --  Disp an error, prepended with program name, and raise option_error.
   --  This is used for errors before initialisation, such as bad option or
   --  bad filename.
   procedure Error_Msg_Option (Msg: String);
   pragma No_Return (Error_Msg_Option);

   --  Same as Error_Msg_Option but do not raise Option_Error.
   procedure Error_Msg_Option_NR (Msg: String);

   --  Warn about an option.
   procedure Warning_Msg_Option (Id : Msgid_Warnings; Msg: String);

   -- Disp a message during scan.
   -- The current location is automatically displayed before the message.
   procedure Error_Msg_Scan (Msg: String);
   procedure Error_Msg_Scan (Msg: String; Arg1 : Earg_Type);
   procedure Error_Msg_Scan (Loc : Location_Type; Msg: String);
   procedure Warning_Msg_Scan (Id : Msgid_Warnings; Msg: String);
   procedure Warning_Msg_Scan (Id : Msgid_Warnings;
                               Msg: String;
                               Arg1 : Earg_Type;
                               Cont : Boolean := False);

   -- Disp a message during parse
   -- The location of the current token is automatically displayed before
   -- the message.
   procedure Error_Msg_Parse_1 (Msg: String);
   procedure Error_Msg_Parse (Msg: String; Arg1 : Earg_Type);
   procedure Error_Msg_Parse
     (Msg: String; Args : Earg_Arr := No_Eargs; Cont : Boolean := False);
   procedure Error_Msg_Parse (Loc : Location_Type; Msg: String);

   -- Disp a message during semantic analysis.
   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Args : Earg_Arr := No_Eargs;
                              Cont : Boolean := False);
   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Arg1 : Earg_Type;
                              Cont : Boolean := False);

   procedure Error_Msg_Sem (Loc: Location_Type;
                            Msg: String;
                            Args : Earg_Arr := No_Eargs;
                            Cont : Boolean := False);
   procedure Error_Msg_Sem
     (Loc: Location_Type; Msg: String; Arg1 : Earg_Type);
   procedure Error_Msg_Sem_1 (Msg: String; Loc : PSL_Node);

   --  Like Error_Msg_Sem, but a warning if -frelaxed or --std=93c.
   procedure Error_Msg_Sem_Relaxed (Loc : Iir;
                                    Id : Msgid_Warnings;
                                    Msg : String;
                                    Args : Earg_Arr := No_Eargs);

   -- Disp a message during elaboration (or configuration).
   procedure Error_Msg_Elab
     (Msg: String; Args : Earg_Arr := No_Eargs);
   procedure Error_Msg_Elab
     (Msg: String; Arg1 : Earg_Type);
   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Args : Earg_Arr := No_Eargs);
   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Arg1 : Earg_Type);

   --  Like Error_Msg_Elab, but a warning if -frelaxed or --std=93c.
   procedure Error_Msg_Elab_Relaxed (Loc : Iir;
                                     Id : Msgid_Warnings;
                                     Msg : String;
                                     Args : Earg_Arr := No_Eargs);

   --  Disp a warning durig elaboration (or configuration).
   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Arg1 : Earg_Type;
                               Cont : Boolean := False);
   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Args : Earg_Arr := No_Eargs;
                               Cont : Boolean := False);

   -- Disp a bug message.
   procedure Error_Internal (Expr: Iir; Msg: String := "");
   pragma No_Return (Error_Internal);

   -- Disp a node.
   -- Used for output of message.
   function Disp_Node (Node: Iir) return String;

   -- Disp a node location.
   -- Used for output of message.
   function Disp_Location (Node: Iir) return String;

   --  Disp non-terminal name from KIND.
   function Disp_Name (Kind : Iir_Kind) return String;

   --  SUBPRG must be a subprogram declaration or an enumeration literal
   --  declaration.
   --  Returns:
   --   "enumeration literal XX [ return TYPE ]"
   --   "function XXX [ TYPE1, TYPE2 return TYPE ]"
   --   "procedure XXX [ TYPE1, TYPE2 ]"
   --   "implicit function XXX [ TYPE1, TYPE2 return TYPE ]"
   --   "implicit procedure XXX [ TYPE1, TYPE2 ]"
   function Disp_Subprg (Subprg : Iir) return String;

   --  Print element POS of discrete type DTYPE.
   function Disp_Discrete (Dtype : Iir; Pos : Iir_Int64) return String;

   --  Disp the name of the type of NODE if known.
   --  Disp "unknown" if it is not known.
   --  Disp all possible types if it is an overload list.
   function Disp_Type_Of (Node : Iir) return String;

   --  Disp an error message when a pure function CALLER calls impure CALLEE.
   procedure Error_Pure
     (Origin : Report_Origin; Caller : Iir; Callee : Iir; Loc : Iir);

   --  Report an error message as type of EXPR does not match A_TYPE.
   --  Location is EXPR.
   procedure Error_Not_Match (Expr: Iir; A_Type: Iir);

   --  Disp interface mode MODE.
   function Get_Mode_Name (Mode : Iir_Mode) return String;

private
   type Earg_Kind is
     (Earg_None,
      Earg_Iir, Earg_Location, Earg_Id, Earg_Char, Earg_Token);

   type Earg_Type (Kind : Earg_Kind := Earg_None) is record
      case Kind is
         when Earg_None =>
            null;
         when Earg_Iir =>
            Val_Iir : Iir;
         when Earg_Location =>
            Val_Loc : Location_Type;
         when Earg_Id =>
            Val_Id : Name_Id;
         when Earg_Char =>
            Val_Char : Character;
         when Earg_Token =>
            Val_Tok : Tokens.Token_Type;
      end case;
   end record;

   No_Eargs : constant Earg_Arr := (1 .. 0 => (Kind => Earg_None));

   type Warning_Control_Type is record
      Enabled : Boolean;
      Error : Boolean;
   end record;

   type Warnings_Setting is array (Msgid_Warnings) of Warning_Control_Type;

   Default_Warnings : constant Warnings_Setting :=
     (Warnid_Binding | Warnid_Library | Warnid_Shared
        | Warnid_Pure | Warnid_Specs | Warnid_Hide
        | Warnid_Port    => (Enabled => True, Error => False),
      others             => (Enabled => False, Error => False));
end Errorout;
