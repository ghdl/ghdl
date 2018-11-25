--  GHDL driver - main part.
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Errorout;

package Ghdlmain is
   type Command_Type;

   type Command_Acc is access all Command_Type'Class;

   type Command_Type is abstract tagged record
      Next : Command_Acc;
   end record;

   --  Return TRUE iff CMD handle action ACTION.
   function Decode_Command (Cmd : Command_Type; Name : String) return Boolean
     is abstract;

   --  Initialize the command, before decoding actions.
   procedure Init (Cmd : in out Command_Type);

   --  Option_OK: OPTION is handled.
   --  Option_Bad: OPTION is unknown.
   --  Option_Err: OPTION has an error (message was displayed).
   --  Option_Arg_Req: OPTION requires an argument.  Must be set only when
   --     ARG = "", the manager will recall Decode_Option.
   --  Option_Arg: OPTION used the argument.
   type Option_Res is
     (Option_Bad, Option_Err,
      Option_Ok, Option_Arg, Option_Arg_Req,
      Option_End);
   procedure Decode_Option (Cmd : in out Command_Type;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   --  Get a one-line help for the command.
   --  If the first character is '!', the string is not displayed by --help
   --  (for internal commands).
   function Get_Short_Help (Cmd : Command_Type) return String
     is abstract;

   --  Disp detailled help.
   procedure Disp_Long_Help (Cmd : Command_Type);

   --  Perform the action.
   procedure Perform_Action (Cmd : in out Command_Type; Args : Argument_List)
     is abstract;

   --  Register a command.
   procedure Register_Command (Cmd : Command_Acc);

   --  Disp MSG on the standard output with the command name.
   procedure Error (Msg : String);
   procedure Warning (Msg : String);

   --  May be raise by perform_action if the arguments are bad.
   Option_Error : exception renames Errorout.Option_Error;

   --  Action failed.
   Compile_Error : exception;

   --  Exec failed: either the program was not found, or failed.
   Exec_Error : exception;

   procedure Main;

   --  Additionnal one-line message displayed by the --version command,
   --  if defined.
   --  Used to customize.
   type String_Cst_Acc is access constant String;
   Version_String : String_Cst_Acc := null;

   --  Registers all commands in this package.
   procedure Register_Commands;
end Ghdlmain;
