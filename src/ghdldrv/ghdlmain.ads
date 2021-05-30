--  GHDL driver - main part.
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Options; use Options;

package Ghdlmain is
   type Command_Type is tagged;

   type Command_Acc is access all Command_Type'Class;

   type Command_Type is abstract tagged record
      Next : Command_Acc;
   end record;

   --  Return TRUE iff CMD handle action ACTION.
   function Decode_Command (Cmd : Command_Type; Name : String) return Boolean
     is abstract;

   --  Initialize the command, before decoding actions.
   procedure Init (Cmd : in out Command_Type);

   procedure Decode_Option (Cmd : in out Command_Type;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);

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

   --  A command that accepts command and help strings.
   type Command_Str_Type is abstract new Command_Type with record
      Cmd_Str : String_Access;
      Help_Str : String_Access;
   end record;
   function Decode_Command (Cmd : Command_Str_Type; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Str_Type) return String;

   --  A command that display a string.
   type String_Func is access function return String;
   type Command_Str_Disp is new Command_Str_Type with record
      Disp : String_Func;
   end record;
   procedure Perform_Action (Cmd : in out Command_Str_Disp;
                             Args : Argument_List);

   --  Register a command.
   procedure Register_Command (Cmd : Command_Acc);

   --  Disp MSG on the standard output with the command name.
   procedure Error (Msg : String);
   procedure Warning (Msg : String);

   --  Return the index of C in STR, or 0 if not found.
   function Index (Str : String; C : Character) return Natural;

   --  Action failed.
   Compile_Error : exception;

   --  Exec failed: either the program was not found, or failed.
   Exec_Error : exception;

   --  Decode options from ARGS for command CMD after initializing CMD.
   --  Return the index of the first non-option argument.
   procedure Decode_Command_Options (Cmd : in out Command_Type'Class;
                                     Args : Argument_List;
                                     First_Arg : out Natural);

   procedure Main;

   --  Additionnal one-line message displayed by the --version command,
   --  if defined.
   --  Used to customize.
   type String_Cst_Acc is access constant String;
   Version_String : String_Cst_Acc := null;

   --  On windows, convert PATH to a unix path, so that a unix shell will
   --  convert it correctly to a windows path.
   --  Return PATH on non-windows platforms.
   function Convert_Path_To_Unix (Path : String) return String;

   --  Registers all commands in this package.
   procedure Register_Commands;
end Ghdlmain;
