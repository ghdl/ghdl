--  Utilities to create a simple CLI with menus.
--  Copyright (C) 2023 Tristan Gingold
--
--  This file is part of GHDL.
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

with Grt.Types; use Grt.Types;

package Debuggers is
   type Menu_Procedure is access procedure (Line : String);

   --  This exception can be raised by a debugger command to directly return
   --  to the prompt.
   Command_Error : exception;

   --  If set (by commands), call this procedure on empty line to repeat
   --  last command.
   Cmd_Repeat : Menu_Procedure;

   type Menu_Kind is (Menu_Command, Menu_Submenu);
   type Menu_Entry (Kind : Menu_Kind);
   type Menu_Entry_Acc is access all Menu_Entry;

   type Cst_String_Acc is access constant String;

   Menu_Top : Menu_Entry_Acc;

   --  Return the index of the first non-blank character.
   function Skip_Blanks (S : String) return Positive;

   --  Return entry for command CMD.
   function Find_Menu (Menu : Menu_Entry_Acc; Cmd : String)
                      return Menu_Entry_Acc;

   --  Append command to MENU.
   procedure Append_Menu (Menu : Menu_Entry;
                          Name : Cst_String_Acc;
                          Help : Cst_String_Acc;
                          Proc : Menu_Procedure);

   --  Return the position of the last character of the word (the last
   --  non-blank character).
   function Get_Word (S : String) return Positive;

   --  Extract command from LINE, return index P of the next argument and
   --  command MENU.
   procedure Parse_Command (Line : String;
                            P : in out Natural;
                            Menu : in out Menu_Entry_Acc);

   --  The status of the debugger.  This status can be modified by a command
   --  as a side effect to resume or quit the debugger.
   type Command_Status_Type is (Status_Default, Status_Quit);
   Command_Status : Command_Status_Type;

   type Menu_Entry (Kind : Menu_Kind) is record
      Name : Cst_String_Acc;
      Help : Cst_String_Acc;
      Next : Menu_Entry_Acc;

      case Kind is
         when Menu_Command =>
            Proc : Menu_Procedure;
         when Menu_Submenu =>
            First : Menu_Entry_Acc := null;
      end case;
   end record;

   procedure Help_Proc (Line : String);

   procedure Debug_Loop (Prompt : Ghdl_C_String);

end Debuggers;
