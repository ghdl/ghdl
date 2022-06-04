--  Debugging during synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Types; use Types;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Elab.Debugger is
   --  True to start debugger on error.
   Flag_Debug_Enable : Boolean := False;

   --  If true, debugging is enabled:
   --  * call Debug_Break() before executing the next sequential statement
   --  * call Debug_Leave when a frame is destroyed.
   Flag_Need_Debug : Boolean := False;

   procedure Debug_Init (Top : Node);

   --  Debug after elaboration.  TOP is the top-level unit.
   procedure Debug_Elab (Top : Synth_Instance_Acc);

   procedure Debug_Break (Inst : Synth_Instance_Acc; Stmt : Node);

   procedure Debug_Leave (Inst : Synth_Instance_Acc);

   --  Debug on a time breakpoint.
   procedure Debug_Time;

   --  To be called in case of execution error, like:
   --  * index out of bounds.
   procedure Debug_Error (Inst : Synth_Instance_Acc; Expr : Node);

   function Debug_Current_Instance return Synth_Instance_Acc;

   type Menu_Procedure is access procedure (Line : String);
   type Cst_String_Acc is access constant String;

   --  Append a command to the main menu.
   procedure Append_Menu_Command (Name : Cst_String_Acc;
                                  Help : Cst_String_Acc;
                                  Proc : Menu_Procedure);

   --  Append a command to the info menu.
   procedure Append_Info_Command (Name : Cst_String_Acc;
                                  Help : Cst_String_Acc;
                                  Proc : Menu_Procedure);

   --  Prepare resume execution.
   procedure Prepare_Continue;

   --  Utilities for menu commands.

   --  Return the position of the first non-blank character.
   function Skip_Blanks (S : String) return Positive;
   function Skip_Blanks (S : String; F : Positive) return Positive;

   --  Return the position of the last character of the word (the last
   --  non-blank character).
   function Get_Word (S : String) return Positive;
   function Get_Word (S : String; F : Positive) return Positive;

   --  Convert STR to number RES, set VALID to true iff the conversion is ok.
   procedure To_Num (Str : String; Res : out Uns32; Valid : out Boolean);
end Elab.Debugger;
