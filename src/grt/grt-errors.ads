--  GHDL Run Time (GRT) - Error handling.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Stdio;

package Grt.Errors is
   pragma Preelaborate (Grt.Errors);

   --  Set the stream for error messages.  Must be called before using this
   --  package.
   procedure Set_Error_Stream (Stream : Grt.Stdio.FILEs);
   function Get_Error_Stream return Grt.Stdio.FILEs;

   --  Use of diagnostics:
   --  Use Error_S/Report_S/Info_S to start a message.
   --  Use Diag_C to continue the message (to display arguments)
   --  Use Error_E/Report_E/Info_E to finish the message.
   --
   --  The XXX_S and XXX_E must match. Diag_C calls are optional.
   --  'S' stands for start, 'C' for continue and 'E' for end.
   --
   --  Example:
   --    Error_S ("option '");
   --    Diag_C (Name);
   --    Error_E ("' needs an argument");
   --
   --  The reason to have 3+ steps is that XXX_S display a different header
   --  (like 'filename:error:'), while XXX_E may return or not.

   --  Continue to display a message during a diagnostic.
   procedure Diag_C (Str : String);
   procedure Diag_C (C : Character);
   procedure Diag_C (N : Integer);
   procedure Diag_C (N : Ghdl_I32);
   procedure Diag_C (Str : Ghdl_C_String);
   procedure Diag_C (Str : Std_String_Ptr);
   procedure Diag_C_Std (Str : Std_String_Uncons);
   procedure Diag_C_Now;

   --  Multi-call error diagnostic.
   procedure Error_S (Str : String := "");
   procedure Error_E (Str : String := "");
   pragma No_Return (Error_E);

   --  Multi-call report diagnostic.  Do not exit at end.
   procedure Report_S (Str : String := "");
   procedure Report_E;

   --  Multi-call warning diagnostic.  Do not exit at end.
   procedure Warning_S (Str : String := "");
   procedure Warning_E;

   --  Complete error message.
   procedure Error (Str : String);
   pragma No_Return (Error);

   procedure Error (Str : String;
                    Filename : Ghdl_C_String;
                    Line : Ghdl_I32);
   pragma No_Return (Error);

   --  Non-fatal error (for the final message in case of previous error).
   --  Set the exit status.
   procedure Error_NF (Str : String);

   --  Warning message.
   procedure Warning (Str : String);

   --  Internal error.  The message must contain the subprogram name which
   --  has called this procedure.
   procedure Internal_Error (Msg : String);
   pragma No_Return (Internal_Error);

   --  Display a message which is not an error.
   procedure Info_S (Str : String := "");
   procedure Info_E;

   --  Called at end of error message.  Central point for failures.
   procedure Fatal_Error;
   pragma No_Return (Fatal_Error);
   pragma Export (C, Fatal_Error, "__ghdl_fatal");

   --  Stop or finish simulation (for VHPI or std.env).
   Exit_Status : Integer := 0;
   procedure Exit_Simulation;

   --  Simulation status,
   --  Runtime error.
   Run_Error : constant Integer := -1;
   --  No process has been run.
   Run_None : constant Integer := 1;
   --  At least one process was run.
   Run_Resumed : constant Integer := 2;
   --  Simulation is finished.
   Run_Finished : constant Integer := 3;
   --  Simulation finished because of a user-defined time or delta limit.
   Run_Limit : constant Integer := 4;
   --  Stop/finish request from user (via std.env).
   Run_Stop : constant Integer := 5;

   --  If true, an error is expected and the exit status is inverted.
   Expect_Failure : Boolean := False;

   --  Internal subprograms, to be called only by the symbolizer.
   procedure Put_Err (C : Character) renames Diag_C;
   procedure Put_Err (Str : String) renames Diag_C;
   procedure Put_Err (Str : Ghdl_C_String) renames Diag_C;
   procedure Put_Err (N : Integer) renames Diag_C;
   procedure Newline_Err;
end Grt.Errors;
