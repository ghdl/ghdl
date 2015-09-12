--  GHDL Run Time (GRT) - Error handling.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Types; use Grt.Types;
with Grt.Hooks;

package Grt.Errors is
   pragma Preelaborate (Grt.Errors);

   --  Multi-call error procedure.
   --  Start and continue with Error_C, finish by an Error_E.
   procedure Error_C (Str : String);
   procedure Error_C (N : Integer);
   procedure Error_C (Str : Ghdl_C_String);
   procedure Error_C_Std (Str : Std_String_Uncons);
   --procedure Error_C (Inst : Ghdl_Instance_Name_Acc);
   procedure Error_E (Str : String := "");
   pragma No_Return (Error_E);

   --  Multi-call report procedure.  Do not exit at end.
   procedure Report_H (Str : String := "");
   procedure Report_C (Str : Ghdl_C_String);
   procedure Report_C (Str : String);
   procedure Report_C (N : Integer);
   procedure Report_Now_C;
   procedure Report_E (Str : String);
   procedure Report_E (Str : Std_String_Ptr);

   --  Complete error message.
   procedure Error (Str : String);
   pragma No_Return (Error);

   --  Warning message.
   procedure Warning (Str : String);

   --  Internal error.  The message must contain the subprogram name which
   --  has called this procedure.
   procedure Internal_Error (Msg : String);
   pragma No_Return (Internal_Error);

   --  Display a message which is not an error.
   procedure Info (Str : String);

   --  Display an error message for an overflow.
   procedure Grt_Overflow_Error;
   pragma No_Return (Grt_Overflow_Error);

   --  Display an error message for a NULL access dereference.
   procedure Grt_Null_Access_Error;
   pragma No_Return (Grt_Null_Access_Error);

   --  Called at end of error message.  Central point for failures.
   procedure Fatal_Error;
   pragma No_Return (Fatal_Error);
   pragma Export (C, Fatal_Error, "__ghdl_fatal");

   --  Stop or finish simulation (for VHPI or std.env).
   Exit_Status : Integer := 0;
   procedure Exit_Simulation;

   --  Hook called in case of error.
   Error_Hook : Grt.Hooks.Proc_Hook_Type := null;

   --  If true, an error is expected and the exit status is inverted.
   Expect_Failure : Boolean := False;

private
   pragma Export (C, Grt_Overflow_Error, "grt_overflow_error");
   pragma Export (C, Grt_Null_Access_Error, "grt_null_access_error");
end Grt.Errors;

