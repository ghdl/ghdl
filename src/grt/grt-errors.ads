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
with Grt.Stdio;

package Grt.Errors is
   pragma Preelaborate (Grt.Errors);

   --  Set the stream for error messages.  Must be called before using this
   --  package.
   procedure Set_Error_Stream (Stream : Grt.Stdio.FILEs);
   function Get_Error_Stream return Grt.Stdio.FILEs;

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
   procedure Report_E (N : Integer);

   --  Complete error message.
   procedure Error (Str : String);
   pragma No_Return (Error);

   procedure Error (Str : String;
                    Filename : Ghdl_C_String;
                    Line : Ghdl_I32);
   pragma No_Return (Error);

   --  Warning message.
   procedure Warning (Str : String);

   --  Internal error.  The message must contain the subprogram name which
   --  has called this procedure.
   procedure Internal_Error (Msg : String);
   pragma No_Return (Internal_Error);

   --  Display a message which is not an error.
   procedure Info (Str : String);

   --  Backtrace used to report call stack in case of error.
   --  Note: for simplicity we assume that a PC is enough to display the
   --  corresponding file name, line number and routine name.  Might not be
   --  true on some platforms.
   --  There is a C version of this record in grt_itf.h
   type Integer_Address_Array is array (Natural range <>) of Integer_Address;
   type Backtrace_Addrs is record
      Size : Natural;
      Skip : Natural;
      Addrs : Integer_Address_Array (0 .. 31);
   end record;
   pragma Convention (C, Backtrace_Addrs);

   type Backtrace_Addrs_Acc is access Backtrace_Addrs;

   --  Save the current backtrace to BT, but skip SKIP frame.  0 means that
   --  the caller of this procedure will be in the backtrace.
   procedure Save_Backtrace (Bt : out Backtrace_Addrs; Skip : Natural);
   pragma Import (C, Save_Backtrace, "grt_save_backtrace");

   --  Finish error message with a call stack.
   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs);
   pragma No_Return (Error_E_Call_Stack);

   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs_Acc);
   pragma No_Return (Error_E_Call_Stack);

   --  Display an error message for an overflow.
   procedure Grt_Overflow_Error (Bt : Backtrace_Addrs_Acc);
   pragma No_Return (Grt_Overflow_Error);

   --  Display an error message for a NULL access dereference.
   procedure Grt_Null_Access_Error (Bt : Backtrace_Addrs_Acc);
   pragma No_Return (Grt_Null_Access_Error);

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
   --  Stop/finish request from user (via std.env).
   Run_Stop : constant Integer := 4;

   --  Hook called in case of error.
   Error_Hook : Grt.Hooks.Proc_Hook_Type := null;

   --  If true, an error is expected and the exit status is inverted.
   Expect_Failure : Boolean := False;

   --  Internal subprograms, to be called only by the symbolizer.
   procedure Put_Err (C : Character);
   procedure Put_Err (Str : String);
   procedure Put_Err (Str : Ghdl_C_String);
   procedure Put_Err (N : Integer);
   procedure Newline_Err;
private
   pragma Export (C, Grt_Overflow_Error, "grt_overflow_error");
   pragma Export (C, Grt_Null_Access_Error, "grt_null_access_error");
end Grt.Errors;
