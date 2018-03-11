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
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio; use Grt.Astdio;
with Grt.Options; use Grt.Options;
with Grt.Hooks; use Grt.Hooks;
with Grt.Backtraces;

package body Grt.Errors is
   --  Output stream to send error messages
   Error_Stream : FILEs;

   procedure Set_Error_Stream (Stream : Grt.Stdio.FILEs) is
   begin
      Error_Stream := Stream;
   end Set_Error_Stream;

   function Get_Error_Stream return Grt.Stdio.FILEs is
   begin
      return Error_Stream;
   end Get_Error_Stream;

   --  Called in case of premature exit.
   --  CODE is 0 for success, 1 for failure.
   procedure Ghdl_Exit (Code : Integer);
   pragma No_Return (Ghdl_Exit);

   procedure Ghdl_Exit (Code : Integer)
   is
      procedure C_Exit (Status : Integer);
      pragma Import (C, C_Exit, "exit");
      pragma No_Return (C_Exit);
   begin
      C_Exit (Code);
   end Ghdl_Exit;

   procedure Maybe_Return_Via_Longjump (Val : Integer);
   pragma Import (C, Maybe_Return_Via_Longjump,
                  "__ghdl_maybe_return_via_longjump");

   procedure Exit_Simulation is
   begin
      Maybe_Return_Via_Longjump (Run_Stop);
      Internal_Error ("exit_simulation");
   end Exit_Simulation;

   procedure Fatal_Error is
   begin
      if Error_Hook /= null then
         --  Call the hook, but avoid infinite loop by reseting it.
         declare
            Current_Hook : constant Proc_Hook_Type := Error_Hook;
         begin
            Error_Hook := null;
            Current_Hook.all;
         end;
      end if;
      Maybe_Return_Via_Longjump (-1);
      if Expect_Failure then
         Ghdl_Exit (0);
      else
         Ghdl_Exit (1);
      end if;
   end Fatal_Error;

   procedure Put_Err (Str : String) is
   begin
      Put (Error_Stream, Str);
   end Put_Err;

   procedure Put_Err (C : Character) is
   begin
      Put (Error_Stream, C);
   end Put_Err;

   procedure Put_Err (Str : Ghdl_C_String) is
   begin
      Put (Error_Stream, Str);
   end Put_Err;

   procedure Put_Err (N : Integer) is
   begin
      Put_I32 (Error_Stream, Ghdl_I32 (N));
   end Put_Err;

   procedure Newline_Err is
   begin
      New_Line (Error_Stream);
   end Newline_Err;

--    procedure Put_Err (Str : Ghdl_Str_Len_Type)
--    is
--       S : String (1 .. 3);
--    begin
--       if Str.Str = null then
--          S (1) := ''';
--          S (2) := Character'Val (Str.Len);
--          S (3) := ''';
--          Put_Err (S);
--       else
--          Put_Err (Str.Str (1 .. Str.Len));
--       end if;
--    end Put_Err;

   procedure Report_H (Str : String := "") is
   begin
      Put_Err (Str);
   end Report_H;

   procedure Report_C (Str : String) is
   begin
      Put_Err (Str);
   end Report_C;

   procedure Report_C (Str : Ghdl_C_String)
   is
      Len : constant Natural := strlen (Str);
   begin
      Put_Err (Str (1 .. Len));
   end Report_C;

   procedure Report_C (N : Integer)
     renames Put_Err;

   procedure Report_Now_C is
   begin
      Put_Time (Error_Stream, Grt.Types.Current_Time);
   end Report_Now_C;

   procedure Report_E (Str : String) is
   begin
      Put_Err (Str);
      Newline_Err;
   end Report_E;

   procedure Report_E (N : Integer) is
   begin
      Put_Err (N);
      Newline_Err;
   end Report_E;

   procedure Report_E (Str : Std_String_Ptr)
   is
      subtype Ada_Str is String (1 .. Natural (Str.Bounds.Dim_1.Length));
   begin
      if Ada_Str'Length > 0 then
         Put_Err (Ada_Str (Str.Base (0 .. Str.Bounds.Dim_1.Length - 1)));
      end if;
      Newline_Err;
   end Report_E;

   procedure Error_H is
   begin
      Put_Err (Progname);
      Put_Err (":error: ");
   end Error_H;

   Cont : Boolean := False;

   procedure Error_C (Str : String) is
   begin
      if not Cont then
         Error_H;
         Cont := True;
      end if;
      Put_Err (Str);
   end Error_C;

   procedure Error_C (Str : Ghdl_C_String)
   is
      Len : constant Natural := strlen (Str);
   begin
      if not Cont then
         Error_H;
         Cont := True;
      end if;
      Put_Err (Str (1 .. Len));
   end Error_C;

   procedure Error_C (N : Integer) is
   begin
      if not Cont then
         Error_H;
         Cont := True;
      end if;
      Put_Err (N);
   end Error_C;

--    procedure Error_C (Inst : Ghdl_Instance_Name_Acc)
--    is
--    begin
--       if not Cont then
--          Error_H;
--          Cont := True;
--       end if;
--       if Inst.Parent /= null then
--          Error_C (Inst.Parent);
--          Put_Err (".");
--       end if;
--       case Inst.Kind is
--          when Ghdl_Name_Architecture =>
--             Put_Err ("(");
--             Put_Err (Inst.Name.all);
--             Put_Err (")");
--          when others =>
--             if Inst.Name /= null then
--                Put_Err (Inst.Name.all);
--             end if;
--       end case;
--    end Error_C;

   procedure Error_E (Str : String := "") is
   begin
      Put_Err (Str);
      Newline_Err;
      Cont := False;
      Fatal_Error;
   end Error_E;

   procedure Error_C_Std (Str : Std_String_Uncons)
   is
      subtype Str_Subtype is String (1 .. Str'Length);
   begin
      Error_C (Str_Subtype (Str));
   end Error_C_Std;

   procedure Error (Str : String) is
   begin
      Error_H;
      Put_Err (Str);
      Newline_Err;
      Fatal_Error;
   end Error;

   procedure Error (Str : String;
                    Filename : Ghdl_C_String;
                    Line : Ghdl_I32) is
   begin
      Error_H;
      Put_Err (Str);
      Put_Err (" at ");
      Put_Err (Filename);
      Put_Err (" line ");
      Put_I32 (Error_Stream, Line);
      Newline_Err;
      Fatal_Error;
   end Error;

   procedure Info (Str : String) is
   begin
      Put_Err (Progname);
      Put_Err (":info: ");
      Put_Err (Str);
      Newline_Err;
   end Info;

   procedure Warning (Str : String) is
   begin
      Put_Err (Progname);
      Put_Err (":warning: ");
      Put_Err (Str);
      Newline_Err;
   end Warning;

   procedure Internal_Error (Msg : String) is
   begin
      Put_Err (Progname);
      Put_Err (":internal error: ");
      Put_Err (Msg);
      Newline_Err;
      Fatal_Error;
   end Internal_Error;

   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs) is
   begin
      Newline_Err;

      Grt.Backtraces.Put_Err_Backtrace (Bt);

      Cont := False;
      Fatal_Error;
   end Error_E_Call_Stack;

   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs_Acc) is
   begin
      if Bt /= null then
         Error_E_Call_Stack (Bt.all);
      else
         Error_E;
      end if;
   end Error_E_Call_Stack;

   procedure Grt_Overflow_Error (Bt : Backtrace_Addrs_Acc) is
   begin
      Error_C ("overflow detected");
      Error_E_Call_Stack (Bt);
   end Grt_Overflow_Error;

   procedure Grt_Null_Access_Error (Bt : Backtrace_Addrs_Acc) is
   begin
      Error_C ("NULL access dereferenced");
      Error_E_Call_Stack (Bt);
   end Grt_Null_Access_Error;
end Grt.Errors;
