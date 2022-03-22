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
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio; use Grt.Astdio;
with Grt.Astdio.Vhdl; use Grt.Astdio.Vhdl;
with Grt.Options; use Grt.Options;
with Grt.Hooks; use Grt.Hooks;

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

   procedure Diag_C (Str : String) is
   begin
      Put (Error_Stream, Str);
   end Diag_C;

   procedure Diag_C (N : Integer) is
   begin
      Put_I32 (Error_Stream, Ghdl_I32 (N));
   end Diag_C;

   procedure Diag_C (N : Ghdl_I32) is
   begin
      Put_I32 (Error_Stream, N);
   end Diag_C;

   procedure Diag_C (Str : Ghdl_C_String) is
   begin
      Put (Error_Stream, Str);
   end Diag_C;

   procedure Diag_C_Std (Str : Std_String_Uncons)
   is
      subtype Str_Subtype is String (1 .. Str'Length);
   begin
      Put (Error_Stream, Str_Subtype (Str));
   end Diag_C_Std;

   procedure Diag_C (Str : Std_String_Ptr)
   is
      subtype Ada_Str is String (1 .. Natural (Str.Bounds.Dim_1.Length));
   begin
      if Ada_Str'Length > 0 then
         Diag_C (Ada_Str (Str.Base (0 .. Str.Bounds.Dim_1.Length - 1)));
      end if;
   end Diag_C;

   procedure Diag_C (C : Character) is
   begin
      Put (Error_Stream, C);
   end Diag_C;

   procedure Diag_C_Now is
   begin
      Put_Time (Error_Stream, Grt.Vhdl_Types.Current_Time);
   end Diag_C_Now;

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

   procedure Report_S (Str : String := "") is
   begin
      Diag_C (Str);
   end Report_S;

   procedure Report_E is
   begin
      Newline_Err;
   end Report_E;

   procedure Warning_S (Str : String := "") is
   begin
      Put_Err ("warning: ");
      Diag_C (Str);
   end Warning_S;

   procedure Warning_E is
   begin
      Newline_Err;
   end Warning_E;

   procedure Error_S (Str : String := "") is
   begin
      Put_Err (Progname);
      Put_Err (":error: ");

      Diag_C (Str);
   end Error_S;

   procedure Error_E (Str : String := "") is
   begin
      Diag_C (Str);
      Newline_Err;
      Fatal_Error;
   end Error_E;

   procedure Error (Str : String) is
   begin
      Error_S (Str);
      Error_E;
   end Error;

   procedure Error (Str : String;
                    Filename : Ghdl_C_String;
                    Line : Ghdl_I32) is
   begin
      Error_S (Str);
      Diag_C (" at ");
      Diag_C (Filename);
      Diag_C (" line ");
      Diag_C (Line);
      Error_E;
   end Error;

   procedure Error_NF (Str : String) is
   begin
      Error_S (Str);
      Newline_Err;
      Exit_Status := 1;
   end Error_NF;

   procedure Info_S (Str : String := "") is
   begin
      Put_Err (Progname);
      Put_Err (":info: ");
      Diag_C (Str);
   end Info_S;

   procedure Info_E is
   begin
      Newline_Err;
   end Info_E;

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
end Grt.Errors;
