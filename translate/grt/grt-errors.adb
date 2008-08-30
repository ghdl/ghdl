--  GHDL Run Time (GRT) - Error handling.
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
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio; use Grt.Astdio;
with Grt.Options; use Grt.Options;

package body Grt.Errors is
   procedure Fatal_Error;
   pragma No_Return (Fatal_Error);
   pragma Export (C, Fatal_Error, "__ghdl_fatal");

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
      if Ghdl_Exit_Cb1 /= null then
         Ghdl_Exit_Cb1.all (Code);
      end if;

      if Ghdl_Exit_Cb /= null then
         Ghdl_Exit_Cb.all (Code);
      end if;
      C_Exit (Code);
   end Ghdl_Exit;

   procedure Maybe_Return_Via_Longjump (Val : Integer);
   pragma Import (C, Maybe_Return_Via_Longjump,
                  "__ghdl_maybe_return_via_longjump");

   procedure Fatal_Error is
   begin
      Maybe_Return_Via_Longjump (-1);
      if Expect_Failure then
         Ghdl_Exit (0);
      else
         Ghdl_Exit (1);
      end if;
   end Fatal_Error;

   procedure Put_Err (Str : String) is
   begin
      Put (stderr, Str);
   end Put_Err;

   procedure Put_Err (Str : Ghdl_C_String) is
   begin
      Put (stderr, Str);
   end Put_Err;

   procedure Put_Err (N : Integer) is
   begin
      Put_I32 (stderr, Ghdl_I32 (N));
   end Put_Err;

   procedure Newline_Err is
   begin
      New_Line (stderr);
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
      Put_Time (stderr, Grt.Types.Current_Time);
   end Report_Now_C;

   procedure Report_E (Str : String) is
   begin
      Put_Err (Str);
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

   procedure Error_E (Str : String) is
   begin
      Put_Err (Str);
      Newline_Err;
      Cont := False;
      Fatal_Error;
   end Error_E;

   procedure Error_E_Std (Str : Std_String_Uncons)
   is
      subtype Str_Subtype is String (1 .. Str'Length);
   begin
      Error_E (Str_Subtype (Str));
   end Error_E_Std;

   procedure Error (Str : String) is
   begin
      Error_H;
      Put_Err (Str);
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

   procedure Internal_Error (Msg : String) is
   begin
      Put_Err (Progname);
      Put_Err (":internal error: ");
      Put_Err (Msg);
      Newline_Err;
      Fatal_Error;
   end Internal_Error;

   procedure Grt_Overflow_Error is
   begin
      Error ("overflow detected");
   end Grt_Overflow_Error;
end Grt.Errors;
