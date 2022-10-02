--  GHDL Run Time (GRT) -  misc subprograms.
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

with Interfaces;
with Grt.Errors; use Grt.Errors;
with Grt.Errors_Exec; use Grt.Errors_Exec;
with Grt.Severity;
with Grt.Options; use Grt.Options;
with Grt.Fcvt;
with Grt.Backtraces;
with Grt.Arith;

package body Grt.Lib is
   --procedure Memcpy (Dst : Address; Src : Address; Size : Size_T);
   --pragma Import (C, Memcpy);

   procedure Ghdl_Memcpy
     (Dest : Ghdl_Ptr; Src : Ghdl_Ptr; Size : Ghdl_Index_Type)
   is
      procedure Memmove
        (Dest : Ghdl_Ptr; Src : Ghdl_Ptr; Size : Ghdl_Index_Type);
      pragma Import (C, Memmove);
   begin
      Memmove (Dest, Src, Size);
   end Ghdl_Memcpy;

   procedure Do_Report (Msg : String;
                        Str : Std_String_Ptr;
                        Default_Str : String;
                        Severity : Integer;
                        Loc : Ghdl_Location_Ptr)
   is
      use Grt.Severity;
      Level : constant Integer := Severity mod 256;
      Bt : Backtrace_Addrs;
   begin
      Report_S;
      Diag_C (Loc.Filename);
      Diag_C (':');
      Diag_C (Loc.Line);
      Diag_C (':');
      Diag_C (Loc.Col);
      Diag_C (":@");
      Diag_C_Now;
      Diag_C (":(");
      Diag_C (Msg);
      Diag_C (" ");
      case Level is
         when Note_Severity =>
            Diag_C ("note");
         when Warning_Severity =>
            Diag_C ("warning");
         when Error_Severity =>
            Diag_C ("error");
         when Failure_Severity =>
            Diag_C ("failure");
         when others =>
            Diag_C ("???");
      end case;
      Diag_C ("): ");
      if Str /= null then
         Diag_C (Str);
      else
         Diag_C (Default_Str);
      end if;
      Report_E;
      if Level >= Grt.Options.Severity_Level then
         Save_Backtrace (Bt, 2);
         Error_S (Msg);
         Diag_C (" failed");
         Error_E_Call_Stack (Bt);
      elsif Level >= Grt.Options.Backtrace_Severity then
         Save_Backtrace (Bt, 2);
         Grt.Backtraces.Put_Err_Backtrace (Bt);
      end if;
   end Do_Report;

   function Is_Assert_Disabled (Policy : Assert_Handling) return Boolean is
   begin
      return Policy = Disable_Asserts
        or else (Policy = Disable_Asserts_At_Time_0 and Current_Time = 0);
   end Is_Assert_Disabled;

   procedure Ghdl_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      if Is_Assert_Disabled (Asserts_Policy) then
         return;
      end if;
      Do_Report ("assertion", Str, "Assertion violation", Severity, Loc);
   end Ghdl_Assert_Failed;

   procedure Ghdl_Ieee_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      if Is_Assert_Disabled (Ieee_Asserts) then
         return;
      end if;
      Do_Report ("assertion", Str, "Assertion violation", Severity, Loc);
   end Ghdl_Ieee_Assert_Failed;

   procedure Ghdl_Psl_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      Do_Report ("psl assertion", Str, "Assertion violation", Severity, Loc);
   end Ghdl_Psl_Assert_Failed;

   procedure Ghdl_Psl_Assume_Failed (Loc : Ghdl_Location_Ptr) is
   begin
      Do_Report ("psl assumption", null, "Assumption violation",
                 Grt.Severity.Error_Severity, Loc);
   end Ghdl_Psl_Assume_Failed;

   procedure Ghdl_Psl_Cover
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      Do_Report ("psl cover", Str, "sequence covered", Severity, Loc);
   end Ghdl_Psl_Cover;

   procedure Ghdl_Psl_Cover_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      if Flag_Psl_Report_Uncovered then
         Do_Report ("psl cover failure",
                    Str, "sequence not covered", Severity, Loc);
      end if;
   end Ghdl_Psl_Cover_Failed;

   procedure Ghdl_Report (Str : Std_String_Ptr;
                          Severity : Integer;
                          Loc      : Ghdl_Location_Ptr) is
   begin
      Do_Report ("report", Str, "Assertion violation", Severity, Loc);
   end Ghdl_Report;

   procedure Ghdl_Program_Error (Filename : Ghdl_C_String;
                                 Line : Ghdl_I32;
                                 Code : Ghdl_Index_Type) is
   begin
      Error_S;
      case Code is
         when 1 =>
            Diag_C ("missing return in function");
         when 2 =>
            Diag_C ("block already configured");
         when 3 =>
            Diag_C ("bad configuration");
         when others =>
            Diag_C ("unknown error code ");
            Diag_C (Integer (Code));
      end case;
      Diag_C (" at ");
      if Filename = null then
         Diag_C ("*unknown*");
      else
         Diag_C (Filename);
      end if;
      Diag_C (":");
      Diag_C (Line);
      Error_E;
   end Ghdl_Program_Error;

   procedure Ghdl_Bound_Check_Failed (Filename : Ghdl_C_String;
                                      Line: Ghdl_I32)
   is
      Bt : Backtrace_Addrs;
   begin
      Save_Backtrace (Bt, 1);
      Error_S ("bound check failure at ");
      Diag_C (Filename);
      Diag_C (":");
      Diag_C (Line);
      Error_E_Call_Stack (Bt);
   end Ghdl_Bound_Check_Failed;

   procedure Ghdl_Direction_Check_Failed (Filename : Ghdl_C_String;
                                          Line: Ghdl_I32)
   is
      Bt : Backtrace_Addrs;
   begin
      Save_Backtrace (Bt, 1);
      Error_S ("slice direction doesn't match index direction at ");
      Diag_C (Filename);
      Diag_C (":");
      Diag_C (Line);
      Error_E_Call_Stack (Bt);
   end Ghdl_Direction_Check_Failed;

   procedure Ghdl_Access_Check_Failed
   is
      Bt : Backtrace_Addrs;
   begin
      Save_Backtrace (Bt, 1);
      Error_S ("NULL access dereferenced");
      Error_E_Call_Stack (Bt);
   end Ghdl_Access_Check_Failed;

   procedure Diag_C_Range (Rng : Std_Integer_Range_Ptr) is
   begin
      Diag_C (Rng.Left);
      case Rng.Dir is
         when Dir_Downto =>
            Diag_C (" downto ");
         when Dir_To =>
            Diag_C (" to ");
      end case;
      Diag_C (Rng.Right);
   end Diag_C_Range;

   procedure Ghdl_Integer_Index_Check_Failed
     (Filename : Ghdl_C_String;
      Line     : Ghdl_I32;
      Val      : Std_Integer;
      Rng      : Std_Integer_Range_Ptr)
   is
      Bt : Backtrace_Addrs;
   begin
      Save_Backtrace (Bt, 1);
      Error_S ("index (");
      Diag_C (Val);
      Diag_C (") out of bounds (");
      Diag_C_Range (Rng);
      Diag_C (") at ");
      Diag_C (Filename);
      Diag_C (":");
      Diag_C (Line);
      Error_E_Call_Stack (Bt);
   end Ghdl_Integer_Index_Check_Failed;

   function Ghdl_I32_Exp (V : Ghdl_I32; E : Std_Integer) return Ghdl_I32
   is
      Res : Ghdl_I32;
      Ovf : Boolean;
   begin
      Grt.Arith.Exp_I32 (V, E, Res, Ovf);
      if Ovf then
         Error ("overflow in exponentiation");
      end if;
      return Res;
   end Ghdl_I32_Exp;

   function Ghdl_I64_Exp (V : Ghdl_I64; E : Std_Integer) return Ghdl_I64
   is
      Res : Ghdl_I64;
      Ovf : Boolean;
   begin
      Grt.Arith.Exp_I64 (V, E, Res, Ovf);
      if Ovf then
         Error ("overflow in exponentiation");
      end if;
      return Res;
   end Ghdl_I64_Exp;

   function Ghdl_I32_Div (L, R : Ghdl_I32) return Ghdl_I32
   is
      pragma Suppress (Overflow_Check);
   begin
      if R = 0 then
         Error ("division by 0");
      elsif R = -1 and L = Ghdl_I32'First then
         Error ("overflow in division");
      end if;
      return L / R;
   end Ghdl_I32_Div;

   function Ghdl_I64_Div (L, R : Ghdl_I64) return Ghdl_I64
   is
      pragma Suppress (Overflow_Check);
   begin
      if R = 0 then
         Error ("division by 0");
      elsif R = -1 and L = Ghdl_I64'First then
         Error ("overflow in division");
      end if;
      return L / R;
   end Ghdl_I64_Div;

   function Ghdl_I32_Mod (L, R : Ghdl_I32) return Ghdl_I32
   is
      pragma Suppress (Overflow_Check);
   begin
      if R = 0 then
         Error ("division by 0");
      end if;
      return L mod R;
   end Ghdl_I32_Mod;

   function Ghdl_I64_Mod (L, R : Ghdl_I64) return Ghdl_I64
   is
      pragma Suppress (Overflow_Check);
   begin
      if R = 0 then
         Error ("division by 0");
      end if;
      return L mod R;
   end Ghdl_I64_Mod;

   procedure Ghdl_Check_Stack_Allocation (Size : Ghdl_Index_Type)
   is
      Bt : Backtrace_Addrs;
   begin
      if Max_Stack_Allocation = 0 then
         return;
      end if;
      if Size > Max_Stack_Allocation then
         Save_Backtrace (Bt, 1);
         Error_S ("declaration of a too large object (");
         Diag_C (Natural (Size / 1024));
         Diag_C (" > --max-stack-alloc=");
         Diag_C (Natural (Max_Stack_Allocation / 1024));
         Diag_C (" KB)");
         Error_E_Call_Stack (Bt);
      end if;
   end Ghdl_Check_Stack_Allocation;

   function C_Malloc (Size : Ghdl_Index_Type) return Ghdl_Ptr;
   pragma Import (C, C_Malloc, "malloc");

   function Ghdl_Malloc (Size : Ghdl_Index_Type) return Ghdl_Ptr is
   begin
      return C_Malloc (Size);
   end Ghdl_Malloc;

   function Ghdl_Malloc0 (Size : Ghdl_Index_Type) return Ghdl_Ptr
   is
      procedure Memset (Ptr : Ghdl_Ptr; C : Integer; Size : Ghdl_Index_Type);
      pragma Import (C, Memset);

      Res : Ghdl_Ptr;
   begin
      Res := C_Malloc (Size);
      Memset (Res, 0, Size);
      return Res;
   end Ghdl_Malloc0;

   procedure Ghdl_Deallocate (Ptr : Ghdl_Ptr)
   is
      procedure C_Free (Ptr : Ghdl_Ptr);
      pragma Import (C, C_Free, "free");
   begin
      C_Free (Ptr);
   end Ghdl_Deallocate;

   function Ghdl_Real_Exp (X : Ghdl_Real; Exp : Ghdl_I32)
     return Ghdl_Real
   is
      R : Ghdl_I32;
      Res : Ghdl_Real;
      P : Ghdl_Real;
   begin
      Res := 1.0;
      P := X;
      R := Exp;
      if R >= 0 then
         loop
            if R mod 2 = 1 then
               Res := Res * P;
            end if;
            R := R / 2;
            exit when R = 0;
            P := P * P;
         end loop;
         return Res;
      else
         R := -R;
         loop
            if R mod 2 = 1 then
               Res := Res * P;
            end if;
            R := R / 2;
            exit when R = 0;
            P := P * P;
         end loop;
         if Res = 0.0 then
            Error ("division per 0.0");
            return 0.0;
         end if;
         return 1.0 / Res;
      end if;
   end Ghdl_Real_Exp;

   function Textio_Read_Real (Str : Std_String_Ptr) return Ghdl_F64
   is
      subtype Str1 is String (1 .. Natural (Str.Bounds.Dim_1.Length));
   begin
      return Ghdl_F64 (Grt.Fcvt.From_String
                         (Str1 (Str.Base (0 .. Str.Bounds.Dim_1.Length - 1))));
   end Textio_Read_Real;

   procedure Textio_Write_Real (Str : Std_String_Ptr;
                                Len : Std_Integer_Acc;
                                V : Ghdl_F64;
                                Ndigits : Std_Integer)
   is
      --  FIXME: avoid that copy.
      S : String (1 .. Natural (Str.Bounds.Dim_1.Length));
      Last : Natural;
   begin
      Grt.Fcvt.Format_Digits
        (S, Last, Interfaces.IEEE_Float_64 (V), Natural (Ndigits));
      Len.all := Std_Integer (Last);
      for I in 1 .. Last loop
         Str.Base (Ghdl_Index_Type (I - 1)) := S (I);
      end loop;
   end Textio_Write_Real;

   function Ghdl_Get_Resolution_Limit return Std_Time is
   begin
      return 1;
   end Ghdl_Get_Resolution_Limit;

   procedure Ghdl_Control_Simulation
     (Stop : Ghdl_B1; Has_Status : Ghdl_B1; Status : Std_Integer) is
   begin
      Report_S;
      --  Report_C (Grt.Options.Progname);
      Diag_C ("simulation ");
      if Stop then
         Diag_C ("stopped");
      else
         Diag_C ("finished");
      end if;
      Diag_C (" @");
      Diag_C_Now;
      if Has_Status then
         Diag_C (" with status ");
         Diag_C (Integer (Status));
      end if;
      Report_E;
      if Has_Status then
         Exit_Status := Integer (Status);
      end if;
      Exit_Simulation;
   end Ghdl_Control_Simulation;

end Grt.Lib;
