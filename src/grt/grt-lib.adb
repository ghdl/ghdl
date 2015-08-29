--  GHDL Run Time (GRT) -  misc subprograms.
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
with Grt.Errors; use Grt.Errors;
with Grt.Options;

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
      Level : constant Integer := Severity mod 256;
   begin
      Report_H;
      Report_C (Loc.Filename);
      Report_C (":");
      Report_C (Loc.Line);
      Report_C (":");
      Report_C (Loc.Col);
      Report_C (":@");
      Report_Now_C;
      Report_C (":(");
      Report_C (Msg);
      Report_C (" ");
      case Level is
         when Note_Severity =>
            Report_C ("note");
         when Warning_Severity =>
            Report_C ("warning");
         when Error_Severity =>
            Report_C ("error");
         when Failure_Severity =>
            Report_C ("failure");
         when others =>
            Report_C ("???");
      end case;
      Report_C ("): ");
      if Str /= null then
         Report_E (Str);
      else
         Report_E (Default_Str);
      end if;
      if Level >= Grt.Options.Severity_Level then
         Error_C (Msg);
         Error_E (" failed");
      end if;
   end Do_Report;

   procedure Ghdl_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr)
   is
   begin
      Do_Report ("assertion", Str, "Assertion violation", Severity, Loc);
   end Ghdl_Assert_Failed;

   procedure Ghdl_Ieee_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr)
   is
      use Grt.Options;
   begin
      if Ieee_Asserts = Disable_Asserts
        or else (Ieee_Asserts = Disable_Asserts_At_Time_0 and Current_Time = 0)
      then
         return;
      else
         Do_Report ("assertion", Str, "Assertion violation", Severity, Loc);
      end if;
   end Ghdl_Ieee_Assert_Failed;

   procedure Ghdl_Psl_Assert_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      Do_Report ("psl assertion", Str, "Assertion violation", Severity, Loc);
   end Ghdl_Psl_Assert_Failed;

   procedure Ghdl_Psl_Cover
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      Do_Report ("psl cover", Str, "sequence covered", Severity, Loc);
   end Ghdl_Psl_Cover;

   procedure Ghdl_Psl_Cover_Failed
     (Str : Std_String_Ptr; Severity : Integer; Loc : Ghdl_Location_Ptr) is
   begin
      Do_Report ("psl cover failure",
                 Str, "sequence not covered", Severity, Loc);
   end Ghdl_Psl_Cover_Failed;

   procedure Ghdl_Report
     (Str : Std_String_Ptr;
      Severity : Integer;
      Loc : Ghdl_Location_Ptr)
   is
   begin
      Do_Report ("report", Str, "Assertion violation", Severity, Loc);
   end Ghdl_Report;

   procedure Ghdl_Program_Error (Filename : Ghdl_C_String;
                                 Line : Ghdl_I32;
                                 Code : Ghdl_Index_Type)
   is
   begin
      case Code is
         when 1 =>
            Error_C ("missing return in function");
         when 2 =>
            Error_C ("block already configured");
         when 3 =>
            Error_C ("bad configuration");
         when others =>
            Error_C ("unknown error code ");
            Error_C (Integer (Code));
      end case;
      Error_C (" at ");
      if Filename = null then
         Error_C ("*unknown*");
      else
         Error_C (Filename);
      end if;
      Error_C (":");
      Error_C (Integer(Line));
      Error_E ("");
   end Ghdl_Program_Error;

   procedure Ghdl_Bound_Check_Failed_L1 (Filename : Ghdl_C_String;
                                         Line: Ghdl_I32)
   is
   begin
      Error_C ("bound check failure at ");
      Error_C (Filename);
      Error_C (":");
      Error_C (Integer (Line));
      Error_E ("");
   end Ghdl_Bound_Check_Failed_L1;

   function Ghdl_Integer_Exp (V : Ghdl_I32; E : Ghdl_I32)
     return Ghdl_I32
   is
      pragma Suppress (Overflow_Check);

      R : Ghdl_I32;
      Res : Ghdl_I32;
      P : Ghdl_I32;
      T : Ghdl_I64;
   begin
      if E < 0 then
         Error ("negative exponent");
      end if;
      Res := 1;
      P := V;
      R := E;
      loop
         if R mod 2 = 1 then
            T := Ghdl_I64 (Res) * Ghdl_I64 (P);
            Res := Ghdl_I32 (T);
            if Ghdl_I64 (Res) /= T then
               Error ("overflow in exponentiation");
            end if;
         end if;
         R := R / 2;
         exit when R = 0;
         P := P * P;
      end loop;
      return Res;
   end Ghdl_Integer_Exp;

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

   function Ghdl_Get_Resolution_Limit return Std_Time is
   begin
      return 1;
   end Ghdl_Get_Resolution_Limit;

   procedure Ghdl_Control_Simulation
     (Stop : Ghdl_B1; Has_Status : Ghdl_B1; Status : Std_Integer) is
   begin
      Report_H;
      --  Report_C (Grt.Options.Progname);
      Report_C ("simulation ");
      if Stop then
         Report_C ("stopped");
      else
         Report_C ("finished");
      end if;
      Report_C (" @");
      Report_Now_C;
      if Has_Status then
         Report_C (" with status ");
         Report_C (Integer (Status));
      end if;
      Report_E ("");
      if Has_Status then
         Exit_Status := Integer (Status);
      end if;
      Exit_Simulation;
   end Ghdl_Control_Simulation;

end Grt.Lib;
