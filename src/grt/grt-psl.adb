--  GHDL Run Time (GRT) - PSL report.
--  Copyright (C) 2016 Tristan Gingold
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

with System;
with Grt.Types; use Grt.Types;
with Grt.Stdio; use Grt.Stdio;
with Grt.Errors; use Grt.Errors;
with Grt.Astdio; use Grt.Astdio;
with Grt.Hooks; use Grt.Hooks;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;

package body Grt.Psl is
   --  Filename of the report. Last character is NUL.
   Report_Filename : String_Access;
   Report_Stream : FILEs;
   Is_First : Boolean := True;
   Nbr_Assert_Failed : Ghdl_U32 := 0;
   Nbr_Assert_Passed : Ghdl_U32 := 0;
   Nbr_Assume_Failed : Ghdl_U32 := 0;
   Nbr_Assume_Passed : Ghdl_U32 := 0;
   Nbr_Cover_Failed : Ghdl_U32 := 0;
   Nbr_Cover_Passed : Ghdl_U32 := 0;

   --  Return TRUE if OPT is an option for PSL.
   function Psl_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
   begin
      if Opt'Length > 13 and then Opt (F .. F + 12) = "--psl-report=" then
         Report_Filename := new String (1 .. Opt'Last - F - 13 + 1 + 1);
         Report_Filename (1 .. Opt'Last - F - 13 + 1) :=
           Opt (F + 13 .. Opt'Last);
         Report_Filename (Report_Filename'Last) := NUL;
         return True;
      else
         return False;
      end if;
   end Psl_Option;

   procedure Psl_Help is
   begin
      Put_Line (" --psl-report=FILE  report psl result at end of simulation");
   end Psl_Help;

   procedure Inc (V : in out Ghdl_U32) is
   begin
      V := V + 1;
   end Inc;

   function Process (Ctxt : Rti_Context; Rti : Ghdl_Rti_Access)
                    return Traverse_Result
   is
      F : constant FILEs := Report_Stream;
      Psl_Dir : Ghdl_Rtin_Psl_Directive_Acc;
      Addr : System.Address;
      Finished_Count : Ghdl_Index_Type;
      Started_Count : Ghdl_Index_Type;
   begin
      case Rti.Kind is
         when Ghdl_Rtiks_Psl =>
            null;
         when Ghdl_Rtik_Process =>
            return Traverse_Skip;
         when others =>
            return Traverse_Ok;
      end case;

      if Is_First then
         Is_First := False;
      else
         Put_Line (F, ",");
      end if;

      Put (F, " { ""directive"": ");
      case Ghdl_Rtiks_Psl (Rti.Kind) is
         when Ghdl_Rtik_Psl_Assert =>
            Put (F, """assertion""");
         when Ghdl_Rtik_Psl_Assume =>
            Put (F, """assumption""");
         when Ghdl_Rtik_Psl_Cover =>
            Put (F, """cover""");
      end case;
      Put_Line (F, ",");
      Put (F, "   ""name"": """);
      Psl_Dir := To_Ghdl_Rtin_Psl_Directive_Acc (Rti);
      Put (F, Ctxt);
      Put (F, '.');
      Put (F, Psl_Dir.Name);
      Put_Line (F, """,");

      Put (F, "   ""file"": """);
      Put (F, Get_Filename (Ctxt));
      Put_Line (F, """,");
      Put (F, "   ""line"": ");
      Put_U32 (F, Get_Linecol_Line (Psl_Dir.Linecol));
      Put_Line (F, ",");

      Put (F, "   ""finished-count"": ");
      Addr := Loc_To_Addr (Psl_Dir.Common.Depth, Psl_Dir.Loc, Ctxt);
      Finished_Count := To_Ghdl_Index_Ptr (Addr).all;
      Put_U32 (F, Ghdl_U32 (Finished_Count));
      Put_Line (F, ",");

      Put (F, "   ""started-count"": ");
      Addr := Loc_To_Addr (Psl_Dir.Common.Depth, Psl_Dir.Loc + 4, Ctxt);
      Started_Count := To_Ghdl_Index_Ptr (Addr).all;
      Put_U32 (F, Ghdl_U32 (Started_Count));
      Put_Line (F, ",");

      Put (F, "   ""status"": """);
      case Rti.Kind is
         when Ghdl_Rtik_Psl_Assert =>
            if Finished_Count = 0 then
               Put (F, "passed");
               Inc (Nbr_Assert_Passed);
            else
               Put (F, "failed");
               Inc (Nbr_Assert_Failed);
            end if;
         when Ghdl_Rtik_Psl_Assume =>
            if Finished_Count = 0 then
               Put (F, "passed");
               Inc (Nbr_Assume_Passed);
            else
               Put (F, "failed");
               Inc (Nbr_Assume_Failed);
            end if;
         when Ghdl_Rtik_Psl_Cover =>
            if Finished_Count = 0 then
               Put (F, "not covered");
               Inc (Nbr_Cover_Failed);
            else
               Put (F, "covered");
               Inc (Nbr_Cover_Passed);
            end if;
         when others =>
            raise Program_Error;
      end case;
      Put (F, """}");

      return Traverse_Ok;
   end Process;

   function Psl_Traverse_Blocks is new Traverse_Blocks (Process);

   --  Called at the end of the simulation.
   procedure Psl_End
   is
      Mode : constant String := "wt" & NUL;
      Status : Traverse_Result;
      F : FILEs;
   begin
      if Report_Filename = null then
         return;
      end if;

      F := fopen (Report_Filename.all'Address, Mode'Address);
      if F = NULL_Stream then
         Error_S ("cannot open ");
         Error_E (Report_Filename (Report_Filename'First
                                     .. Report_Filename'Last - 1));
         return;
      end if;

      Put_Line (F, "{ ""details"" : [");

      Report_Stream := F;
      Status := Psl_Traverse_Blocks (Get_Top_Context);
      pragma Assert (Status = Traverse_Ok or Status = Traverse_Skip);

      Put_Line (F, "],");
      Put_Line (F, " ""summary"" : {");

      Put (F, "  ""assert"": ");
      Put_U32 (F, Nbr_Assert_Failed + Nbr_Assert_Passed);
      Put_Line (F, ",");
      Put (F, "  ""assert-failure"": ");
      Put_U32 (F, Nbr_Assert_Failed);
      Put_Line (F, ",");
      Put (F, "  ""assert-pass"": ");
      Put_U32 (F, Nbr_Assert_Passed);
      Put_Line (F, ",");

      Put (F, "  ""assume"": ");
      Put_U32 (F, Nbr_Assume_Failed + Nbr_Assume_Passed);
      Put_Line (F, ",");
      Put (F, "  ""assume-failure"": ");
      Put_U32 (F, Nbr_Assume_Failed);
      Put_Line (F, ",");
      Put (F, "  ""assume-pass"": ");
      Put_U32 (F, Nbr_Assume_Passed);
      Put_Line (F, ",");

      Put (F, "  ""cover"": ");
      Put_U32 (F, Nbr_Cover_Failed + Nbr_Cover_Passed);
      Put_Line (F, ",");
      Put (F, "  ""cover-failure"": ");
      Put_U32 (F, Nbr_Cover_Failed);
      Put_Line (F, ",");
      Put (F, "  ""cover-pass"": ");
      Put_U32 (F, Nbr_Cover_Passed);

      Put_Line (F, "}");

      Put_Line (F, "}");
      fclose (F);
   end Psl_End;

   Psl_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("psl: display status of psl assertion and cover"),
      Option => Psl_Option'Access,
      Help => Psl_Help'Access,
      Init => null,
      Start => null,
      Finish => Psl_End'Access);

   procedure Register is
   begin
      Register_Hooks (Psl_Hooks'Access);
   end Register;
end Grt.Psl;
