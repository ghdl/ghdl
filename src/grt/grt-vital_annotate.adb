--  GHDL Run Time (GRT) - VITAL annotator.
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
with Grt.Hooks; use Grt.Hooks;
with Grt.Astdio; use Grt.Astdio;
with Grt.Stdio; use Grt.Stdio;
with Grt.Options;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Avhpi_Utils; use Grt.Avhpi_Utils;
with Grt.Errors; use Grt.Errors;

package body Grt.Vital_Annotate is
   --  Point of the annotation.
   Sdf_Top : VhpiHandleT;

   --  Instance being annotated.
   Sdf_Inst : VhpiHandleT;

   Flag_Dump : Boolean := False;
   Flag_Verbose : constant Boolean := False;

   --  Note: RES may alias CUR.
   procedure Find_Instance (Cur : VhpiHandleT;
                            Res : out VhpiHandleT;
                            Name : String;
                            Ok : out Boolean)
   is
      Error : AvhpiErrorT;
      It : VhpiHandleT;
   begin
      Ok := False;
      Vhpi_Iterator (VhpiInternalRegions, Cur, It, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;
      loop
         Vhpi_Scan (It, Res, Error);
         exit when Error /= AvhpiErrorOk;
         if Name_Compare (Res, Name) then
            Ok := True;
            return;
         end if;
      end loop;
      return;
--       Put ("find instance: ");
--       Put (Name);
--       New_Line;
   end Find_Instance;

   procedure Find_Generic (Gen_Name : String;
                           Gen_Handle : out VhpiHandleT;
                           Port1_Name : String;
                           Port1_Handle : out VhpiHandleT;
                           Port2_Name : String;
                           Port2_Handle : out VhpiHandleT)
   is
      Error : AvhpiErrorT;
      It : VhpiHandleT;
      Decl : VhpiHandleT;
   begin
      Gen_Handle := Null_Handle;
      Port1_Handle := Null_Handle;
      Port2_Handle := Null_Handle;

      Vhpi_Iterator (VhpiDecls, Sdf_Inst, It, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;

      --  Look for the generic.
      loop
         Vhpi_Scan (It, Decl, Error);
         if Error /= AvhpiErrorOk then
            return;
         end if;
         exit when Vhpi_Get_Kind (Decl) /= VhpiGenericDeclK;
         if Name_Compare (Decl, Gen_Name) then
            Gen_Handle := Decl;
            exit;
         end if;
      end loop;

      --  Skip generics.
      while Vhpi_Get_Kind (Decl) = VhpiGenericDeclK loop
         Vhpi_Scan (It, Decl, Error);
         if Error /= AvhpiErrorOk then
            return;
         end if;
      end loop;

      --  Look for ports.
      loop
         exit when Vhpi_Get_Kind (Decl) /= VhpiPortDeclK;
         if Name_Compare (Decl, Port1_Name) then
            Port1_Handle := Decl;
            exit when Port2_Name'Length = 0;
         end if;
         if Port2_Name'Length > 0
           and then Name_Compare (Decl, Port2_Name)
         then
            Port2_Handle := Decl;
            exit when Vhpi_Get_Kind (Port1_Handle) /= VhpiUndefined;
         end if;
         Vhpi_Scan (It, Decl, Error);
         if Error /= AvhpiErrorOk then
            return;
         end if;
      end loop;

   end Find_Generic;

   procedure Sdf_Header (Context : Sdf_Context_Type)
   is
   begin
      if Flag_Dump then
         case Context.Version is
            when Sdf_2_1 =>
               Put ("found SDF file version 2.1");
            when Sdf_Version_Unknown =>
               Put ("found SDF file without version");
            when Sdf_Version_Bad =>
               Put ("found SDF file with unknown version");
         end case;
         New_Line;
      end if;
   end Sdf_Header;

   procedure Sdf_Celltype (Context : Sdf_Context_Type)
   is
   begin
      if Flag_Dump then
         Put ("celltype: ");
         Put (Context.Celltype (1 .. Context.Celltype_Len));
         New_Line;
         Put ("instance:");
         return;
      end if;
      Sdf_Inst := Sdf_Top;
   end Sdf_Celltype;

   procedure Sdf_Instance (Context : in out Sdf_Context_Type;
                           Instance : String;
                           Status : out Boolean)
   is
      pragma Unreferenced (Context);
      New_Inst : VhpiHandleT;
   begin
      if Flag_Dump then
         Put (' ');
         Put (Instance);
         Status := True;
         return;
      end if;

      Find_Instance (Sdf_Inst, New_Inst, Instance, Status);
      Sdf_Inst := New_Inst;
   end Sdf_Instance;

   procedure Sdf_Instance_End (Context : Sdf_Context_Type;
                               Status : out Boolean)
   is
   begin
      if Flag_Dump then
         Status := True;
         New_Line;
         return;
      end if;
      case Vhpi_Get_Kind (Sdf_Inst) is
         when VhpiRootInstK =>
            declare
               Hdl : VhpiHandleT;
            begin
               Hdl := Get_Root_Entity (Sdf_Inst);
               Status := Name_Compare
                 (Hdl, Context.Celltype (1 .. Context.Celltype_Len));
            end;
         when VhpiCompInstStmtK =>
            Status := Name_Compare
              (Sdf_Inst,
               Context.Celltype (1 .. Context.Celltype_Len),
               VhpiCompNameP);
         when others =>
            Status := False;
      end case;
   end Sdf_Instance_End;

   VitalDelayType01 : VhpiHandleT;
   VitalDelayType01Z : VhpiHandleT;
   VitalDelayType01ZX : VhpiHandleT;
   VitalDelayArrayType01 : VhpiHandleT;
   VitalDelayType : VhpiHandleT;
   VitalDelayArrayType : VhpiHandleT;

   type Map_Type is array (1 .. 12) of Natural;
   Map_1 : constant Map_Type := (1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
   Map_2 : constant Map_Type := (1, 2, 1, 1, 2, 2, 0, 0, 0, 0, 0, 0);
   Map_3 : constant Map_Type := (1, 2, 3, 1, 3, 2, 0, 0, 0, 0, 0, 0);
   Map_6 : constant Map_Type := (1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0);
   --Map_12 : constant Map_Type := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);

   function Write_Td_Delay_Generic (Context : Sdf_Context_Type;
                                    Gen : VhpiHandleT;
                                    Nbr : Natural;
                                    Map : Map_Type)
                                   return Boolean
   is
      It : VhpiHandleT;
      El : VhpiHandleT;
      Error : AvhpiErrorT;
      N : Natural;
   begin
      Vhpi_Iterator (VhpiIndexedNames, Gen, It, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("vhpiIndexedNames");
         return False;
      end if;
      for I in 1 .. Nbr loop
         Vhpi_Scan (It, El, Error);
         if Error /= AvhpiErrorOk then
            Internal_Error ("scan on vhpiIndexedNames");
            return False;
         end if;
         N := Map (I);
         if Context.Timing_Set (N) then
            if Vhpi_Put_Value (El, Context.Timing (N) * 1000) /= AvhpiErrorOk
            then
               Internal_Error ("vhpi_put_value");
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Write_Td_Delay_Generic;

   function Write_Td_Delay_Generic (Context : Sdf_Context_Type;
                                    Gen : VhpiHandleT)
                                   return Boolean
   is
      Gen_Basetype : VhpiHandleT;
      Error : AvhpiErrorT;
   begin
      Vhpi_Handle (VhpiBaseType, Gen, Gen_Basetype, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("write_td_delay_generic: vhpiBaseType");
         return False;
      end if;
      if Vhpi_Compare_Handles (Gen_Basetype, VitalDelayType01) then
         case Context.Timing_Nbr is
            when 1 =>
               return Write_Td_Delay_Generic (Context, Gen, 2, Map_1);
            when 2 =>
               return Write_Td_Delay_Generic (Context, Gen, 2, Map_2);
            when others =>
               Errors.Error
                 ("timing generic type mismatch SDF timing specification");
         end case;
      elsif Vhpi_Compare_Handles (Gen_Basetype, VitalDelayType01Z) then
         case Context.Timing_Nbr is
            when 1 =>
               return Write_Td_Delay_Generic (Context, Gen, 6, Map_1);
            when 2 =>
               return Write_Td_Delay_Generic (Context, Gen, 6, Map_2);
            when 3 =>
               return Write_Td_Delay_Generic (Context, Gen, 6, Map_3);
            when 6 =>
               return Write_Td_Delay_Generic (Context, Gen, 6, Map_6);
            when others =>
               Errors.Error
                 ("timing generic type mismatch SDF timing specification");
         end case;
      elsif Vhpi_Compare_Handles (Gen_Basetype, VitalDelayType) then
         if Vhpi_Put_Value (Gen, Context.Timing (1) * 1000) /= AvhpiErrorOk
         then
            Internal_Error ("vhpi_put_value (vitaldelaytype)");
         else
            return True;
         end if;
      else
         Internal_Error ("write_td_delay_generic: unhandled generic type");
      end if;
   end Write_Td_Delay_Generic;

   procedure Generic_Get_Bounds (Port : VhpiHandleT;
                                 Left : out Ghdl_I32;
                                 Len : out Ghdl_Index_Type;
                                 Up : out Boolean)
   is
      Port_Type, Port_Range : VhpiHandleT;
      Error : AvhpiErrorT;
      Right : VhpiIntT;
   begin
      Vhpi_Handle (VhpiSubtype, Port, Port_Type, Error);
      Left := 0;
      Len := 0;
      Up := True;
      if Error /= AvhpiErrorOk then
         Internal_Error ("vhpiSubtype - port");
         return;
      end if;
      Vhpi_Handle_By_Index (VhpiConstraints, Port_Type, 1, Port_Range, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("vhpiIndexConstraints - port");
         return;
      end if;
      Vhpi_Get (VhpiLeftBoundP, Port_Range, Left, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("vhpiLeftBoundP - port");
         return;
      end if;
      Vhpi_Get (VhpiRightBoundP, Port_Range, Right, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("vhpiRightBoundP - port");
         return;
      end if;
      Vhpi_Get (VhpiIsUpP, Port_Range, Up, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("vhpiIsUpP - port");
         return;
      end if;
      if Up then
         Len := Ghdl_Index_Type (Right - Left) + 1;
      else
         Len := Ghdl_Index_Type (Left - Right) + 1;
      end if;
   end Generic_Get_Bounds;

   procedure Sdf_Generic (Context : in out Sdf_Context_Type;
                          Name : String;
                          Ok : out Boolean)
   is
      Gen : VhpiHandleT;
      Gen_Basetype : VhpiHandleT;
      Port1, Port2 : VhpiHandleT;
      Error : AvhpiErrorT;
   begin
      if Flag_Dump then
         Put ("generic: ");
         Put (Name);
         if Context.Timing_Nbr = 0 then
            Put (' ');
            Put_I64 (stdout, Context.Timing (1));
         else
            for I in 1 .. 12 loop
               Put (' ');
               if Context.Timing_Set (I) then
                  Put_I64 (stdout, Context.Timing (I));
               else
                  Put ('?');
               end if;
            end loop;
         end if;

         New_Line;
         Ok := True;
         return;
      end if;

      Ok := False;

      if Context.Port_Num = 1 then
         Context.Ports (2).Name_Len := 0;
      end if;
      Find_Generic
        (Name, Gen,
         Context.Ports (1).Name (1 .. Context.Ports (1).Name_Len), Port1,
         Context.Ports (2).Name (1 .. Context.Ports (2).Name_Len), Port2);
      if Vhpi_Get_Kind (Gen) = VhpiUndefined
        or else Vhpi_Get_Kind (Port1) = VhpiUndefined
        or else (Context.Port_Num = 2
                 and then Vhpi_Get_Kind (Port2) = VhpiUndefined)
      then
         return;
      end if;

      --  Extract subtype.
      Vhpi_Handle (VhpiBaseType, Gen, Gen_Basetype, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("vhpiBaseType");
         return;
      end if;
      if Vhpi_Compare_Handles (Gen_Basetype, VitalDelayType01)
        or else Vhpi_Compare_Handles (Gen_Basetype, VitalDelayType01Z)
        or else Vhpi_Compare_Handles (Gen_Basetype, VitalDelayType01ZX)
        or else Vhpi_Compare_Handles (Gen_Basetype, VitalDelayType)
      then
         Ok := Write_Td_Delay_Generic (Context, Gen);
      elsif Vhpi_Compare_Handles (Gen_Basetype, VitalDelayArrayType01)
        or else Vhpi_Compare_Handles (Gen_Basetype, VitalDelayArrayType)
      then
         declare
            Left_Gen, Left1, Left2 : Ghdl_I32;
            Len_Gen, Len1, Len2 : Ghdl_Index_Type;
            Up_Gen, Up1, Up2 : Boolean;
            Pos : Ghdl_Index_Type;
            Gen_El : VhpiHandleT;
         begin
            Generic_Get_Bounds (Gen, Left_Gen, Len_Gen, Up_Gen);
            if Context.Port_Num >= 1
              and then Context.Ports (1).L /= Invalid_Dnumber
            then
               Generic_Get_Bounds (Port1, Left1, Len1, Up1);
               if Up1 then
                  Pos := Ghdl_Index_Type (Context.Ports (1).L - Left1);
               else
                  Pos := Ghdl_Index_Type (Left1 - Context.Ports (1).L);
               end if;
            else
               Pos := 0;
            end if;
            if Context.Port_Num >= 2
              and then Context.Ports (2).L /= Invalid_Dnumber
            then
               Generic_Get_Bounds (Port2, Left2, Len2, Up2);
               Pos := Pos * Len2;
               if Up2 then
                  Pos := Pos + Ghdl_Index_Type (Context.Ports (2).L - Left2);
               else
                  Pos := Pos + Ghdl_Index_Type (Left2 - Context.Ports (2).L);
               end if;
            end if;
            Vhpi_Handle_By_Index
              (VhpiIndexedNames, Gen, Integer (Pos), Gen_El, Error);
            if Error /= AvhpiErrorOk then
               Internal_Error ("vhpiIndexedNames - gen_el");
               return;
            end if;
            Ok := Write_Td_Delay_Generic (Context, Gen_El);
         end;
      else
         Errors.Error_S ("vital: unhandled generic type for generic ");
         Errors.Error_E (Name);
      end if;
   end Sdf_Generic;

   procedure Annotate (Arg : String)
   is
      S, E : Natural;
      Ok : Boolean;
      New_Top : VhpiHandleT;
   begin
      if Flag_Verbose then
         Put ("sdf annotate: ");
         Put (Arg);
         New_Line;
      end if;

      --  Find scope by name.
      Get_Root_Inst (Sdf_Top);
      E := Arg'First;
      S := E;
      L1: loop
         --  Skip path separator.
         while Arg (E) = '/' or Arg (E) = '.' loop
            E := E + 1;
            exit L1 when E > Arg'Last;
         end loop;

         exit L1 when E > Arg'Last or else Arg (E) = '=';

         --  Instance element.
         S := E;
         while Arg (E) /= '=' and Arg (E) /= '.' and Arg (E) /= '/' loop
            E := E + 1;
            exit L1 when E > Arg'Last;
         end loop;

         --  Path element.
         if E - 1 >= S then
            Find_Instance (Sdf_Top, New_Top, Arg (S .. E - 1), Ok);
            if not Ok then
               Error_S ("cannot find instance '");
               Diag_C (Arg (S .. E - 1));
               Error_E ("' for sdf annotation");
               return;
            end if;
            Sdf_Top := New_Top;
         end if;
      end loop L1;

      --  start annotation.
      if E >= Arg'Last or else Arg (E) /= '=' then
         Error_S ("no filename in sdf option '");
         Diag_C (Arg);
         Error_E ("'");
         return;
      end if;
      if not Sdf.Parse_Sdf_File (Arg (E + 1 .. Arg'Last)) then
         null;
      end if;
   end Annotate;

   procedure Extract_Vital_Delay_Type
   is
      It : VhpiHandleT;
      Pkg : VhpiHandleT;
      Decl : VhpiHandleT;
      Basetype : VhpiHandleT;
      Status : AvhpiErrorT;
   begin
      Get_Package_Inst (It);
      loop
         Vhpi_Scan (It, Pkg, Status);
         exit when Status /= AvhpiErrorOk;
         exit when Name_Compare (Pkg, "vital_timing")
           and then Name_Compare (Pkg, "ieee", VhpiLibLogicalNameP);
      end loop;
      if Status /= AvhpiErrorOk then
         Error ("package ieee.vital_timing not found, SDF annotation aborted");
         return;
      end if;
      Vhpi_Iterator (VhpiDecls, Pkg, It, Status);
      if Status /= AvhpiErrorOk then
         Error ("cannot iterate on vital_timing");
         return;
      end if;
      loop
         Vhpi_Scan (It, Decl, Status);
         exit when Status /= AvhpiErrorOk;
         if Vhpi_Get_Kind (Decl) = VhpiSubtypeDeclK
           or else Vhpi_Get_Kind (Decl) = VhpiArrayTypeDeclK
         then
            Vhpi_Handle (VhpiBaseType, Decl, Basetype, Status);
            if Status = AvhpiErrorOk then
               if Name_Compare (Decl, "vitaldelaytype01") then
                  VitalDelayType01 := Basetype;
               elsif Name_Compare (Decl, "vitaldelaytype01z") then
                  VitalDelayType01Z := Basetype;
               elsif Name_Compare (Decl, "vitaldelaytype01zx") then
                  VitalDelayType01ZX := Basetype;
               elsif Name_Compare (Decl, "vitaldelayarraytype01") then
                  VitalDelayArrayType01 := Basetype;
               elsif Name_Compare (Decl, "vitaldelaytype") then
                  VitalDelayType := Basetype;
               elsif Name_Compare (Decl, "vitaldelayarraytype") then
                  VitalDelayArrayType := Basetype;
               end if;
            end if;
         end if;
      end loop;
      if Vhpi_Get_Kind (VitalDelayType01) = VhpiUndefined then
         Error ("cannot find VitalDelayType01 in ieee.vital_timing");
         return;
      end if;
      if Vhpi_Get_Kind (VitalDelayType01Z) = VhpiUndefined then
         Error ("cannot find VitalDelayType01Z in ieee.vital_timing");
         return;
      end if;
      if Vhpi_Get_Kind (VitalDelayType01ZX) = VhpiUndefined then
         Error ("cannot find VitalDelayType01ZX in ieee.vital_timing");
         return;
      end if;
      if Vhpi_Get_Kind (VitalDelayArrayType01) = VhpiUndefined then
         Error ("cannot find VitalDelayArrayType01 in ieee.vital_timing");
         return;
      end if;
      if Vhpi_Get_Kind (VitalDelayType) = VhpiUndefined then
         Error ("cannot find VitalDelayType in ieee.vital_timing");
         return;
      end if;
   end Extract_Vital_Delay_Type;

   Has_Sdf_Option : Boolean := False;

   procedure Sdf_Start
   is
      use Grt.Options;
      Len : Integer;
      Beg : Integer;
      Arg : Ghdl_C_String;
   begin
      if not Has_Sdf_Option then
         --  Nothing to do.
         return;
      end if;
      Flag_Dump := False;

      --  Extract VitalDelayType(s) from VITAL_Timing package.
      Extract_Vital_Delay_Type;

      --  Annotate.
      for I in 1 .. Last_Opt loop
         Arg := Argv (I);
         Len := strlen (Arg);
         if Len > 5 and then Arg (1 .. 6) = "--sdf=" then
            Sdf_Mtm := Typical;
            Beg := 7;
            if Len > 10 then
               if Arg (7 .. 10) = "typ=" then
                  Beg := 11;
               elsif Arg (7 .. 10) = "min=" then
                  Sdf_Mtm := Minimum;
                  Beg := 11;
               elsif Arg (7 .. 10) = "max=" then
                  Sdf_Mtm := Maximum;
                  Beg := 11;
               end if;
            end if;
            Annotate (Arg (Beg .. Len));
         end if;
      end loop;
   end Sdf_Start;

   function Sdf_Option (Option : String) return Boolean
   is
      Opt : constant String (1 .. Option'Length) := Option;
   begin
      if Opt'Length > 11 and then Opt (1 .. 11) = "--sdf-dump=" then
         Flag_Dump := True;
         if Sdf.Parse_Sdf_File (Opt (12 .. Opt'Last)) then
            null;
         end if;
         return True;
      end if;
      if Opt'Length > 5 and then Opt (1 .. 6) = "--sdf=" then
         Has_Sdf_Option := True;
         return True;
      else
         return False;
      end if;
   end Sdf_Option;

   procedure Sdf_Help is
   begin
      Put_Line (" --sdf=[min=|typ=|max=]TOP=FILENAME");
      Put_Line ("    annotate TOP with SDF delay file FILENAME");
   end Sdf_Help;

   Sdf_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'
        ("sdf-annotate: annotate vital generics from an sdf file"),
      Option => Sdf_Option'Access,
      Help => Sdf_Help'Access,
      Init => Proc_Hook_Nil'Access,
      Start => Sdf_Start'Access,
      Finish => Proc_Hook_Nil'Access);

   procedure Register is
   begin
      Register_Hooks (Sdf_Hooks'Access);
   end Register;
end Grt.Vital_Annotate;
