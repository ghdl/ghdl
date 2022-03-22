--  GHDL Run Time (GRT) - RTI dumper.
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

with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Astdio; use Grt.Astdio;
with Grt.Astdio.Vhdl; use Grt.Astdio.Vhdl;
with Grt.Errors; use Grt.Errors;
with Grt.Hooks; use Grt.Hooks;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;
with Grt.Signals;

package body Grt.Disp_Rti is
   procedure Disp_Kind (Kind : Ghdl_Rtik);

   procedure Disp_Name (Name : Ghdl_C_String) is
   begin
      if Name = null then
         Put (stdout, "<anonymous>");
      else
         Put (stdout, Name);
      end if;
   end Disp_Name;

   --  Disp value stored at ADDR and whose type is described by RTI.
   procedure Disp_Enum_Value
     (Stream : FILEs; Rti : Ghdl_Rti_Access; Val : Ghdl_Index_Type)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
   begin
      Put (Stream, Enum_Rti.Names (Val));
   end Disp_Enum_Value;

   procedure Peek_Value_And_Update (Rti : Ghdl_Rti_Access;
                                    Val : out Ghdl_Value_Ptr;
                                    Addr : in out Address;
                                    Is_Sig : Boolean)
   is
      Sz : Ghdl_Index_Type;
   begin
      if Is_Sig then
         --  ADDR is the address of the object.
         --  The object contains a pointer to the signal.
         --  The first field of the signal is a pointer to the value.
         Val := Grt.Signals.To_Ghdl_Signal_Ptr
           (To_Addr_Acc (Addr).all).Value_Ptr;
         Sz := Address'Size / Storage_Unit;
      else
         Val := To_Ghdl_Value_Ptr (Addr);
         case Rti.Kind is
            when Ghdl_Rtik_Type_E8
              | Ghdl_Rtik_Type_B1 =>
               Sz := 1;
            when Ghdl_Rtik_Type_I32
              | Ghdl_Rtik_Type_E32
              | Ghdl_Rtik_Type_P32 =>
               Sz := 4;
            when Ghdl_Rtik_Type_F64
              | Ghdl_Rtik_Type_P64 =>
               Sz := 8;
            when others =>
               Internal_Error ("disp_rti.peek_value_and_update");
         end case;
      end if;
      Addr := Addr + Sz;
   end Peek_Value_And_Update;

   procedure Disp_Scalar_Value (Stream : FILEs;
                                Rti : Ghdl_Rti_Access;
                                Addr : in out Address;
                                Is_Sig : Boolean)
   is
      Vptr : Ghdl_Value_Ptr;
   begin
      Peek_Value_And_Update (Rti, Vptr, Addr, Is_Sig);

      case Rti.Kind is
         when Ghdl_Rtik_Type_I32 =>
            Put_I32 (Stream, Vptr.I32);
         when Ghdl_Rtik_Type_E8 =>
            Disp_Enum_Value (Stream, Rti, Ghdl_Index_Type (Vptr.E8));
         when Ghdl_Rtik_Type_E32 =>
            Disp_Enum_Value (Stream, Rti, Ghdl_Index_Type (Vptr.E32));
         when Ghdl_Rtik_Type_B1 =>
            Disp_Enum_Value (Stream, Rti,
                             Ghdl_Index_Type (Ghdl_B1'Pos (Vptr.B1)));
         when Ghdl_Rtik_Type_F64 =>
            Put_F64 (Stream, Vptr.F64);
         when Ghdl_Rtik_Type_P64 =>
            Put_I64 (Stream, Vptr.I64);
            Put (Stream, " ");
            Put (Stream,
                 Get_Physical_Unit_Name
                   (To_Ghdl_Rtin_Type_Physical_Acc (Rti).Units (0)));
         when Ghdl_Rtik_Type_P32 =>
            Put_I32 (Stream, Vptr.I32);
            Put (Stream, " ");
            Put (Stream,
                 Get_Physical_Unit_Name
                   (To_Ghdl_Rtin_Type_Physical_Acc (Rti).Units (0)));
         when others =>
            Internal_Error ("disp_rti.disp_scalar_value");
      end case;
   end Disp_Scalar_Value;

   procedure Disp_Array_As_String (Stream : FILEs;
                                   El_Rti : Ghdl_Rti_Access;
                                   Length : Ghdl_Index_Type;
                                   Obj : in out Address;
                                   Is_Sig : Boolean)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (El_Rti);
      Name : Ghdl_C_String;

      In_String : Boolean;
      Val : Ghdl_Value_Ptr;
   begin
      In_String := False;

      for I in 1 .. Length loop
         Peek_Value_And_Update (El_Rti, Val, Obj, Is_Sig);
         case El_Rti.Kind is
            when Ghdl_Rtik_Type_E8 =>
               Name := Enum_Rti.Names (Ghdl_Index_Type (Val.E8));
            when Ghdl_Rtik_Type_B1 =>
               Name := Enum_Rti.Names (Ghdl_B1'Pos (Val.B1));
            when others =>
               Internal_Error ("disp_rti.disp_array_as_string");
         end case;
         if Name (1) = ''' then
            --  A character.
            if not In_String then
               if I /= 1 then
                  Put (Stream, " & ");
               end if;
               Put (Stream, '"');
               In_String := True;
            end if;
            Put (Stream, Name (2));
         else
            if In_String then
               Put (Stream, '"');
               In_String := False;
            end if;
            if I /= 1 then
               Put (Stream, " & ");
            end if;
            Put (Stream, Name);
         end if;
      end loop;
      if In_String then
         Put (Stream, '"');
      end if;
   end Disp_Array_As_String;

--    function Get_Scalar_Type_Kind (Rti : Ghdl_Rti_Access) return Ghdl_Rtik
--    is
--       Ndef : Ghdl_Rti_Access;
--    begin
--       if Rti.Kind = Ghdl_Rtik_Subtype_Scalar then
--          Ndef := To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti).Basetype;
--       else
--          Ndef := Rti;
--       end if;
--       case Ndef.Kind is
--          when Ghdl_Rtik_Type_I32 =>
--             return Ndef.Kind;
--          when others =>
--             return Ghdl_Rtik_Error;
--       end case;
--    end Get_Scalar_Type_Kind;

   procedure Disp_Array_Value_1 (Stream : FILEs;
                                 Arr_Rti : Ghdl_Rtin_Type_Array_Acc;
                                 Ctxt : Rti_Context;
                                 Index : Ghdl_Index_Type;
                                 Obj : in out Address;
                                 Bounds : in out Address;
                                 Is_Sig : Boolean)
   is
      El_Rti : constant Ghdl_Rti_Access := Arr_Rti.Element;
      Idx_Rti : constant Ghdl_Rti_Access :=
        Get_Base_Type (Arr_Rti.Indexes (Index));
      Last_Idx : constant Ghdl_Index_Type := Arr_Rti.Nbr_Dim - 1;
      Rng : Ghdl_Range_Ptr;
      Length : Ghdl_Index_Type;
      Bounds2 : Address;
   begin
      Extract_Range (Bounds, Idx_Rti, Rng);
      Length := Range_To_Length (Rng, Idx_Rti);

      if Index = Last_Idx
        and then (El_Rti.Kind = Ghdl_Rtik_Type_B1
                    or else El_Rti.Kind = Ghdl_Rtik_Type_E8)
      then
         --  Disp as string.
         Disp_Array_As_String (Stream, El_Rti, Length, Obj, Is_Sig);
         return;
      end if;

      Put (Stream, "(");
      if Length = 0 then
         Put (Stream, "<>");
         --  FIXME: need to update bounds.
      else
         for I in 1 .. Length loop
            if I /= 1 then
               Put (Stream, ", ");
            end if;
            if Index = Last_Idx then
               Bounds2 := Array_Layout_To_Element (Bounds, El_Rti);
               Disp_Value (Stream, El_Rti, Ctxt, Obj, Bounds2, Is_Sig);
            else
               Bounds2 := Bounds;
               Disp_Array_Value_1
                 (Stream, Arr_Rti, Ctxt, Index + 1, Obj, Bounds2, Is_Sig);
            end if;
         end loop;
         Bounds := Bounds2;
      end if;
      Put (Stream, ")");
   end Disp_Array_Value_1;

   procedure Disp_Record_Value (Stream : FILEs;
                                Rti : Ghdl_Rtin_Type_Record_Acc;
                                Ctxt : Rti_Context;
                                Obj : Address;
                                Obj_Layout : Address;
                                Is_Sig : Boolean)
   is
      El : Ghdl_Rtin_Element_Acc;
      El_Addr : Address;
      El_Bounds : Address;
   begin
      Put (Stream, "(");
      for I in 1 .. Rti.Nbrel loop
         El := To_Ghdl_Rtin_Element_Acc (Rti.Elements (I - 1));
         if I /= 1 then
            Put (", ");
         end if;
         Put (Stream, El.Name);
         Put (" => ");
         Record_To_Element
           (Obj, El, Is_Sig, Obj_Layout, El_Addr, El_Bounds);
         Disp_Value (Stream, El.Eltype, Ctxt, El_Addr, El_Bounds, Is_Sig);
      end loop;
      Put (")");
      --  FIXME: update ADDR.
   end Disp_Record_Value;

   procedure Disp_Value (Stream : FILEs;
                         Rti : Ghdl_Rti_Access;
                         Ctxt : Rti_Context;
                         Obj : in out Address;
                         Bounds : in out Address;
                         Is_Sig : Boolean)
   is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            Disp_Scalar_Value
              (Stream, To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti).Basetype,
               Obj, Is_Sig);
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B1 =>
            Disp_Scalar_Value (Stream, Rti, Obj, Is_Sig);
         when Ghdl_Rtik_Type_Array =>
            Disp_Array_Value_1
              (Stream, To_Ghdl_Rtin_Type_Array_Acc (Rti), Ctxt, 0,
               Obj, Bounds, Is_Sig);
         when Ghdl_Rtik_Subtype_Unbounded_Array =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
               Bt : constant Ghdl_Rtin_Type_Array_Acc :=
                 To_Ghdl_Rtin_Type_Array_Acc (St.Basetype);
            begin
               Disp_Array_Value_1 (Stream, Bt, Ctxt, 0, Obj, Bounds, Is_Sig);
            end;
         when Ghdl_Rtik_Subtype_Array =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
               Bt : constant Ghdl_Rtin_Type_Array_Acc :=
                 To_Ghdl_Rtin_Type_Array_Acc (St.Basetype);
               Layout : Address;
               Bounds : Address;
            begin
               Layout := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
               Bounds := Array_Layout_To_Bounds (Layout);
               Disp_Array_Value_1 (Stream, Bt, Ctxt, 0, Obj, Bounds, Is_Sig);
            end;
         when Ghdl_Rtik_Type_File =>
            declare
               Vptr : Ghdl_Value_Ptr;
            begin
               Vptr := To_Ghdl_Value_Ptr (Obj);
               Put (Stream, "File#");
               Put_I32 (Stream, Vptr.I32);
               --  FIXME: update OBJ (not very useful since never in a
               --   composite type).
            end;
         when Ghdl_Rtik_Type_Record =>
            declare
               Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Rti);
               Rec_Layout : Address;
            begin
               if Rti_Complex_Type (Rti) then
                  Rec_Layout := Loc_To_Addr (Bt.Common.Depth, Bt.Layout, Ctxt);
               else
                  Rec_Layout := Bounds;
               end if;
               Disp_Record_Value (Stream, Bt, Ctxt, Obj, Rec_Layout, Is_Sig);
            end;
         when Ghdl_Rtik_Type_Unbounded_Record =>
            declare
               Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Rti);
            begin
               Disp_Record_Value (Stream, Bt, Ctxt, Obj, Bounds, Is_Sig);
            end;
         when Ghdl_Rtik_Subtype_Unbounded_Record =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
               Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (St.Basetype);
            begin
               Disp_Record_Value (Stream, Bt, Ctxt, Obj, Bounds, Is_Sig);
            end;
         when Ghdl_Rtik_Subtype_Record =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
               Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (St.Basetype);
               Layout : Address;
            begin
               Layout := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
               Disp_Record_Value (Stream, Bt, Ctxt, Obj, Layout, Is_Sig);
            end;
         when Ghdl_Rtik_Type_Protected =>
            Put (Stream, "Unhandled protected type");
         when others =>
            Put (Stream, "Unknown Rti Kind : ");
            Disp_Kind(Rti.Kind);
      end case;
      --  Put_Line(":");
   end Disp_Value;

   procedure Disp_Kind (Kind : Ghdl_Rtik) is
   begin
      case Kind is
         when Ghdl_Rtik_Top =>
            Put ("ghdl_rtik_top");
         when Ghdl_Rtik_Package =>
            Put ("ghdl_rtik_package");
         when Ghdl_Rtik_Package_Body =>
            Put ("ghdl_rtik_package_body");
         when Ghdl_Rtik_Entity =>
            Put ("ghdl_rtik_entity");
         when Ghdl_Rtik_Architecture =>
            Put ("ghdl_rtik_architecture");

         when Ghdl_Rtik_Process =>
            Put ("ghdl_rtik_process");
         when Ghdl_Rtik_Component =>
            Put ("ghdl_rtik_component");
         when Ghdl_Rtik_Attribute =>
            Put ("ghdl_rtik_attribute");

         when Ghdl_Rtik_Attribute_Quiet =>
            Put ("ghdl_rtik_attribute_quiet");
         when Ghdl_Rtik_Attribute_Stable =>
            Put ("ghdl_rtik_attribute_stable");
         when Ghdl_Rtik_Attribute_Transaction =>
            Put ("ghdl_rtik_attribute_transaction");

         when Ghdl_Rtik_Constant =>
            Put ("ghdl_rtik_constant");
         when Ghdl_Rtik_Iterator =>
            Put ("ghdl_rtik_iterator");
         when Ghdl_Rtik_Signal =>
            Put ("ghdl_rtik_signal");
         when Ghdl_Rtik_Variable =>
            Put ("ghdl_rtik_variable");
         when Ghdl_Rtik_Guard =>
            Put ("ghdl_rtik_guard");
         when Ghdl_Rtik_File =>
            Put ("ghdl_rtik_file");
         when Ghdl_Rtik_Port =>
            Put ("ghdl_rtik_port");
         when Ghdl_Rtik_Generic =>
            Put ("ghdl_rtik_generic");
         when Ghdl_Rtik_Alias =>
            Put ("ghdl_rtik_alias");

         when Ghdl_Rtik_Instance =>
            Put ("ghdl_rtik_instance");
         when Ghdl_Rtik_Block =>
            Put ("ghdl_rtik_block");
         when Ghdl_Rtik_If_Generate =>
            Put ("ghdl_rtik_if_generate");
         when Ghdl_Rtik_Case_Generate =>
            Put ("ghdl_rtik_case_generate");
         when Ghdl_Rtik_For_Generate =>
            Put ("ghdl_rtik_for_generate");
         when Ghdl_Rtik_Generate_Body =>
            Put ("ghdl_rtik_generate_body");

         when Ghdl_Rtik_Type_B1 =>
            Put ("ghdl_rtik_type_b1");
         when Ghdl_Rtik_Type_E8 =>
            Put ("ghdl_rtik_type_e8");
         when Ghdl_Rtik_Type_E32 =>
            Put ("ghdl_rtik_type_e32");
         when Ghdl_Rtik_Type_P64 =>
            Put ("ghdl_rtik_type_p64");
         when Ghdl_Rtik_Type_I32 =>
            Put ("ghdl_rtik_type_i32");

         when Ghdl_Rtik_Type_Array =>
            Put ("ghdl_rtik_type_array");
         when Ghdl_Rtik_Subtype_Array =>
            Put ("ghdl_rtik_subtype_array");
         when Ghdl_Rtik_Subtype_Unbounded_Array =>
            Put ("ghdl_rtik_subtype_unbounded_array");

         when Ghdl_Rtik_Type_Record =>
            Put ("ghdl_rtik_type_record");
         when Ghdl_Rtik_Type_Unbounded_Record =>
            Put ("ghdl_rtik_type_unbounded_record");
         when Ghdl_Rtik_Subtype_Unbounded_Record =>
            Put ("ghdl_rtik_subtype_unbounded_record");
         when Ghdl_Rtik_Subtype_Record =>
            Put ("ghdl_rtik_subtype_record");

         when Ghdl_Rtik_Type_Access =>
            Put ("ghdl_rtik_type_access");
         when Ghdl_Rtik_Type_File =>
            Put ("ghdl_rtik_type_file");
         when Ghdl_Rtik_Type_Protected =>
            Put ("ghdl_rtik_type_protected");

         when Ghdl_Rtik_Subtype_Scalar =>
            Put ("ghdl_rtik_subtype_scalar");

         when Ghdl_Rtik_Element =>
            Put ("ghdl_rtik_element");
         when Ghdl_Rtik_Unit64 =>
            Put ("ghdl_rtik_unit64");
         when Ghdl_Rtik_Unitptr =>
            Put ("ghdl_rtik_unitptr");

         when Ghdl_Rtik_Psl_Assert =>
            Put ("ghdl_rtik_psl_assert");
         when Ghdl_Rtik_Psl_Assume =>
            Put ("ghdl_rtik_psl_assume");
         when Ghdl_Rtik_Psl_Cover =>
            Put ("ghdl_rtik_psl_cover");
         when Ghdl_Rtik_Psl_Endpoint =>
            Put ("ghdl_rtik_psl_endpoint");

         when others =>
            --  Should never happen, except when not synchronized.
            Put ("ghdl_rtik_#");
            Put_I32 (stdout, Ghdl_Rtik'Pos (Kind));
      end case;
   end Disp_Kind;

   procedure Disp_Depth (Depth : Ghdl_Rti_Depth) is
   begin
      Put (", D=");
      Put_I32 (stdout, Ghdl_I32 (Depth));
   end Disp_Depth;

   procedure Disp_Indent (Indent : Natural) is
   begin
      for I in 1 .. Indent loop
         Put (' ');
      end loop;
   end Disp_Indent;

   --  Disp a subtype_indication.
   --  OBJ may be necessary when the subtype is an unconstrained array type,
   --  whose bounds are stored with the object.
   procedure Disp_Subtype_Indication
     (Def : Ghdl_Rti_Access; Ctxt : Rti_Context; Obj : Address);

   procedure Disp_Range
     (Stream : FILEs; Def : Ghdl_Rti_Access; Rng : Ghdl_Range_Ptr) is
   begin
      case Def.Kind is
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_P32 =>
            Put_I32 (Stream, Rng.I32.Left);
            Put_Dir (Stream, Rng.I32.Dir);
            Put_I32 (Stream, Rng.I32.Right);
         when Ghdl_Rtik_Type_F64 =>
            Put_F64 (Stream, Rng.F64.Left);
            Put_Dir (Stream, Rng.F64.Dir);
            Put_F64 (Stream, Rng.F64.Right);
         when Ghdl_Rtik_Type_P64 =>
            Put_I64 (Stream, Rng.P64.Left);
            Put_Dir (Stream, Rng.P64.Dir);
            Put_I64 (Stream, Rng.P64.Right);
         when Ghdl_Rtik_Type_B1 =>
            declare
               Enum : constant Ghdl_Rtin_Type_Enum_Acc :=
                 To_Ghdl_Rtin_Type_Enum_Acc (Def);
            begin
               Disp_Name (Enum.Names (Ghdl_B1'Pos (Rng.B1.Left)));
               Put_Dir (Stream, Rng.B1.Dir);
               Disp_Name (Enum.Names (Ghdl_B1'Pos (Rng.B1.Right)));
            end;
         when Ghdl_Rtik_Type_E8 =>
            declare
               Enum : constant Ghdl_Rtin_Type_Enum_Acc :=
                 To_Ghdl_Rtin_Type_Enum_Acc (Def);
            begin
               Disp_Name (Enum.Names (Ghdl_E8'Pos (Rng.E8.Left)));
               Put_Dir (Stream, Rng.E8.Dir);
               Disp_Name (Enum.Names (Ghdl_E8'Pos (Rng.E8.Right)));
            end;
         when others =>
            Put ("?Scal");
      end case;
   end Disp_Range;

   procedure Disp_Scalar_Type_Name (Def : Ghdl_Rti_Access) is
   begin
      case Def.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Rti : Ghdl_Rtin_Subtype_Scalar_Acc;
            begin
               Rti := To_Ghdl_Rtin_Subtype_Scalar_Acc (Def);
               if Rti.Name /= null then
                  Disp_Name (Rti.Name);
               else
                  Disp_Scalar_Type_Name (Rti.Basetype);
               end if;
            end;
         when Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            Disp_Name (To_Ghdl_Rtin_Type_Enum_Acc (Def).Name);
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64 =>
            Disp_Name (To_Ghdl_Rtin_Type_Scalar_Acc (Def).Name);
         when others =>
            Put ("#disp_scalar_type_name#");
      end case;
   end Disp_Scalar_Type_Name;

   procedure Disp_Type_Composite_Bounds
     (Def : Ghdl_Rti_Access; Bounds : Address);

   procedure Disp_Type_Array_Bounds (Def : Ghdl_Rtin_Type_Array_Acc;
                                     Bounds : Address)
   is
      Rng : Ghdl_Range_Ptr;
      Idx_Base : Ghdl_Rti_Access;
      Bounds1 : Address;
      El_Type : Ghdl_Rti_Access;
   begin
      Bounds1 := Bounds;
      Put (" (");
      for I in 0 .. Def.Nbr_Dim - 1 loop
         if I /= 0 then
            Put (", ");
         end if;
         if Boolean'(False) then
            Disp_Scalar_Type_Name (Def.Indexes (I));
            Put (" range ");
         end if;
         Idx_Base := Get_Base_Type (Def.Indexes (I));
         Extract_Range (Bounds1, Idx_Base, Rng);
         Disp_Range (stdout, Idx_Base, Rng);
      end loop;
      Put (")");
      El_Type := Def.Element;
      if Is_Unbounded (El_Type) then
         Disp_Type_Composite_Bounds (El_Type, Bounds1);
      end if;
   end Disp_Type_Array_Bounds;

   procedure Disp_Type_Record_Bounds (Def : Ghdl_Rtin_Type_Record_Acc;
                                      Layout : Address)
   is
      El : Ghdl_Rtin_Element_Acc;
      El_Layout : Address;
      El_Type : Ghdl_Rti_Access;
      First : Boolean;
   begin
      Put (" (");
      First := True;
      for I in 1 .. Def.Nbrel loop
         El := To_Ghdl_Rtin_Element_Acc (Def.Elements (I - 1));
         El_Type := El.Eltype;
         if Is_Unbounded (El_Type) then
            if First then
               First := False;
            else
               Put (", ");
            end if;
            Put (El.Name);
            El_Layout := Layout + El.Layout_Off;
            Disp_Type_Composite_Bounds (El_Type, El_Layout);
         end if;
      end loop;
      Put (")");
   end Disp_Type_Record_Bounds;


   procedure Disp_Type_Composite_Bounds
     (Def : Ghdl_Rti_Access; Bounds : Address)
   is
      El_Type : constant Ghdl_Rti_Access := Get_Base_Type (Def);
   begin
      case El_Type.Kind is
         when Ghdl_Rtik_Type_Array =>
            Disp_Type_Array_Bounds
              (To_Ghdl_Rtin_Type_Array_Acc (El_Type),
               Array_Layout_To_Bounds (Bounds));
         when Ghdl_Rtik_Type_Unbounded_Record =>
            Disp_Type_Record_Bounds
              (To_Ghdl_Rtin_Type_Record_Acc (El_Type), Bounds);
         when others =>
            raise Program_Error;
      end case;
   end Disp_Type_Composite_Bounds;

   procedure Disp_Type_Array_Name (Def : Ghdl_Rtin_Type_Array_Acc;
                                   Bounds_Ptr : Address)
   is
      Bounds : Address;
   begin
      Disp_Name (Def.Name);
      if Bounds_Ptr = Null_Address then
         return;
      end if;
      Bounds := Bounds_Ptr;
      Disp_Type_Array_Bounds (Def, Bounds);
   end Disp_Type_Array_Name;

   procedure Disp_Type_Record_Name (Def : Ghdl_Rtin_Type_Record_Acc;
                                    Layout_Ptr : Address)
   is
      Layout : Address;
   begin
      Disp_Name (Def.Name);
      if Layout_Ptr = Null_Address then
         return;
      end if;
      Layout := Layout_Ptr;
      Disp_Type_Record_Bounds (Def, Layout);
   end Disp_Type_Record_Name;

   procedure Disp_Subtype_Scalar_Range
     (Stream : FILEs; Def : Ghdl_Rtin_Subtype_Scalar_Acc; Ctxt : Rti_Context)
   is
      Range_Addr : Address;
      Rng : Ghdl_Range_Ptr;
   begin
      Range_Addr := Loc_To_Addr (Def.Common.Depth,
                                 Def.Range_Loc, Ctxt);
      Rng := To_Ghdl_Range_Ptr (Range_Addr);
      Disp_Range (Stream, Def.Basetype, Rng);
   end Disp_Subtype_Scalar_Range;

   procedure Disp_Subtype_Indication
     (Def : Ghdl_Rti_Access; Ctxt : Rti_Context; Obj : Address)
   is
   begin
      case Def.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Rti : Ghdl_Rtin_Subtype_Scalar_Acc;
            begin
               Rti := To_Ghdl_Rtin_Subtype_Scalar_Acc (Def);
               if Rti.Name /= null then
                  Disp_Name (Rti.Name);
               else
                  Disp_Subtype_Indication
                    (Rti.Basetype, Null_Context, Null_Address);
                  Put (" range ");
                  Disp_Subtype_Scalar_Range (stdout, Rti, Ctxt);
               end if;
            end;
            --Disp_Scalar_Subtype_Name (To_Ghdl_Rtin_Scalsubtype_Acc (Def),
            --                          Base);
         when Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            Disp_Name (To_Ghdl_Rtin_Type_Enum_Acc (Def).Name);
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64 =>
            Disp_Name (To_Ghdl_Rtin_Type_Scalar_Acc (Def).Name);
         when Ghdl_Rtik_Type_File
           | Ghdl_Rtik_Type_Access =>
            Disp_Name (To_Ghdl_Rtin_Type_Fileacc_Acc (Def).Name);
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Type_Unbounded_Record =>
            Disp_Name (To_Ghdl_Rtin_Type_Record_Acc (Def).Name);
         when Ghdl_Rtik_Subtype_Record =>
            declare
               Sdef : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Def);
            begin
               if Sdef.Name /= null then
                  Disp_Name (Sdef.Name);
               else
                  Disp_Type_Record_Name
                    (To_Ghdl_Rtin_Type_Record_Acc (Sdef.Basetype),
                     Loc_To_Addr (Sdef.Common.Depth, Sdef.Layout, Ctxt));
               end if;
            end;
         when Ghdl_Rtik_Type_Array =>
            declare
               Bounds : Address;
            begin
               if Obj = Null_Address then
                  Bounds := Null_Address;
               else
                  Bounds := To_Ghdl_Uc_Array_Acc (Obj).Bounds;
               end if;
               Disp_Type_Array_Name (To_Ghdl_Rtin_Type_Array_Acc (Def),
                                     Bounds);
            end;
         when Ghdl_Rtik_Subtype_Array =>
            declare
               Sdef : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Def);
               Layout : Address;
            begin
               if Sdef.Name /= null then
                  Disp_Name (Sdef.Name);
               else
                  Layout := Loc_To_Addr (Sdef.Common.Depth, Sdef.Layout, Ctxt);
                  Disp_Type_Array_Name
                    (To_Ghdl_Rtin_Type_Array_Acc (Sdef.Basetype),
                     Array_Layout_To_Bounds (Layout));
               end if;
            end;
         when Ghdl_Rtik_Subtype_Unbounded_Array =>
            declare
               Sdef : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Def);
            begin
               if Sdef.Name /= null then
                  Disp_Name (Sdef.Name);
               else
                  Put ("?sub-arr?");
               end if;
            end;
         when Ghdl_Rtik_Type_Protected =>
            Disp_Name (To_Ghdl_Rtin_Type_Scalar_Acc (Def).Name);
         when others =>
            Disp_Kind (Def.Kind);
            Put (' ');
      end case;
   end Disp_Subtype_Indication;

   procedure Disp_Linecol (Linecol : Ghdl_Index_Type) is
   begin
      Put ("sloc=");
      Put_U32 (stdout, Get_Linecol_Line (Linecol));
      Put (":");
      Put_U32 (stdout, Get_Linecol_Col (Linecol));
   end Disp_Linecol;

   procedure Disp_Rti (Rti : Ghdl_Rti_Access;
                       Ctxt : Rti_Context;
                       Indent : Natural);

   procedure Disp_Rti_Arr (Nbr : Ghdl_Index_Type;
                           Arr : Ghdl_Rti_Arr_Acc;
                           Ctxt : Rti_Context;
                           Indent : Natural)
   is
   begin
      for I in 1 .. Nbr loop
         Disp_Rti (Arr (I - 1), Ctxt, Indent);
      end loop;
   end Disp_Rti_Arr;

   procedure Disp_Block (Blk : Ghdl_Rtin_Block_Acc;
                         Ctxt : Rti_Context;
                         Indent : Natural)
   is
      Nctxt : Rti_Context;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Blk.Common.Kind);
      Disp_Depth (Blk.Common.Depth);
      Put (", ");
      Disp_Linecol (Blk.Linecol);
      Put (": ");
      Disp_Name (Blk.Name);
      New_Line;
      case Blk.Common.Kind is
         when Ghdl_Rtik_Package
           | Ghdl_Rtik_Package_Body
           | Ghdl_Rtik_Entity
           | Ghdl_Rtik_Architecture =>
            Disp_Indent (Indent);
            Put (" filename: ");
            Disp_Name (To_Ghdl_Rtin_Block_Filename_Acc
                         (To_Ghdl_Rti_Access (Blk)).Filename);
            New_Line;
         when others =>
            null;
      end case;
      if Blk.Parent /= null then
         case Blk.Common.Kind is
            when Ghdl_Rtik_Architecture =>
               --  Disp entity.
               Disp_Rti (Blk.Parent, Ctxt, Indent + 1);
            when others =>
               null;
         end case;
      end if;
      case Blk.Common.Kind is
         when Ghdl_Rtik_Package
           | Ghdl_Rtik_Package_Body
           | Ghdl_Rtik_Entity
           | Ghdl_Rtik_Architecture
           | Ghdl_Rtik_Block
           | Ghdl_Rtik_Process =>
            Nctxt := (Base => Ctxt.Base + Blk.Loc,
                      Block => To_Ghdl_Rti_Access (Blk));
            Disp_Rti_Arr (Blk.Nbr_Child, Blk.Children,
                          Nctxt, Indent + 1);
         when Ghdl_Rtik_Generate_Body =>
            Disp_Rti_Arr (Blk.Nbr_Child, Blk.Children,
                          Ctxt, Indent + 1);
         when Ghdl_Rtik_If_Generate
           | Ghdl_Rtik_Case_Generate =>
            Nctxt := Get_If_Case_Generate_Child
              (Ctxt, To_Ghdl_Rti_Access (Blk));
            if Nctxt /= Null_Context then
               --  There might be no blocks.
               Disp_Block
                 (To_Ghdl_Rtin_Block_Acc (Nctxt.Block), Nctxt, Indent + 1);
            end if;
         when others =>
            Internal_Error ("disp_block");
      end case;
   end Disp_Block;

   procedure Disp_For_Generate (Gen : Ghdl_Rtin_Generate_Acc;
                                Ctxt : Rti_Context;
                                Indent : Natural)
   is
      Nctxt : Rti_Context;
      Length : Ghdl_Index_Type;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Gen.Common.Kind);
      Disp_Depth (Gen.Common.Depth);
      Put (", ");
      Disp_Linecol (Gen.Linecol);
      Put (": ");
      Disp_Name (Gen.Name);
      New_Line;

      Nctxt := (Base => To_Addr_Acc (Ctxt.Base + Gen.Loc).all,
                Block => Gen.Child);
      Length := Get_For_Generate_Length (Gen, Ctxt);
      for I in 1 .. Length loop
         Disp_Block (To_Ghdl_Rtin_Block_Acc (Gen.Child),
                     Nctxt, Indent + 1);
         Nctxt.Base := Nctxt.Base + Gen.Size;
      end loop;
   end Disp_For_Generate;

   procedure Disp_Obj_Header (Obj : Ghdl_Rtin_Object_Acc; Indent : Natural) is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Obj.Common.Kind);
      Disp_Depth (Obj.Common.Depth);
      Put (", ");
      Disp_Linecol (Obj.Linecol);
      Put ("; ");
      Disp_Name (Obj.Name);
      Put (": ");
   end Disp_Obj_Header;

   procedure Disp_Psl_Header (Psl_Dir : Ghdl_Rtin_Psl_Directive_Acc;
                              Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Psl_Dir.Common.Kind);
      Disp_Depth (Psl_Dir.Common.Depth);
      Put (", ");
      Disp_Linecol (Psl_Dir.Linecol);
      Put ("; ");
      Disp_Name (Psl_Dir.Name);
      Put (": ");
   end Disp_Psl_Header;

   procedure Disp_Object (Obj : Ghdl_Rtin_Object_Acc;
                          Is_Sig : Boolean;
                          Ctxt : Rti_Context;
                          Indent : Natural)
   is
      Obj_Addr, Base, Bounds : Address;
      Obj_Type : Ghdl_Rti_Access;
   begin
      Disp_Obj_Header (Obj, Indent);

      Obj_Addr := Loc_To_Addr (Obj.Common.Depth, Obj.Loc, Ctxt);
      Obj_Type := Obj.Obj_Type;
      Disp_Subtype_Indication (Obj_Type, Ctxt, Obj_Addr);
      Put (" := ");

      Object_To_Base_Bounds (Obj_Type, Obj_Addr, Base, Bounds);
      Disp_Value (stdout, Obj_Type, Ctxt, Base, Bounds, Is_Sig);
      New_Line;
   end Disp_Object;

   procedure Disp_Psl_Directive (Psl_Dir : Ghdl_Rtin_Psl_Directive_Acc;
                                 Ctxt : Rti_Context;
                                 Indent : Natural)
   is
      Addr : Address;
   begin
      Disp_Psl_Header(Psl_Dir, Indent);
      Put ("count = ");
      Addr := Loc_To_Addr (Psl_Dir.Common.Depth, Psl_Dir.Loc, Ctxt);
      Put_U32 (stdout, Ghdl_U32 (To_Ghdl_Index_Ptr (Addr).all));
      New_Line;
   end Disp_Psl_Directive;

   procedure Disp_Psl_Endpoint (Psl_Dir : Ghdl_Rtin_Psl_Directive_Acc;
                                Ctxt : Rti_Context;
                                Indent : Natural)
   is
      Addr : Address;
      C : Character;
   begin
      Disp_Psl_Header(Psl_Dir, Indent);
      Put ("endpoint = ");
      Addr := Loc_To_Addr (Psl_Dir.Common.Depth, Psl_Dir.Loc, Ctxt);
      if To_Ghdl_Value_Ptr (Addr).B1 then
         C := 'T';
      else
         C := 'F';
      end if;
      Put (stdout, C);
      New_Line;
   end Disp_Psl_Endpoint;

   procedure Disp_Attribute (Obj : Ghdl_Rtin_Object_Acc;
                             Ctxt : Rti_Context;
                             Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Obj.Common.Kind);
      Disp_Depth (Obj.Common.Depth);
      Put ("; ");
      Disp_Name (Obj.Name);
      Put (": ");
      Disp_Subtype_Indication (Obj.Obj_Type, Ctxt, Null_Address);
      New_Line;
   end Disp_Attribute;

   procedure Disp_Component (Comp : Ghdl_Rtin_Component_Acc;
                             Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Comp.Common.Kind);
      Disp_Depth (Comp.Common.Depth);
      Put (": ");
      Disp_Name (Comp.Name);
      New_Line;
      --Disp_Rti_Arr (Comp.Nbr_Child, Comp.Children, Base, Ident + 1);
   end Disp_Component;

   procedure Disp_Instance (Inst : Ghdl_Rtin_Instance_Acc;
                            Ctxt : Rti_Context;
                            Indent : Natural)
   is
      Inst_Addr : Address;
      Inst_Base : Address;
      Inst_Rti : Ghdl_Rti_Access;
      Nindent : Natural;
      Nctxt : Rti_Context;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Inst.Common.Kind);
      Put (", ");
      Disp_Linecol (Inst.Linecol);
      Put (": ");
      Disp_Name (Inst.Name);
      New_Line;

      Inst_Addr := Ctxt.Base + Inst.Loc;
      --  Read sub instance.
      Inst_Base := To_Addr_Acc (Inst_Addr).all;

      Nindent := Indent + 1;

      case Inst.Instance.Kind is
         when Ghdl_Rtik_Component =>
            declare
               Comp : Ghdl_Rtin_Component_Acc;
            begin
               Comp := To_Ghdl_Rtin_Component_Acc (Inst.Instance);
               Disp_Indent (Nindent);
               Disp_Kind (Comp.Common.Kind);
               Put (": ");
               Disp_Name (Comp.Name);
               New_Line;
               --  Disp components generics and ports.
               --  FIXME: the data to disp are at COMP_BASE.
               Nctxt := (Base => Inst_Addr,
                         Block => Inst.Instance);
               Nindent := Nindent + 1;
               Disp_Rti_Arr (Comp.Nbr_Child, Comp.Children, Nctxt, Nindent);
               Nindent := Nindent + 1;
            end;
         when Ghdl_Rtik_Entity =>
            null;
         when others =>
            null;
      end case;

      --  Read instance RTI.
      if Inst_Base /= Null_Address then
         Inst_Rti := To_Ghdl_Rti_Acc_Acc (Inst_Base).all;
         Nctxt := (Base => Inst_Base,
                   Block => Inst_Rti);
         Disp_Block (To_Ghdl_Rtin_Block_Acc (Inst_Rti),
                     Nctxt, Nindent);
      end if;
   end Disp_Instance;

   procedure Disp_Type_Enum_Decl (Enum : Ghdl_Rtin_Type_Enum_Acc;
                                  Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Enum.Common.Kind);
      Put (": ");
      Disp_Name (Enum.Name);
      Put (" is (");
      Disp_Name (Enum.Names (0));
      for I in 1 .. Enum.Nbr - 1 loop
         Put (", ");
         Disp_Name (Enum.Names (I));
      end loop;
      Put (")");
      New_Line;
   end Disp_Type_Enum_Decl;

   procedure Disp_Subtype_Scalar_Decl (Def : Ghdl_Rtin_Subtype_Scalar_Acc;
                                       Ctxt : Rti_Context;
                                       Indent : Natural)
   is
      Bt : Ghdl_Rti_Access;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Disp_Depth (Def.Common.Depth);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      Bt := Def.Basetype;
      case Bt.Kind is
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_F64
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            declare
               Bdef : Ghdl_Rtin_Type_Scalar_Acc;
            begin
               Bdef := To_Ghdl_Rtin_Type_Scalar_Acc (Bt);
               if Bdef.Name /= Def.Name then
                  Disp_Name (Bdef.Name);
                  Put (" range ");
               end if;
               --  This is the type definition.
               Disp_Subtype_Scalar_Range (stdout, Def, Ctxt);
            end;
         when Ghdl_Rtik_Type_P64
           | Ghdl_Rtik_Type_P32 =>
            declare
               Bdef : Ghdl_Rtin_Type_Physical_Acc;
               Unit : Ghdl_Rti_Access;
            begin
               Bdef := To_Ghdl_Rtin_Type_Physical_Acc (Bt);
               if Bdef.Name /= Def.Name then
                  Disp_Name (Bdef.Name);
                  Put (" range ");
               end if;
               --  This is the type definition.
               Disp_Subtype_Scalar_Range (stdout, Def, Ctxt);
               if Bdef.Name = Def.Name then
                  for I in 0 .. Bdef.Nbr - 1 loop
                     Unit := Bdef.Units (I);
                     New_Line;
                     Disp_Indent (Indent + 1);
                     Disp_Kind (Unit.Kind);
                     Put (": ");
                     Disp_Name (Get_Physical_Unit_Name (Unit));
                     Put (" = ");
                     case Unit.Kind is
                        when Ghdl_Rtik_Unit64 =>
                           Put_I64 (stdout,
                                    To_Ghdl_Rtin_Unit64_Acc (Unit).Value);
                        when Ghdl_Rtik_Unitptr =>
                           case Bt.Kind is
                              when Ghdl_Rtik_Type_P64 =>
                                 Put_I64
                                   (stdout,
                                    To_Ghdl_Rtin_Unitptr_Acc (Unit).Addr.I64);
                              when Ghdl_Rtik_Type_P32 =>
                                 Put_I32
                                   (stdout,
                                    To_Ghdl_Rtin_Unitptr_Acc (Unit).Addr.I32);
                              when others =>
                                 Internal_Error
                                   ("disp_rti.subtype.scalar_decl(P32/P64)");
                           end case;
                        when others =>
                           Internal_Error
                             ("disp_rti.subtype.scalar_decl(P32/P64)");
                     end case;
                  end loop;
               end if;
            end;
         when others =>
            Disp_Subtype_Indication
              (To_Ghdl_Rti_Access (Def), Ctxt, Null_Address);
      end case;
      New_Line;
   end Disp_Subtype_Scalar_Decl;

   procedure Disp_Type_Array_Decl (Def : Ghdl_Rtin_Type_Array_Acc;
                                   Ctxt : Rti_Context;
                                   Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is array (");
      for I in 0 .. Def.Nbr_Dim - 1 loop
         if I /= 0 then
            Put (", ");
         end if;
         Disp_Subtype_Indication (Def.Indexes (I), Ctxt, Null_Address);
         Put (" range <>");
      end loop;
      Put (") of ");
      Disp_Subtype_Indication (Def.Element, Ctxt, Null_Address);
      New_Line;
   end Disp_Type_Array_Decl;

   procedure Disp_Subtype_Array_Decl (Def : Ghdl_Rtin_Subtype_Composite_Acc;
                                      Ctxt : Rti_Context;
                                      Indent : Natural)
   is
      Basetype : constant Ghdl_Rtin_Type_Array_Acc :=
        To_Ghdl_Rtin_Type_Array_Acc (Def.Basetype);
      Layout : Address;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      Layout := Loc_To_Addr (Def.Common.Depth, Def.Layout, Ctxt);
      Disp_Type_Array_Name (Basetype, Array_Layout_To_Bounds (Layout));
      if Rti_Anonymous_Type (To_Ghdl_Rti_Access (Basetype)) then
         Put (" of ");
         Disp_Subtype_Indication (Basetype.Element, Ctxt, Null_Address);
      end if;
      New_Line;
   end Disp_Subtype_Array_Decl;

   procedure Disp_Subtype_Unbounded_Array_Decl
     (Def : Ghdl_Rtin_Subtype_Composite_Acc;
      Ctxt : Rti_Context;
      Indent : Natural)
   is
      pragma Unreferenced (Ctxt);
      Basetype : constant Ghdl_Rtin_Type_Array_Acc :=
        To_Ghdl_Rtin_Type_Array_Acc (Def.Basetype);
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      Disp_Name (Basetype.Name);
      New_Line;
   end Disp_Subtype_Unbounded_Array_Decl;

   procedure Disp_Type_File_Or_Access (Def : Ghdl_Rtin_Type_Fileacc_Acc;
                                       Ctxt : Rti_Context;
                                       Indent : Natural)
   is
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      case Def.Common.Kind is
         when Ghdl_Rtik_Type_Access =>
            Put ("access ");
         when Ghdl_Rtik_Type_File =>
            Put ("file ");
         when others =>
            Put ("?? ");
      end case;
      Disp_Subtype_Indication (Def.Base, Ctxt, Null_Address);
      New_Line;
   end Disp_Type_File_Or_Access;

   procedure Disp_Type_Record (Def : Ghdl_Rtin_Type_Record_Acc;
                               Ctxt : Rti_Context;
                               Indent : Natural)
   is
      El : Ghdl_Rtin_Element_Acc;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is record");
      New_Line;
      for I in 1 .. Def.Nbrel loop
         El := To_Ghdl_Rtin_Element_Acc (Def.Elements (I - 1));
         Disp_Indent (Indent + 1);
         Disp_Kind (El.Common.Kind);
         Put (": ");
         Disp_Name (El.Name);
         Put (": ");
         Disp_Subtype_Indication (El.Eltype, Ctxt, Null_Address);
         New_Line;
      end loop;
   end Disp_Type_Record;

   procedure Disp_Subtype_Record_Decl (Def : Ghdl_Rtin_Subtype_Composite_Acc;
                                       Ctxt : Rti_Context;
                                       Indent : Natural)
   is
      Basetype : constant Ghdl_Rtin_Type_Record_Acc :=
        To_Ghdl_Rtin_Type_Record_Acc (Def.Basetype);
      Layout : Address;
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is ");
      Disp_Name (Basetype.Name);
      if Def.Common.Kind = Ghdl_Rtik_Subtype_Record then
         Layout := Loc_To_Addr (Def.Common.Depth, Def.Layout, Ctxt);
         Disp_Type_Record_Bounds (Basetype, Layout);
      end if;
      New_Line;
   end Disp_Subtype_Record_Decl;

   procedure Disp_Type_Protected (Def : Ghdl_Rtin_Type_Scalar_Acc;
                                  Ctxt : Rti_Context;
                                  Indent : Natural)
   is
      pragma Unreferenced (Ctxt);
   begin
      Disp_Indent (Indent);
      Disp_Kind (Def.Common.Kind);
      Put (": ");
      Disp_Name (Def.Name);
      Put (" is protected");
      New_Line;
   end Disp_Type_Protected;

   procedure Disp_Rti (Rti : Ghdl_Rti_Access;
                       Ctxt : Rti_Context;
                       Indent : Natural)
   is
   begin
      if Rti = null then
         return;
      end if;

      case Rti.Kind is
         when Ghdl_Rtik_Entity
           | Ghdl_Rtik_Architecture
           | Ghdl_Rtik_Package
           | Ghdl_Rtik_Process
           | Ghdl_Rtik_Block =>
            Disp_Block (To_Ghdl_Rtin_Block_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_If_Generate
           | Ghdl_Rtik_Case_Generate =>
            Disp_Block (To_Ghdl_Rtin_Block_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_For_Generate =>
            Disp_For_Generate (To_Ghdl_Rtin_Generate_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Package_Body =>
            Disp_Rti (To_Ghdl_Rtin_Block_Acc (Rti).Parent, Ctxt, Indent);
            Disp_Block (To_Ghdl_Rtin_Block_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Port
           | Ghdl_Rtik_Signal
           | Ghdl_Rtik_Guard
           | Ghdl_Rtik_Attribute_Quiet
           | Ghdl_Rtik_Attribute_Stable
           | Ghdl_Rtik_Attribute_Transaction =>
            Disp_Object (To_Ghdl_Rtin_Object_Acc (Rti), True, Ctxt, Indent);
         when Ghdl_Rtik_Generic
           | Ghdl_Rtik_Constant
           | Ghdl_Rtik_Variable
           | Ghdl_Rtik_Iterator
           | Ghdl_Rtik_File =>
            Disp_Object (To_Ghdl_Rtin_Object_Acc (Rti), False, Ctxt, Indent);
         when Ghdl_Rtik_Component =>
            Disp_Component (To_Ghdl_Rtin_Component_Acc (Rti), Indent);
         when Ghdl_Rtik_Attribute =>
            Disp_Attribute (To_Ghdl_Rtin_Object_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Instance =>
            Disp_Instance (To_Ghdl_Rtin_Instance_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            Disp_Type_Enum_Decl (To_Ghdl_Rtin_Type_Enum_Acc (Rti), Indent);
         when Ghdl_Rtik_Subtype_Scalar =>
            Disp_Subtype_Scalar_Decl (To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti),
                                      Ctxt, Indent);
         when Ghdl_Rtik_Type_Array =>
            Disp_Type_Array_Decl
              (To_Ghdl_Rtin_Type_Array_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Subtype_Array =>
            Disp_Subtype_Array_Decl
              (To_Ghdl_Rtin_Subtype_Composite_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Subtype_Unbounded_Array =>
            Disp_Subtype_Unbounded_Array_Decl
              (To_Ghdl_Rtin_Subtype_Composite_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Type_Access
           | Ghdl_Rtik_Type_File =>
            Disp_Type_File_Or_Access
              (To_Ghdl_Rtin_Type_Fileacc_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Type_Unbounded_Record =>
            Disp_Type_Record
              (To_Ghdl_Rtin_Type_Record_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Subtype_Record
           | Ghdl_Rtik_Subtype_Unbounded_Record =>
            Disp_Subtype_Record_Decl
              (To_Ghdl_Rtin_Subtype_Composite_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Type_Protected =>
            Disp_Type_Protected
              (To_Ghdl_Rtin_Type_Scalar_Acc (Rti), Ctxt, Indent);
         when Ghdl_Rtik_Psl_Cover
           | Ghdl_Rtik_Psl_Assume
           | Ghdl_Rtik_Psl_Assert =>
            Disp_Psl_Directive (To_Ghdl_Rtin_Psl_Directive_Acc (Rti),
                                Ctxt, Indent);
         when Ghdl_Rtik_Psl_Endpoint =>
            Disp_Psl_Endpoint (To_Ghdl_Rtin_Psl_Directive_Acc (Rti),
                               Ctxt, Indent);
         when others =>
            Disp_Indent (Indent);
            Disp_Kind (Rti.Kind);
            Put_Line (" ? ");
      end case;
   end Disp_Rti;

   Disp_Rti_Flag : Boolean := False;

   procedure Disp_All
   is
      Ctxt : Rti_Context;
   begin
      if not Disp_Rti_Flag then
         return;
      end if;

      Put ("DISP_RTI.Disp_All: ");
      Disp_Kind (Ghdl_Rti_Top.Common.Kind);
      New_Line;
      Ctxt := (Base => Ghdl_Rti_Top_Instance,
               Block => Ghdl_Rti_Top.Parent);
      Disp_Rti_Arr (Ghdl_Rti_Top.Nbr_Child,
                    Ghdl_Rti_Top.Children,
                    Ctxt, 0);
      Disp_Rti (Ghdl_Rti_Top.Parent, Ctxt, 0);

      --Disp_Hierarchy;
   end Disp_All;

   function Disp_Rti_Option (Opt : String) return Boolean
   is
   begin
      if Opt = "--dump-rti" then
         Disp_Rti_Flag := True;
         return True;
      else
         return False;
      end if;
   end Disp_Rti_Option;

   procedure Disp_Rti_Help
   is
      procedure P (Str : String) renames Put_Line;
   begin
      P (" --dump-rti         dump Run Time Information");
   end Disp_Rti_Help;

   Disp_Rti_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("dump-rti: implement --dump-rti"),
      Option => Disp_Rti_Option'Access,
      Help => Disp_Rti_Help'Access,
      Init => null,
      Start => Disp_All'Access,
      Finish => null);

   procedure Register is
   begin
      Register_Hooks (Disp_Rti_Hooks'Access);
   end Register;

end Grt.Disp_Rti;
