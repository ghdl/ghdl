--  GHDL Run Time (GRT) - Override top entity generics
--  Copyright (C) 2015 Tristan Gingold
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
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Lib; use Grt.Lib;
with Grt.Options; use Grt.Options;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Avhpi_Utils; use Grt.Avhpi_Utils;
with Grt.Errors; use Grt.Errors;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Values;

package body Grt.Change_Generics is
   procedure Error_Override (Msg : String; Over : Generic_Override_Acc) is
   begin
      Error_S (Msg);
      Diag_C (" '");
      Diag_C (Over.Name.all);
      Error_E ("'");
   end Error_Override;

   procedure Error_Range (Over : Generic_Override_Acc) is
   begin
      Error_Override ("value not in range for generic", Over);
   end Error_Range;

   --  Convert C to E8 values
   procedure Ghdl_Value_E8_Char (Res : out Ghdl_E8;
                                 Err : out Boolean;
                                 C : Character;
                                 Rti : Ghdl_Rti_Access)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Lit_Name : Ghdl_C_String;
   begin
      for I in 0 .. Enum_Rti.Nbr - 1 loop
         Lit_Name := Enum_Rti.Names (I);
         if Lit_Name (1) = ''' and Lit_Name (2) = C and Lit_Name (3) = ''' then
            Res := Ghdl_E8 (I);
            Err := False;
            return;
         end if;
      end loop;
      Res := 0;
      Err := True;
   end Ghdl_Value_E8_Char;

   --  Override for unconstrained array generic.
   procedure Override_Generic_Array (Obj_Rti : Ghdl_Rtin_Object_Acc;
                                     Arr_Rti : Ghdl_Rtin_Type_Array_Acc;
                                     Ctxt : Rti_Context;
                                     Over : Generic_Override_Acc)
   is
      El_Rti : constant Ghdl_Rti_Access := Arr_Rti.Element;
      Idx_Rti : constant Ghdl_Rti_Access := Arr_Rti.Indexes (0);
      El_Base_Rti : Ghdl_Rti_Access;
      Idx_Base_Rti : Ghdl_Rti_Access;
      St_Rng, Rng : Ghdl_Range_Ptr;
      Arr : Ghdl_E8_Array_Base_Ptr;
      Err : Boolean;
      Len : Ghdl_Index_Type;
      Uc_Array : Ghdl_Uc_Array_Acc;
   begin
      --  Check array type:
      --  - Must be one dimension
      if Arr_Rti.Nbr_Dim /= 1 then
         Error_Override ("multi-dimension array not supported for "
                           & "override of generic", Over);
         return;
      end if;
      --  - Index must be a scalar integer
      if Idx_Rti.Kind /= Ghdl_Rtik_Subtype_Scalar then
         Internal_Error ("override_generic_array");
      end if;
      Idx_Base_Rti := To_Ghdl_Rtin_Subtype_Scalar_Acc (Idx_Rti).Basetype;
      if Idx_Base_Rti.Kind /= Ghdl_Rtik_Type_I32 then
         Error_Override ("non Integer array index not supported for "
                           & "override of generic", Over);
         return;
      end if;
      --  - Element must be E8 enum.
      if El_Rti.Kind = Ghdl_Rtik_Subtype_Scalar then
         El_Base_Rti := To_Ghdl_Rtin_Subtype_Scalar_Acc (El_Rti).Basetype;
      else
         El_Base_Rti := El_Rti;
      end if;
      if El_Base_Rti.Kind /= Ghdl_Rtik_Type_E8 then
         Error_Override ("non enumerated element type not supported for "
                           & "override of generic", Over);
         return;
      end if;

      --  The real work can start.
      St_Rng := To_Ghdl_Range_Ptr
        (Loc_To_Addr (Idx_Rti.Depth,
                      To_Ghdl_Rtin_Subtype_Scalar_Acc (Idx_Rti).Range_Loc,
                      Ctxt));

      --  Create the value.
      Len := Over.Value'Length;
      Arr := To_Ghdl_E8_Array_Base_Ptr (Ghdl_Malloc (Len));
      for I in Over.Value'range loop
         Ghdl_Value_E8_Char (Arr (Ghdl_Index_Type (I - Over.Value'First)), Err,
                             Over.Value (I), El_Base_Rti);
         if Err then
            Error_Override ("invalid character for override of generic", Over);
            return;
         end if;
      end loop;

      --  Create the range.
      Rng := new Ghdl_Range_Type (Mode_I32);
      Rng.I32.Left := St_Rng.I32.Left;
      Rng.I32.Dir := St_Rng.I32.Dir;
      case Rng.I32.Dir is
         when Dir_To =>
            Rng.I32.Right := Rng.I32.Left + Ghdl_I32 (Len - 1);
         when Dir_Downto =>
            Rng.I32.Right := Rng.I32.Left - Ghdl_I32 (Len - 1);
      end case;
      Rng.I32.Len := Len;

      --  Override the generic.  Don't try to free previous value as it may
      --  not have been dynamically allocated.
      Uc_Array := To_Ghdl_Uc_Array_Acc
        (Loc_To_Addr (Obj_Rti.Common.Depth, Obj_Rti.Loc, Ctxt));
      Uc_Array.all := (Base => Arr (0)'Address,
                       Bounds => Rng.all'Address);
   end Override_Generic_Array;

   procedure Override_Generic_I32 (Obj_Rti : Ghdl_Rtin_Object_Acc;
                                   Ctxt : Rti_Context;
                                   Over : Generic_Override_Acc)
   is
      Subtype_Rti : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
        To_Ghdl_Rtin_Subtype_Scalar_Acc (Obj_Rti.Obj_Type);
      Rng : Ghdl_Range_Ptr;
      Res : Ghdl_I64;
      Ptr : Ghdl_Value_Ptr;
   begin
      Res := Grt.Values.Value_I64
        (To_Std_String_Basep (Over.Value.all'Address), Over.Value'Length, 0);

      --  Check range.
      Rng := To_Ghdl_Range_Ptr
        (Loc_To_Addr (Subtype_Rti.Common.Depth, Subtype_Rti.Range_Loc, Ctxt));
      case Rng.I32.Dir is
         when Dir_To =>
            if Res < Ghdl_I64 (Rng.I32.Left)
              or else Res > Ghdl_I64 (Rng.I32.Right)
            then
               Error_Range (Over);
            end if;
         when Dir_Downto =>
            if Res > Ghdl_I64 (Rng.I32.Left)
              or else Res < Ghdl_I64 (Rng.I32.Right)
            then
               Error_Range (Over);
            end if;
      end case;

      --  Assign.
      Ptr := To_Ghdl_Value_Ptr
        (Loc_To_Addr (Obj_Rti.Common.Depth, Obj_Rti.Loc, Ctxt));
      Ptr.I32 := Ghdl_I32 (Res);
   end Override_Generic_I32;

   procedure Override_Generic_Enum (Obj_Rti : Ghdl_Rtin_Object_Acc;
                                    Ctxt : Rti_Context;
                                    Over : Generic_Override_Acc;
                                    Type_Rti : Ghdl_Rti_Access)
   is
      Res : Ghdl_Index_Type;
      Ptr : Ghdl_Value_Ptr;
   begin
      Res := Grt.Values.Value_Enum
        (To_Std_String_Basep (Over.Value.all'Address),
         Over.Value'Length, Type_Rti);

      --  Assign.
      Ptr := To_Ghdl_Value_Ptr
        (Loc_To_Addr (Obj_Rti.Common.Depth, Obj_Rti.Loc, Ctxt));

      case Obj_Rti.Obj_Type.Kind is
         when Ghdl_Rtik_Type_E8 =>
            Ptr.E8 := Ghdl_E8 (Res);
         when Ghdl_Rtik_Type_B1 =>
            Ptr.B1 := Ghdl_B1'Val (Res);
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Subtype_Rti : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
                 To_Ghdl_Rtin_Subtype_Scalar_Acc (Obj_Rti.Obj_Type);
               Rng : Ghdl_Range_Ptr;
            begin
               Rng := To_Ghdl_Range_Ptr
                 (Loc_To_Addr (Subtype_Rti.Common.Depth,
                               Subtype_Rti.Range_Loc, Ctxt));
               case Subtype_Rti.Basetype.Kind is
                  when Ghdl_Rtik_Type_E8 =>
                     case Rng.E8.Dir is
                        when Dir_To =>
                           if Res < Ghdl_Index_Type (Rng.E8.Left)
                             or else Res > Ghdl_Index_Type (Rng.E8.Right)
                           then
                              Error_Range (Over);
                           end if;
                        when Dir_Downto =>
                           if Res > Ghdl_Index_Type (Rng.E8.Left)
                             or else Res < Ghdl_Index_Type (Rng.E8.Right)
                           then
                              Error_Range (Over);
                           end if;
                     end case;
                     Ptr.E8 := Ghdl_E8 (Res);
                  when others =>
                     Internal_Error ("override_generic_enum");
               end case;
            end;
         when others =>
            Internal_Error ("override_generic_enum");
      end case;
   end Override_Generic_Enum;

   --  Override DECL with OVER.  Dispatch according to generic type.
   procedure Override_Generic_Value (Decl : VhpiHandleT;
                                     Over : Generic_Override_Acc)
   is
      Rti : constant Ghdl_Rti_Access := Avhpi_Get_Rti (Decl);
      Obj_Rti : constant Ghdl_Rtin_Object_Acc :=
        To_Ghdl_Rtin_Object_Acc (Rti);
      Type_Rti : constant Ghdl_Rti_Access := Obj_Rti.Obj_Type;
      Ctxt : constant Rti_Context := Avhpi_Get_Context (Decl);
   begin
      pragma Assert (Rti.Kind = Ghdl_Rtik_Generic);
      case Type_Rti.Kind is
         when Ghdl_Rtik_Type_Array =>
            Override_Generic_Array
              (Obj_Rti, To_Ghdl_Rtin_Type_Array_Acc (Type_Rti), Ctxt, Over);
         when Ghdl_Rtik_Subtype_Unbounded_Array =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Type_Rti);
               Bt : constant Ghdl_Rtin_Type_Array_Acc :=
                 To_Ghdl_Rtin_Type_Array_Acc (St.Basetype);
            begin
               if Bt.Common.Kind /= Ghdl_Rtik_Type_Array then
                  Error_Override
                    ("unhandled array subtype for generic override of", Over);
               else
                  Override_Generic_Array (Obj_Rti, Bt, Ctxt, Over);
               end if;
            end;
         when Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_E8 =>
            Override_Generic_Enum (Obj_Rti, Ctxt, Over, Type_Rti);
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Subtype_Rti : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
                 To_Ghdl_Rtin_Subtype_Scalar_Acc (Type_Rti);
            begin
               case Subtype_Rti.Basetype.Kind is
                  when Ghdl_Rtik_Type_I32 =>
                     Override_Generic_I32 (Obj_Rti, Ctxt, Over);
                  when Ghdl_Rtik_Type_E8 =>
                     Override_Generic_Enum
                       (Obj_Rti, Ctxt, Over, Subtype_Rti.Basetype);
                  when others =>
                     Error_Override
                       ("unhandled type for generic override of", Over);
               end case;
            end;
         when others =>
            Error_Override ("unhandled type for generic override of", Over);
      end case;
   end Override_Generic_Value;

   --  Handle generic override OVER.  Find the generic declaration.
   procedure Override_Generic (Over : Generic_Override_Acc)
   is
      Root, It, Decl : VhpiHandleT;
      Error : AvhpiErrorT;
   begin
      Get_Root_Inst (Root);

      --  Find generic.
      Vhpi_Iterator (VhpiDecls, Root, It, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("override_generic(1)");
         return;
      end if;

      --  Look for the generic.
      loop
         Vhpi_Scan (It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Internal_Error ("override_generic(2)");
            return;
         end if;
         exit when Vhpi_Get_Kind (Decl) /= VhpiGenericDeclK;
         if Name_Compare (Decl, Over.Name.all) then
            Override_Generic_Value (Decl, Over);
            return;
         end if;
      end loop;

      Error_Override ("cannot find in top entity generic", Over);
   end Override_Generic;

   procedure Change_All_Generics
   is
      Over : Generic_Override_Acc;
   begin
      --  Handle overrides one by one (in order).
      Over := First_Generic_Override;
      while Over /= null loop
         Override_Generic (Over);
         Over := Over.Next;
      end loop;
   end Change_All_Generics;

   procedure Check_Required_Generic_Override
   is
      Root, It, Decl : VhpiHandleT;
      Error : AvhpiErrorT;
   begin
      Get_Root_Inst (Root);

      --  Find generic.
      Vhpi_Iterator (VhpiDecls, Root, It, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("override_generic(1)");
         return;
      end if;

      --  Look for the generic.
      loop
         Vhpi_Scan (It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Internal_Error ("override_generic(2)");
            return;
         end if;
         exit when Vhpi_Get_Kind (Decl) /= VhpiGenericDeclK;

         declare
            use System;
            Rti : constant Ghdl_Rti_Access := Avhpi_Get_Rti (Decl);
            Obj_Rti : constant Ghdl_Rtin_Object_Acc :=
              To_Ghdl_Rtin_Object_Acc (Rti);
            Type_Rti : constant Ghdl_Rti_Access := Obj_Rti.Obj_Type;
            Ctxt : constant Rti_Context := Avhpi_Get_Context (Decl);
         begin
            pragma Assert (Rti.Kind = Ghdl_Rtik_Generic);
            if Type_Rti.Kind = Ghdl_Rtik_Type_Array then
               declare
                  Uc_Array : Ghdl_Uc_Array_Acc;
               begin
                  Uc_Array := To_Ghdl_Uc_Array_Acc
                    (Loc_To_Addr (Obj_Rti.Common.Depth, Obj_Rti.Loc, Ctxt));
                  if Uc_Array.Base = Null_Address then
                     Error_S ("top-level generic '");
                     Diag_C (Obj_Rti.Name);
                     Error_E ("' must be overriden (use -gGEN=VAL)");
                  end if;
               end;
            end if;
         end;
      end loop;
   end Check_Required_Generic_Override;

end Grt.Change_Generics;
