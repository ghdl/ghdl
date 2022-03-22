--  GHDL Run Time (GRT) - RTI utilities.
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
with Grt.Errors; use Grt.Errors;
with Grt.To_Strings; use Grt.To_Strings;
with Grt.Vstrings_IO; use Grt.Vstrings_IO;

package body Grt.Rtis_Utils is

   function Traverse_Blocks (Ctxt : Rti_Context) return Traverse_Result
   is
      function Traverse_Instance (Ctxt : Rti_Context) return Traverse_Result;

      function Traverse_Blocks_1 (Ctxt : Rti_Context) return Traverse_Result
      is
         Blk : Ghdl_Rtin_Block_Acc;

         Res : Traverse_Result;
         Nctxt : Rti_Context;
         Index : Ghdl_Index_Type;
         Child : Ghdl_Rti_Access;
      begin
         Res := Process (Ctxt, Ctxt.Block);
         if Res /= Traverse_Ok then
            return Res;
         end if;

         Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
         Index := 0;
         while Index < Blk.Nbr_Child loop
            Child := Blk.Children (Index);
            Index := Index + 1;
            case Child.Kind is
               when Ghdl_Rtik_Process
                 | Ghdl_Rtik_Block =>
                  declare
                     Nblk : Ghdl_Rtin_Block_Acc;
                  begin
                     Nblk := To_Ghdl_Rtin_Block_Acc (Child);
                     Nctxt := (Base => Ctxt.Base + Nblk.Loc,
                               Block => Child);
                     Res := Traverse_Blocks_1 (Nctxt);
                  end;
               when Ghdl_Rtik_For_Generate =>
                  declare
                     Gen : constant Ghdl_Rtin_Generate_Acc :=
                       To_Ghdl_Rtin_Generate_Acc (Child);
                     Length : Ghdl_Index_Type;
                  begin
                     Nctxt := (Base => To_Addr_Acc (Ctxt.Base + Gen.Loc).all,
                               Block => Gen.Child);
                     Length := Get_For_Generate_Length (Gen, Ctxt);
                     for I in 1 .. Length loop
                        Res := Traverse_Blocks_1 (Nctxt);
                        exit when Res = Traverse_Stop;
                        Nctxt.Base := Nctxt.Base + Gen.Size;
                     end loop;
                  end;
               when Ghdl_Rtik_If_Generate
                 | Ghdl_Rtik_Case_Generate =>
                  Nctxt := Get_If_Case_Generate_Child (Ctxt, Child);
                  if Nctxt.Base /= Null_Address then
                     Res := Traverse_Blocks_1 (Nctxt);
                  end if;
               when Ghdl_Rtik_Instance =>
                  Res := Process (Ctxt, Child);
                  if Res = Traverse_Ok then
                     declare
                        Obj : Ghdl_Rtin_Instance_Acc;
                     begin
                        Obj := To_Ghdl_Rtin_Instance_Acc (Child);

                        Get_Instance_Context (Obj, Ctxt, Nctxt);
                        if Nctxt /= Null_Context then
                           Res := Traverse_Instance (Nctxt);
                        end if;
                     end;
                  end if;
               when Ghdl_Rtik_Package
                 | Ghdl_Rtik_Entity
                 | Ghdl_Rtik_Architecture =>
                  Internal_Error ("traverse_blocks");
               when others =>
                  Res := Process (Ctxt, Child);
            end case;
            exit when Res = Traverse_Stop;
         end loop;

         return Res;
      end Traverse_Blocks_1;

      function Traverse_Instance (Ctxt : Rti_Context) return Traverse_Result
      is
         Blk : Ghdl_Rtin_Block_Acc;

         Res : Traverse_Result;
         Nctxt : Rti_Context;

      begin
         Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
         case Blk.Common.Kind is
            when Ghdl_Rtik_Architecture =>
               Nctxt := (Base => Ctxt.Base,
                         Block => Blk.Parent);
               --  The entity.
               Res := Traverse_Blocks_1 (Nctxt);
               if Res /= Traverse_Stop then
                  --  The architecture.
                  Res := Traverse_Blocks_1 (Ctxt);
               end if;
            when Ghdl_Rtik_Package_Body =>
               Nctxt := (Base => Ctxt.Base,
                         Block => Blk.Parent);
               Res := Traverse_Blocks_1 (Nctxt);
            when others =>
               Internal_Error ("traverse_blocks");
         end case;
         return Res;
      end Traverse_Instance;
   begin
      return Traverse_Instance (Ctxt);
   end Traverse_Blocks;

   --  Disp value stored at ADDR and whose type is described by RTI.
   procedure Get_Enum_Value
     (Vstr : in out Vstring; Rti : Ghdl_Rti_Access; Val : Ghdl_Index_Type)
   is
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;
   begin
      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Append (Vstr, Enum_Rti.Names (Val));
   end Get_Enum_Value;

   procedure Object_To_Base_Bounds (Obj_Type : Ghdl_Rti_Access;
                                    Obj_Loc : Address;
                                    Addr : out Address;
                                    Bounds : out Address) is
   begin
      --  FIXME: put this into a function.
      Bounds := Null_Address;
      Addr := Obj_Loc;
      case Obj_Type.Kind is
         when  Ghdl_Rtik_Subtype_Array
           | Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Subtype_Record =>
            --  If the type is complex then the location
            --  contains a pointer to the instantiation data.
            if Rti_Complex_Type (Obj_Type) then
               Addr := To_Addr_Acc (Obj_Loc).all;
            end if;
         when Ghdl_Rtik_Type_Array
            | Ghdl_Rtik_Subtype_Unbounded_Array
            | Ghdl_Rtik_Type_Unbounded_Record
            | Ghdl_Rtik_Subtype_Unbounded_Record =>
            --  If the type is unbounded then the location
            --  for the object containts a pointer to the bounds
            --  and a pointer to the data.
            Bounds := To_Ghdl_Uc_Array_Acc (Obj_Loc).Bounds;
            Addr := To_Ghdl_Uc_Array_Acc (Obj_Loc).Base;
         when others =>
            null;
      end case;
   end Object_To_Base_Bounds;

   procedure Record_To_Element (Obj : Address;
                                El : Ghdl_Rtin_Element_Acc;
                                Is_Sig : Boolean;
                                Rec_Layout : Address;
                                El_Addr : out Address;
                                El_Bounds : out Address)
   is
      Off : Ghdl_Index_Type;
      Off_Addr : Address;
   begin
      if Is_Sig then
         Off := El.Sig_Off;
      else
         Off := El.Val_Off;
      end if;

      case El.Common.Mode is
         when Ghdl_Rti_Element_Static =>
            El_Addr := Obj + Off;
            El_Bounds := Null_Address;
         when Ghdl_Rti_Element_Complex =>
            Off_Addr := Rec_Layout + Off;
            El_Addr := Obj + To_Ghdl_Index_Ptr (Off_Addr).all;
            El_Bounds := Null_Address;
         when Ghdl_Rti_Element_Unbounded =>
            Off_Addr := Rec_Layout + Off;
            El_Addr := Obj + To_Ghdl_Index_Ptr (Off_Addr).all;
            El_Bounds := Rec_Layout + El.Layout_Off;
            case El.Eltype.Kind is
               when Ghdl_Rtik_Type_Array
                 | Ghdl_Rtik_Subtype_Unbounded_Array =>
                  El_Bounds := Array_Layout_To_Bounds (El_Bounds);
               when others =>
                  --  Keep layout.
                  null;
            end case;
         when others =>
            Internal_Error ("record_to_element");
      end case;
   end Record_To_Element;

   function Is_Unbounded (Rti : Ghdl_Rti_Access) return Boolean is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Unbounded_Array
           | Ghdl_Rtik_Type_Unbounded_Record
           | Ghdl_Rtik_Subtype_Unbounded_Record =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Unbounded;

   procedure Foreach_Scalar (Ctxt : Rti_Context;
                             Obj_Type : Ghdl_Rti_Access;
                             Obj_Addr : Address;
                             Is_Sig : Boolean;
                             Param : Param_Type)
   is
      --  Current address.
      Addr : Address;
      Bounds : Address;

      Name : Vstring;

      procedure Handle_Any (Rti : Ghdl_Rti_Access);

      procedure Handle_Scalar (Rti : Ghdl_Rti_Access)
      is
         procedure Update (S : Ghdl_Index_Type) is
         begin
            Addr := Addr + (S / Storage_Unit);
         end Update;
      begin
         Process (Addr, Name, Rti, Param);

         if Is_Sig then
            Update (Address'Size);
         else
            case Rti.Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Update (32);
               when Ghdl_Rtik_Type_E8 =>
                  Update (8);
               when Ghdl_Rtik_Type_E32 =>
                  Update (32);
               when Ghdl_Rtik_Type_B1 =>
                  Update (8);
               when Ghdl_Rtik_Type_F64 =>
                  Update (64);
               when Ghdl_Rtik_Type_P64 =>
                  Update (64);
               when others =>
                  Internal_Error ("handle_scalar");
            end case;
         end if;
      end Handle_Scalar;

      procedure Range_Pos_To_Val (Rti : Ghdl_Rti_Access;
                                  Rng : Ghdl_Range_Ptr;
                                  Pos : Ghdl_Index_Type;
                                  Val : out Value_Union)
      is
      begin
         case Rti.Kind is
            when Ghdl_Rtik_Type_I32 =>
               case Rng.I32.Dir is
                  when Dir_To =>
                     Val.I32 := Rng.I32.Left + Ghdl_I32 (Pos);
                  when Dir_Downto =>
                     Val.I32 := Rng.I32.Left - Ghdl_I32 (Pos);
               end case;
            when Ghdl_Rtik_Type_E8 =>
               case Rng.E8.Dir is
                  when Dir_To =>
                     Val.E8 := Rng.E8.Left + Ghdl_E8 (Pos);
                  when Dir_Downto =>
                     Val.E8 := Rng.E8.Left - Ghdl_E8 (Pos);
               end case;
            when Ghdl_Rtik_Type_E32 =>
               case Rng.E32.Dir is
                  when Dir_To =>
                     Val.E32 := Rng.E32.Left + Ghdl_E32 (Pos);
                  when Dir_Downto =>
                     Val.E32 := Rng.E32.Left - Ghdl_E32 (Pos);
               end case;
            when Ghdl_Rtik_Type_B1 =>
               case Pos is
                  when 0 =>
                     Val.B1 := Rng.B1.Left;
                  when 1 =>
                     Val.B1 := Rng.B1.Right;
                  when others =>
                     Val.B1 := False;
               end case;
            when others =>
               Internal_Error ("grt.rtis_utils.range_pos_to_val");
         end case;
      end Range_Pos_To_Val;

      procedure Pos_To_Vstring (Vstr : in out Vstring;
                                Rti : Ghdl_Rti_Access;
                                Rng : Ghdl_Range_Ptr;
                                Pos : Ghdl_Index_Type)
      is
         V : Value_Union;
      begin
         Range_Pos_To_Val (Rti, Rng, Pos, V);
         case Rti.Kind is
            when Ghdl_Rtik_Type_I32 =>
               declare
                  S : String (1 .. 12);
                  F : Natural;
               begin
                  To_String (S, F, V.I32);
                  Append (Vstr, S (F .. S'Last));
               end;
            when Ghdl_Rtik_Type_E8 =>
               Get_Enum_Value (Vstr, Rti, Ghdl_Index_Type (V.E8));
            when Ghdl_Rtik_Type_E32 =>
               Get_Enum_Value (Vstr, Rti, Ghdl_Index_Type (V.E32));
            when Ghdl_Rtik_Type_B1 =>
               Get_Enum_Value (Vstr, Rti, Ghdl_B1'Pos (V.B1));
            when others =>
               Append (Vstr, '?');
         end case;
      end Pos_To_Vstring;

      procedure Handle_Array_1 (Arr_Rti : Ghdl_Rtin_Type_Array_Acc;
                                Index : Ghdl_Index_Type)
      is
         Idx_Rti : constant Ghdl_Rti_Access := Arr_Rti.Indexes (Index);
         Base_Type : constant Ghdl_Rti_Access := Get_Base_Type (Idx_Rti);
         El_Rti : constant Ghdl_Rti_Access := Arr_Rti.Element;
         Last_Index : constant Ghdl_Index_Type := Arr_Rti.Nbr_Dim - 1;
         Rng : Ghdl_Range_Ptr;
         Len : Ghdl_Index_Type;
         P : Natural;
         Cur_Bounds : Address;
      begin
         P := Length (Name);
         if Index = 0 then
            Append (Name, '(');
         else
            Append (Name, ',');
         end if;

         Extract_Range (Bounds, Base_Type, Rng);
         Len := Range_To_Length (Rng, Base_Type);

         Cur_Bounds := Bounds;
         for I in 1 .. Len loop
            Pos_To_Vstring (Name, Base_Type, Rng, I - 1);
            if Index = Last_Index then
               --  FIXME: not always needed.
               Bounds := Array_Layout_To_Element (Cur_Bounds, El_Rti);
               Append (Name, ')');
               Handle_Any (El_Rti);
            else
               Bounds := Cur_Bounds;
               Handle_Array_1 (Arr_Rti, Index + 1);
            end if;
            Truncate (Name, P + 1);
         end loop;
         Truncate (Name, P);
      end Handle_Array_1;

      procedure Handle_Record (Rti : Ghdl_Rtin_Type_Record_Acc)
      is
         Rec_Addr : constant Address := Addr;
         Rec_Bounds : constant Address := Bounds;
         Sizes : constant Ghdl_Indexes_Ptr :=
           To_Ghdl_Indexes_Ptr (Bounds);
         El : Ghdl_Rtin_Element_Acc;
         P : Natural;
      begin
         P := Length (Name);
         for I in 1 .. Rti.Nbrel loop
            El := To_Ghdl_Rtin_Element_Acc (Rti.Elements (I - 1));
            Record_To_Element
              (Rec_Addr, El, Is_Sig, Rec_Bounds, Addr, Bounds);
            Append (Name, '.');
            Append (Name, El.Name);
            Handle_Any (El.Eltype);
            Truncate (Name, P);
         end loop;
         if Is_Sig then
            Addr := Rec_Addr + Sizes.Signal;
         else
            Addr := Rec_Addr + Sizes.Value;
         end if;
         --  Bounds was fully used, no need to restore it.
         Bounds := Null_Address;
      end Handle_Record;

      procedure Handle_Any (Rti : Ghdl_Rti_Access) is
      begin
         case Rti.Kind is
            when Ghdl_Rtik_Subtype_Scalar =>
               Handle_Scalar (To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti).Basetype);
            when Ghdl_Rtik_Type_I32
              | Ghdl_Rtik_Type_E8
              | Ghdl_Rtik_Type_E32
              | Ghdl_Rtik_Type_B1 =>
               Handle_Scalar (Rti);
            when Ghdl_Rtik_Type_Array =>
               Handle_Array_1 (To_Ghdl_Rtin_Type_Array_Acc (Rti), 0);
            when Ghdl_Rtik_Subtype_Unbounded_Array =>
               declare
                  St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                    To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
                  Bt : constant Ghdl_Rtin_Type_Array_Acc :=
                    To_Ghdl_Rtin_Type_Array_Acc (St.Basetype);
               begin
                  Handle_Array_1 (Bt, 0);
               end;
            when Ghdl_Rtik_Subtype_Array =>
               declare
                  St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                    To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
                  Bt : constant Ghdl_Rtin_Type_Array_Acc :=
                    To_Ghdl_Rtin_Type_Array_Acc (St.Basetype);
                  Prev_Bounds : constant Address := Bounds;
                  Layout : Address;
               begin
                  Layout := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
                  Bounds := Array_Layout_To_Bounds (Layout);
                  Handle_Array_1 (Bt, 0);
                  Bounds := Prev_Bounds;
               end;
--          when Ghdl_Rtik_Type_File =>
--             declare
--                Vptr : Ghdl_Value_Ptr;
--             begin
--                Vptr := To_Ghdl_Value_Ptr (Obj);
--                Put (Stream, "File#");
--                Put_I32 (Stream, Vptr.I32);
--                --  FIXME: update OBJ (not very useful since never in a
--                --   composite type).
--             end;
            when Ghdl_Rtik_Type_Record =>
               declare
                  Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                    To_Ghdl_Rtin_Type_Record_Acc (Rti);
                  Prev_Bounds : constant Address := Bounds;
               begin
                  Bounds := Loc_To_Addr (Bt.Common.Depth, Bt.Layout, Ctxt);
                  Handle_Record (Bt);
                  Bounds := Prev_Bounds;
               end;
            when Ghdl_Rtik_Type_Unbounded_Record =>
               --  Bounds (layout) must have been extracted.
               Handle_Record (To_Ghdl_Rtin_Type_Record_Acc (Rti));
            when Ghdl_Rtik_Subtype_Unbounded_Record =>
               declare
                  St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                    To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
                  Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                    To_Ghdl_Rtin_Type_Record_Acc (St.Basetype);
               begin
                  Handle_Record (Bt);
               end;
            when Ghdl_Rtik_Subtype_Record =>
               declare
                  St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                    To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
                  Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                    To_Ghdl_Rtin_Type_Record_Acc (St.Basetype);
                  Prev_Bounds : constant Address := Bounds;
               begin
                  Bounds := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
                  Handle_Record (Bt);
                  Bounds := Prev_Bounds;
               end;
            when others =>
               Internal_Error ("grt.rtis_utils.foreach_scalar.handle_any");
         end case;
      end Handle_Any;
   begin
      Object_To_Base_Bounds (Obj_Type, Obj_Addr, Addr, Bounds);
      Handle_Any (Obj_Type);
      Free (Name);
   end Foreach_Scalar;

   procedure Get_Value (Str : in out Vstring;
                        Value : Value_Union;
                        Type_Rti : Ghdl_Rti_Access)
   is
   begin
      case Type_Rti.Kind is
         when Ghdl_Rtik_Type_I32 =>
            declare
               S : String (1 .. 12);
               F : Natural;
            begin
               To_String (S, F, Value.I32);
               Append (Str, S (F .. S'Last));
            end;
         when Ghdl_Rtik_Type_E8 =>
            Get_Enum_Value (Str, Type_Rti, Ghdl_Index_Type (Value.E8));
         when Ghdl_Rtik_Type_E32 =>
            Get_Enum_Value (Str, Type_Rti, Ghdl_Index_Type (Value.E32));
         when Ghdl_Rtik_Type_B1 =>
            Get_Enum_Value
              (Str, Type_Rti, Ghdl_Index_Type (Ghdl_B1'Pos (Value.B1)));
         when Ghdl_Rtik_Type_F64 =>
            declare
               S : String (1 .. 32);
               L : Integer;

               function Snprintf_G (Cstr : Address;
                                    Size : Natural;
                                    Arg : Ghdl_F64)
                 return Integer;
               pragma Import (C, Snprintf_G, "__ghdl_snprintf_g");

            begin
               L := Snprintf_G (S'Address, S'Length, Value.F64);
               if L < 0 then
                  --  FIXME.
                  Append (Str, "?");
               else
                  Append (Str, S (1 .. L));
               end if;
            end;
         when Ghdl_Rtik_Type_P32 =>
            declare
               S : String (1 .. 12);
               F : Natural;
            begin
               To_String (S, F, Value.I32);
               Append (Str, S (F .. S'Last));
               Append
                 (Str, Get_Physical_Unit_Name
                    (To_Ghdl_Rtin_Type_Physical_Acc (Type_Rti).Units (0)));
            end;
         when Ghdl_Rtik_Type_P64 =>
            declare
               S : String (1 .. 21);
               F : Natural;
            begin
               To_String (S, F, Value.I64);
               Append (Str, S (F .. S'Last));
               Append
                 (Str, Get_Physical_Unit_Name
                    (To_Ghdl_Rtin_Type_Physical_Acc (Type_Rti).Units (0)));
            end;
         when others =>
            Internal_Error ("grt.rtis_utils.get_value");
      end case;
   end Get_Value;

   procedure Disp_Value (Stream : FILEs;
                         Value : Value_Union;
                         Type_Rti : Ghdl_Rti_Access)
   is
      Name : Vstring;
   begin
      Rtis_Utils.Get_Value (Name, Value, Type_Rti);
      Put (Stream, Name);
      Free (Name);
   end Disp_Value;

   function Get_Physical_Unit_Name (Unit : Ghdl_Rti_Access)
                                   return Ghdl_C_String
   is
   begin
      case Unit.Kind is
         when Ghdl_Rtik_Unit64 =>
            return To_Ghdl_Rtin_Unit64_Acc (Unit).Name;
         when Ghdl_Rtik_Unitptr =>
            return To_Ghdl_Rtin_Unitptr_Acc (Unit).Name;
         when others =>
            Internal_Error ("rtis_utils.physical_unit_name");
      end case;
   end Get_Physical_Unit_Name;

   function Get_Physical_Unit_Value (Unit : Ghdl_Rti_Access;
                                     Type_Rti : Ghdl_Rti_Access)
                                    return Ghdl_I64 is
   begin
      case Unit.Kind is
         when Ghdl_Rtik_Unit64 =>
            return To_Ghdl_Rtin_Unit64_Acc (Unit).Value;
         when Ghdl_Rtik_Unitptr =>
            case Type_Rti.Kind is
               when Ghdl_Rtik_Type_P64 =>
                  return To_Ghdl_Rtin_Unitptr_Acc (Unit).Addr.I64;
               when Ghdl_Rtik_Type_P32 =>
                  return Ghdl_I64
                    (To_Ghdl_Rtin_Unitptr_Acc (Unit).Addr.I32);
               when others =>
                  Internal_Error ("get_physical_unit_value(1)");
            end case;
         when others =>
            Internal_Error ("get_physical_unit_value(2)");
      end case;
   end Get_Physical_Unit_Value;

   procedure Get_Enum_Value
     (Rstr : in out Rstring; Rti : Ghdl_Rti_Access; Val : Ghdl_Index_Type)
   is
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;
   begin
      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Prepend (Rstr, Enum_Rti.Names (Val));
   end Get_Enum_Value;


   procedure Get_Value (Rstr : in out Rstring;
                        Addr : Address;
                        Type_Rti : Ghdl_Rti_Access)
   is
      Value : constant Ghdl_Value_Ptr := To_Ghdl_Value_Ptr (Addr);
   begin
      case Type_Rti.Kind is
         when Ghdl_Rtik_Type_I32 =>
            declare
               S : String (1 .. 12);
               F : Natural;
            begin
               To_String (S, F, Value.I32);
               Prepend (Rstr, S (F .. S'Last));
            end;
         when Ghdl_Rtik_Type_E8 =>
            Get_Enum_Value (Rstr, Type_Rti, Ghdl_Index_Type (Value.E8));
         when Ghdl_Rtik_Type_E32 =>
            Get_Enum_Value (Rstr, Type_Rti, Ghdl_Index_Type (Value.E32));
         when Ghdl_Rtik_Type_B1 =>
            Get_Enum_Value
              (Rstr, Type_Rti, Ghdl_Index_Type (Ghdl_B1'Pos (Value.B1)));
         when others =>
            Internal_Error ("grt.rtis_utils.get_value(rstr)");
      end case;
   end Get_Value;

   procedure Get_Path_Name (Rstr : in out Rstring;
                            Last_Ctxt : Rti_Context;
                            Sep : Character;
                            Is_Instance : Boolean := True)
   is
      Blk : Ghdl_Rtin_Block_Acc;
      Ctxt : Rti_Context;
   begin
      Ctxt := Last_Ctxt;
      loop
         Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
         if Blk = null then
            Prepend (Rstr, "???");
            return;
         end if;
         case Ctxt.Block.Kind is
            when Ghdl_Rtik_Entity =>
               declare
                  Link : Ghdl_Entity_Link_Acc;
               begin
                  Link := To_Ghdl_Entity_Link_Acc (Ctxt.Base);
                  Ctxt := (Base => Ctxt.Base,
                           Block => Link.Rti);
                  if Ctxt.Block = null then
                     --  Process in an entity.
                     --  FIXME: check.
                     Prepend (Rstr, Blk.Name);
                     return;
                  end if;
               end;
            when Ghdl_Rtik_Architecture =>
               declare
                  Entity_Ctxt: Rti_Context;
                  Link : Ghdl_Entity_Link_Acc;
                  Parent_Inst : Ghdl_Rti_Access;
               begin
                  --  Architecture name.
                  if Is_Instance then
                     Prepend (Rstr, ')');
                     Prepend (Rstr, Blk.Name);
                     Prepend (Rstr, '(');
                  end if;

                  Entity_Ctxt := Get_Parent_Context (Ctxt);

                  --  Instance parent.
                  Link := To_Ghdl_Entity_Link_Acc (Entity_Ctxt.Base);
                  Get_Instance_Link (Link, Ctxt, Parent_Inst);

                  --  Add entity name.
                  if Is_Instance or Parent_Inst = null then
                     Prepend (Rstr,
                              To_Ghdl_Rtin_Block_Acc (Entity_Ctxt.Block).Name);
                  end if;

                  if Parent_Inst = null then
                     --  Top reached.
                     Prepend (Rstr, Sep);
                     return;
                  else
                     --  Instantiation statement label.
                     if Is_Instance then
                        Prepend (Rstr, '@');
                     end if;
                     Prepend (Rstr,
                              To_Ghdl_Rtin_Object_Acc (Parent_Inst).Name);
                     Prepend (Rstr, Sep);
                  end if;
               end;
            when Ghdl_Rtik_Process
              | Ghdl_Rtik_Block =>
               Prepend (Rstr, Blk.Name);
               Prepend (Rstr, Sep);
               Ctxt := Get_Parent_Context (Ctxt);
            when Ghdl_Rtik_Generate_Body =>
               declare
                  Gen : constant Ghdl_Rtin_Generate_Acc :=
                    To_Ghdl_Rtin_Generate_Acc (Blk.Parent);
                  Iter : Ghdl_Rtin_Object_Acc;
                  Addr : Address;
               begin
                  if Blk.Parent.Kind = Ghdl_Rtik_For_Generate then
                     Prepend (Rstr, ')');
                     Iter := To_Ghdl_Rtin_Object_Acc (Blk.Children (0));
                     Addr := Loc_To_Addr (Iter.Common.Depth, Iter.Loc, Ctxt);
                     Get_Value (Rstr, Addr, Get_Base_Type (Iter.Obj_Type));
                     Prepend (Rstr, '(');
                  end if;
                  Prepend (Rstr, Gen.Name);
                  Prepend (Rstr, Sep);
                  Ctxt := Get_Parent_Context (Ctxt);
               end;
            when Ghdl_Rtiks_Psl =>
               declare
                  Psl_Directive : constant Ghdl_Rtin_Psl_Directive_Acc :=
                     To_Ghdl_Rtin_Psl_Directive_Acc(Ctxt.Block);
               begin
                  Prepend (Rstr, Psl_Directive.Name);
                  Prepend (Rstr, Sep);
                  Ctxt := Get_Parent_Context (Ctxt);
               end;

            when others =>
               Internal_Error ("grt.rtis_utils.get_path_name");
         end case;
      end loop;
   end Get_Path_Name;

   procedure Put (Stream : FILEs; Ctxt : Rti_Context)
   is
      Rstr : Rstring;
   begin
      Get_Path_Name (Rstr, Ctxt, '.');
      Put (Stream, Rstr);
      Free (Rstr);
   end Put;

   function Get_Linecol_Line (Linecol : Ghdl_Index_Type) return Ghdl_U32 is
   begin
      return Ghdl_U32 (Linecol / 256);
   end Get_Linecol_Line;

   function Get_Linecol_Col (Linecol : Ghdl_Index_Type) return Ghdl_U32 is
   begin
      return Ghdl_U32 (Linecol mod 256);
   end Get_Linecol_Col;

   function Get_Filename (Ctxt : Rti_Context) return Ghdl_C_String
   is
      C : Rti_Context;
   begin
      C := Ctxt;
      loop
         case C.Block.Kind is
            when Ghdl_Rtik_Package
              | Ghdl_Rtik_Package_Body
              | Ghdl_Rtik_Architecture
              | Ghdl_Rtik_Entity =>
               declare
                  Blk : constant Ghdl_Rtin_Block_Filename_Acc :=
                    To_Ghdl_Rtin_Block_Filename_Acc (C.Block);
               begin
                  return Blk.Filename;
               end;
            when others =>
               C := Get_Parent_Context (C);
         end case;
      end loop;
   end Get_Filename;
end Grt.Rtis_Utils;
