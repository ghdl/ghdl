--  GHDL Run Time (GRT) -  Run Time Information Instances.
--  Copyright (C) 2020 Tristan Gingold
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

package body Grt.Rtiis is

   function Ghdl_Index_Type_Image (Idx : Ghdl_Index_Type)
                                  return Ghdl_Object_Rtii_Name
   is
      V : Ghdl_Index_Type := Idx;
      Working_Str : String (1 .. 32);
      Pos_In_Str : Natural := 0;
   begin
      loop
         Working_Str (Pos_In_Str) := Character'Val (48 - (V rem 10));
         V := V / 10;
         exit when V = 0;
         Pos_In_Str := Pos_In_Str + 1;
         if Pos_In_Str > 32 then
            -- Ghdl_Index_Type should only be 32 bits which is way less
            -- that what we've allowed.
            Internal_Error("ghdl_index_type_image");
         end if;
      end loop;
      return New_Name(Working_Str(1 .. Pos_In_Str));
   end Ghdl_Index_Type_Image;

   function New_Name (S : String) return Ghdl_Object_Rtii_Name is
      Name : Ghdl_Object_Rtii_Name;
   begin
      Name.Length := S'Length;
      Name.Value(1 .. S'Length) := S(S'First .. S'Last);
      Name.Value(S'Length+1) := NUL;
      return Name;
   end New_Name;

   function New_Name (S : Ghdl_C_String) return Ghdl_Object_Rtii_Name is
      Name : Ghdl_Object_Rtii_Name;
   begin
      Name.Length := strlen(S);
      Name.Value(1 .. Name.Length) := S(1 .. Name.Length);
      Name.Value(Name.Length+1) := NUL;
      return Name;
   end New_Name;

   function Append_To_Name (Name : Ghdl_Object_Rtii_Name; S : String)
                           return Ghdl_Object_Rtii_Name is
      New_Name : Ghdl_Object_Rtii_Name := Name;
   begin
      if S'Length + Name.Length > Rtii_Name_Length - 4 then
         -- If we can't fit the name in then just put '....' in the rest of
         -- the space.
         New_Name.Length := Rtii_Name_Length-1;
         New_Name.Value(Name.Length+1 .. Rtii_Name_Length-1) :=
           (others => '.');
      else
         New_Name.Value(Name.Length+1 .. Name.Length + S'Length) := S;
         New_Name.Length := New_Name.Length + S'Length;
      end if;
      New_Name.Value(New_Name.Length+1) := NUL;
      return New_Name;
   end Append_To_Name;

   function Append_To_Name (Name : Ghdl_Object_Rtii_Name; S : Ghdl_C_String)
                           return Ghdl_Object_Rtii_Name is
      Length : constant Natural := strlen(S);
      Str : String(1 .. Length);
   begin
      Str(1 .. Length) := S(1 .. Length);
      return Append_To_Name(Name, Str);
   end Append_To_Name;

   function Append_To_Name (Name : Ghdl_Object_Rtii_Name;
                            S : Ghdl_Object_Rtii_Name)
                           return Ghdl_Object_Rtii_Name is
      Str : constant String := S.Value(1 .. S.Length);
   begin
      return Append_To_Name(Name, Str);
   end Append_To_Name;

   function To_Ghdl_C_String(Name : Ghdl_Object_Rtii_Name)
                            return Ghdl_C_String is
      S : Ghdl_C_String;
   begin
      S := To_Ghdl_C_String(Name'Address);
      return S;
   end To_Ghdl_C_String;

   function Rti_Record_Base_Type (Rti : Ghdl_Rti_Access)
                                 return Ghdl_Rtin_Type_Record_Acc is
      Kind : Ghdl_Rtik;
      Base_Record : Ghdl_Rtin_Type_Record_Acc;
   begin
      Kind := Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Record =>
            Base_Record := To_Ghdl_Rtin_Type_Record_Acc(Rti);
         when Ghdl_Rtik_Subtype_Record =>
            Base_Record := To_Ghdl_Rtin_Type_Record_Acc(
               To_Ghdl_Rtin_Subtype_Composite_Acc(Rti).Basetype);
         when others =>
            Internal_Error("get_record_base_type");
      end case;
      return Base_Record;
   end Rti_Record_Base_Type;

   -- Find the layout information address for a Rti type given it's context.
   -- For types that have no layout information a null address is returned.
   function Get_Type_Layout(Rti : Ghdl_Rti_Access; Ctxt : Rti_Context)
                           return Address is
      Layout_Addr : Address;
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               St : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
                 To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti);
            begin
               Layout_Addr := Loc_To_Addr (
                  St.Common.Depth, St.Range_Loc, Ctxt);
            end;
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B1 =>
            Layout_Addr := Null_Address;
         when Ghdl_Rtik_Type_Array =>
            Internal_Error("get_type_layout: invalid for unbound types");
         when Ghdl_Rtik_Subtype_Array =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
            begin
               Layout_Addr := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
            end;
         when Ghdl_Rtik_Type_Record =>
            declare
               Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Rti);
            begin
               if Rti_Complex_Type (Rti) then
                  Layout_Addr := Loc_To_Addr (
                     Bt.Common.Depth, Bt.Layout, Ctxt);
               else
                  -- FIXME:  Is this right?
                  --Layout_Addr := Bt.Layout;
                  null;
               end if;
            end;
         when Ghdl_Rtik_Subtype_Record =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
            begin
               Layout_Addr := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
            end;
         when others =>
            Internal_Error("get_type_layout: unknown type");
      end case;
      return Layout_Addr;
   end Get_Type_Layout;

   type Array_Base_And_Layout is record
      Base : Ghdl_Rtin_Type_Array_Acc;
      Layout : Address;
   end record;

   function Get_Array_Base_And_Layout (Rtii : Ghdl_Object_Rtii)
                                      return Array_Base_And_Layout is
      Kind : Ghdl_Rtik;
      Res : Array_Base_And_Layout;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Array =>
            Res.Base := To_Ghdl_Rtin_Type_Array_Acc(Rtii.Typ.Rti);
            Res.Layout := Rtii.Typ.Binding_Layout_Addr;
         when Ghdl_Rtik_Subtype_Array =>
            Res.Base := To_Ghdl_Rtin_Type_Array_Acc(
               To_Ghdl_Rtin_Subtype_Composite_Acc(Rtii.Typ.Rti).Basetype);
            Res.Layout := Rtii.Typ.Layout_Addr;
         when others =>
            Internal_Error("get_array_element_type");
      end case;
      return Res;
   end Get_Array_Base_And_Layout;

   -- Get the length of a flattened multidimensional array.
   -- index into an equivalent flattened array.  Also returns
   -- the length of the flattened array.
   function Get_Flattened_Length (
      Nbr_Dim : Ghdl_Index_Type;
      -- An array of the index types.
      Index_Types : Ghdl_Rti_Arr_Acc;
      -- The location of the bounds of the indices.
      Bounds_Address : Address)
      return Ghdl_Index_Type is
      Idx_Rti : Ghdl_Rti_Access;
      Rng : Ghdl_Range_Ptr;
      Length : Ghdl_Index_Type;
      Index: Ghdl_Index_Type;
      Mutable_Bounds_Address : Address := Bounds_Address;
   begin
      Length := 1;
      Index := Nbr_Dim;
      for Dim in 1 .. Nbr_Dim loop
         Idx_Rti := Get_Base_Type (Index_Types (Dim));
         Extract_Range (Mutable_Bounds_Address, Idx_Rti, Rng);
         Length := Length * Range_To_Length(Rng, Idx_Rti);
         Index := Index - 1;
      end loop;
      return Length;
   end Get_Flattened_Length;

   function Get_Array_Nbr_Children (Rtii : Ghdl_Object_Rtii)
                                   return Ghdl_Index_Type is
      Bounds_Addr : Address;
      Length : Ghdl_Index_Type;
      Base_And_Layout : Array_Base_And_Layout;
   begin
      Base_And_Layout := Get_Array_Base_And_Layout(Rtii);
      Bounds_Addr := Base_And_Layout.Layout + Ghdl_Index_Type'(
         Ghdl_Indexes_Type'Size / 8);
      Length := Get_Flattened_Length(
         Nbr_Dim => Base_And_Layout.Base.Nbr_Dim,
         Index_Types => Base_And_Layout.Base.Indexes,
         Bounds_Address => Bounds_Addr);
      return Length;
   end Get_Array_Nbr_Children;

   function Get_Array_Child (Rtii : Ghdl_Object_Rtii; Index : Ghdl_Index_Type)
                            return Ghdl_Object_Rtii is
      Base_And_Layout : Array_Base_And_Layout;
      El_Type : Ghdl_Type_Rtii;
      Binding_Layout_Addr : Address;
      Bounds_Addr : Address;
      Child : Ghdl_Object_Rtii;
      El_Layout_Addr : Address;
      Child_Addr : Address;
      Sizes : Ghdl_Indexes_Ptr;
      El_Size : Ghdl_Index_Type;
      El_Name : Ghdl_Object_Rtii_Name;
      Length : Ghdl_Index_Type;
   begin
      Base_And_Layout := Get_Array_Base_And_Layout(Rtii);
      Sizes := To_Ghdl_Indexes_Ptr(Base_And_Layout.Layout);
      if Rtii.Is_Sig then
         El_Size := Sizes.Signal;
      else
         El_Size := Sizes.Value;
      end if;
      Bounds_Addr := Base_And_Layout.Layout + Ghdl_Index_Type'(
                        Ghdl_Indexes_Type'Size / 8);
      -- FIXME: At the moment we're treating the indices at 0-based integer
      -- indices.  It would be much nicer for the type of these indices to
      -- correspond to the actual type used in the VHDL.
      -- e.g. for a signal `blah(6 downto 1)`, current the index 0 will
      -- point at blah[6] which is confusing.
      -- Also we're currently use a flattened index for
      -- multidimensional arrays.
      Length := Get_Flattened_Length(
         Nbr_Dim => Base_And_Layout.Base.Nbr_Dim,
         Index_Types => Base_And_Layout.Base.Indexes,
         Bounds_Address => Bounds_Addr);
      if Index >= Length then
         Internal_Error("get_array_child");
      end if;
      El_Name := Rtii.Name;
      El_Name := Append_To_Name(El_Name, "[");
      El_Name := Append_To_Name(El_Name, Ghdl_Index_Type_Image(Index));
      El_Name := Append_To_Name(El_Name, "]");
      Child_Addr := Rtii.Addr + El_Size * Index;
      El_Layout_Addr := Get_Type_Layout(
         To_Ghdl_Rtin_Element_Acc(Base_And_Layout.Base.Element).Eltype,
         Rtii.Typ.Ctxt);
      --  There is no way for an array type to pass bounds to it's element.
      --  This is implemented with an anonymous subtype so we don't
      --  need to worry about passing the bounds here.
      Binding_Layout_Addr := Null_Address;
      --  The child is one of the array elements.
      El_Type := Ghdl_Type_Rtii'(
         Rti => Base_And_Layout.Base.Element,
         Ctxt => Rtii.Typ.Ctxt,
         Layout_Addr => El_Layout_Addr,
         Binding_Layout_Addr => Binding_Layout_Addr);
      Child := Ghdl_Object_Rtii'(
         Is_Sig => Rtii.Is_Sig,
         Typ => El_Type,
         Name => El_Name,
         Base_Rti => Rtii.Base_Rti,
         Addr => Child_Addr);
      return Child;
   end Get_Array_Child;

   function Get_Record_Nbr_Children (Rtii : Ghdl_Object_Rtii)
                                    return Ghdl_Index_Type is
   begin
      return Rti_Record_Base_Type(Rtii.Typ.Rti).Nbrel;
   end Get_Record_Nbr_Children;

   function Get_Record_Child (Rtii : Ghdl_Object_Rtii; Index : Ghdl_Index_Type)
                             return Ghdl_Object_Rtii is
      Base_Record : Ghdl_Rtin_Type_Record_Acc;
      El_Type : Ghdl_Type_Rtii;
      Child_Addr : Address;
      El_Rti : Ghdl_Rtin_Element_Acc;
      El_Layout_Addr : Address;
      El_Name : Ghdl_Object_Rtii_Name;
      Type_Layout_Addr : Address;
      Child : Ghdl_Object_Rtii;
   begin
      Base_Record := Rti_Record_Base_Type(Rtii.Typ.Rti);
      if Index >= Base_Record.Nbrel then
         Internal_Error("get_record_child");
      else
         El_Rti := To_Ghdl_Rtin_Element_Acc(Base_Record.Elements(Index));
         El_Layout_Addr := Rtii.Typ.Layout_Addr + El_Rti.Layout_Off;
         Type_Layout_Addr := Get_Type_Layout(El_Rti.Eltype, Rtii.Typ.Ctxt);
         El_Type := Ghdl_Type_Rtii'(
            Rti => El_Rti.Eltype,
            Ctxt => Rtii.Typ.Ctxt,
            Layout_Addr => Type_Layout_Addr,
            Binding_Layout_Addr => El_Layout_Addr);
         El_Name := Rtii.Name;
         El_Name := Append_To_Name(El_Name, ".");
         El_Name := Append_To_Name(El_Name, El_Rti.Name);
         --  FIXME : We need to deal with complex and simple layouts.
         if Rtii.Is_Sig then
            Child_Addr := Rtii.Addr + El_Rti.Sig_Off;
         else
            Child_Addr := Rtii.Addr + El_Rti.Val_Off;
         end if;
         Child := Ghdl_Object_Rtii'(
            Is_Sig => Rtii.Is_Sig,
            Typ => El_Type,
            Name => El_Name,
            Base_Rti => Rtii.Base_Rti,
            Addr => Child_Addr);
      end if;
      return Child;
   end Get_Record_Child;

   --  Get an indexed child of an Rtii.
   function Get_Rtii_Child (Rtii : Ghdl_Object_Rtii; Index : Ghdl_Index_Type)
                           return Ghdl_Object_Rtii is
      Kind : Ghdl_Rtik;
      Child : Ghdl_Object_Rtii;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Record |
              Ghdl_Rtik_Subtype_Record =>
            Child := Get_Record_Child(Rtii, Index);
         when Ghdl_Rtik_Subtype_Array |
              Ghdl_Rtik_Type_Array =>
            Child := Get_Array_Child(Rtii, Index);
         when others =>
            Internal_Error("get_rtii_child");
      end case;
      return Child;
   end Get_Rtii_Child;

   --  Get number of children in a Rtii.
   function Get_Rtii_Nbr_Children (Rtii : Ghdl_Object_Rtii)
                                  return Ghdl_Index_Type is
      Kind : Ghdl_Rtik;
      Nbr_Children : Ghdl_Index_Type;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Record |
              Ghdl_Rtik_Subtype_Record =>
            Nbr_Children := Get_Record_Nbr_Children(Rtii);
         when Ghdl_Rtik_Subtype_Array |
              Ghdl_Rtik_Type_Array =>
            Nbr_Children := Get_Array_Nbr_Children(Rtii);
         when others =>
            Internal_Error("get_rtii_nbr_children");
      end case;
      return Nbr_Children;

   end Get_Rtii_Nbr_Children;

   function Is_Bound (Rti : Ghdl_Rti_Access) return Boolean is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Type_Unbounded_Record
           | Ghdl_Rtik_Subtype_Unbounded_Record
           | Ghdl_Rtik_Type_Array =>
            return False;
         when others =>
            return True;
      end case;
   end Is_Bound;

   function To_Ghdl_Object_Rtii (Rti : Ghdl_Rtin_Object_Acc;
                                 Ctxt : Rti_Context)
                                return Ghdl_Object_Rtii is
      Is_Sig : Boolean;
      Addr : Address;
      Layout_Addr : Address;
      Binding_Layout_Addr : Address;
      Typ : Ghdl_Type_Rtii;
      Rtii : Ghdl_Object_Rtii;
   begin
      case Rti.Common.Kind is
         when Ghdl_Rtik_Port |
              Ghdl_Rtik_Signal =>
            Is_Sig := True;
         when Ghdl_Rtik_Constant |
              Ghdl_Rtik_Generic =>
            Is_Sig := False;
         when Ghdl_Rtik_Variable =>
            Internal_Error("to_ghdl_rtii : Variable not supported");
         when Ghdl_Rtik_Iterator =>
            --  Assume this is a the generate variable in a for/generate
            --  loop. Treat is as a constant.
            Is_Sig := False;
         when Ghdl_Rtik_Guard
           | Ghdl_Rtik_Attribute_Quiet
           | Ghdl_Rtik_Attribute_Stable
           | Ghdl_Rtik_Attribute_Transaction
           | Ghdl_Rtik_File =>
            Internal_Error("to_ghdl_rtii : Unimplemented Kind");
         when others =>
            Internal_Error("to_ghdl_rtii : Unknown Kind");
      end case;
      Addr := Loc_To_Addr(Depth => Rti.Common.Depth,
                          Loc => Rti.Loc,
                          Ctxt => Ctxt);
      if Is_Bound(Rti.Obj_Type) then
         Layout_Addr := Get_Type_Layout(Rti.Obj_Type, Ctxt);
         Binding_Layout_Addr := Null_Address;
      else
         Layout_Addr := Null_Address;
         Binding_Layout_Addr := Addr;
      end if;
      Typ := (
         Rti => Rti.Obj_Type,
         Ctxt => Ctxt,
         Layout_Addr => Layout_Addr,
         Binding_Layout_Addr => Binding_Layout_Addr);
      Rtii := (
         Is_Sig => Is_Sig,
         Base_Rti => Rti,
         Typ => Typ,
         Name => New_Name(Rti.Name),
         Addr => Addr);
      return Rtii;
   end To_Ghdl_Object_Rtii;

   function Is_Record (Rtii : Ghdl_Object_Rtii) return Boolean is
   begin
      case Rtii.Typ.Rti.Kind is
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Subtype_Record
           | Ghdl_Rtik_Type_Unbounded_Record
           | Ghdl_Rtik_Subtype_Unbounded_Record =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Record;

   function Is_Array (Rtii : Ghdl_Object_Rtii) return Boolean is
   begin
      case Rtii.Typ.Rti.Kind is
         when Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Array =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Array;

   function Get_Bit_Size (Rtii : Ghdl_Object_Rtii) return Ghdl_Index_Type is
   begin
      case Rtii.Typ.Rti.Kind is
         -- For records we sum the bit sizes of all the elements.
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Subtype_Record =>
            declare
               Size : Ghdl_Index_Type := 0;
               N_Els : Ghdl_Index_Type;
               Child : Ghdl_Object_Rtii;
            begin
               N_Els := Get_Size(Rtii);
               for El_Index in 0 .. N_Els-1 loop
                  Child := Get_Rtii_Child(Rtii, El_Index);
                  Size := Size + Get_Bit_Size(Child);
               end loop;
               return Size;
            end;
         --  For arrays we find the bit size of a single element and then
         --  multiply by the number of elements.
         when Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Array =>
            declare
               Size : Ghdl_Index_Type;
               Child : Ghdl_Object_Rtii;
               Child_Bit_Size : Ghdl_Index_Type;
            begin
               Size := Get_Size(Rtii);
               if Size = 0 then
                  return 0;
               else
                  Child := Get_Rtii_Child(Rtii, 0);
                  Child_Bit_Size := Get_Bit_Size(Child);
                  return Size * Child_Bit_Size;
               end if;
            end;
         when others =>
            return Get_Size(Rtii);
      end case;
   end Get_Bit_Size;

   function Get_Size (Rtii : Ghdl_Object_Rtii) return Ghdl_Index_Type is
   begin
      case Rtii.Typ.Rti.Kind is
         --  For records and arrays the size is the number of elements.
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Subtype_Record
           | Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Array =>
            return Get_Rtii_Nbr_Children(Rtii);
         --  Otherwise it's just the number of bits used.
         when Ghdl_Rtik_Type_B1 =>
            return 1;
         when Ghdl_Rtik_Type_E8 =>
            return 8;
         when Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_P32
           | Ghdl_Rtik_Type_I32 =>
            return 32;
         when Ghdl_Rtik_Type_I64
           | Ghdl_Rtik_Type_F64
           | Ghdl_Rtik_Type_P64 =>
            return 64;
         when others =>
            Internal_Error("get_size: unknown type kind");
      end case;
   end Get_Size;

end Grt.Rtiis;
