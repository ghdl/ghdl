--  Values in synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Ada.Unchecked_Conversion;
with System; use System;

with Mutils; use Mutils;

package body Elab.Vhdl_Objtypes is
   function To_Rec_El_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Rec_El_Array_Acc);

   function To_Type_Acc is new Ada.Unchecked_Conversion
     (System.Address, Type_Acc);

   function "+" (L, R : Value_Offsets) return Value_Offsets is
   begin
      return (L.Net_Off + R.Net_Off, L.Mem_Off + R.Mem_Off);
   end "+";

   function Is_Bounded_Type (Typ : Type_Acc) return Boolean is
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float
           | Type_Vector
           | Type_Slice
           | Type_Array
           | Type_Record
           | Type_Access
           | Type_File =>
            return True;
         when Type_Unbounded_Array
           | Type_Unbounded_Vector
           | Type_Unbounded_Record
           | Type_Protected =>
            return False;
      end case;
   end Is_Bounded_Type;

   function Are_Types_Equal (L, R : Type_Acc) return Boolean is
   begin
      if L.Kind /= R.Kind
        or else L.W /= R.W
      then
         return False;
      end if;
      if L = R then
         return True;
      end if;

      case L.Kind is
         when Type_Bit
           | Type_Logic =>
            return True;
         when Type_Discrete =>
            return L.Drange = R.Drange;
         when Type_Float =>
            return L.Frange = R.Frange;
         when Type_Array
           | Type_Vector =>
            if L.Alast /= R.Alast then
               return False;
            end if;
            if L.Abound /= R.Abound then
               return False;
            end if;
            return Are_Types_Equal (L.Arr_El, R.Arr_El);
         when Type_Unbounded_Array
           | Type_Unbounded_Vector =>
            if L.Ulast /= R.Ulast then
               return False;
            end if;
            --  Also check index ?
            return Are_Types_Equal (L.Uarr_El, R.Uarr_El);
         when Type_Slice =>
            return Are_Types_Equal (L.Slice_El, R.Slice_El);
         when Type_Record
           | Type_Unbounded_Record =>
            if L.Rec.Len /= R.Rec.Len then
               return False;
            end if;
            for I in L.Rec.E'Range loop
               if not Are_Types_Equal (L.Rec.E (I).Typ, R.Rec.E (I).Typ) then
                  return False;
               end if;
            end loop;
            return True;
         when Type_Access =>
            return Are_Types_Equal (L.Acc_Acc, R.Acc_Acc);
         when Type_File =>
            return Are_Types_Equal (L.File_Typ, R.File_Typ);
         when Type_Protected =>
            return False;
      end case;
   end Are_Types_Equal;

   function Is_Last_Dimension (Arr : Type_Acc) return Boolean is
   begin
      case Arr.Kind is
         when Type_Vector
           | Type_Array =>
            return Arr.Alast;
         when Type_Unbounded_Vector =>
            return True;
         when Type_Unbounded_Array =>
            return Arr.Ulast;
         when others =>
            raise Internal_Error;
      end case;
   end Is_Last_Dimension;

   function Is_Null_Range (Rng : Discrete_Range_Type) return Boolean is
   begin
      case Rng.Dir is
         when Dir_To =>
            return Rng.Left > Rng.Right;
         when Dir_Downto =>
            return Rng.Left < Rng.Right;
      end case;
   end Is_Null_Range;

   function Is_Scalar_Subtype_Compatible (L, R : Type_Acc) return Boolean is
   begin
      pragma Assert (L.Kind = R.Kind);
      case L.Kind is
         when Type_Bit
           | Type_Logic =>
            --  We have no bounds for that...
            return True;
         when Type_Discrete =>
            if Is_Null_Range (L.Drange) then
               return True;
            end if;
            return In_Range (R.Drange, L.Drange.Left)
              and then In_Range (R.Drange, L.Drange.Right);
         when Type_Float =>
            return L.Frange = R.Frange;
         when others =>
            raise Internal_Error;
      end case;
   end Is_Scalar_Subtype_Compatible;

   function Discrete_Range_Width (Rng : Discrete_Range_Type) return Uns32
   is
      Lo, Hi : Int64;
      W : Uns32;
   begin
      case Rng.Dir is
         when Dir_To =>
            Lo := Rng.Left;
            Hi := Rng.Right;
         when Dir_Downto =>
            Lo := Rng.Right;
            Hi := Rng.Left;
      end case;
      if Lo > Hi then
         --  Null range.
         W := 0;
      elsif Lo >= 0 then
         --  Positive.
         W := Uns32 (Clog2 (Uns64 (Hi) + 1));
      elsif Lo = Int64'First then
         --  Handle possible overflow.
         W := 64;
      elsif Hi < 0 then
         --  Negative only.
         W := Uns32 (Clog2 (Uns64 (-Lo))) + 1;
      else
         declare
            Wl : constant Uns32 := Uns32 (Clog2 (Uns64 (-Lo)));
            Wh : constant Uns32 := Uns32 (Clog2 (Uns64 (Hi) + 1));
         begin
            W := Uns32'Max (Wl, Wh) + 1;
         end;
      end if;
      return W;
   end Discrete_Range_Width;

   function In_Bounds (Bnd : Bound_Type; V : Int32) return Boolean is
   begin
      case Bnd.Dir is
         when Dir_To =>
            return V >= Bnd.Left and then V <= Bnd.Right;
         when Dir_Downto =>
            return V <= Bnd.Left and then V >= Bnd.Right;
      end case;
   end In_Bounds;

   function In_Range (Rng : Discrete_Range_Type; V : Int64) return Boolean is
   begin
      case Rng.Dir is
         when Dir_To =>
            return V >= Rng.Left and then V <= Rng.Right;
         when Dir_Downto =>
            return V <= Rng.Left and then V >= Rng.Right;
      end case;
   end In_Range;

   function Build_Discrete_Range_Type
     (L : Int64; R : Int64; Dir : Direction_Type) return Discrete_Range_Type is
   begin
      return (Dir => Dir,
              Left => L,
              Right => R,
              Is_Signed => L < 0 or R < 0);
   end Build_Discrete_Range_Type;

   procedure Realign (Res : in out Size_Type;
                      Align : Size_Type) is
   begin
      Res := (Res + Align - 1) and not (Align - 1);
   end Realign;

   --  For Compute_Size_Type.
   procedure Add_Size_Type (Typ : Type_Acc;
                            Sz : in out Size_Type;
                            Align : in out Size_Type);

   procedure Add_Array_Size_Type (El_Typ : Type_Acc;
                                  Sz : in out Size_Type;
                                  Align : in out Size_Type)
   is
      subtype T is Type_Type (Type_Array);
   begin
      Align := Size_Type'Max (Align, T'Alignment);
      Realign (Sz, Align);
      Sz := Sz + (T'Size / System.Storage_Unit);
      Add_Size_Type (El_Typ, Sz, Align);
   end Add_Array_Size_Type;

   procedure Add_Size_Type (Typ : Type_Acc;
                            Sz : in out Size_Type;
                            Align : in out Size_Type) is
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            --  Never copied.
            return;
         when Type_Access
           | Type_File
           | Type_Protected =>
            --  Never copied
            return;
         when Type_Array
           | Type_Vector =>
            Add_Array_Size_Type (Typ.Arr_El, Sz, Align);
         when Type_Unbounded_Array
           | Type_Unbounded_Vector =>
            Add_Array_Size_Type (Typ.Uarr_El, Sz, Align);
         when Type_Record
           | Type_Unbounded_Record =>
            declare
               subtype T is Type_Type (Type_Record);
               subtype T_El is Rec_El_Array (Typ.Rec.Len);
            begin
               --  The type
               Align := Size_Type'Max (Align, T'Alignment);
               Realign (Sz, Align);
               Sz := Sz + (T'Size / System.Storage_Unit);
               --  The el array
               Align := Size_Type'Max (Align, T_El'Alignment);
               Realign (Sz, Align);
               Sz := Sz + (T_El'Size / System.Storage_Unit);
               --  The elements
               for I in Typ.Rec.E'Range loop
                  Add_Size_Type (Typ.Rec.E (I).Typ, Sz, Align);
               end loop;
            end;
         when Type_Slice =>
            raise Internal_Error;
      end case;
   end Add_Size_Type;

   --  Compute the memory size needed to store T.
   function Compute_Size_Type (T : Type_Acc) return Size_Type
   is
      Align : Size_Type;
      Size : Size_Type;
   begin
      Size := 0;
      Align := 1;
      Add_Size_Type (T, Size, Align);
      return Size;
   end Compute_Size_Type;

   function Create_Bit_Type return Type_Acc
   is
      subtype Bit_Type_Type is Type_Type (Type_Bit);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Bit_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Bit,
                                                Wkind => Wkind_Net,
                                                Drange => (Left => 0,
                                                           Right => 1,
                                                           Dir => Dir_To,
                                                           Is_Signed => False),
                                                Al => 0,
                                                Is_Global => False,
                                                Sz => 1,
                                                W => 1)));
   end Create_Bit_Type;

   function Create_Logic_Type return Type_Acc
   is
      subtype Logic_Type_Type is Type_Type (Type_Logic);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Logic_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Logic,
                                                Wkind => Wkind_Net,
                                                Drange => (Left => 0,
                                                           Right => 8,
                                                           Dir => Dir_To,
                                                           Is_Signed => False),
                                                Al => 0,
                                                Is_Global => False,
                                                Sz => 1,
                                                W => 1)));
   end Create_Logic_Type;

   function Create_Discrete_Type (Rng : Discrete_Range_Type;
                                  Sz : Size_Type;
                                  W : Uns32)
                                 return Type_Acc
   is
      subtype Discrete_Type_Type is Type_Type (Type_Discrete);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Discrete_Type_Type);
      Al : Palign_Type;
   begin
      if Sz <= 1 then
         Al := 0;
      elsif Sz <= 4 then
         Al := 2;
      else
         pragma Assert (Sz <= 8);
         Al := 3;
      end if;
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Discrete,
                                                Wkind => Wkind_Net,
                                                Al => Al,
                                                Is_Global => False,
                                                Sz => Sz,
                                                W => W,
                                                Drange => Rng)));
   end Create_Discrete_Type;

   function Create_Float_Type (Rng : Float_Range_Type) return Type_Acc
   is
      subtype Float_Type_Type is Type_Type (Type_Float);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Float_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Float,
                                                Wkind => Wkind_Net,
                                                Al => 3,
                                                Is_Global => False,
                                                Sz => 8,
                                                W => 64,
                                                Frange => Rng)));
   end Create_Float_Type;

   function Create_Vector_Type (Bnd : Bound_Type; El_Type : Type_Acc)
                               return Type_Acc
   is
      subtype Vector_Type_Type is Type_Type (Type_Vector);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Vector_Type_Type);
   begin
      pragma Assert (El_Type.Kind in Type_Nets);
      return To_Type_Acc
        (Alloc (Current_Pool, (Kind => Type_Vector,
                               Wkind => El_Type.Wkind,
                               Al => El_Type.Al,
                               Is_Global => False,
                               Sz => El_Type.Sz * Size_Type (Bnd.Len),
                               W => Bnd.Len,
                               Alast => True,
                               Abound => Bnd,
                               Arr_El => El_Type)));
   end Create_Vector_Type;

   function Create_Slice_Type (Len : Uns32; El_Type : Type_Acc)
                              return Type_Acc
   is
      subtype Slice_Type_Type is Type_Type (Type_Slice);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Slice_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool,
                                 (Kind => Type_Slice,
                                  Wkind => El_Type.Wkind,
                                  Al => El_Type.Al,
                                  Is_Global => False,
                                  Sz => Size_Type (Len) * El_Type.Sz,
                                  W => Len * El_Type.W,
                                  Slice_El => El_Type)));
   end Create_Slice_Type;

   function Create_Vec_Type_By_Length (Len : Uns32; El : Type_Acc)
                                      return Type_Acc is
   begin
      return Create_Vector_Type ((Dir => Dir_Downto,
                                  Left => Int32 (Len) - 1,
                                  Right => 0,
                                  Len => Len),
                                 El);
   end Create_Vec_Type_By_Length;

   function Create_Array_Type
     (Bnd : Bound_Type; Last : Boolean; El_Type : Type_Acc) return Type_Acc
   is
      subtype Array_Type_Type is Type_Type (Type_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Array_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool,
                                 (Kind => Type_Array,
                                  Wkind => El_Type.Wkind,
                                  Al => El_Type.Al,
                                  Is_Global => False,
                                  Sz => El_Type.Sz * Size_Type (Bnd.Len),
                                  W => El_Type.W * Bnd.Len,
                                  Abound => Bnd,
                                  Alast => Last,
                                  Arr_El => El_Type)));
   end Create_Array_Type;

   function Create_Unbounded_Array
     (Idx : Type_Acc; Last : Boolean; El_Type : Type_Acc) return Type_Acc
   is
      subtype Unbounded_Type_Type is Type_Type (Type_Unbounded_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Unbounded_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Unbounded_Array,
                                                Wkind => El_Type.Wkind,
                                                Al => El_Type.Al,
                                                Is_Global => False,
                                                Sz => 0,
                                                W => 0,
                                                Ulast => Last,
                                                Uarr_El => El_Type,
                                                Uarr_Idx => Idx)));
   end Create_Unbounded_Array;

   function Create_Unbounded_Vector (El_Type : Type_Acc; Idx : Type_Acc)
                                    return Type_Acc
   is
      subtype Unbounded_Type_Type is Type_Type (Type_Unbounded_Vector);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Unbounded_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Unbounded_Vector,
                                                Wkind => El_Type.Wkind,
                                                Al => El_Type.Al,
                                                Is_Global => False,
                                                Sz => 0,
                                                W => 0,
                                                Ulast => True,
                                                Uarr_El => El_Type,
                                                Uarr_Idx => Idx)));
   end Create_Unbounded_Vector;

   function Get_Array_Element (Arr_Type : Type_Acc) return Type_Acc is
   begin
      case Arr_Type.Kind is
         when Type_Vector
           | Type_Array =>
            return Arr_Type.Arr_El;
         when Type_Unbounded_Array
           | Type_Unbounded_Vector =>
            return Arr_Type.Uarr_El;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Element;

   function Get_Array_Bound (Typ : Type_Acc) return Bound_Type is
   begin
      case Typ.Kind is
         when Type_Vector
           | Type_Array =>
            return Typ.Abound;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Bound;

   function Get_Uarray_Index (Typ : Type_Acc) return Type_Acc is
   begin
      case Typ.Kind is
         when Type_Unbounded_Vector
           | Type_Unbounded_Array =>
            return Typ.Uarr_Idx;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Uarray_Index;

   function Get_Range_Length (Rng : Discrete_Range_Type) return Uns32
   is
      Len : Int64;
   begin
      case Rng.Dir is
         when Dir_To =>
            Len := Rng.Right - Rng.Left + 1;
         when Dir_Downto =>
            Len := Rng.Left - Rng.Right + 1;
      end case;
      if Len < 0 then
         return 0;
      else
         return Uns32 (Len);
      end if;
   end Get_Range_Length;

   function Create_Rec_El_Array (Nels : Iir_Index32; Pool : Areapool_Acc)
                                return Rec_El_Array_Acc
   is
      subtype Data_Type is Rec_El_Array (Nels);
      Res : Address;
   begin
      --  Manually allocate the array to handle large arrays without
      --  creating a large temporary value.
      Areapools.Allocate
        (Pool.all, Res,
         Data_Type'Size / Storage_Unit, Data_Type'Alignment);

      declare
         --  Discard the warnings for no pragma Import as we really want
         --  to use the default initialization.
         pragma Warnings (Off);
         Addr1 : constant Address := Res;
         Init : Data_Type;
         for Init'Address use Addr1;
         pragma Warnings (On);
      begin
         null;
      end;

      return To_Rec_El_Array_Acc (Res);
   end Create_Rec_El_Array;

   function Create_Rec_El_Array (Nels : Iir_Index32) return Rec_El_Array_Acc is
   begin
      return Create_Rec_El_Array (Nels, Current_Pool);
   end Create_Rec_El_Array;

   function Align (Off : Size_Type; Al : Palign_Type) return Size_Type
   is
      Mask : constant Size_Type := 2 ** Natural (Al) - 1;
   begin
      return (Off + Mask) and not Mask;
   end Align;

   function Create_Record_Type (Els : Rec_El_Array_Acc) return Type_Acc
   is
      subtype Record_Type_Type is Type_Type (Type_Record);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Record_Type_Type);
      Wkind : Wkind_Type;
      W : Uns32;
      Al : Palign_Type;
      Sz : Size_Type;
   begin
      --  Layout the record.
      Wkind := Wkind_Net;
      Al := 0;
      Sz := 0;
      W := 0;
      for I in Els.E'Range loop
         declare
            E : Rec_El_Type renames Els.E (I);
         begin
            --  For nets.
            E.Offs.Net_Off := W;
            if E.Typ.Wkind /= Wkind_Net then
               Wkind := Wkind_Undef;
            end if;
            W := W + E.Typ.W;

            --  For memory.
            Al := Palign_Type'Max (Al, E.Typ.Al);
            Sz := Align (Sz, E.Typ.Al);
            E.Offs.Mem_Off := Sz;
            Sz := Sz + E.Typ.Sz;
         end;
      end loop;
      Sz := Align (Sz, Al);

      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Record,
                                                Wkind => Wkind,
                                                Al => Al,
                                                Is_Global => False,
                                                Sz => Sz,
                                                W => W,
                                                Rec => Els)));
   end Create_Record_Type;

   function Create_Unbounded_Record (Els : Rec_El_Array_Acc) return Type_Acc
   is
      subtype Unbounded_Record_Type_Type is Type_Type (Type_Unbounded_Record);
      function Alloc is
         new Areapools.Alloc_On_Pool_Addr (Unbounded_Record_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Unbounded_Record,
                                                Wkind => Wkind_Net,
                                                Al => 0,
                                                Is_Global => False,
                                                Sz => 0,
                                                W => 0,
                                                Rec => Els)));
   end Create_Unbounded_Record;

   function Create_Access_Type (Acc_Type : Type_Acc) return Type_Acc
   is
      subtype Access_Type_Type is Type_Type (Type_Access);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Access_Type_Type);
      Bnd_Sz : Size_Type;
   begin
      if Acc_Type = null then
         --  For incomplete type.
         Bnd_Sz := 0;
      else
         Bnd_Sz := Compute_Size_Type (Acc_Type);
      end if;
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Access,
                                                Wkind => Wkind_Sim,
                                                Al => 2,
                                                Is_Global => False,
                                                Sz => 4,
                                                W => 1,
                                                Acc_Acc => Acc_Type,
                                                Acc_Bnd_Sz => Bnd_Sz)));
   end Create_Access_Type;

   procedure Complete_Access_Type (Acc_Type : Type_Acc; Des_Typ : Type_Acc) is
   begin
      Acc_Type.Acc_Acc := Des_Typ;
      Acc_Type.Acc_Bnd_Sz := Compute_Size_Type (Des_Typ);
   end Complete_Access_Type;

   function Create_File_Type (File_Type : Type_Acc) return Type_Acc
   is
      subtype File_Type_Type is Type_Type (Type_File);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (File_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_File,
                                                Wkind => Wkind_Sim,
                                                Al => 2,
                                                Is_Global => False,
                                                Sz => 4,
                                                W => 1,
                                                File_Typ => File_Type,
                                                File_Signature => null)));
   end Create_File_Type;

   function Create_Protected_Type return Type_Acc
   is
      subtype Protected_Type_Type is Type_Type (Type_Protected);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Protected_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Protected,
                                                Wkind => Wkind_Sim,
                                                Al => 2,
                                                Is_Global => False,
                                                Sz => 4,
                                                W => 1)));
   end Create_Protected_Type;

   function Vec_Length (Typ : Type_Acc) return Iir_Index32 is
   begin
      return Iir_Index32 (Typ.Abound.Len);
   end Vec_Length;

   function Get_Array_Flat_Length (Typ : Type_Acc) return Iir_Index32 is
   begin
      case Typ.Kind is
         when Type_Vector =>
            return Iir_Index32 (Typ.Abound.Len);
         when Type_Array =>
            declare
               Len : Uns32;
               T : Type_Acc;
            begin
               Len := 1;
               T := Typ;
               loop
                  Len := Len * T.Abound.Len;
                  exit when T.Alast;
                  T := T.Arr_El;
               end loop;
               return Iir_Index32 (Len);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Flat_Length;

   function Get_Type_Width (Atype : Type_Acc) return Uns32 is
   begin
      pragma Assert (Atype.Kind /= Type_Unbounded_Array);
      return Atype.W;
   end Get_Type_Width;

   function Get_Bound_Length (T : Type_Acc) return Uns32 is
   begin
      case T.Kind is
         when Type_Vector
           | Type_Array =>
            return T.Abound.Len;
         when Type_Slice =>
            return T.W;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Bound_Length;

   function Is_Matching_Bounds (L, R : Type_Acc) return Boolean is
   begin
      case L.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            pragma Assert (L.Kind = R.Kind);
            return True;
         when Type_Vector
           | Type_Slice =>
            return Get_Bound_Length (L) = Get_Bound_Length (R);
         when Type_Array =>
            pragma Assert (L.Alast = R.Alast);
            if Get_Bound_Length (L) /= Get_Bound_Length (R) then
               return False;
            end if;
            if L.Alast then
               return True;
            end if;
            return Get_Bound_Length (L.Arr_El) = Get_Bound_Length (R.Arr_El);
         when Type_Unbounded_Array
           | Type_Unbounded_Vector
           | Type_Unbounded_Record =>
            raise Internal_Error;
         when Type_Record =>
            --  FIXME: handle vhdl-08
            return True;
         when Type_Access =>
            return True;
         when Type_File
           |  Type_Protected =>
            raise Internal_Error;
      end case;
   end Is_Matching_Bounds;

   function Read_U8 (Mt : Memtyp) return Ghdl_U8
   is
      pragma Assert (Mt.Typ.Sz = 1);
   begin
      return Read_U8 (Mt.Mem);
   end Read_U8;


   function Read_Fp64 (Mt : Memtyp) return Fp64 is
   begin
      return Read_Fp64 (Mt.Mem);
   end Read_Fp64;

   function Read_Discrete (Mem : Memory_Ptr; Typ : Type_Acc) return Int64 is
   begin
      case Typ.Sz is
         when 1 =>
            return Int64 (Read_U8 (Mem));
         when 4 =>
            return Int64 (Read_I32 (Mem));
         when 8 =>
            return Int64 (Read_I64 (Mem));
         when others =>
            raise Internal_Error;
      end case;
   end Read_Discrete;

   function Read_Discrete (Mt : Memtyp) return Int64 is
   begin
      return Read_Discrete (Mt.Mem, Mt.Typ);
   end Read_Discrete;

   procedure Write_Discrete (Mem : Memory_Ptr; Typ : Type_Acc; Val : Int64) is
   begin
      case Typ.Sz is
         when 1 =>
            Write_U8 (Mem, Ghdl_U8 (Val));
         when 4 =>
            Write_I32 (Mem, Ghdl_I32 (Val));
         when 8 =>
            Write_I64 (Mem, Ghdl_I64 (Val));
         when others =>
            raise Internal_Error;
      end case;
   end Write_Discrete;

   function Alloc_Memory (Sz : Size_Type;
                          Align2 : Natural;
                          Pool : Areapool_Acc) return Memory_Ptr
   is
      function To_Memory_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Memory_Ptr);
      M : System.Address;
   begin
      Areapools.Allocate (Pool.all, M, Sz, Size_Type (2 ** Align2));
      return To_Memory_Ptr (M);
   end Alloc_Memory;

   function Alloc_Memory (Vtype : Type_Acc; Pool : Areapool_Acc)
                         return Memory_Ptr is
   begin
      return Alloc_Memory (Vtype.Sz, Natural (Vtype.Al), Pool);
   end Alloc_Memory;

   function Create_Memory (Vtype : Type_Acc) return Memtyp is
   begin
      return (Vtype, Alloc_Memory (Vtype, Current_Pool));
   end Create_Memory;

   function Create_Memory_Zero (Vtype : Type_Acc) return Memtyp
   is
      Mem : Memory_Ptr;
   begin
      Mem := Alloc_Memory (Vtype, Current_Pool);
      for I in 1 .. Vtype.Sz loop
         Write_U8 (Mem + (I - 1), 0);
      end loop;
      return (Vtype, Mem);
   end Create_Memory_Zero;

   function Create_Memory_U8 (Val : Ghdl_U8; Vtype : Type_Acc)
                             return Memtyp
   is
      pragma Assert (Vtype.Sz = 1);
      Res : Memory_Ptr;
   begin
      Res := Alloc_Memory (Vtype, Current_Pool);
      Write_U8 (Res, Val);
      return (Vtype, Res);
   end Create_Memory_U8;

   function Create_Memory_Fp64 (Val : Fp64; Vtype : Type_Acc)
                               return Memtyp
   is
      pragma Assert (Vtype.Sz = 8);
      Res : Memory_Ptr;
   begin
      Res := Alloc_Memory (Vtype, Current_Pool);
      Write_Fp64 (Res, Val);
      return (Vtype, Res);
   end Create_Memory_Fp64;

   function Create_Memory_Discrete (Val : Int64; Vtype : Type_Acc)
                                   return Memtyp
   is
      Res : Memory_Ptr;
   begin
      Res := Alloc_Memory (Vtype, Current_Pool);
      case Vtype.Sz is
         when 1 =>
            Write_U8 (Res, Ghdl_U8 (Val));
         when 4 =>
            Write_I32 (Res, Ghdl_I32 (Val));
         when 8 =>
            Write_I64 (Res, Ghdl_I64 (Val));
         when others =>
            raise Internal_Error;
      end case;
      return (Vtype, Res);
   end Create_Memory_Discrete;

   function Create_Memory_U32 (Val : Uns32) return Memtyp
   is
      Res : Memory_Ptr;
   begin
      Res := Alloc_Memory (4, 2, Current_Pool);
      Write_U32 (Res, Ghdl_U32 (Val));
      return (null, Res);
   end Create_Memory_U32;

   function Is_Equal (L, R : Memtyp) return Boolean is
   begin
      if L = R then
         return True;
      end if;

      if L.Typ.Sz /= R.Typ.Sz then
         return False;
      end if;

      case L.Typ.Kind is
         when Type_Bit
           | Type_Logic =>
            return L.Mem (0) = R.Mem (0);
         when Type_Discrete =>
            return Read_Discrete (L.Mem, L.Typ) = Read_Discrete (R.Mem, R.Typ);
         when Type_Float =>
            return Read_Fp64 (L.Mem) = Read_Fp64 (R.Mem);
         when Type_Vector =>
            pragma Assert (L.Typ.Arr_El.Sz = 1);
            for I in 1 .. Size_Type (L.Typ.Abound.Len) loop
               if L.Mem (I - 1) /= R.Mem (I - 1) then
                  return False;
               end if;
            end loop;
            return True;
         when Type_Array =>
            declare
               Etl, Etr : Type_Acc;
               Len : Uns32;
               Off : Size_Type;
            begin
               Len := 1;
               Etl := L.Typ;
               Etr := R.Typ;
               loop
                  if Etl.Abound.Len /= Etr.Abound.Len then
                     return False;
                  end if;
                  Len := Len * Etl.Abound.Len;
                  exit when Etl.Alast;
                  Etl := Etl.Arr_El;
                  Etr := Etr.Arr_El;
               end loop;
               Etl := Etl.Arr_El;
               Etr := Etr.Arr_El;
               Off := 0;
               for I in 1 .. Len loop
                  if not Is_Equal ((Etl, L.Mem + Off),
                                   (Etr, R.Mem + Off))
                  then
                     return False;
                  end if;
                  Off := Off + Etl.Sz;
               end loop;
               return True;
            end;
         when Type_Record =>
            for I in L.Typ.Rec.E'Range loop
               declare
                  El : Rec_El_Type renames L.Typ.Rec.E (I);
               begin
                  if not Is_Equal ((El.Typ, L.Mem + El.Offs.Mem_Off),
                                   (El.Typ, R.Mem + El.Offs.Mem_Off))
                  then
                     return False;
                  end if;
               end;
            end loop;
            return True;
         when Type_Access =>
            pragma Assert (L.Typ.Sz = 4);
            return Read_U32 (L.Mem) = Read_U32 (R.Mem);
         when Type_Slice =>
            raise Internal_Error;
         when Type_Unbounded_Vector
           | Type_Unbounded_Array
           | Type_Unbounded_Record
           | Type_Protected
           | Type_File =>
            raise Internal_Error;
      end case;
   end Is_Equal;

   procedure Copy_Memory (Dest : Memory_Ptr; Src : Memory_Ptr; Sz : Size_Type)
   is
   begin
      for I in 1 .. Sz loop
         Dest (I - 1) := Src (I - 1);
      end loop;
   end Copy_Memory;

   function Unshare (Src : Memtyp; Pool : Areapool_Acc) return Memtyp
   is
      Res : Memory_Ptr;
   begin
      Res := Alloc_Memory (Src.Typ, Pool);
      Copy_Memory (Res, Src.Mem, Src.Typ.Sz);
      return (Src.Typ, Res);
   end Unshare;

   function Unshare (Src : Memtyp) return Memtyp
   is
      Res : Memory_Ptr;
   begin
      Res := Alloc_Memory (Src.Typ, Current_Pool);
      Copy_Memory (Res, Src.Mem, Src.Typ.Sz);
      return (Src.Typ, Res);
   end Unshare;

   function Raw_Copy (T : Type_Acc; Pool : Areapool_Acc) return Type_Acc
   is
      Addr : System.Address;
      Sz : Size_Type;
   begin
      Sz := T.all'Size / Storage_Unit;
      Allocate (Pool.all, Addr, Sz, T.all'Alignment);
      Copy_Memory (To_Memory_Ptr (Addr), To_Memory_Ptr (T.all'Address), Sz);
      return To_Type_Acc (Addr);
   end Raw_Copy;

   function Unshare (T : Type_Acc; Pool : Areapool_Acc) return Type_Acc
   is
      Res : Type_Acc;
   begin
      if T.Is_Global then
         return T;
      end if;

      Res := Raw_Copy (T, Pool);
      Res.Is_Global := True;

      case Res.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            null;
         when Type_Slice =>
            Res.Slice_El := Unshare (T.Slice_El, Pool);
         when Type_Array
           | Type_Vector =>
            Res.Arr_El := Unshare (T.Arr_El, Pool);
         when Type_Unbounded_Array
           | Type_Unbounded_Vector =>
            Res.Uarr_El := Unshare (T.Uarr_El, Pool);
            Res.Uarr_Idx := Unshare (T.Uarr_Idx, Pool);
         when Type_Record
           | Type_Unbounded_Record =>
            Res.Rec := Create_Rec_El_Array (T.Rec.Len, Pool);
            for I in T.Rec.E'Range loop
               Res.Rec.E (I) := (Offs => T.Rec.E (I).Offs,
                                 Typ => Unshare (T.Rec.E (I).Typ, Pool));
            end loop;
         when Type_Access =>
            if T.Acc_Acc /= null then
               Res.Acc_Acc := Unshare (T.Acc_Acc, Pool);
            else
               --  For incomplete types
               Res.Acc_Acc := null;
            end if;
         when Type_File =>
            Res.File_Typ := Unshare (T.File_Typ, Pool);
         when Type_Protected =>
            raise Internal_Error;
      end case;
      return Res;
   end Unshare;

   function Unshare_Type (Typ : Type_Acc;
                          Base : Type_Acc;
                          Global : Boolean;
                          Pool : Areapool_Acc) return Type_Acc
   is
      Res : Type_Acc;
   begin
      if Typ = Base or else Typ.Is_Global = Global then
         return Typ;
      end if;
      Res := Raw_Copy (Typ, Pool);
      Res.Is_Global := Global;

      case Res.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            null;
         when Type_Slice =>
            Res.Slice_El := Unshare_Type (Typ.Slice_El,
                                          Get_Array_Element (Base),
                                          Global, Pool);
         when Type_Array
           | Type_Vector =>
            Res.Arr_El := Unshare_Type (Typ.Arr_El,
                                        Get_Array_Element (Base),
                                        Global, Pool);
         when Type_Unbounded_Array
           | Type_Unbounded_Vector
           | Type_Unbounded_Record =>
            raise Internal_Error;
         when Type_Record =>
            Res.Rec := Create_Rec_El_Array (Typ.Rec.Len, Pool);
            for I in Typ.Rec.E'Range loop
               Res.Rec.E (I) := (Offs => Typ.Rec.E (I).Offs,
                                 Typ => Unshare_Type (Typ.Rec.E (I).Typ,
                                                      Base.Rec.E (I).Typ,
                                                      Global, Pool));
            end loop;
         when Type_Access =>
            raise Internal_Error;
         when Type_File =>
            raise Internal_Error;
         when Type_Protected =>
            raise Internal_Error;
      end case;
      return Res;
   end Unshare_Type;

   function Unshare_Type_Expr (Typ : Type_Acc; Base : Type_Acc)
                              return Type_Acc is
   begin
      return Unshare_Type (Typ, Base, False, Expr_Pool'Access);
   end Unshare_Type_Expr;

   function Unshare_Type_Instance (Typ : Type_Acc; Base : Type_Acc)
                                  return Type_Acc is
   begin
      return Unshare_Type (Typ, Base, True, Instance_Pool);
   end Unshare_Type_Instance;

   procedure Save_Type (Typ : Type_Acc;
                        Res : out Type_Acc;
                        Mem : Memory_Ptr;
                        Off : in out Size_Type;
                        Mem_Sz : Size_Type)
   is
      Sz : constant Size_Type := Typ.all'Size / Storage_Unit;
      Raw_Res : Address;
   begin
      --  Don't copy scalar types.
      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float
           | Type_Access =>
            Res := Typ;
            return;
         when others =>
            null;
      end case;

      --  Copy Typ.
      Realign (Off, Typ.all'Alignment);
      pragma Assert (Off + Sz <= Mem_Sz);
      Raw_Res := To_Address (Mem + Off);
      Off := Off + Sz;
      Res := To_Type_Acc (Raw_Res);
      Copy_Memory (To_Memory_Ptr (Raw_Res),
                   To_Memory_Ptr (Typ.all'Address), Sz);
      Res.Is_Global := True;

      --  Copy elements.
      case Res.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            raise Internal_Error;
         when Type_Slice =>
            raise Internal_Error;
         when Type_Array
           | Type_Vector =>
            Save_Type (Typ.Arr_El, Res.Arr_El, Mem, Off, Mem_Sz);
         when Type_Unbounded_Array
           | Type_Unbounded_Vector
           | Type_Unbounded_Record =>
            raise Internal_Error;
         when Type_Record =>
            declare
               subtype Data_Type is Rec_El_Array (Typ.Rec.Len);
               Rec_Sz : constant Size_Type := Data_Type'Size / Storage_Unit;
            begin
               Realign (Off, Data_Type'Alignment);
               pragma Assert (Off + Rec_Sz <= Mem_Sz);
               Raw_Res := To_Address (Mem + Off);
               Off := Off + Rec_Sz;
               Copy_Memory (To_Memory_Ptr (Raw_Res),
                            To_Memory_Ptr (Typ.Rec.all'Address), Rec_Sz);
               Res.Rec := To_Rec_El_Array_Acc (Raw_Res);
               for I in Typ.Rec.E'Range loop
                  Res.Rec.E (I).Offs := Typ.Rec.E (I).Offs;
                  Save_Type (Res.Rec.E (I).Typ,
                             Typ.Rec.E (I).Typ,
                             Mem, Off, Mem_Sz);
               end loop;
            end;
         when Type_Access =>
            raise Internal_Error;
         when Type_File =>
            raise Internal_Error;
         when Type_Protected =>
            raise Internal_Error;
      end case;
   end Save_Type;

   function Save_Type (Typ : Type_Acc;
                       Mem : Memory_Ptr;
                       Mem_Sz : Size_Type) return Type_Acc
   is
      Off : Size_Type;
      Res : Type_Acc;
   begin
      Off := 0;
      Save_Type (Typ, Res, Mem, Off, Mem_Sz);
      pragma Assert (Off <= Mem_Sz);
      return Res;
   end Save_Type;

   procedure Mark_Expr_Pool (M : out Mark_Type) is
   begin
      Mark (M, Expr_Pool);
   end Mark_Expr_Pool;

   procedure Release_Expr_Pool (M : Mark_Type) is
   begin
      Release (M, Expr_Pool);
   end Release_Expr_Pool;

   function Is_Expr_Pool_Empty return Boolean is
   begin
      return Is_Empty (Expr_Pool);
   end Is_Expr_Pool_Empty;

   Bit0_Mem : constant Memory_Element := 0;
   Bit1_Mem : constant Memory_Element := 1;

   function To_Memory_Ptr is new Ada.Unchecked_Conversion
     (Address, Memory_Ptr);

   procedure Initialize is
   begin
      if Boolean_Type /= null then
         --  Restarting.  Free the global pool.
         Release (Empty_Marker, Global_Pool);
      end if;

      --  Alloc fundamental types (on the global pool).
      Current_Pool := Global_Pool'Access;
      Boolean_Type := Create_Bit_Type;
      Logic_Type := Create_Logic_Type;
      Bit_Type := Create_Bit_Type;
      Protected_Type := Create_Protected_Type;

      Boolean_Type.Is_Global := True;
      Logic_Type.Is_Global := True;
      Bit_Type.Is_Global := True;
      Protected_Type.Is_Global := True;

      Current_Pool := Expr_Pool'Access;

      Bit0 := (Bit_Type, To_Memory_Ptr (Bit0_Mem'Address));
      Bit1 := (Bit_Type, To_Memory_Ptr (Bit1_Mem'Address));
   end Initialize;

   procedure Finalize is
   begin
      pragma Assert (Boolean_Type /= null);
      Release (Empty_Marker, Global_Pool);

      Instance_Pool := null;
      Boolean_Type := null;
      Logic_Type := null;
      Bit_Type := null;
      Protected_Type := null;

      Bit0 := Null_Memtyp;
      Bit1 := Null_Memtyp;
   end Finalize;
end Elab.Vhdl_Objtypes;
