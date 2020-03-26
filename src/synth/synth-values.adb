--  Values in synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Ada.Unchecked_Conversion;
with System;
with Mutils; use Mutils;

with Netlists.Utils;

package body Synth.Values is
   function To_Bound_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Bound_Array_Acc);

   function To_Rec_El_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Rec_El_Array_Acc);

   function To_Type_Acc is new Ada.Unchecked_Conversion
     (System.Address, Type_Acc);

   function To_Value_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Acc);
   function To_Value_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Values.Value_Array_Acc);

   function Is_Static (Val : Value_Acc) return Boolean is
   begin
      case Val.Kind is
         when Value_Discrete
           | Value_Float =>
            return True;
         when Value_Net
           | Value_Wire =>
            return False;
         when Value_Const_Array
           | Value_Const_Record =>
            return True;
         when Value_Array
           | Value_Record =>
            return False;
         when Value_Access
           | Value_File =>
            return True;
         when Value_Alias =>
            return Is_Static (Val.A_Obj);
         when Value_Const =>
            return True;
         when Value_Instance
           | Value_Subtype =>
            --  Not really a value.
            raise Internal_Error;
      end case;
   end Is_Static;

   function Is_Static_Val (Val : Value_Acc) return Boolean is
   begin
      case Val.Kind is
         when Value_Discrete
           | Value_Float =>
            return True;
         when Value_Net =>
            return Netlists.Utils.Is_Const_Net (Val.N);
         when Value_Wire =>
            return Is_Const_Wire (Val.W);
         when Value_Const_Array
           | Value_Const_Record =>
            return True;
         when Value_Array
           | Value_Record =>
            return False;
         when Value_Access
           | Value_File =>
            return True;
         when Value_Const =>
            return True;
         when Value_Alias =>
            return Is_Static_Val (Val.A_Obj);
         when Value_Instance
           | Value_Subtype =>
            --  Not really a value.
            raise Internal_Error;
      end case;
   end Is_Static_Val;

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
           | Type_Unbounded_Vector =>
            return False;
      end case;
   end Is_Bounded_Type;

   function Strip_Alias_Const (V : Value_Acc) return Value_Acc
   is
      Res : Value_Acc;
   begin
      Res := V;
      loop
         case Res.Kind is
            when Value_Const =>
               Res := Res.C_Val;
            when Value_Alias =>
               if Res.A_Off /= 0 then
                  raise Internal_Error;
               end if;
               Res := Res.A_Obj;
            when others =>
               return Res;
         end case;
      end loop;
   end Strip_Alias_Const;

   function Is_Equal (L, R : Value_Acc) return Boolean
   is
      L1 : constant Value_Acc := Strip_Alias_Const (L);
      R1 : constant Value_Acc := Strip_Alias_Const (R);
   begin
      pragma Unreferenced (L, R);
      if L1.Kind /= R1.Kind then
         return False;
      end if;
      if L1 = R1 then
         return True;
      end if;

      case L1.Kind is
         when Value_Discrete =>
            return L1.Scal = R1.Scal;
         when Value_Float =>
            return L1.Fp = R1.Fp;
         when Value_Const_Array =>
            if L1.Arr.Len /= R1.Arr.Len then
               return False;
            end if;
            for I in L1.Arr.V'Range loop
               if not Is_Equal (L1.Arr.V (I), R1.Arr.V (I)) then
                  return False;
               end if;
            end loop;
            return True;
         when Value_Const =>
            raise Internal_Error;
         when others =>
            --  TODO.
            raise Internal_Error;
      end case;
   end Is_Equal;

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
         when Type_Vector =>
            return L.Vbound = R.Vbound
              and then Are_Types_Equal (L.Vec_El, R.Vec_El);
         when Type_Unbounded_Vector =>
            return Are_Types_Equal (L.Uvec_El, R.Uvec_El);
         when Type_Slice =>
            return Are_Types_Equal (L.Slice_El, R.Slice_El);
         when Type_Array =>
            if L.Abounds.Len /= R.Abounds.Len then
               return False;
            end if;
            for I in L.Abounds.D'Range loop
               if L.Abounds.D (I) /= R.Abounds.D (I) then
                  return False;
               end if;
            end loop;
            return Are_Types_Equal (L.Arr_El, R.Arr_El);
         when Type_Unbounded_Array =>
            return L.Uarr_Ndim = R.Uarr_Ndim
              and then Are_Types_Equal (L.Uarr_El, R.Uarr_El);
         when Type_Record =>
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
      end case;
   end Are_Types_Equal;

   function Discrete_Range_Width (Rng : Discrete_Range_Type) return Width
   is
      Lo, Hi : Int64;
      W : Width;
   begin
      case Rng.Dir is
         when Iir_To =>
            Lo := Rng.Left;
            Hi := Rng.Right;
         when Iir_Downto =>
            Lo := Rng.Right;
            Hi := Rng.Left;
      end case;
      if Lo > Hi then
         --  Null range.
         W := 0;
      elsif Lo >= 0 then
         --  Positive.
         W := Width (Clog2 (Uns64 (Hi) + 1));
      elsif Lo = Int64'First then
         --  Handle possible overflow.
         W := 64;
      elsif Hi < 0 then
         --  Negative only.
         W := Width (Clog2 (Uns64 (-Lo))) + 1;
      else
         declare
            Wl : constant Width := Width (Clog2 (Uns64 (-Lo)));
            Wh : constant Width := Width (Clog2 (Uns64 (Hi)));
         begin
            W := Width'Max (Wl, Wh) + 1;
         end;
      end if;
      return W;
   end Discrete_Range_Width;

   function Create_Bit_Type return Type_Acc
   is
      subtype Bit_Type_Type is Type_Type (Type_Bit);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Bit_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Bit,
                                                Is_Synth => True,
                                                W => 1)));
   end Create_Bit_Type;

   function Create_Logic_Type return Type_Acc
   is
      subtype Logic_Type_Type is Type_Type (Type_Logic);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Logic_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Logic,
                                                Is_Synth => True,
                                                W => 1)));
   end Create_Logic_Type;

   function Create_Discrete_Type (Rng : Discrete_Range_Type; W : Width)
                                 return Type_Acc
   is
      subtype Discrete_Type_Type is Type_Type (Type_Discrete);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Discrete_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Discrete,
                                                Is_Synth => True,
                                                W => W,
                                                Drange => Rng)));
   end Create_Discrete_Type;

   function Create_Float_Type (Rng : Float_Range_Type) return Type_Acc
   is
      subtype Float_Type_Type is Type_Type (Type_Float);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Float_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Float,
                                                Is_Synth => True,
                                                W => 64,
                                                Frange => Rng)));
   end Create_Float_Type;

   function Create_Vector_Type (Bnd : Bound_Type; El_Type : Type_Acc)
                               return Type_Acc
   is
      subtype Vector_Type_Type is Type_Type (Type_Vector);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Vector_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Vector,
                                                Is_Synth => True,
                                                W => Bnd.Len,
                                                Vbound => Bnd,
                                                Vec_El => El_Type)));
   end Create_Vector_Type;

   function Create_Slice_Type (W : Width; El_Type : Type_Acc) return Type_Acc
   is
      subtype Slice_Type_Type is Type_Type (Type_Slice);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Slice_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Slice,
                                                Is_Synth => El_Type.Is_Synth,
                                                W => W,
                                                Slice_El => El_Type)));
   end Create_Slice_Type;

   function Create_Vec_Type_By_Length (Len : Width; El : Type_Acc)
                                      return Type_Acc is
   begin
      return Create_Vector_Type ((Dir => Iir_Downto,
                                  Left => Int32 (Len) - 1,
                                  Right => 0,
                                  Len => Len),
                                 El);
   end Create_Vec_Type_By_Length;

   function Create_Bound_Array (Ndims : Iir_Index32) return Bound_Array_Acc
   is
      use System;
      subtype Data_Type is Bound_Array (Ndims);
      Res : Address;
   begin
      --  Manually allocate the array to handle large arrays without
      --  creating a large temporary value.
      Areapools.Allocate
        (Current_Pool.all, Res,
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

      return To_Bound_Array_Acc (Res);
   end Create_Bound_Array;

   function Create_Array_Type (Bnd : Bound_Array_Acc; El_Type : Type_Acc)
                              return Type_Acc
   is
      subtype Array_Type_Type is Type_Type (Type_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Array_Type_Type);
      W : Width;
   begin
      W := El_Type.W;
      for I in Bnd.D'Range loop
         W := W * Bnd.D (I).Len;
      end loop;
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Array,
                                                Is_Synth => El_Type.Is_Synth,
                                                W => W,
                                                Abounds => Bnd,
                                                Arr_El => El_Type)));
   end Create_Array_Type;

   function Create_Unbounded_Array (Ndim : Iir_Index32; El_Type : Type_Acc)
                                   return Type_Acc
   is
      subtype Unbounded_Type_Type is Type_Type (Type_Unbounded_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Unbounded_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Unbounded_Array,
                                                Is_Synth => El_Type.Is_Synth,
                                                W => 0,
                                                Uarr_Ndim => Ndim,
                                                Uarr_El => El_Type)));
   end Create_Unbounded_Array;

   function Create_Unbounded_Vector (El_Type : Type_Acc) return Type_Acc
   is
      subtype Unbounded_Type_Type is Type_Type (Type_Unbounded_Vector);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Unbounded_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Unbounded_Vector,
                                                Is_Synth => El_Type.Is_Synth,
                                                W => 0,
                                                Uvec_El => El_Type)));
   end Create_Unbounded_Vector;

   function Get_Array_Element (Arr_Type : Type_Acc) return Type_Acc is
   begin
      case Arr_Type.Kind is
         when Type_Vector =>
            return Arr_Type.Vec_El;
         when Type_Array =>
            return Arr_Type.Arr_El;
         when Type_Unbounded_Array =>
            return Arr_Type.Uarr_El;
         when Type_Unbounded_Vector =>
            return Arr_Type.Uvec_El;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Element;

   function Get_Array_Bound (Typ : Type_Acc; Dim : Dim_Type)
                            return Bound_Type is
   begin
      case Typ.Kind is
         when Type_Vector =>
            if Dim /= 1 then
               raise Internal_Error;
            end if;
            return Typ.Vbound;
         when Type_Array =>
            return Typ.Abounds.D (Iir_Index32 (Dim));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Bound;

   function Create_Rec_El_Array (Nels : Iir_Index32) return Rec_El_Array_Acc
   is
      use System;
      subtype Data_Type is Rec_El_Array (Nels);
      Res : Address;
   begin
      --  Manually allocate the array to handle large arrays without
      --  creating a large temporary value.
      Areapools.Allocate
        (Current_Pool.all, Res,
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

   function Create_Record_Type (Els : Rec_El_Array_Acc; W : Width)
                               return Type_Acc
   is
      subtype Record_Type_Type is Type_Type (Type_Record);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Record_Type_Type);
      Is_Synth : Boolean;
   begin
      Is_Synth := True;
      for I in Els.E'Range loop
         if not Els.E (I).Typ.Is_Synth then
            Is_Synth := False;
            exit;
         end if;
      end loop;
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Record,
                                                Is_Synth => Is_Synth,
                                                W => W,
                                                Rec => Els)));
   end Create_Record_Type;

   function Create_Access_Type (Acc_Type : Type_Acc) return Type_Acc
   is
      subtype Access_Type_Type is Type_Type (Type_Access);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Access_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Access,
                                                Is_Synth => False,
                                                W => 32,
                                                Acc_Acc => Acc_Type)));
   end Create_Access_Type;

   function Create_File_Type (File_Type : Type_Acc) return Type_Acc
   is
      subtype File_Type_Type is Type_Type (Type_File);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (File_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_File,
                                                Is_Synth => False,
                                                W => 32,
                                                File_Typ => File_Type)));
   end Create_File_Type;

   function Create_Value_Wire (W : Wire_Id; Wtype : Type_Acc) return Value_Acc
   is
      subtype Value_Type_Wire is Value_Type (Values.Value_Wire);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Wire);
   begin
      pragma Assert (Wtype /= null);
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Wire,
                                   W => W,
                                   Typ => Wtype)));
   end Create_Value_Wire;

   function Create_Value_Net (N : Net; Ntype : Type_Acc) return Value_Acc
   is
      subtype Value_Type_Net is Value_Type (Value_Net);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Net);
   begin
      pragma Assert (Ntype /= null);
      return To_Value_Acc
        (Alloc (Current_Pool,
                Value_Type_Net'(Kind => Value_Net, N => N, Typ => Ntype)));
   end Create_Value_Net;

   function Create_Value_Discrete (Val : Int64; Vtype : Type_Acc)
                                  return Value_Acc
   is
      subtype Value_Type_Discrete is Value_Type (Value_Discrete);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Discrete);
   begin
      pragma Assert (Vtype /= null);
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Discrete, Scal => Val,
                                   Typ => Vtype)));
   end Create_Value_Discrete;

   function Create_Value_Float (Val : Fp64; Vtype : Type_Acc) return Value_Acc
   is
      subtype Value_Type_Float is Value_Type (Value_Float);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Float);
   begin
      pragma Assert (Vtype /= null);
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Float,
                                   Typ => Vtype,
                                   Fp => Val)));
   end Create_Value_Float;

   function Create_Value_Access (Vtype : Type_Acc; Acc : Heap_Index)
                                return Value_Acc
   is
      subtype Value_Type_Access is Value_Type (Value_Access);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Access);
   begin
      pragma Assert (Vtype /= null);
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Access,
                                   Typ => Vtype,
                                   Acc => Acc)));
   end Create_Value_Access;

   function Create_Value_File (Vtype : Type_Acc; File : File_Index)
                              return Value_Acc
   is
      subtype Value_Type_File is Value_Type (Value_File);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_File);
   begin
      pragma Assert (Vtype /= null);
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_File,
                                   Typ => Vtype,
                                   File => File)));
   end Create_Value_File;

   function Create_Value_Array (Len : Iir_Index32) return Value_Array_Acc
   is
      use System;
      subtype Data_Type is Values.Value_Array_Type (Len);
      Res : Address;
   begin
      --  Manually allocate the array to handle large arrays without
      --  creating a large temporary value.
      Areapools.Allocate
        (Current_Pool.all, Res,
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

      return To_Value_Array_Acc (Res);
   end Create_Value_Array;

   function Create_Value_Array (Bounds : Type_Acc; Arr : Value_Array_Acc)
                               return Value_Acc
   is
      subtype Value_Type_Array is Value_Type (Value_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Array);

      Res : Value_Acc;
   begin
      pragma Assert (Bounds /= null);
      Res := To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Array,
                                   Arr => Arr, Typ => Bounds)));
      return Res;
   end Create_Value_Array;

   function Create_Value_Const_Array (Bounds : Type_Acc; Arr : Value_Array_Acc)
                               return Value_Acc
   is
      subtype Value_Type_Const_Array is Value_Type (Value_Const_Array);
      function Alloc is
         new Areapools.Alloc_On_Pool_Addr (Value_Type_Const_Array);

      Res : Value_Acc;
   begin
      pragma Assert (Bounds /= null);
      Res := To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Const_Array,
                                   Arr => Arr, Typ => Bounds)));
      return Res;
   end Create_Value_Const_Array;

   function Get_Array_Flat_Length (Typ : Type_Acc) return Width is
   begin
      case Typ.Kind is
         when Type_Vector =>
            return Typ.Vbound.Len;
         when Type_Array =>
            declare
               Len : Width;
            begin
               Len := 1;
               for I in Typ.Abounds.D'Range loop
                  Len := Len * Typ.Abounds.D (I).Len;
               end loop;
               return Len;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Flat_Length;

   procedure Create_Array_Data (Arr : Value_Acc)
   is
      Len : Width;
   begin
      case Arr.Typ.Kind is
         when Type_Array =>
            Len := Get_Array_Flat_Length (Arr.Typ);
         when Type_Vector =>
            Len := Arr.Typ.Vbound.Len;
         when others =>
            raise Internal_Error;
      end case;

      Arr.Arr := Create_Value_Array (Iir_Index32 (Len));
   end Create_Array_Data;

   function Create_Value_Array (Bounds : Type_Acc) return Value_Acc
   is
      Res : Value_Acc;
   begin
      Res := Create_Value_Array (Bounds, null);
      Create_Array_Data (Res);
      return Res;
   end Create_Value_Array;

   function Create_Value_Record (Typ : Type_Acc; Els : Value_Array_Acc)
                                return Value_Acc
   is
      subtype Value_Type_Record is Value_Type (Value_Record);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Record);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Record,
                                   Typ => Typ,
                                   Rec => Els)));
   end Create_Value_Record;

   function Create_Value_Const_Record (Typ : Type_Acc; Els : Value_Array_Acc)
                                      return Value_Acc
   is
      subtype Value_Type_Const_Record is Value_Type (Value_Const_Record);
      function Alloc is
         new Areapools.Alloc_On_Pool_Addr (Value_Type_Const_Record);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Const_Record,
                                   Typ => Typ,
                                   Rec => Els)));
   end Create_Value_Const_Record;

   function Create_Value_Instance (Inst : Instance_Id) return Value_Acc
   is
      subtype Value_Type_Instance is Value_Type (Value_Instance);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Instance);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                (Kind => Value_Instance, Instance => Inst, Typ => null)));
   end Create_Value_Instance;

   function Create_Value_Subtype (Typ : Type_Acc) return Value_Acc
   is
      subtype Value_Type_Subtype is Value_Type (Value_Subtype);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Subtype);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Subtype, Typ => Typ)));
   end Create_Value_Subtype;

   function Create_Value_Alias (Obj : Value_Acc; Off : Uns32; Typ : Type_Acc)
                               return Value_Acc
   is
      subtype Value_Type_Alias is Value_Type (Value_Alias);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Alias);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Alias,
                                   A_Obj => Obj,
                                   A_Off => Off,
                                   Typ => Typ)));
   end Create_Value_Alias;

   function Create_Value_Const (Val : Value_Acc; Loc : Syn_Src)
                               return Value_Acc
   is
      subtype Value_Type_Const is Value_Type (Value_Const);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Const);
   begin
      pragma Assert (Val = null or else Val.Kind /= Value_Const);
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Const,
                                   C_Val => Val,
                                   C_Loc => Loc,
                                   C_Net => No_Net,
                                   Typ => Val.Typ)));
   end Create_Value_Const;

   procedure Strip_Const (Val : in out Value_Acc) is
   begin
      if Val.Kind = Value_Const then
         Val := Val.C_Val;
      end if;
   end Strip_Const;

   function Strip_Const (Val : Value_Acc) return Value_Acc is
   begin
      if Val.Kind = Value_Const then
         return Val.C_Val;
      else
         return Val;
      end if;
   end Strip_Const;

   function Copy (Src : Value_Acc) return Value_Acc;

   function Copy_Array (Arr : Value_Array_Acc) return Value_Array_Acc
   is
      Res : Value_Array_Acc;
   begin
      Res := Create_Value_Array (Arr.Len);
      for I in Res.V'Range loop
         Res.V (I) := Copy (Arr.V (I));
      end loop;
      return Res;
   end Copy_Array;

   function Copy (Src : Value_Acc) return Value_Acc
   is
      Res : Value_Acc;
      Arr : Value_Array_Acc;
   begin
      case Src.Kind is
         when Value_Net =>
            Res := Create_Value_Net (Src.N, Src.Typ);
         when Value_Wire =>
            Res := Create_Value_Wire (Src.W, Src.Typ);
         when Value_Discrete =>
            Res := Create_Value_Discrete (Src.Scal, Src.Typ);
         when Value_Float =>
            Res := Create_Value_Float (Src.Fp, Src.Typ);
         when Value_Subtype =>
            Res := Create_Value_Subtype (Src.Typ);
         when Value_Array =>
            Arr := Copy_Array (Src.Arr);
            Res := Create_Value_Array (Src.Typ, Arr);
         when Value_Const_Array =>
            Arr := Copy_Array (Src.Arr);
            Res := Create_Value_Const_Array (Src.Typ, Arr);
         when Value_Record =>
            Arr := Copy_Array (Src.Rec);
            Res := Create_Value_Record (Src.Typ, Arr);
         when Value_Const_Record =>
            Arr := Copy_Array (Src.Rec);
            Res := Create_Value_Const_Record (Src.Typ, Arr);
         when Value_Access =>
            Res := Create_Value_Access (Src.Typ, Src.Acc);
         when Value_File =>
            Res := Create_Value_File (Src.Typ, Src.File);
         when Value_Instance =>
            raise Internal_Error;
         when Value_Const =>
            raise Internal_Error;
         when Value_Alias =>
            raise Internal_Error;
      end case;
      return Res;
   end Copy;

   function Unshare (Src : Value_Acc; Pool : Areapool_Acc)
                    return Value_Acc
   is
      Prev_Pool : constant Areapool_Acc := Current_Pool;
      Res : Value_Acc;
   begin
      Current_Pool := Pool;
      Res := Copy (Src);
      Current_Pool := Prev_Pool;
      return Res;
   end Unshare;

   function Get_Type_Width (Atype : Type_Acc) return Width is
   begin
      pragma Assert (Atype.Kind /= Type_Unbounded_Array);
      return Atype.W;
   end Get_Type_Width;

   function Get_Bound_Length (T : Type_Acc; Dim : Iir_Index32) return Width is
   begin
      case T.Kind is
         when Type_Vector =>
            if Dim /= 1 then
               raise Internal_Error;
            end if;
            return T.Vbound.Len;
         when Type_Slice =>
            if Dim /= 1 then
               raise Internal_Error;
            end if;
            return T.W;
         when Type_Array =>
            return T.Abounds.D (Dim).Len;
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
            return Get_Bound_Length (L, 1) = Get_Bound_Length (R, 1);
         when Type_Array =>
            for I in L.Abounds.D'Range loop
               if Get_Bound_Length (L, I) /= Get_Bound_Length (R, I) then
                  return False;
               end if;
            end loop;
            return True;
         when Type_Unbounded_Array
           | Type_Unbounded_Vector =>
            raise Internal_Error;
         when Type_Record =>
            --  FIXME: handle vhdl-08
            return True;
         when Type_Access =>
            return True;
         when Type_File =>
            raise Internal_Error;
      end case;
   end Is_Matching_Bounds;

   function Create_Value_Default (Typ : Type_Acc) return Value_Acc is
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic =>
            --  FIXME: what about subtype ?
            return Create_Value_Discrete (0, Typ);
         when Type_Discrete =>
            return Create_Value_Discrete (Typ.Drange.Left, Typ);
         when Type_Float =>
            return Create_Value_Float (Typ.Frange.Left, Typ);
         when Type_Vector =>
            declare
               El_Typ : constant Type_Acc := Typ.Vec_El;
               Arr : Value_Array_Acc;
            begin
               Arr := Create_Value_Array (Iir_Index32 (Typ.Vbound.Len));
               for I in Arr.V'Range loop
                  Arr.V (I) := Create_Value_Default (El_Typ);
               end loop;
               return Create_Value_Const_Array (Typ, Arr);
            end;
         when Type_Unbounded_Vector =>
            raise Internal_Error;
         when Type_Slice =>
            raise Internal_Error;
         when Type_Array =>
            declare
               El_Typ : constant Type_Acc := Get_Array_Element (Typ);
               Arr : Value_Array_Acc;
            begin
               Arr := Create_Value_Array
                 (Iir_Index32 (Get_Array_Flat_Length (Typ)));
               for I in Arr.V'Range loop
                  Arr.V (I) := Create_Value_Default (El_Typ);
               end loop;
               return Create_Value_Const_Array (Typ, Arr);
            end;
         when Type_Unbounded_Array =>
            raise Internal_Error;
         when Type_Record =>
            declare
               Els : Value_Array_Acc;
            begin
               Els := Create_Value_Array (Typ.Rec.Len);
               for I in Els.V'Range loop
                  Els.V (I) := Create_Value_Default (Typ.Rec.E (I).Typ);
               end loop;
               return Create_Value_Const_Record (Typ, Els);
            end;
         when Type_Access =>
            return Create_Value_Access (Typ, Null_Heap_Index);
         when Type_File =>
            raise Internal_Error;
      end case;
   end Create_Value_Default;

   function Value_To_String (Val : Value_Acc) return String
   is
      Str : String (1 .. Natural (Val.Arr.Len));
   begin
      for I in Val.Arr.V'Range loop
         Str (Natural (I)) := Character'Val (Val.Arr.V (I).Scal);
      end loop;
      return Str;
   end Value_To_String;

   procedure Init is
   begin
      Instance_Pool := Global_Pool'Access;
      Boolean_Type := Create_Bit_Type;
      Logic_Type := Create_Logic_Type;
      Bit_Type := Create_Bit_Type;
   end Init;
end Synth.Values;
