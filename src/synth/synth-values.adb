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
with Mutils;

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

   function Is_Equal (L, R : Value_Acc) return Boolean is
   begin
      if L.Kind /= R.Kind then
         return False;
      end if;

      case L.Kind is
         when Value_Discrete =>
            return L.Scal = R.Scal;
         when others =>
            --  TODO.
            raise Internal_Error;
      end case;
   end Is_Equal;

   function Create_Bit_Type return Type_Acc
   is
      subtype Bit_Type_Type is Type_Type (Type_Bit);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Bit_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Bit)));
   end Create_Bit_Type;

   function Create_Discrete_Type (Rng : Discrete_Range_Type) return Type_Acc
   is
      subtype Discrete_Type_Type is Type_Type (Type_Discrete);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Discrete_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Discrete,
                                                Drange => Rng)));
   end Create_Discrete_Type;

   function Create_Float_Type (Rng : Float_Range_Type) return Type_Acc
   is
      subtype Float_Type_Type is Type_Type (Type_Float);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Float_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Float,
                                                Frange => Rng)));
   end Create_Float_Type;

   function Create_Vector_Type (Bnd : Bound_Type; El_Type : Type_Acc)
                               return Type_Acc
   is
      subtype Vector_Type_Type is Type_Type (Type_Vector);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Vector_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Vector,
                                                Vbound => Bnd,
                                                Vec_El => El_Type)));
   end Create_Vector_Type;

   function Create_Vec_Type_By_Length (Len : Width; El : Type_Acc)
                                      return Type_Acc
   is
      W : constant Width := Uns32 (Mutils.Clog2 (Uns64 (Len)));
   begin
      return Create_Vector_Type ((Dir => Iir_Downto,
                                  Wlen => W,
                                  Wbounds => W,
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
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Array,
                                                Abounds => Bnd,
                                                Arr_El => El_Type)));
   end Create_Array_Type;

   function Create_Unbounded_Array (El_Type : Type_Acc) return Type_Acc
   is
      subtype Unbounded_Type_Type is Type_Type (Type_Unbounded_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Unbounded_Type_Type);
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Unbounded_Array,
                                                Uarr_El => El_Type)));
   end Create_Unbounded_Array;

   function Get_Array_Element (Arr_Type : Type_Acc) return Type_Acc is
   begin
      case Arr_Type.Kind is
         when Type_Vector =>
            return Arr_Type.Vec_El;
         when Type_Array =>
            return Arr_Type.Arr_El;
         when Type_Unbounded_Array =>
            return Arr_Type.Uarr_El;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Element;

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
   begin
      return To_Type_Acc (Alloc (Current_Pool, (Kind => Type_Record,
                                                Rec_W => W,
                                                Rec => Els)));
   end Create_Record_Type;

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

   function Create_Value_Mux2 (Cond : Value_Acc; T : Value_Acc; F : Value_Acc)
                              return Value_Acc
   is
      subtype Value_Type_Mux2 is Value_Type (Value_Mux2);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Mux2);
      pragma Assert (F = null or else T.Typ = F.Typ);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                (Kind => Value_Mux2,
                 Typ => T.Typ,
                 M_Cond => Cond, M_T => T, M_F => F)));
   end Create_Value_Mux2;

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

   function Get_Array_Flat_Length (Typ : Type_Acc) return Width
   is
      Len : Width;
   begin
      Len := 1;
      for I in Typ.Abounds.D'Range loop
         Len := Len * Typ.Abounds.D (I).Len;
      end loop;
      return Len;
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

   function Create_Value_Alias (Wid : Wire_Id; Off : Uns32; Typ : Type_Acc)
                               return Value_Acc
   is
      subtype Value_Type_Alias is Value_Type (Value_Alias);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Alias);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Alias,
                                   A_Wid => Wid,
                                   A_Off => Off,
                                   Typ => Typ)));
   end Create_Value_Alias;

   function Copy (Src: in Value_Acc) return Value_Acc
   is
      Res: Value_Acc;
   begin
      case Src.Kind is
         when Value_Wire =>
            Res := Create_Value_Wire (Src.W, Src.Typ);
         when others =>
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
      case Atype.Kind is
         when Type_Bit =>
            return 1;
         when Type_Discrete =>
            return Atype.Drange.W;
         when Type_Vector =>
            return Atype.Vbound.Len;
         when Type_Array =>
            declare
               Res : Width;
            begin
               Res := Get_Type_Width (Atype.Arr_El);
               for I in Atype.Abounds.D'Range loop
                  Res := Res * Atype.Abounds.D (I).Len;
               end loop;
               return Res;
            end;
         when Type_Record =>
            return Atype.Rec_W;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Type_Width;

   procedure Init is
   begin
      Instance_Pool := Global_Pool'Access;
      Boolean_Type := Create_Bit_Type;
      Logic_Type := Create_Bit_Type;
      Bit_Type := Create_Bit_Type;
   end Init;
end Synth.Values;
