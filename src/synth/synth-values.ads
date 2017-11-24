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

with Types; use Types;
with Netlists; use Netlists;
with Synth.Environment; use Synth.Environment;
with Simul.Environments; use Simul.Environments;
with Iirs; use Iirs;

package Synth.Values is
   --  Values is how signals and variables are decomposed.  This is similar to
   --  values in simulation, but simplified (no need to handle files,
   --  accesses...)

   type Value_Kind is (Value_Net, Value_Wire, Value_Array, Value_Record,
                       Value_Lit);

   type Value_Type (Kind : Value_Kind);

   type Value_Acc is access Value_Type;

   type Value_Type_Array is array (Iir_Index32 range <>) of Value_Acc;

   type Value_Array_Type (Len : Iir_Index32) is record
      V : Value_Type_Array (1 .. Len);
   end record;

   type Value_Array_Acc is access Value_Array_Type;

   type Value_Range is record
      Dir : Iir_Direction;
      Len : Width;
      Left : Int32;
      Right : Int32;
   end record;

   type Value_Range_Acc is access Value_Range;
   No_Range : constant Value_Range_Acc := null;

   type Value_Type (Kind : Value_Kind) is record
      case Kind is
         when Value_Net =>
            N : Net;
            N_Range : Value_Range_Acc;
         when Value_Wire =>
            W : Wire_Id;
            W_Range : Value_Range_Acc;
         when Value_Lit =>
            Lit : Simul.Environments.Iir_Value_Literal_Acc;
            Lit_Type : Iir;
         when Value_Array =>
            Arr : Value_Array_Acc;
            Bounds : Value_Bounds_Array_Acc;
         when Value_Record =>
           Rec : Value_Array_Acc;
      end case;
   end record;

   --  Create a Value_Net.
   function Create_Value_Net (N : Net; Rng : Value_Range_Acc) return Value_Acc;

   --  Create a Value_Wire.  For a bit wire, RNG must be null.
   function Create_Value_Wire (W : Wire_Id; Rng : Value_Range_Acc)
                              return Value_Acc;

   --  Create a Value_Lit.
   function Create_Value_Lit (Val : Iir_Value_Literal_Acc; Typ : Iir)
                             return Value_Acc;

   --  Create a Value_Array.
   function Create_Array_Value (Bounds : Value_Bounds_Array_Acc)
                               return Value_Acc;

   --  Allocate the ARR component of the Value_Type ARR, using BOUNDS.
   procedure Create_Array_Data (Arr : Value_Acc);

   --  Allocate a Value_Range.
   function Create_Range_Value (Rng : Value_Range) return Value_Range_Acc;

   --  Create a Value_Range from a simulation bound.
   function Bounds_To_Range (Val : Iir_Value_Literal_Acc)
                            return Value_Range_Acc;

   --  Values are stored into Synth_Instance, which is parallel to simulation
   --  Block_Instance_Type.
   type Objects_Array is array (Object_Slot_Type range <>) of Value_Acc;

   type Synth_Instance_Type (Max_Objs : Object_Slot_Type) is record
      --  Module which owns gates created for this instance.
      M : Module;

      --  Name prefix for declarations.
      Name : Sname;

      Sim : Block_Instance_Acc;
      Objects : Objects_Array (1 .. Max_Objs);
   end record;

   type Synth_Instance_Acc is access Synth_Instance_Type;

end Synth.Values;
