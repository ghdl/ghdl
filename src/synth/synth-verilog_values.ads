--  Values definition for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Ada.Unchecked_Deallocation;

with Types; use Types;

with Verilog.Nodes; use Verilog.Nodes;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

with Elab.Memtype; use Elab.Memtype;
with Synth.Verilog_Environment; use Synth.Verilog_Environment.Env;

package Synth.Verilog_Values is
   type Value_Kind is (Value_None, Value_Net, Value_Wire, Value_Memory);

   type Valtyp (Kind : Value_Kind := Value_None) is record
      Typ : Node;
      case Kind is
         when Value_None =>
            null;
         when Value_Net =>
            N : Net;
         when Value_Wire =>
            W : Wire_Id;
         when Value_Memory =>
            Mem : Memory_Ptr;
      end case;
   end record;

   No_Valtyp : constant Valtyp := (Kind => Value_None,
                                   Typ => Null_Node);

   --  Can be useful: array of Valtyp.
   type Valtyp_Array is array (Nat32 range <>) of Valtyp;
   type Valtyp_Array_Acc is access Valtyp_Array;

   procedure Free_Valtyp_Array is new Ada.Unchecked_Deallocation
     (Valtyp_Array, Valtyp_Array_Acc);

   function Is_Static (V : Valtyp) return Boolean;

   function Create_Value_Net (N : Net; Typ : Node) return Valtyp;
   pragma Inline (Create_Value_Net);

   function Create_Value_Memory (Mem : Memory_Ptr; Typ : Node) return Valtyp;
   pragma Inline (Create_Value_Memory);

   function Get_Net (Ctxt : Context_Acc; V : Valtyp) return Net;
end Synth.Verilog_Values;
