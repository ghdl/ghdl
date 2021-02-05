--  Build utilities
--  Copyright (C) 2019 Tristan Gingold
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

with Netlists.Builders; use Netlists.Builders;

package Netlists.Butils is
   type Case_Element is record
      Sel : Uns64;
      Val : Net;
   end record;

   type Case_Element_Array is array (Natural range <>) of Case_Element;
   type Case_Element_Array_Acc is access Case_Element_Array;
   procedure Free_Case_Element_Array is new Ada.Unchecked_Deallocation
     (Case_Element_Array, Case_Element_Array_Acc);

   --  Generate a netlist for a 'big' mux selected by SEL.  The inputs are
   --  described by ELS: E.Val must be selected when SEL = E.Sel; if there
   --  is no E in Els for a value, DEFAULT is selected.
   --  The result of the netlist is stored in RES.
   --
   --  A tree of MUX4 is built.
   --
   --  ELS must be sorted by SEL values.
   --  ELS is overwritten/modified so after the call it contains garbage.  The
   --  reason is that ELS might be large, so temporary arrays are not allocated
   --  on the stack, and ELS is expected to be built only for this subprogram.
   procedure Synth_Case (Ctxt : Context_Acc;
                         Sel : Net;
                         Els : in out Case_Element_Array;
                         Default : Net;
                         Res : out Net;
                         Sel_Loc : Location_Type);
end Netlists.Butils;
