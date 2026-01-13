--  Library interface for the synthesizer.
--  Copyright (C) 2026 Tristan Gingold
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

with Types; use Types;
with Grt.Types; use Grt.Types;

with Netlists; use Netlists;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Libghdl_Synth is
   --  When used from a library, this init procedure must be called before
   --  ghdl_synth.
   procedure Init_For_Ghdl_Synth;
   pragma Export (Ada, Init_For_Ghdl_Synth, "ghdlsynth__init_for_ghdl_synth");

   type C_String_Array is array (Natural) of Ghdl_C_String;
   type C_String_Array_Acc is access C_String_Array;

   function Ghdl_Synth
     (Init : Natural; Argc : Natural; Argv : C_String_Array_Acc)
     return Module;
   pragma Export (Ada, Ghdl_Synth, "ghdlsynth__ghdl_synth");

   type Pointer_Generic is access Character;

   type Ghdl_Synth_Read_Cb is access procedure
     (Id : Name_Id; N : Node; Arg: Pointer_Generic);
   pragma Convention (C, Ghdl_Synth_Read_Cb);

   function Ghdl_Synth_Read (Init : Natural;
                             Argc : Natural;
                             Argv : C_String_Array_Acc;
                             Cb : Ghdl_Synth_Read_Cb;
                             Cb_Arg : Pointer_Generic) return Integer;

   type Pval_Cstring_Tuple is record
      Str : Ghdl_C_String;
      Val : Pval;
   end record;

   type Pval_Cstring_Array is array (Natural) of Pval_Cstring_Tuple;
   type Pval_Cstring_Array_Acc is access Pval_Cstring_Array;

   function Ghdl_Synth_With_Params (Entity_Decl : Node;
                                    Params : Pval_Cstring_Array_Acc;
                                    Nparams : Natural)
                                   return Synth_Instance_Acc;
end Libghdl_Synth;
