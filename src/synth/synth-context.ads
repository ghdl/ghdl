--  Synthesis context.
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

with Synth.Environment; use Synth.Environment;
with Synth.Values; use Synth.Values;
with Simul.Environments; use Simul.Environments;
with Netlists; use Netlists;
with Netlists.Builders;
with Iirs; use Iirs;

package Synth.Context is
   type Instance_Map_Array is array (Block_Instance_Id range <>)
     of Synth_Instance_Acc;
   type Instance_Map_Array_Acc is access Instance_Map_Array;

   --  Map between simulation instance and synthesis instance.
   Instance_Map : Instance_Map_Array_Acc;

   Build_Context : Netlists.Builders.Context_Acc;

   function Make_Instance (Sim_Inst : Block_Instance_Acc)
                          return Synth_Instance_Acc;
   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc);

   procedure Make_Object (Syn_Inst : Synth_Instance_Acc;
                          Kind : Wire_Kind;
                          Obj : Iir);

   function Get_Net (Val : Value_Acc) return Net;

   function Get_Value (Inst : Synth_Instance_Acc; Obj : Iir) return Value_Acc;

end Synth.Context;
