--  Inference in synthesis.
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

with Netlists.Builders; use Netlists.Builders;

package Netlists.Inference is
   --  If True, latches are allowed.
   Flag_Latches : Boolean := False;

   --  Walk the And-net N, and extract clock (posedge/negedge) if found.
   --  ENABLE is N without the clock.
   --  If not found, CLK and ENABLE are set to No_Net.
   procedure Extract_Clock (N : Net; Clk : out Net; Enable : out Net);

   --  Called when there is an assignment to a enable gate.
   function Infere_Assert (Ctxt : Context_Acc;
                           Val : Net;
                           En_Gate : Net;
                           Loc : Location_Type) return Net;

   --  Infere a tri-state buffer.
   --  Replace VAL <= sel ? inp : 'Z'  with VAL <= tri(sel, inp)
   function Infere_Tri (Ctxt : Context_Acc; Val : Net) return Net;

   procedure Infere_Pass (Ctxt : Context_Acc; M : Module);
end Netlists.Inference;
