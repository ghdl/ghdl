--  Inference in synthesis.
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

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

package Synth.Inference is
   --  To be called when there is an assignment to a signal/output of VAL and
   --  the previous value is PREV_VAL (an Id_Signal or Id_Output).
   --  If there is a loop, infere a dff or a latch or emit an error.
   function Infere (Ctxt : Context_Acc; Val : Net; Prev_Val : Net) return Net;
end Synth.Inference;
