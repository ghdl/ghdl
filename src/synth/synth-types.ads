--  Types synthesis.
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
with Synth.Values; use Synth.Values;
with Iirs; use Iirs;

package Synth.Types is
   --  All known enumeration type that are translated to a single bit.
   function Is_Bit_Type (Atype : Iir) return Boolean;

   function Is_Vector_Type (Atype : Iir) return Boolean;

   function Get_Width (Syn_Inst : Synth_Instance_Acc; Atype : Iir)
                      return Width;
end Synth.Types;
