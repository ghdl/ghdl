--  Heap for synthesis.
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

with Synth.Objtypes; use Synth.Objtypes;
with Synth.Values; use Synth.Values;

package Synth.Heap is
   --  Allocate a value.
   function Allocate_By_Type (T : Type_Acc) return Heap_Index;
   function Allocate_By_Value (V : Valtyp) return Heap_Index;

   function Synth_Dereference (Idx : Heap_Index) return Valtyp;

   procedure Synth_Deallocate (Idx : Heap_Index);
end Synth.Heap;
