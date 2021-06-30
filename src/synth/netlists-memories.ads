--  Extract memories.
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

with Netlists.Builders; use Netlists.Builders;

package Netlists.Memories is
   --  True iff a RAM can be infered from VAL (the input of an assignment).
   --  TODO: handle partial write (offset)
   --  TODO: directly check with assignment target.
   function Can_Infere_RAM (Val : Net; Prev_Val : Net) return Boolean;

   --  Pre-transform VAL to a RAM: try to merge Mux2 into the dyn_insert.
   function Infere_RAM
     (Ctxt : Context_Acc; Val : Net; Tail : Net; Clk : Net; En : Net)
      return Net;

   --  Try to convert dyn_insert/dyn_extract to memory ports.
   procedure Extract_Memories (Ctxt : Context_Acc; M : Module);

   --  Count the number of memidx in a memory address.
   function Count_Memidx (Addr : Net) return Natural;

end Netlists.Memories;
