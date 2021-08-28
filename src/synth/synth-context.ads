--  Synthesis context.
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

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

package Synth.Context is
   type Base_Instance_Type is limited record
      Builder : Context_Acc;
      Top_Module : Module;

      Cur_Module : Module;
   end record;

   type Base_Instance_Acc is access Base_Instance_Type;
end Synth.Context;
