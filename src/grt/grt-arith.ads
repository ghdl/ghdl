--  GHDL Run Time (GRT) -  support for exp
--  Copyright (C) 2022 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;

package Grt.Arith is
   --  Compute V**E.
   --  Set OVF to true in case of overflow, or E < 0.
   procedure Exp_I32 (V : Ghdl_I32;
                      E : Std_Integer;
                      Res : out Ghdl_I32;
                      Ovf : out Boolean);

   procedure Exp_I64 (V : Ghdl_I64;
                      E : Std_Integer;
                      Res : out Ghdl_I64;
                      Ovf : out Boolean);
end Grt.Arith;
