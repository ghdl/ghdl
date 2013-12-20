
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

library device_lib;

configuration last_pass_spice of carry_chain is

  for device_level

    for bit_array ( 0 to n - 1 )

      for bit_0
        for all : nmos
          use entity device_lib.nmos(ideal);
        end for;
        for all : pmos
          use entity device_lib.pmos(ideal);
        end for;
      end for;

      for middle_bit
        for all : nmos
          use entity device_lib.nmos(ideal);
        end for;
        for all : pmos
          use entity device_lib.pmos(ideal);
        end for;
      end for;

    end for;

    for bit_array ( n )

      for bit_n
        for p_pass : nmos
          use entity device_lib.nmos(spice_equivalent);
        end for;
        for others : nmos
          use entity device_lib.nmos(ideal);
        end for;
        for all : pmos
          use entity device_lib.pmos(ideal);
        end for;
      end for;

    end for;

  end for;

end configuration last_pass_spice;
