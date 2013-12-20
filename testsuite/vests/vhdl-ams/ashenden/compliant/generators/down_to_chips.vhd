
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

library ieee;  use ieee.std_logic_1164.all;

entity DRAM_4M_by_4 is
  port ( a :  in std_logic_vector(0 to 10);
         d :  inout std_logic_vector(0 to 3);
         cs, we, ras, cas : in std_logic );
end entity DRAM_4M_by_4;


architecture chip_function of DRAM_4M_by_4 is
begin
  d <= (others => 'Z');
end architecture chip_function;


-- code from book

library chip_lib;  use chip_lib.all;

configuration down_to_chips of memory_board is

  for chip_level

    for bank_array

      for nibble_array

        for a_DRAM : DRAM
          use entity DRAM_4M_by_4(chip_function);
        end for;

      end for;

    end for;

    -- . . .    -- configurations of other component instances

  end for;

end configuration down_to_chips;

-- end code from book
