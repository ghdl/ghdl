
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

entity real_subcircuit is
  port ( a, b : in bit;  y1, y2 : out bit );
end entity real_subcircuit;


architecture basic of real_subcircuit is
begin
  y1 <= a and b after 10 ns;
  y2 <= a nand b after 10 ns;
end architecture basic;



-- code from book

configuration full of circuit is

  for with_pad_delays  -- configure the architecture

    for functionality    -- configure the block

      for all : subcircuit
        use entity work.real_subcircuit(basic);
      end for;

    end for;

  end for;

end configuration full;

-- end code from book
