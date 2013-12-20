
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

-- analyze into resource library chips

entity XYZ3000_cpu is
  port ( clock : in bit;  addr_data : inout bit_vector(31 downto 0);
         other_port : in bit := '0' );
end entity XYZ3000_cpu;


architecture full_function of XYZ3000_cpu is
begin
end architecture full_function;


-- analyze into work library

entity memory_array is
  port ( addr : in bit_vector(25 downto 0);  other_port : in bit := '0' );
end entity memory_array;


architecture behavioral of memory_array is
begin
end architecture behavioral;



-- code from book

library chips;

configuration intermediate of single_board_computer is

  for structural

    for cpu : processor
      use entity chips.XYZ3000_cpu(full_function)
      port map ( clock => clk, addr_data => a_d, -- . . . );
      -- not in book
                 other_port => open );
      -- end not in book
    end for;

    for main_memory : memory
      use entity work.memory_array(behavioral);
    end for;

    for all : serial_interface
      use open;
    end for;

    -- . . .

  end for;

end configuration intermediate;

-- end code from book
