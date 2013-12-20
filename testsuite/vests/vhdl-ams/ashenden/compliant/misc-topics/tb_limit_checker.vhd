
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
use work.project_util.all;

entity tb_limit_checker is
end entity tb_limit_checker;


architecture test of tb_limit_checker is

  signal input : word;
  signal out_of_bounds : std_logic;

begin

  dut : entity work.limit_checker(behavioral)
    port map ( input => input,
               lower_bound => X"FFFFFFF0", upper_bound => X"00000010",
	       out_of_bounds => out_of_bounds );

  stimulus : input <= X"00000000",
                      X"00000008" after 10 ns,
                      X"00000010" after 20 ns,
                      X"00000018" after 30 ns,
                      X"FFFFFFF8" after 40 ns,
                      X"FFFFFFF0" after 50 ns,
                      X"FFFFFF00" after 60 ns;

end architecture test;
