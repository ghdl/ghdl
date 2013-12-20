
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

entity tb_byte_swap is

end entity tb_byte_swap;


----------------------------------------------------------------


use work.byte_swap_types.all;


architecture test_behavior of tb_byte_swap is

  signal input, output : halfword := x"0000";

begin

  dut : entity work.byte_swap(behavior)
    port map ( input => input, output => output );

  stumulus : process is
  begin
			wait for 10 ns;
    input <= x"ff00";	wait for 10 ns;
    input <= x"00ff";	wait for 10 ns;
    input <= x"aa33";	wait for 10 ns;

    wait;
  end process stumulus;

end architecture test_behavior;
