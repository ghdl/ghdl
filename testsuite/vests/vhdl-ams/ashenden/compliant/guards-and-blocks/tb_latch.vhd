
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

entity tb_latch is
end entity tb_latch;


architecture test of tb_latch is

 signal enable : bit := '0';
 signal d, q : bit_vector(0 to 7);

begin

  dut : entity work.latch(behavioral)
    generic map ( width => 8 )
    port map ( enable => enable, d => d, q => q );

  stimulus : process is
  begin
    wait for 10 ns;
    d <= X"11";  wait for 10 ns;
    enable <= '1';  wait for 10 ns;
    d <= X"AA";  wait for 10 ns;
    enable <= '0';  wait for 10 ns;
    d <= X"00";  wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;
