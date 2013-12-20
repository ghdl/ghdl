
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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

-- ---------------------------------------------------------------------
--
-- $Id: ch_06_multt-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               architecture bench of multiplier_test is

                 signal a, b : std_ulogic_vector(15 downto 0) := (others => '0');
                 signal p : std_ulogic_vector(31 downto 0);

               begin

                 dut : entity work.multiplier(behavioral)
                   port map (a, b, p);

                 stimulus : process is
                 begin
                   a <= X"8000";  b <= X"8000";  --  -1 * -1
                   wait for 50 ns;
                   a <= X"0001";  b <= X"0001";  --  2**-15 * 2**-15
                   wait for 50 ns;
                   a <= X"0001";  b <= X"0000";  --  2**-15 * 0
                   wait for 50 ns;
                   a <= X"0000";  b <= X"0001";  --  0 * 2**-15
                   wait for 50 ns;
                   a <= X"0001";  b <= X"8000";  --  2**-15 * -1
                   wait for 50 ns;
                   a <= X"8000";  b <= X"0001";  --  -1 * 2**-15
                   wait for 50 ns;
                   a <= X"4000";  b <= X"4000";  --  0.5 * 0.5
                   wait for 50 ns;
                   a <= X"C000";  b <= X"4000";  --  -0.5 * 0.5
                   wait for 50 ns;
                   a <= X"4000";  b <= X"C000";  --  0.5 * -0.5
                   wait for 50 ns;
                   a <= X"C000";  b <= X"C000";  --  -0.5 * -0.5
                   wait for 50 ns;
                   wait;
                 end process stimulus;

               end architecture bench;
