
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
-- $Id: ch_06_tofpt-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               architecture bench of to_fp_test is

                 signal vec : std_ulogic_vector(15 downto 0);
                 signal r : real;

               begin

                 dut : entity work.to_fp(behavioral)
                   port map (vec, r);

                 stimulus : process is
                 begin
                   vec <= X"0000";  wait for 10 ns;
                   vec <= X"8000";  wait for 10 ns;
                   vec <= X"7FFF";  wait for 10 ns;
                   vec <= X"4000";  wait for 10 ns;
                   vec <= X"C000";  wait for 10 ns;

                   wait;
                 end process stimulus;

               end architecture bench;
