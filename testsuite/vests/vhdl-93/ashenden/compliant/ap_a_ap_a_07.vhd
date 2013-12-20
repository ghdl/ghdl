
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
-- $Id: ap_a_ap_a_07.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity bidir_buffer is
                 port ( bidir : inout std_logic_vector;
                        ena : in std_ulogic;
                        going_out : in std_ulogic_vector;
                        coming_in : out std_ulogic_vector );
               end entity bidir_buffer;

--------------------------------------------------

               architecture behavior of bidir_buffer is
-- code from book

                 constant hi_impedance : std_logic_vector(bidir'range) := (others => 'Z');
                 -- . . .

-- end code from book
               begin
-- code from book

                 bidir <= To_stdlogicvector(going_out) when ena = '1' else
                          hi_impedance;
                 coming_in <= To_stdulogicvector(bidir);

-- end code from book
               end architecture behavior;



               entity ap_a_07 is
               end entity ap_a_07;


               library ieee;  use ieee.std_logic_1164.all;
               architecture test of ap_a_07 is

                 signal bidir : std_logic_vector(3 downto 0);
                 signal going_out, coming_in : std_ulogic_vector(3 downto 0);
                 signal ena : std_ulogic;

               begin

                 dut : entity work.bidir_buffer
                   port map ( bidir, ena, going_out, coming_in );

                 ena <= '0', '1' after 10 ns, '0' after 30 ns;

                 going_out <= "0000", "1111" after 20 ns;

                 bidir <= "ZZZZ", "0000" after 40 ns, "1111" after 50 ns, "ZZZZ" after 60 ns;

               end architecture test;
