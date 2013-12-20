
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
-- $Id: ch_08_fg_08_04.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

library ieee;  use ieee.std_logic_1164.all;

               entity phase_locked_clock_gen is
                 port ( reference : in std_ulogic;
                        phi1, phi2 : out std_ulogic );
               end entity phase_locked_clock_gen;


               architecture std_cell of phase_locked_clock_gen is

                 --use work.clock_pkg.Tpw;
                 use work.clock_pkg.all;

               begin

                 phi1_gen : phi1 <= '1', '0' after Tpw when rising_edge(reference);

                 phi2_gen : phi2 <= '1', '0' after Tpw when falling_edge(reference);

               end architecture std_cell;

-- end not in book



               library ieee;  use ieee.std_logic_1164.all;

               entity io_controller is
                 port ( ref_clock : in std_ulogic;  -- . . . );
                        -- not in book
                        other_port : in std_ulogic );
                 -- end not in book
               end entity io_controller;

--------------------------------------------------

               architecture top_level of io_controller is

                 -- . . .

                 -- not in book
                 signal rd, wr, sel, width, burst : std_ulogic;
                 signal addr : std_ulogic_vector(1 downto 0);
                 signal ready : std_ulogic;
                 signal control_reg_wr, status_reg_rd, data_fifo_wr, data_fifo_rd,
                   other_signal : std_ulogic;
                 -- end not in book

               begin

                 internal_clock_gen : entity work.phase_locked_clock_gen(std_cell)
                   port map ( reference => ref_clock,
                              phi1 => work.clock_pkg.clock_phase1,
                              phi2 => work.clock_pkg.clock_phase2 );

                 the_bus_sequencer : entity work.bus_sequencer(fsm)
                   port map ( rd, wr, sel, width, burst, addr(1 downto 0), ready,
                              control_reg_wr, status_reg_rd, data_fifo_wr, data_fifo_rd,
                              -- . . . );
                              other_signal );
                 -- not in book

                 -- . . .

               end architecture top_level;
