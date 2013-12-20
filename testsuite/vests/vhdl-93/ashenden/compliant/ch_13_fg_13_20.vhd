
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
-- $Id: ch_13_fg_13_20.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

library ieee;  use ieee.std_logic_1164.all;

               entity control_section is
               end entity control_section;

-- end not in book


               architecture structural of control_section is

                 component reg is
                                 generic ( width : positive );
                               port ( clk : in std_logic;
                                      d : in std_logic_vector(0 to width - 1);
                                      q : out std_logic_vector(0 to width - 1) );
                 end component reg;

                 for flag_reg : reg
                   use entity work.reg(gate_level)
                   -- workaround for MTI bug mt023
                   --  reverted for ghdl
                   port map ( clock => clk, data_in => d, data_out => q );
                   -- port map ( clock => clk, data_in => d, data_out => q, reset_n => '1' );
                 -- end workaround

                 -- . . .

                 -- not in book
                 signal clock_phase1, zero_result, neg_result, overflow_result,
                   zero_flag, neg_flag, overflow_flag : std_logic;
                 -- end not in book

               begin

                 flag_reg : component reg
                   generic map ( width => 3 )
                   port map ( clk => clock_phase1,
                              d(0) => zero_result, d(1) => neg_result,
                              d(2) => overflow_result,
                              q(0) => zero_flag, q(1) => neg_flag,
                              q(2) => overflow_flag );

                 -- . . .

                 -- not in book

                 stimulus : process is
                 begin
                   clock_phase1 <= '0';
                   zero_result <= '0'; neg_result <= '0'; overflow_result <= '0'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;
                   zero_result <= '0'; neg_result <= '0'; overflow_result <= '1'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;
                   zero_result <= '0'; neg_result <= '1'; overflow_result <= '0'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;
                   zero_result <= '0'; neg_result <= '1'; overflow_result <= '1'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;
                   zero_result <= '1'; neg_result <= '0'; overflow_result <= '0'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;
                   zero_result <= '1'; neg_result <= '0'; overflow_result <= '1'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;
                   zero_result <= '1'; neg_result <= '1'; overflow_result <= '0'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;
                   zero_result <= '1'; neg_result <= '1'; overflow_result <= '1'; wait for 10 ns;
                   clock_phase1 <= '1', '0' after 5 ns;  wait for 10 ns;

                   wait;
                 end process stimulus;

                 -- end not in book

               end architecture structural;
