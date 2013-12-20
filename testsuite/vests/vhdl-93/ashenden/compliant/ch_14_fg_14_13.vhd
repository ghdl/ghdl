
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
-- $Id: ch_14_fg_14_13.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity ms_flipflop is
                 port ( phi1, phi2 : in std_logic;
                        d : in std_logic;
                        q : out std_logic );
               end entity ms_flipflop;


               architecture normal_drive of ms_flipflop is
                 signal master_d : std_logic;
               begin
                 master_d <= d when phi1 = '1';
                 q <= master_d when phi2 = '1';
               end architecture normal_drive;


               architecture high_drive of ms_flipflop is
                 signal master_d : std_logic;
               begin
                 master_d <= d when phi1 = '1';
                 q <= master_d when phi2 = '1';
               end architecture high_drive;



-- code from book

               library cell_lib;

               configuration last_high_drive of shift_reg is

                 for cell_level

                   -- workaround for MTI bug mt026
                   -- for reg_array ( 0 to parallel_data'length - 2 )
                   for reg_array ( 0 to 2 )
                   -- end workaround

                   for first_cell
                   for cell : master_slave_flipflop
                   use entity cell_lib.ms_flipflop(normal_drive);
               end for;
               end for;

               for other_cell
                 for cell : master_slave_flipflop
                 use entity cell_lib.ms_flipflop(normal_drive);
               end for;
               end for;

               end for;

               -- workaround for MTI bug mt026
               -- for reg_array ( parallel_data'length - 1 )
               for reg_array ( 3 )
                 -- end workaround

                 for other_cell
                 for cell : master_slave_flipflop
                 use entity cell_lib.ms_flipflop(high_drive);
               end for;
               end for;

               end for;

               end for;

               end configuration last_high_drive;

-- end code from book


               library ieee;  use ieee.std_logic_1164.all;

               entity fg_14_13 is
               end entity fg_14_13;


               architecture test of fg_14_13 is

                 signal phi1, phi2, serial_data_in : std_logic := '0';
                 signal parallel_data : std_logic_vector(3 downto 0);

               begin

                 dut : configuration work.last_high_drive
                   port map ( phi1 => phi1, phi2 => phi2,
                              serial_data_in => serial_data_in,
                              parallel_data => parallel_data );

                 clock_gen : process is
                 begin
                   phi1 <= '1', '0' after 4 ns;
                   phi2 <= '1' after 5 ns, '0' after 9 ns;
                   wait for 10 ns;
                 end process clock_gen;

                 stimulus : process is
                 begin
                   serial_data_in <= '0';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '0';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '0';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '0';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '0';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '1';  wait until phi2 = '1';
                   serial_data_in <= '0';

                   wait;
                 end process stimulus;

               end architecture test;
