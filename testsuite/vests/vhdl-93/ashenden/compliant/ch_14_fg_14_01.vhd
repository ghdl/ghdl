
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
-- $Id: ch_14_fg_14_01.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity D_flipflop is
                 port ( clk : in std_logic;  d : in std_logic;
                 q : out std_logic );
               end entity D_flipflop;


               architecture synthesized of D_flipflop is
               begin
                 q <= d when not clk'stable and (To_X01(clk) = '1') and 
                      (To_X01(clk'last_value) = '0');
               end architecture synthesized;


               library ieee;  use ieee.std_logic_1164.all;

               entity tristate_buffer is
                 port ( a : in std_logic;
                        en : in std_logic;
                        y : out std_logic );
               end entity tristate_buffer;


               architecture synthesized of tristate_buffer is
               begin
                 y <= 'X' when is_X(en) else
                      a  when To_X01(en) = '1' else
                      'Z';
               end architecture synthesized;



-- code from book (in Figure 14-1)

               library ieee;  use ieee.std_logic_1164.all;

               entity register_tristate is
                 generic ( width : positive );
                 port ( clock : in std_logic;
                        out_enable : in std_logic;
                        data_in : in std_logic_vector(0 to width - 1);
                        data_out : out std_logic_vector(0 to width - 1) );
               end entity register_tristate;

--------------------------------------------------

               architecture cell_level of register_tristate is

                 component D_flipflop is
                                        port ( clk : in std_logic;  d : in std_logic;
                                        q : out std_logic );
                 end component D_flipflop;

                 component tristate_buffer is
                                             port ( a : in std_logic;
                                                    en : in std_logic;
                                                    y : out std_logic );
                 end component tristate_buffer;

               begin

                 cell_array : for bit_index in 0 to width - 1 generate

                   signal data_unbuffered : std_logic;

                 begin

                   cell_storage : component D_flipflop
                     port map ( clk => clock, d => data_in(bit_index),
                                q => data_unbuffered );

                   cell_buffer : component tristate_buffer
                     port map ( a => data_unbuffered, en => out_enable,
                                y => data_out(bit_index) );

                 end generate cell_array;

               end architecture cell_level;

-- end code from book (in Figure 14-1)


-- code from book (in Figure 14-11)

               library cell_lib;

               configuration identical_cells of register_tristate is

                 for cell_level

                   for cell_array

                   for cell_storage : D_flipflop
                   use entity cell_lib.D_flipflop(synthesized);
               end for;

               for cell_buffer : tristate_buffer
                 use entity cell_lib.tristate_buffer(synthesized);
               end for;

               end for;

               end for;

               end configuration identical_cells;

-- code from book (in Figure 14-11)



               library ieee;  use ieee.std_logic_1164.all;

               entity fg_14_01 is
               end entity fg_14_01;


               architecture test of fg_14_01 is

                 signal clk, en : std_logic;
                 signal d_in, d_out : std_logic_vector(0 to 3);

               begin

                 dut : configuration work.identical_cells
                   generic map ( width => d_in'length )
                   port map ( clock => clk, out_enable => en,
                              data_in => d_in, data_out => d_out );

                 stimulus : process is
                 begin
                   wait for 10 ns;
                   d_in <= "0000";  en <= '0';  clk <= '0';  wait for 10 ns;
                   clk <= '1', '0' after 5 ns;  wait for 10 ns;
                   en <= '1', '0' after 5 ns;   wait for 10 ns;
                   d_in <= "0101";              wait for 10 ns;
                   clk <= '1', '0' after 5 ns;  wait for 10 ns;
                   en <= 'H', '0' after 5 ns;   wait for 10 ns;

                   wait;
                 end process stimulus;

               end architecture test;

-- end not in book
