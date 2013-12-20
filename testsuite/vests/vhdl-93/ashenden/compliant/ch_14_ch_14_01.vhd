
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
-- $Id: ch_14_ch_14_01.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity buf is
                 port ( a : in std_logic;  y : out std_logic );
               end buf;


               architecture basic of buf is
               begin
                 y <= a;
               end basic;




               library ieee;  use ieee.std_logic_1164.all;

               entity fanout_tree is
                 generic ( height : natural );
                 port ( input : in std_logic;
                        output : out std_logic_vector (0 to 2**height - 1) );
               end fanout_tree;

--------------------------------------------------

               architecture recursive of fanout_tree is

                 component buf
                   port ( a : in std_logic;  y : out std_logic );
                 end component;

                 component fanout_tree
                   generic ( height : natural );
                   port ( input : in std_logic;
                          output : out std_logic_vector (0 to 2**height - 1) );
                 end component;

                 signal buffered_input_0, buffered_input_1 : std_logic;

               begin

                 degenerate_tree : if height = 0 generate
                   output(0) <= input;
                 end generate degenerate_tree;

                 compound_tree : if height > 0 generate

                   buf_0 : buf
                     port map ( a => input, y => buffered_input_0 );

                   -- code from book

                   block_0 : block
                     for subtree_0 : fanout_tree
                       use entity work.fanout_tree(recursive);
                   begin
                     subtree_0 : fanout_tree
                       generic map ( height => height - 1 )
                       port map (  input => buffered_input_0,
                                   output => output(0 to 2**(height - 1) - 1) );
                   end block block_0;

                   -- end code from book

                   buf_1 : buf
                     port map ( a => input, y => buffered_input_1 );

                   block_1 : block
                     for subtree_1 : fanout_tree
                       use entity work.fanout_tree(recursive);
                   begin
                     subtree_1 : fanout_tree
                       generic map ( height => height - 1 )
                       port map ( input => buffered_input_1,
                                  output => output(2**(height - 1) to 2**height - 1) );
                   end block block_1;

                 end generate compound_tree;

               end recursive;




               library ieee;  use ieee.std_logic_1164.all;

               entity ch_14_01 is
               end ch_14_01;


               architecture test of ch_14_01 is

                 component fanout_tree
                   generic ( height : natural );
                   port ( input : in std_logic;
                          output : out std_logic_vector (0 to 2**height - 1) );
                 end component;

                 for clock_buffer_tree : fanout_tree
                   use entity work.fanout_tree(recursive);

                 signal unbuffered_clock : std_logic;
                 signal buffered_clock_array : std_logic_vector(0 to 7);

               begin

                 clock_buffer_tree : fanout_tree
                   generic map ( height => 3 )
                   port map ( input => unbuffered_clock,
                              output => buffered_clock_array );

                 clock_gen : process
                 begin
                   unbuffered_clock <= '1' after 5 ns, '0' after 10 ns;
                   wait for 10 ns;
                 end process clock_gen;

               end test;
