
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
-- $Id: ch_13_fg_13_11.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book
library ieee;  use ieee.std_logic_1164.all;

               entity controller is
               end entity controller;

-- end not in book


               architecture structural of controller is

                 component reg is
                                 generic ( width : positive );
                               port ( clock : in std_logic;
                                      data_in : in std_logic_vector(0 to width - 1);
                                      data_out : out std_logic_vector(0 to width - 1) );
                 end component reg;

                 -- . . .

                 -- not in book
                 subtype state_type is std_logic_vector(0 to 5);
                 signal clock_phase1 : std_logic;
                 signal next_state, current_state : state_type;
                 -- end not in book

               begin

                 state_reg : component reg
                   generic map ( width => state_type'length )
                   port map ( clock => clock_phase1,
                              data_in => next_state,
                              data_out => current_state );

                 -- . . .

               end architecture structural;
