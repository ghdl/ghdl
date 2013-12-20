
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
-- $Id: ch_13_fg_13_21.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity reg is
                 generic ( t_setup, t_hold, t_pd : delay_length;
                           width : positive );
                 port ( clock : in std_logic;
                        reset_n : in std_logic;
                        data_in : in std_logic_vector(0 to width - 1);
                        data_out : out std_logic_vector(0 to width - 1) );
               end entity reg;



-- not in book

               architecture gate_level of reg is

               begin

                 store : process (clock, reset_n) is
                 begin
                   if reset_n = '0' or reset_n = 'L' then
                     data_out <= (others => '0') after t_pd;
                   elsif rising_edge(clock) then
                     data_out <= data_in after t_pd;
                   end if;
                 end process store;

               end architecture gate_level;

-- end not in book
