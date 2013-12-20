
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
-- $Id: ch_05_fg_05_16.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity and2 is
                 port ( a, b : in std_ulogic;  y : out std_ulogic );
               end entity and2;

--------------------------------------------------

               architecture detailed_delay of and2 is

                 signal result : std_ulogic;

               begin

                 gate : process (a, b) is
                 begin
                   result <= a and b;
                 end process gate;

                 delay : process (result) is
                 begin
                   if result = '1' then
                     y <= reject 400 ps inertial '1' after 1.5 ns;
                   elsif result = '0' then
                     y <= reject 300 ps inertial '0' after 1.2 ns;
                   else
                     y <= reject 300 ps inertial 'X' after 500 ps;
                   end if;
                 end process delay;

               end architecture detailed_delay;
