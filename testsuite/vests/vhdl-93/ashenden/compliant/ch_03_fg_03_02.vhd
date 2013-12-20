
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
-- $Id: ch_03_fg_03_02.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

-- test code:

use work.test_bench_03_02.all;

-- end test code

library ieee;  use ieee.std_logic_1164.all;

               entity mux4 is
                 port ( sel : in sel_range;
                        d0, d1, d2, d3 : in std_ulogic;
                        z : out std_ulogic );
               end entity mux4;

               architecture demo of mux4 is
               begin

                 out_select : process (sel, d0, d1, d2, d3) is
                 begin
                   case sel is
                     when 0 => 
                       z <= d0;
                     when 1 => 
                       z <= d1;
                     when 2 => 
                       z <= d2;
                     when 3 => 
                       z <= d3;
                   end case;
                 end process out_select;

               end architecture demo;
