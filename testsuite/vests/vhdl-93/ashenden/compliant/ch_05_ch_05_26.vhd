
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
-- $Id: ch_05_ch_05_26.vhd,v 1.2 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_05_26 is
end entity ch_05_26;

-- code from book:

library widget_cells, wasp_lib;

use widget_cells.reg32;

-- end of code from book


architecture test of ch_05_26 is

  signal filter_clk, accum_en : bit;
  signal sum, result : bit_vector(31 downto 0);

begin


  -- code from book:

  accum : entity reg32
    port map ( en => accum_en, clk => filter_clk, d => sum,
               q => result );

  -- end of code from book


end architecture test;
