
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
-- $Id: ch_01_fg_01_11.vhd,v 1.1.1.1 2001-08-22 18:20:47 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

architecture struct of reg4 is

  signal int_clk : bit;

begin

  bit0 : entity work.d_latch(basic)
    port map (d0, int_clk, q0);
  bit1 : entity work.d_latch(basic)
    port map (d1, int_clk, q1);
  bit2 : entity work.d_latch(basic)
    port map (d2, int_clk, q2);
  bit3 : entity work.d_latch(basic)
    port map (d3, int_clk, q3);

  gate : entity work.and2(basic)
    port map (en, clk, int_clk);

end architecture struct;
