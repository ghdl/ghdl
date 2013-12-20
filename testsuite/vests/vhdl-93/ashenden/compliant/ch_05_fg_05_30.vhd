
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
-- $Id: ch_05_fg_05_30.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

library widget_cells, wasp_lib;

architecture cell_based of filter is

  -- declaration of signals, etc
  -- . . .

  -- not in book

  signal clk, filter_clk, accum_en, carry : bit;
  signal sum, alu_op1, alu_op2, result : bit_vector(31 downto 0);

  -- end not in book

begin

  clk_pad : entity wasp_lib.in_pad
    port map ( i => clk, z => filter_clk );

  accum : entity widget_cells.reg32
    port map ( en => accum_en, clk => filter_clk, d => sum,
               q => result );

  alu : entity work.adder
    port map ( a => alu_op1, b => alu_op2, y => sum, c => carry );

  -- other component instantiations
  -- . . .

end architecture cell_based;
