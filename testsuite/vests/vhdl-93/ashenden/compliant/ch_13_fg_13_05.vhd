
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
-- $Id: ch_13_fg_13_05.vhd,v 1.3 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

-- code from book

library star_lib;
--use star_lib.edge_triggered_Dff;
use star_lib.all;

configuration reg4_gate_level of reg4 is

  for struct  -- architecture of reg4

    for bit0 : flipflop
      use entity star_lib.edge_triggered_Dff(hi_fanout);
    end for;

    for others : flipflop
      use entity star_lib.edge_triggered_Dff(basic);
    end for;

  end for;  -- end of architecture struct

end configuration reg4_gate_level;

-- end code from book


entity fg_13_05 is
end entity fg_13_05;


architecture test of fg_13_05 is

  component reg4 is
                   port ( clk, clr : in bit;  d : in bit_vector(0 to 3);
                   q : out bit_vector(0 to 3) );
  end component reg4;

  signal clk, clr : bit;
  signal d, q : bit_vector(0 to 3);

begin

  flag_reg : component reg4
    port map ( clk => clk, clr => clr, d => d, q => q );

end architecture test;


configuration fg_13_05_test of fg_13_05 is

  for test

    -- code from book (in text)

    for flag_reg : reg4
      use configuration work.reg4_gate_level;
    end for;

    -- end code from book

  end for;

end configuration fg_13_05_test;
