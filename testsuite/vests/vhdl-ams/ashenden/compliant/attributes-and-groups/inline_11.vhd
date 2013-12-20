
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

entity inline_11 is

end entity inline_11;


----------------------------------------------------------------


architecture test of inline_11 is

  component comp is
  end component comp;

  signal clk_phase1, clk_phase2 : bit;

  -- code from book:

  group signal_pair is (signal, signal);

  group clock_pair : signal_pair ( clk_phase1, clk_phase2 );

  attribute max_skew : time;

  attribute max_skew of clock_pair : group is 200 ps;

  group component_instances is ( label <> );

  group U1 : component_instances ( nand1, nand2, nand3 );
  group U2 : component_instances ( inv1, inv2 );

  attribute IC_allocation : string;

  attribute IC_allocation of U1 : group is "74LS00";
  attribute IC_allocation of U2 : group is "74LS04";

  -- end of code from book

begin


  nand1 : component comp;
  nand2 : component comp;
  nand3 : component comp;
  inv1 : component comp;
  inv2 : component comp;


end architecture test;
