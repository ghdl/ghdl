
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
-- $Id: ch_05_fg_05_18.vhd,v 1.5 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.5 $
--
-- ---------------------------------------------------------------------

entity fg_05_18 is
end entity fg_05_18;

library stimulus;

architecture test of fg_05_18 is

  use stimulus.stimulus_generators.all;

  signal sel0, sel1, d0, d1, d2, d3 : bit := '0';
  signal functional_z, equivalent_z : bit;

begin

  functional_mux : block is
                           port ( z : out bit );
                         port map ( z => functional_z );
  begin

    -- code from book

    zmux : z <= d0 when sel1 = '0' and sel0 = '0' else
                d1 when sel1 = '0' and sel0 = '1' else
                d2 when sel1 = '1' and sel0 = '0' else
                d3;

    -- end code from book

  end block functional_mux;

  equivalent_mux : block is
                           port ( z : out bit );
                         port map ( z => equivalent_z );
  begin

    -- code from book

    zmux : process is
    begin
      if sel1 = '0' and sel0 = '0' then
        z <= d0;
      elsif sel1 = '0' and sel0 = '1' then
        z <= d1;
      elsif sel1 = '1' and sel0 = '0' then
        z <= d2;
      else
        z <= d3;
      end if;
      wait on d0, d1, d2, d3, sel0, sel1;
    end process zmux;

    -- end code from book

  end block equivalent_mux;

  stimulus_proc :
    all_possible_values( bv(0) => sel0, bv(1) => sel1,
			 bv(2) => d0, bv(3) => d1,
			 bv(4) => d2, bv(5) => d3,
			 delay_between_values => 10 ns );

  verifier :
    assert functional_z = equivalent_z
      report "Functional and equivalent models give different results";

end architecture test;
