
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
-- $Id: ch_05_ch_05_23.vhd,v 1.2 2001-10-24 23:30:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- code from book:

entity and_or_inv is
  port ( a1, a2, b1, b2 : in bit := '1';
         y : out bit );
end entity and_or_inv;

-- end of code from book

architecture functional of and_or_inv is
begin

  func : y <= not ((a1 and a2) or (b1 and b2));

end architecture functional;

entity ch_05_23 is

end entity ch_05_23;

library stimulus;

architecture test of ch_05_23 is

  signal A, B, C, F : bit;
  signal test_input : bit_vector(2 downto 0);

  use stimulus.stimulus_generators.all;

begin

  -- code from book:

  f_cell : entity work.and_or_inv
    port map (a1 => A, a2 => B, b1 => C, b2 => open, y => F);

  -- end of code from book

  stimulus_proc : all_possible_values( bv => test_input,
				  delay_between_values => 10 ns );

  (A, B, C) <= test_input;

  verifier :
    postponed assert F = not ((A and B) or C)
      report "function model produced unexpected result";

end architecture test;
