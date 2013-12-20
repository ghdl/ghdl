
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
-- $Id: ap_a_ap_a_10.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity ap_a_10 is
end entity ap_a_10;

library ieee;  
use ieee.std_logic_1164.all;

library stimulus;  
use stimulus.stimulus_generators.all;

architecture test of ap_a_10 is

  signal a, b, c, d : std_ulogic;
  signal test_vector : std_ulogic_vector(1 to 4);

begin

  b1 : block is
               signal y : std_ulogic;
  begin
    -- code from book

    y <= a or b or c or d;

    -- end code from book
  end block b1;

  b2 : block is
               signal y : std_ulogic;
  begin
    -- code from book

    y <= ( a or b ) or ( c or d );

    -- end code from book
  end block b2;

  b3 : block is
               signal y : std_ulogic;
  begin
    -- code from book (syntax error)

    -- y <= a or b or c and d;

    -- end code from book
  end block b3;

  b4 : block is
               signal y : std_ulogic;
  begin
    -- code from book

    y <= ( a or b ) or ( c and d );

    -- end code from book
  end block b4;

  stimulus : all_possible_values(test_vector, 10 ns);

  (a, b, c, d) <= test_vector;

end architecture test;

