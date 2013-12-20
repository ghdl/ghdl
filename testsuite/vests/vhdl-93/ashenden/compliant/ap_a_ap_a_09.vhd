
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
-- $Id: ap_a_ap_a_09.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ap_a_09 is

end entity ap_a_09;


library ieee;  use ieee.std_logic_1164.all;

architecture test of ap_a_09 is

  signal a, b, c, d : integer := 0;

begin

  b1 : block is
               signal y : integer;
  begin
    -- code from book

    y <= a + b + c + d;

    -- end code from book
  end block b1;

  b2 : block is
               signal y : integer;
  begin
    -- code from book

    y <= ( a + b ) + ( c + d );

    -- end code from book
  end block b2;

  stimulus : process is
  begin
    a <= 1; wait for 10 ns;
    b <= 2; wait for 10 ns;
    c <= 3; wait for 10 ns;
    d <= 4; wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;

