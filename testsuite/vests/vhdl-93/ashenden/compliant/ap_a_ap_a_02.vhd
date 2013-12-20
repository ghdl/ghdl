
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
-- $Id: ap_a_ap_a_02.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ap_a_02 is

end entity ap_a_02;


library ieee;  use ieee.std_logic_1164.all;

architecture test of ap_a_02 is

  -- code from book

  -- end code from book

begin

  b1 : block is
               signal sulv : std_ulogic_vector(7 downto 0);
             signal slv : std_logic_vector(7 downto 0);
  begin
    -- code from book

    sulv <= To_stdulogicvector ( slv );

    -- end code from book
    slv <= "10101010";
  end block b1;

  b2 : block is
               signal sulv : std_ulogic_vector(7 downto 0);
             signal slv : std_logic_vector(7 downto 0);
  begin
    -- code from book

    slv <= To_stdlogicvector ( sulv );

    -- end code from book
    sulv <= "00001111";
  end block b2;

  b3 : block is
               signal a, ena, y : std_logic;
  begin
    -- code from book

    y <= a when ena = '1' else
         'Z';

    -- end code from book
    ena <= '0', '1' after 20 ns, '0' after 40 ns;
    a <= '0', '1' after 10 ns, '0' after 30 ns, '1' after 50 ns;
  end block b3;

  b4 : block is
               signal a, ena, y : std_logic;
  begin
    -- code from book

    y <= a when ena = '1' else
         'H';

    -- end code from book
    ena <= '0', '1' after 20 ns, '0' after 40 ns;
    a <= '0', '1' after 10 ns, '0' after 30 ns, '1' after 50 ns;
  end block b4;

  b5 : block is
               signal a, b, x, s, y : std_logic;
  begin
    -- code from book

    y <= a when x = '1' else
         b when s = '1' else
         '-';

    -- end code from book
    x <= '0', '1' after 20 ns, '0' after 40 ns;
    s <= '0', '1' after 60 ns, '0' after 80 ns;
    a <= '0', '1' after 10 ns, '0' after 30 ns,
         '1' after 50 ns, '0' after 70 ns,
         '1' after 90 ns;
    b <= '0', '1' after 15 ns, '0' after 35 ns,
         '1' after 55 ns, '0' after 75 ns,
         '1' after 95 ns;
  end block b5;

end architecture test;

