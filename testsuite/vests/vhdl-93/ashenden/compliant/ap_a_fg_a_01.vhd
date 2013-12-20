
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
-- $Id: ap_a_fg_a_01.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_a_01 is
end entity fg_a_01;



library ieee;  use ieee.std_logic_1164.all;

architecture test of fg_a_01 is

  signal clk, d : std_ulogic;

begin

  stimulus : process is
  begin
    clk <= '0';  d <= '0';  wait for 10 ns;
    clk <= '1', '0' after 10 ns;  wait for 20 ns;
    d <= '1';  wait for 10 ns;
    clk <= '1', '0' after 20 ns;  d <= '0' after 10 ns;

    wait;
  end process stimulus;


  b1 : block is
               signal q : std_ulogic;
  begin

    -- code from book

    process (clk) is
    begin
      if rising_edge(clk) then
        q <= d;
      end if;
    end process;

    -- end code from book

  end block b1;


  b2 : block is
               signal q : std_ulogic;
  begin

    -- code from book

    process is
    begin
      wait until rising_edge(clk);
      q <= d;
    end process;

    -- end code from book

  end block b2;


  b3 : block is
               signal q : std_ulogic;
  begin

    -- code from book

    q <= d when rising_edge(clk) else
         q;

    -- end code from book

  end block b3;


  b4 : block is
               signal q : std_ulogic;
  begin

    -- code from book

    b : block ( rising_edge(clk)
                and not clk'stable ) is
    begin
      q <= guarded d;
    end block b;

    -- end code from book

  end block b4;

end architecture test;
