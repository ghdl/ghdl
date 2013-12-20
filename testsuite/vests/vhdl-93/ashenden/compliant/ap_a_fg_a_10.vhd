
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
-- $Id: ap_a_fg_a_10.vhd,v 1.1.1.1 2001-08-22 18:20:47 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity fg_a_10 is
end entity fg_a_10;


library ieee;  use ieee.std_logic_1164.all;

architecture test of fg_a_10 is

  signal clk25M, resetl : std_ulogic;
  signal data, odat : std_ulogic_vector(7 downto 0);

begin

  -- code from book

  right_way : process ( clk25M, resetl )
  begin
    if resetl = '0' then
      odat <= B"0000_0000";
    elsif rising_edge(clk25M) then
      if data = B"0000_0000" then
        odat <= B"0000_0001";
      else
        odat <= data;
      end if;
    end if;
  end process right_way;

  -- end code from book

  data <= odat(6 downto 0) & '0';

  clk_gen : process is
  begin
    clk25M <= '0', '1' after 10 ns;
    wait for 20 ns;
  end process clk_gen;

  resetl <= '1', '0' after 20 ns, '1' after 60 ns;

end architecture test;
