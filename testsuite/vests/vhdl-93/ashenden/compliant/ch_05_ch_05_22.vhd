
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
-- $Id: ch_05_ch_05_22.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- code from book:

entity mux4 is
  port ( i0, i1, i2, i3, sel0, sel1 : in bit;
         z : out bit );
end entity mux4;

-- end of code from book


----------------------------------------------------------------


architecture functional of mux4 is
begin

  out_select : process (sel0, sel1, i0, i1, i2, i3) is
                                                      subtype bits_2 is bit_vector(1 downto 0);
  begin
    case bits_2'(sel1, sel0) is
      when "00" =>  z <= i0;
      when "01" =>  z <= i1;
      when "10" =>  z <= i2;
      when "11" =>  z <= i3;
    end case;
  end process out_select;

end architecture functional;


----------------------------------------------------------------


entity ch_05_22 is

end entity ch_05_22;


----------------------------------------------------------------


architecture test of ch_05_22 is

  signal select_line, line0, line1, result_line : bit;

begin


  -- code from book:

  a_mux : entity work.mux4
    port map ( sel0 => select_line, i0 => line0, i1 => line1,
               z => result_line,
               sel1 => '0', i2 => '1', i3 => '1' );

  -- end of code from book


  ----------------


  stimulus : process is
  begin
    wait for 5 ns;
    line0 <= '1';				wait for 5 ns;
    line1 <= '1';				wait for 5 ns;
    select_line <= '1';	wait for 5 ns;
    line1 <= '0';				wait for 5 ns;
    line0 <= '0';				wait for 5 ns;

    wait;
  end process stimulus;


end architecture test;
