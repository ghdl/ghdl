
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
-- $Id: ch_05_ch_05_25.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- VHDL-87


entity mux4 is
  port ( i0, i1, i2, i3, sel0, sel1 : in bit;
         z : out bit );
end mux4;


----------------------------------------------------------------


architecture functional of mux4 is
begin

  out_select : process (sel0, sel1, i0, i1, i2, i3)
    subtype bits_2 is bit_vector(1 downto 0);
  begin
    case bits_2'(sel1, sel0) is
      when "00" =>  z <= i0;
      when "01" =>  z <= i1;
      when "10" =>  z <= i2;
      when "11" =>  z <= i3;
    end case;
  end process out_select;

end functional;


----------------------------------------------------------------


entity ch_05_25 is

end ch_05_25;


----------------------------------------------------------------


architecture test of ch_05_25 is

  signal select_line, line0, line1, result_line : bit;

  -- code from book:

  signal tied_0 : bit := '0';
  signal tied_1 : bit := '1';

  -- end of code from book

  component mux4
    port ( i0, i1, i2, i3, sel0, sel1 : in bit;
           z : out bit );
  end component;

  for all : mux4
    use entity work.mux4;

begin


  a_mux : mux4

    -- code from book:

    port map ( sel0 => select_line, i0 => line0, i1 => line1,
               z => result_line,
               sel1 => tied_0, i2 => tied_1, i3 => tied_1 );

  -- end of code from book


  ----------------


  stimulus : process
  begin
    wait for 5 ns;
    line0 <= '1';				wait for 5 ns;
    line1 <= '1';				wait for 5 ns;
    select_line <= '1';	wait for 5 ns;
    line1 <= '0';				wait for 5 ns;
    line0 <= '0';				wait for 5 ns;

    wait;
  end process stimulus;


end test;
