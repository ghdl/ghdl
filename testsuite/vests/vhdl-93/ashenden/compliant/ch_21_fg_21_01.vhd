
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
-- $Id: ch_21_fg_21_01.vhd,v 1.2 2001-10-26 16:29:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity D_flipflop is
  port ( clk, d : in bit;  q : buffer bit );
end entity D_flipflop;


architecture behavioral of D_flipflop is
begin
  q <= d when clk'event and clk = '1';
end architecture behavioral;



entity inverter is
  port ( a : in bit;  y : out bit );
end entity inverter;


architecture behavioral of inverter is
begin
  y <= not a;
end architecture behavioral;



-- code from book

entity count2 is
  port ( clk : in bit;  q0, q1 : buffer bit );
end entity count2;

--------------------------------------------------

architecture buffered_outputs of count2 is

  component D_flipflop is
                         port ( clk, d : in bit;  q : buffer bit );
  end component D_flipflop;

  component inverter is
                       port ( a : in bit;  y : out bit );
  end component inverter;

  signal q0_n, q1_n : bit;

begin

  bit0 : component D_flipflop
    port map ( clk => clk, d => q0_n, q => q0 );

  inv0 : component inverter
    port map ( a => q0, y => q0_n );

  bit1 : component D_flipflop
    port map ( clk => q0_n, d => q1_n, q => q1 );

  inv1 : component inverter
    port map ( a => q1, y => q1_n );

end architecture buffered_outputs;

-- end code from book



entity fg_21_01 is
end entity fg_21_01;


architecture test of fg_21_01 is

  signal clk, q0, q1 : bit;

begin

  dut : entity work.count2(buffered_outputs)
    port map ( clk => clk, q0 => q0, q1 => q1 );

  clk_gen : clk <= not clk after 10 ns;

end architecture test;
