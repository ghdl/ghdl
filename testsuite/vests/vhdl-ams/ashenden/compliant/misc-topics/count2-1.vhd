
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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
