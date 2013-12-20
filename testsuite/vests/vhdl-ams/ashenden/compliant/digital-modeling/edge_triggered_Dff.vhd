
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

entity edge_triggered_Dff is
  port ( D : in bit;  clk : in bit;  clr : in bit;
         Q : out bit );
end entity edge_triggered_Dff;

--------------------------------------------------

architecture behavioral of edge_triggered_Dff is
begin

  state_change : process (clk, clr) is
  begin
    if clr = '1' then
      Q <= '0' after 2 ns;
    elsif clk'event and clk = '1' then
      Q <= D after 2 ns;
    end if;
  end process state_change;

end architecture behavioral;
