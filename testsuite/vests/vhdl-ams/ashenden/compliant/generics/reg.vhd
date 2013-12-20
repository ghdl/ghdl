
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

entity reg is
  generic ( width : positive );
  port ( d :  in  bit_vector(0 to width - 1);
         q :  out  bit_vector(0 to width - 1);
         clk, reset : in bit );
end entity reg;

--------------------------------------------------

architecture behavioral of reg is
begin

  behavior : process (clk, reset) is
    constant zero : bit_vector(0 to width - 1) := (others => '0');
  begin
    if reset = '1' then
      q <= zero;
    elsif clk'event and clk = '1' then
      q <= d;
    end if;
  end process behavior;

end architecture behavioral;
