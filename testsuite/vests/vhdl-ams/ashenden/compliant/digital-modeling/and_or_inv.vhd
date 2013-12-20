
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

-- not in book

entity and_or_inv is
  port ( a1, a2, b1, b2 : in bit := '1';
	 y : out bit );
end entity and_or_inv;

-- end not in book


architecture primitive of and_or_inv is

  signal and_a, and_b : bit;
  signal or_a_b : bit;

begin

  and_gate_a : process (a1, a2) is
  begin
    and_a <= a1 and a2;
  end process and_gate_a;

  and_gate_b : process (b1, b2) is
  begin
    and_b <= b1 and b2;
  end process and_gate_b;

  or_gate : process (and_a, and_b) is
  begin
    or_a_b <= and_a or and_b;
  end process or_gate;

  inv : process (or_a_b) is
  begin
    y <= not or_a_b;
  end process inv;

end architecture primitive;
