
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
-- $Id: ch_05_tb_05_10.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity add_1 is
  port ( d0, d1, d2, d3 : in bit;
         y0, y1, y2, y3 : out  bit );
end entity add_1;


architecture boolean_eqn of add_1 is
begin

  y0 <= not d0 after 4 ns;

  y1 <= (not d1 and d0)
        or (d1 and not d0) after 4 ns;

  y2 <= (not d2 and d1 and d0)
	or (d2 and not (d1 and d0)) after 4 ns;

  y3 <= (not d3 and d2 and d1 and d0)
	or (d3 and not (d2 and d1 and d0)) after 4 ns;

end architecture boolean_eqn;


entity buf4 is
  port ( a0, a1, a2, a3 : in bit;
         y0, y1, y2, y3 : out  bit );
end entity buf4;


architecture basic of buf4 is
begin

  y0 <= a0 after 2 ns;
  y1 <= a1 after 2 ns;
  y2 <= a2 after 2 ns;
  y3 <= a3 after 2 ns;

end architecture basic;


package counter_types is

  subtype digit is bit_vector(3 downto 0);

end package counter_types;

