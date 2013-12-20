
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
-- $Id: ch_06_acca-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavioral of accumulator_adder is
begin

  behavior : process (a, b) is

                              constant Tpd_in_out : time := 3 ns;
                            variable carry_in : std_ulogic;
                            variable carry_out : std_ulogic := '0';

  begin
    for index in 0 to 21 loop
      carry_in := carry_out;  -- of previous bit
      s(index) <= a(index) xor b(index) xor carry_in after Tpd_in_out;
      carry_out := (a(index) and b(index))
      	      	   or (carry_in and (a(index) xor b(index)));
    end loop;
    ovf <= carry_out xor carry_in after Tpd_in_out;  -- ovf is carry_out /= carry_in
  end process behavior;

end architecture behavioral;
