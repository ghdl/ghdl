
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
-- $Id: ch_06_pas-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavioral of product_adder_subtracter is
begin

  behavior : process (a, b) is

                              constant Tpd_in_out : time := 3 ns;
                            variable op2 : std_ulogic_vector(b'range);
                            variable carry_in : std_ulogic;
                            variable carry_out : std_ulogic;

  begin
    carry_out := To_X01(mode);
    if To_X01(mode) = '1' then
      op2 := not b;
    else
      op2 := b;
    end if;
    for index in 0 to 31 loop
      carry_in := carry_out;  -- of previous bit
      s(index) <= a(index) xor op2(index) xor carry_in after Tpd_in_out;
      carry_out := (a(index) and op2(index))
      	      	   or (carry_in and (a(index) xor op2(index)));
    end loop;
    s(32) <= a(31) xor op2(31) xor carry_out after Tpd_in_out;
  end process behavior;

end architecture behavioral;
