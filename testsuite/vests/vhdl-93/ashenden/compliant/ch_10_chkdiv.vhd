
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
-- $Id: ch_10_chkdiv.vhd,v 1.3 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity check_div is
end entity check_div;

library bv_utilities;
use bv_utilities.bv_arithmetic.all;

architecture behav of check_div is

begin

  checker : process is

                      variable bv_a, bv_b, bv_quotient, bv_remainder : bit_vector(3 downto 0);
                    variable div_by_zero : boolean;

  begin
    for a in 0 to 15 loop
      for b in 0 to 15 loop
	bv_a := natural_to_bv(a, bv_a'length);
        bv_b := natural_to_bv(b, bv_b'length);
        bv_divu(bv_a, bv_b, bv_quotient, bv_remainder, div_by_zero);
	if b = 0 then
	  assert div_by_zero
	    report integer'image(a) & '/' & integer'image(b)
            & ": div_by_zero not true";
	else
	  assert not div_by_zero
	    report integer'image(a) & '/' & integer'image(b)
            & ": div_by_zero not false";
	  assert bv_to_natural(bv_quotient) = a / b
	    report integer'image(a) & '/' & integer'image(b)
            & ": quotient = " & integer'image(bv_to_natural(bv_quotient));
	  assert bv_to_natural(bv_remainder) = a rem b
	    report integer'image(a) & '/' & integer'image(b)
            & ": remainder = " & integer'image(bv_to_natural(bv_remainder));
	end if;
      end loop;
    end loop;
    wait;
  end process checker;

end architecture behav;
