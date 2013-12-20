
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
-- $Id: ch_08_fg_08_08.vhd,v 1.3 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

package bit_vector_signed_arithmetic is

  function "+" ( bv1, bv2 : bit_vector ) return bit_vector;

  function "-" ( bv : bit_vector ) return bit_vector;

  function "*" ( bv1, bv2 : bit_vector ) return bit_vector;

  -- . . .

end package bit_vector_signed_arithmetic;

-- not in book
library bv_utilities;
use bv_utilities.bv_arithmetic;
-- end not in book

package body bit_vector_signed_arithmetic is

  function "+" ( bv1, bv2 : bit_vector ) return bit_vector is -- . . .
    -- not in book
  begin
    return bv_arithmetic."+"(bv1, bv2);
  end function "+";
  -- end not in book

  function "-" ( bv : bit_vector ) return bit_vector is -- . . .
    -- not in book
  begin
    return bv_arithmetic."-"(bv);
  end function "-";
  -- end not in book

  function mult_unsigned ( bv1, bv2 : bit_vector ) return bit_vector is
    -- . . .
  begin
    -- not in book
    -- . . .
    return bv_arithmetic.bv_multu(bv1, bv2);
    -- end not in book
  end function mult_unsigned;

  function "*" ( bv1, bv2 : bit_vector ) return bit_vector is
  begin
    if bv1(bv1'left) = '0' and bv2(bv2'left) = '0' then
      return mult_unsigned(bv1, bv2);
    elsif bv1(bv1'left) = '0' and bv2(bv2'left) = '1' then
      return -mult_unsigned(bv1, -bv2);
    elsif bv1(bv1'left) = '1' and bv2(bv2'left) = '0' then
      return -mult_unsigned(-bv1, bv2);
    else
      return mult_unsigned(-bv1, -bv2);
    end if;
  end function "*";

  -- . . .

end package body bit_vector_signed_arithmetic;

-- not in book

entity fg_08_08 is
end entity fg_08_08;

library bv_utilities;
use bv_utilities.bit_vector_signed_arithmetic.all;

use std.textio.all;

architecture test of fg_08_08 is
begin

  stimulus : process is
                       variable L : line;
  begin
    write(L, X"0002" + X"0005");
    writeline(output, L);
    write(L, X"0002" + X"FFFE");
    writeline(output, L);
    write(L, - X"0005");
    writeline(output, L);
    write(L, - X"FFFE");
    writeline(output, L);
    write(L, X"0002" * X"0005");
    writeline(output, L);
    write(L, X"0002" * X"FFFD");
    writeline(output, L);

    wait;
  end process stimulus;

end architecture test;

-- end not in book
