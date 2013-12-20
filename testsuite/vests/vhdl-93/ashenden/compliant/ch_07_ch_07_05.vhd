
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
-- $Id: ch_07_ch_07_05.vhd,v 1.3 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity ch_07_05 is
end entity ch_07_05;

library bv_utilities;
use bv_utilities.bv_arithmetic.all;

architecture test of ch_07_05 is

begin

  process_07_5_a : process is

                             -- code from book:

                             procedure increment ( a : inout integer;   n : in integer := 1 ) is -- . . .
    -- not in book
  begin
    a := a + n;
  end procedure increment;
  -- end not in book;

  procedure increment ( a : inout bit_vector;  n : in bit_vector := B"1" ) is -- . . .
    -- not in book
  begin
    a := a + n;
  end procedure increment;
  -- end not in book;

  procedure increment ( a : inout bit_vector;  n : in integer := 1 ) is -- . . .
    -- not in book
  begin
    a := a + integer_to_bv(n, a'length);
  end procedure increment;
  -- end not in book;

  variable count_int : integer := 2;
  variable count_bv : bit_vector (15 downto 0) := X"0002";

  -- end of code from book

  begin

    -- code from book:

    increment ( count_int, 2 );
    increment ( count_int );

    increment ( count_bv, X"0002");
    increment ( count_bv, 1 );

    -- increment ( count_bv );

    -- end of code from book

    wait;
  end process process_07_5_a;


end architecture test;
