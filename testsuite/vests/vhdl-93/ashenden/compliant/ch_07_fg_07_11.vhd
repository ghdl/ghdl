
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
-- $Id: ch_07_fg_07_11.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_07_11 is
end entity fg_07_11;



architecture test of fg_07_11 is

  subtype word32 is bit_vector(31 downto 0);

  -- code from book

  procedure increment ( a : inout word32;  by : in word32 := X"0000_0001" ) is
    variable sum : word32;
    variable carry : bit := '0';
  begin
    for index in a'reverse_range loop
      sum(index) := a(index) xor by(index) xor carry;
      carry := ( a(index) and by(index) ) or ( carry and ( a(index) xor by(index) ) );
    end loop;
    a := sum;
  end procedure increment;

  -- end code from book

begin

  stimulus : process is

                       variable count : word32 := X"0001_1100";

  begin

    -- code from book (in text)

    increment(count, X"0000_0004");

    increment(count);

    increment(count, by => open);

    -- end code from book

    wait;
  end process stimulus;

end architecture test;
