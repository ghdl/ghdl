
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
-- $Id: ch_07_fg_07_08.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_07_08 is
end entity fg_07_08;


architecture test of fg_07_08 is

  subtype word32 is bit_vector(31 downto 0);

  -- code in book

  procedure negate ( a : inout word32 ) is
    variable carry_in : bit := '1';
    variable carry_out : bit;
  begin
    a := not a;
    for index in a'reverse_range loop
      carry_out :=  a(index) and carry_in;
      a(index) := a(index) xor carry_in;
      carry_in := carry_out;
    end loop;
  end procedure negate;

  -- end code in book

begin

  stimulus : process is

                       -- code in book (in text)

                       variable op1 : word32;
                     -- . . .

                     -- end code in book

  begin
    op1 := X"0000_0002";

    -- code in book (in text)

    negate ( op1 );

    -- end code in book

    wait;
  end process stimulus;

end architecture test;
