
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
-- $Id: ch_06_tofp-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavioral of to_fp is

begin

  behavior : process (vec) is

                             variable temp : bit_vector(vec'range);
                           variable negative : boolean;
                           variable int_result : integer;

  begin
    temp := to_bitvector(vec);
    negative := temp(temp'left) = '1';
    if negative then
      temp := not temp;
    end if;
    int_result := 0;
    for index in vec'range loop	  -- sign bit of temp = '0'
      int_result := int_result * 2 + bit'pos(temp(index));
    end loop;
    if negative then
      int_result := (-int_result) - 1;
    end if;
    -- convert to floating point and scale to [-1, +1)
    r <= real(int_result) / real(2**15);
  end process behavior;

end architecture behavioral;
