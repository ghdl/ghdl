
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
-- $Id: ch_16_ch_16_03.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity ch_16_03 is

end entity ch_16_03;


----------------------------------------------------------------


architecture test of ch_16_03 is

  function pulled_up ( drivers : bit_vector ) return bit is
  begin
    for index in drivers'range loop
      if drivers(index) = '0' then
        return '0';
      end if;
    end loop;
    return '1';
  end function pulled_up;

  signal s : pulled_up bit bus;

begin


  process is
  begin

    s <= '1' after 11 ns, '0' after 16 ns, '1' after 18 ns,
         null after 19 ns, '0' after 25 ns;
    wait for 10 ns;

    -- code from book:

    s <= reject 3 ns inertial null after 10 ns;

    -- end of code from book

    wait;
  end process;


end architecture test;
