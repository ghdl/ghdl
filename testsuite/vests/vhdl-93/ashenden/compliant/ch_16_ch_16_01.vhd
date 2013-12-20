
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
-- $Id: ch_16_ch_16_01.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity ch_16_01 is

end entity ch_16_01;


----------------------------------------------------------------


architecture test of ch_16_01 is

  function pulled_up ( drivers : bit_vector ) return bit is
  begin
    for index in drivers'range loop
      if drivers(index) = '0' then
        return '0';
      end if;
    end loop;
    return '1';
  end function pulled_up;

  type state_type is (init_state, state1, state2, state3);
  type state_vector is array (integer range <>) of state_type;

  function resolve_state ( drivers : state_vector ) return state_type is
  begin
    return drivers(drivers'left);
  end function resolve_state;


  -- code from book:

  signal interrupt_request : pulled_up bit bus;

  signal stored_state : resolve_state state_type register := init_state;

  -- end of code from book

begin


end architecture test;
