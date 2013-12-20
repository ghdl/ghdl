
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
-- $Id: ch_11_fg_11_05.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

package words is

  type X01Z is ('X', '0', '1', 'Z');
  type uword is array (0 to 31) of X01Z;

  type uword_vector is array (natural range <>) of uword;

  function resolve_word ( contribution : uword_vector ) return uword;

  subtype word is resolve_word uword;

  -- not in book
  type ubyte is array (0 to 7) of X01Z;
  -- end not in book

end package words;

--------------------------------------------------

package body words is

  type table is array (X01Z, X01Z) of X01Z;

  constant resolution_table : table :=
    --  'X'  '0'  '1'  'Z'
    --  ------------------
    ( ( 'X', 'X', 'X', 'X' ),    -- 'X'
      ( 'X', '0', 'X', '0' ),    -- '0'
      ( 'X', 'X', '1', '1' ),    -- '1'
      ( 'X', '0', '1', 'Z' ) );  -- 'Z'

  function resolve_word ( contribution : uword_vector ) return uword is
    variable result : uword := (others => 'Z');
  begin
    for index in contribution'range loop
      for element in uword'range loop
        result(element) :=
          resolution_table( result(element), contribution(index)(element) );
      end loop;
    end loop;
    return result;
  end function resolve_word;

end package body words;
