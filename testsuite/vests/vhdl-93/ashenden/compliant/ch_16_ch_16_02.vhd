
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
-- $Id: ch_16_ch_16_02.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity ch_16_02 is

end entity ch_16_02;


----------------------------------------------------------------


architecture test of ch_16_02 is

  -- code from book:

  subtype word is bit_vector(0 to 31);
  type word_array is array (integer range <>) of word;

  function resolve_words ( words : word_array ) return word;

  signal s : resolve_words word bus;

  -- end of code from book

  function resolve_words ( words : word_array ) return word is
  begin
    if words'length > 0 then
      return words(words'left);
    else
      return X"00000000";
    end if;
  end function resolve_words;

  constant T_delay : delay_length := 2 ns;

begin


  process is
  begin

    -- code from book (should fail)

    s(0 to 15) <= X"003F" after T_delay;
    s(16 to 31) <= null after T_delay;

    -- end of code from book

    wait;
  end process;


end architecture test;
