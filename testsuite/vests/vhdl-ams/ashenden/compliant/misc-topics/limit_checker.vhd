
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

library ieee;  use ieee.std_logic_1164.all;

package project_util is

  -- code from book (in text)

  function "<" ( bv1, bv2 : bit_vector ) return boolean;

  subtype word is std_logic_vector(31 downto 0);

  -- end code from book

end package project_util;


package body project_util is

  function "<" ( bv1, bv2 : bit_vector ) return boolean is
    variable tmp1 : bit_vector(bv1'range) := bv1;
    variable tmp2 : bit_vector(bv2'range) := bv2;
  begin
    assert bv1'length = bv2'length
      report "vectors are of different length in ""<"" comparison"
      severity failure;
    tmp1(tmp1'left) := not tmp1(tmp1'left);
    tmp2(tmp2'left) := not tmp2(tmp2'left);
    return std.standard."<" ( tmp1, tmp2 );
  end function "<";

end package body project_util;



-- code from book

library ieee;  use ieee.std_logic_1164.all;
use work.project_util.all;

entity limit_checker is
  port ( input, lower_bound, upper_bound : in word;
         out_of_bounds : out std_logic );
end entity limit_checker;

--------------------------------------------------

architecture behavioral of limit_checker is

  subtype bv_word is bit_vector(31 downto 0);

  function word_to_bitvector ( w : in word ) return bv_word is
  begin
    return To_bitvector ( w, xmap => '0' );
  end function word_to_bitvector;

begin

  algorithm : process (input, lower_bound, upper_bound) is
  begin
    if "<" ( bv1 => word_to_bitvector(input),
             bv2 => word_to_bitvector(lower_bound) )
       or "<" ( bv1 => word_to_bitvector(upper_bound),
                bv2 => word_to_bitvector(input) ) then
      out_of_bounds <= '1';
    else
      out_of_bounds <= '0';
    end if;
  end process algorithm;

end architecture behavioral;

-- end code from book
