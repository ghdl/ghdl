
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

-- code from book (in text)

entity random_source is
  generic ( min, max : natural;
            seed : natural;
            interval : delay_length );
  port ( number : out natural );
end entity random_source;

-- end code from book


architecture fudged of random_source is
begin

  process is
    variable next_number : natural := seed;
  begin
    if next_number > max then
      next_number := min;
    end if;
    number <= next_number;
    next_number := next_number + 1;
    wait for interval;
  end process;

end architecture fudged;



entity test_bench is
end entity test_bench;


-- code from book

architecture random_test of test_bench is

  subtype bv11 is bit_vector(10 downto 0);

  function natural_to_bv11 ( n : natural ) return bv11 is
    variable result : bv11 := (others => '0');
    variable remaining_digits : natural := n;
  begin
    for index in result'reverse_range loop
      result(index) := bit'val(remaining_digits mod 2);
      remaining_digits := remaining_digits / 2;
      exit when remaining_digits = 0;
    end loop;
    return result;
  end function natural_to_bv11;

  signal stimulus_vector : bv11;
  -- . . .

begin

  stimulus_generator : entity work.random_source
    generic map ( min => 0, max => 2**10 - 1, seed => 0,
                  interval => 100 ns )
    port map ( natural_to_bv11(number) => stimulus_vector );

  -- . . .

end architecture random_test;

-- end code from book
