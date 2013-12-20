
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

entity nor_gate is
  generic ( width : positive;
            Tpd01, Tpd10 : delay_length );
  port ( input : in std_logic_vector(0 to width - 1);
         output : out std_logic );
end entity nor_gate;


architecture primitive of nor_gate is

  function max ( a, b : delay_length ) return delay_length is
  begin
    if a > b then
      return a;
    else
      return b;
    end if;
  end function max;

begin

  reducer : process (input) is
    variable result : std_logic;
  begin
    result := '0';
    for index in input'range loop
      result := result or input(index);
    end loop;
    if not result = '1' then
      output <= not result after Tpd01;
    elsif not result = '0' then
      output <= not result after Tpd10;
    else
      output <= not result after max(Tpd01, Tpd10);
    end if;
  end process reducer;

end architecture primitive;


library ieee;  use ieee.std_logic_1164.all;
library cell_lib;

entity interlock_control is
end entity interlock_control;


-- code from book

architecture detailed_timing of interlock_control is

  component nor_gate is
    generic ( input_width : positive );
    port ( input : in std_logic_vector(0 to input_width - 1);
           output : out std_logic );
  end component nor_gate;

  for ex_interlock_gate : nor_gate
    use entity cell_lib.nor_gate(primitive)
    generic map ( width => input_width,
                  Tpd01 => 250 ps, Tpd10 => 200 ps );  -- estimates

  -- . . .

  -- not in book
  signal reg_access_hazard, load_hazard, stall_ex_n : std_logic;
  -- end not in book

begin

  ex_interlock_gate : component nor_gate
    generic map ( input_width => 2 )
    port map ( input(0) => reg_access_hazard,
               input(1) => load_hazard,
               output => stall_ex_n);

  -- . . .

  -- not in book

  reg_access_hazard <= '0' after 10 ns, '1' after 20 ns, 'X' after 30 ns;

  load_hazard <= '0' after 2 ns, '1' after 4 ns, 'X' after 6 ns,
                 '0' after 12 ns, '1' after 14 ns, 'X' after 16 ns,
                 '0' after 22 ns, '1' after 24 ns, 'X' after 26 ns,
                 '0' after 32 ns, '1' after 34 ns, 'X' after 36 ns;

  -- end not in book

end architecture detailed_timing;

-- end code from book
