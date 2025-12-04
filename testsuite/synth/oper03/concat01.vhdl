library ieee;
use ieee.std_logic_1164.all;

entity concat01 is
end;

architecture behav of concat01 is
begin
  process
    type my_arr is array (severity_level range <>) of bit;
    variable c : my_arr(severity_level);
  begin
    c := (others => '1');
    c := c & c;
    wait;
  end process;
end;

