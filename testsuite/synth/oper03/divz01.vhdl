library ieee;
use ieee.std_logic_1164.all;

entity divz01 is
end;

architecture behav of divz01 is
begin
  process
    variable v, r : integer;
  begin
    v := 0;
    r := 4 / v;
    wait;
  end process;
end;

