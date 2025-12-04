library ieee;
use ieee.std_logic_1164.all;

entity divz03 is
end;

architecture behav of divz03 is
begin
  process
    variable v, r : integer;
  begin
    v := 0;
    r := 4 rem v;
    wait;
  end process;
end;

