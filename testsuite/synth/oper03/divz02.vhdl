library ieee;
use ieee.std_logic_1164.all;

entity divz02 is
end;

architecture behav of divz02 is
begin
  process
    variable v, r : integer;
  begin
    v := 0;
    r := 4 mod v;
    wait;
  end process;
end;

