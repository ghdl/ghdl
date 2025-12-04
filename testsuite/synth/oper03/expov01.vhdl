library ieee;
use ieee.std_logic_1164.all;

entity expov01 is
end;

architecture behav of expov01 is
begin
  process
    variable v, r : integer;
  begin
    v := 71;
    r := 2**v;
    wait;
  end process;
end;

