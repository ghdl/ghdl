library ieee;
use ieee.std_logic_1164.all;

entity div01 is
end;

architecture behav of div01 is
begin
  process
    variable c : character;
    variable r : real;
  begin
    c := 'A';
    r := real(195.0 / character'pos(c));
    assert r = 3.0 severity failure;
    wait;
  end process;
end;

