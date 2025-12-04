library ieee;
use ieee.std_logic_1164.all;

entity mul02 is
end;

architecture behav of mul02 is
begin
  process
    variable c : character;
    variable r : real;
  begin
    c := 'C';
    r := real(2.0 * character'pos(c));
    assert r = 134.0 severity failure;
    wait;
  end process;
  end;

