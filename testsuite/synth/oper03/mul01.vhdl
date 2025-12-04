library ieee;
use ieee.std_logic_1164.all;

entity mul01 is
end;

architecture behav of mul01 is
begin
  process
    variable c : character;
    variable r : real;
  begin
    c := 'A';
    r := real(character'pos(c) * 2.0);
    assert r = 130.0 severity failure;
    wait;
  end process;
end;

