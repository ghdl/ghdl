library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity match01 is
end;

architecture behav of match01 is
begin
  process
    variable u5 : unsigned(0 to 3) := x"5";
    variable c : character;
    variable r : real;
  begin
    assert (u5 ?>= 4) = '1' severity failure;
    assert (u5 ?= 4) = '0' severity failure;
    assert (u5 ?/= 4) = '1' severity failure;
    wait;
  end process;
end;

