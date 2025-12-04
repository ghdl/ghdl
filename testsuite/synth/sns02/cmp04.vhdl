library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity cmp04 is
end;

architecture arch of cmp04 is
begin
  process
    variable s0 : signed(3 downto 0);
    variable u0 : unsigned(3 downto 0);
  begin
    s0 := x"4";
    u0 := x"5";
    assert u0 > s0 severity failure;
    u0 := x"2";
    assert s0 > u0 severity failure;
    assert u0 > 1 severity failure;
    assert 6 > u0 severity failure;
    wait;
  end process;
end;
