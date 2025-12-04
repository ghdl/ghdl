library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity cmp07 is
end;

architecture arch of cmp07 is
begin
  process
    variable s0 : signed(3 downto 0);
    variable u0 : unsigned(3 downto 0);
  begin
    s0 := x"4";
    u0 := x"5";
    assert u0 /= s0 severity failure;
    assert s0 /= u0 severity failure;
    assert u0 /= 4 severity failure;
    assert 4 /= u0 severity failure;
    wait;
  end process;
end;
