library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity cmp08 is
end;

architecture arch of cmp08 is
begin
  process
    variable s0 : signed(4 downto 0);
    variable u0 : unsigned(3 downto 0);
  begin
    s0 := b"0_0100";
    u0 := x"3";
    assert u0 < s0 severity failure;
    u0 := x"7";
    assert s0 < u0 severity failure;
    assert u0 < 8 severity failure;
    assert 6 < u0 severity failure;
    wait;
  end process;
end;
