library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_bit.all;

entity not01 is
end;

architecture arch of not01 is
begin
  process
    variable sv : signed(3 downto 0);
    variable uv : unsigned(3 downto 0);
  begin
    sv := x"4";
    assert not sv = x"b" severity failure;
    uv := x"5";
    assert not uv = x"a" severity failure;
    wait;
  end process;
end;
