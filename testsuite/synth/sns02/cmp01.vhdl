library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity cmp01 is
end;

architecture arch of cmp01 is
begin
  process
    variable v0 : signed(3 downto 0);
  begin
    v0 := x"4";
    assert v0 /= 5 severity failure;
    v0 := x"d";
    assert 13 /= v0 severity failure;
    wait;
  end process;
end;
