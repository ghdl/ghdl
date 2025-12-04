library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity neg01 is
end;

architecture arch of neg01 is
begin
  process
    variable v0 : signed(3 downto 0);
  begin
    v0 := x"3";
    assert -v0 = signed'(x"d") severity failure;
    wait;
  end process;
end;
