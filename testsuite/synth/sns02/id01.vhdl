library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity id01 is
end;

architecture arch of id01 is
begin
  process
    variable v0 : signed(3 downto 0);
  begin
    v0 := x"3";
    assert +v0 = v0 severity failure;
    wait;
  end process;
end;
