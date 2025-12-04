library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity abs01 is
end;

architecture arch of abs01 is
begin
  process
    variable v0 : signed(3 downto 0);
  begin
    v0 := x"4";
    assert abs v0 = signed'(x"4") severity failure;
    v0 := x"d";
    assert abs v0 = signed'(x"3") severity failure;
    wait;
  end process;
end;
