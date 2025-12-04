library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity ext01 is
end;

architecture arch of ext01 is
begin
  process
    variable v0 : std_logic_vector(3 downto 0);
  begin
    v0 := x"9";
    assert ext(v0, 8) = x"09" severity failure;
    assert sxt(v0, 8) = x"f9" severity failure;
    wait;
  end process;
end;
