library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity shift01 is
end;

architecture arch of shift01 is
begin
  process
    variable v0, cnt : unsigned(3 downto 0);
    variable s0 : signed(3 downto 0);
  begin
    v0 := x"5";
    cnt := x"2";
    assert shl(v0, cnt) = unsigned'(x"4") severity failure;
    assert shr(v0, cnt) = unsigned'(x"1") severity failure;

    s0 := x"5";
    assert shl(s0, cnt) = signed'(x"4") severity failure;
    assert shr(s0, cnt) = signed'(x"1") severity failure;
    wait;
  end process;
end;
