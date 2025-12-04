library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity mul01 is
end;

architecture arch of mul01 is
begin
  process
    variable s0 : signed(3 downto 0);
    variable u0 : unsigned(3 downto 0);
    variable one : std_logic := '1';
  begin
    s0 := x"4";
    u0 := x"3";
    assert u0 * unsigned'("10") = unsigned'(x"6") severity failure;
    assert s0 * signed'("11") = signed'(x"c") severity failure;

    assert u0 * s0 = signed'(x"0c") severity failure;
    assert s0 * u0 = signed'(x"0c") severity failure;
    wait;
  end process;
end;
