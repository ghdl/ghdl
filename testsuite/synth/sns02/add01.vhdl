library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity add01 is
end;

architecture arch of add01 is
begin
  process
    variable s0 : signed(3 downto 0);
    variable u0 : unsigned(3 downto 0);
    variable one : std_logic := '1';
  begin
    s0 := x"4";
    u0 := x"3";
    assert u0 + s0 = unsigned'(x"7") severity failure;
    assert s0 + u0 = unsigned'(x"7") severity failure;
    assert u0 + 2 = unsigned'(x"5") severity failure;
    assert 6 + u0 = unsigned'(x"9") severity failure;
    assert s0 + 1 = signed'(x"5") severity failure;
    assert -1 + s0 = signed'(x"3") severity failure;

    assert u0 + one = unsigned'(x"4") severity failure;
    assert one + u0 = unsigned'(x"4") severity failure;

    assert s0 + one = signed'(x"5") severity failure;
    assert one + s0 = signed'(x"5") severity failure;

    wait;
  end process;
end;
