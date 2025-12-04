library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tox01 is
end;

architecture behav of tox01 is
begin
  process
    variable x : std_logic := 'X';
    variable v0 : std_ulogic := '0';
    variable b : bit := '1';
  begin
    assert to_x01z(x) = 'X' severity failure;
    assert to_x01(b) = '1' severity failure;
    assert to_01(v0) = std_ulogic'('0') severity failure;
    assert to_01(x, '1') = std_ulogic'('1') severity failure;
    wait;
  end process;
end;

