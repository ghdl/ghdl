library ieee;
use ieee.std_logic_1164.all;

entity Shift02 is
end;

architecture rtl of Shift02 is
begin
  process
    variable l, res : bit_vector(3 downto 0);
  begin
    l := x"4";
    assert l sla 1 = x"8" severity failure;
    l := x"5";
    assert l sla 1 = x"b" severity failure;
    wait;
  end process;
end rtl;
