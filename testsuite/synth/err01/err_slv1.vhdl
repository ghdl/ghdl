library ieee;
use ieee.std_logic_1164.all;

entity err_slv1 is
end;

architecture tb of err_slv1 is
begin
  process
    variable l, r : std_logic_vector(3 downto 0);
  begin
    l := "0110";
    r := l and "110";
    wait;
  end process;
end tb;
