library ieee;
use ieee.std_logic_1164.all;

entity err_vect1 is
end;

architecture tb of err_vect1 is
begin
  process
    variable l, res : bit_vector(3 downto 0);
  begin
    l := "0110";
    res := l and "110";
    wait;
  end process;
end tb;
