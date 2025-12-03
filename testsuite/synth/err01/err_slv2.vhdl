library ieee;
use ieee.std_logic_1164.all;

entity err_slv2 is
end;

architecture tb of err_slv2 is
begin
  process
    variable l : std_logic_vector(3 downto 0);
    variable r : std_logic;
  begin
    l := "0110";
    r := l ?= "110";
    wait;
  end process;
end tb;
