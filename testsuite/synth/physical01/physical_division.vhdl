library ieee;
  use ieee.std_logic_1164.all;

entity physical_division is
  port (
    clk_cycles : out integer
  );
end physical_division;

architecture rtl of physical_division is
  constant CLK_PERIOD : time := 83.333 ns;
begin
  clk_cycles <= 100 ms / CLK_PERIOD;
end rtl;
