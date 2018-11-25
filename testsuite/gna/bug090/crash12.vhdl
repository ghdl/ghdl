library ieee;
use ieee.std_logic_1164.all;

entity clkgen is
  generic (period : time := 10 ns);
  port (signal clk : out std_logic := '0');
end clkgen;

architecture behav of clkgen is
begin
  process
  begin
    "xxx" . null;
  end process;
end behav;

