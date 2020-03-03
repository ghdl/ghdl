library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity repro_arith is
end repro_arith;

architecture behav of repro_arith is
  signal s : unsigned (7 downto 0) := x"00";
begin
  process
  begin
    s <= s + 1;
    wait for 1 ns;
  end process;
end behav;
