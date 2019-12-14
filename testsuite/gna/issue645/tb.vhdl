entity tb is
end tb;

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;

architecture behav of tb is
  signal clk : std_logic;
  signal l : std_logic_vector(31 downto 0);
  signal res : t(3 downto 0)(31 downto 0);
begin
  dut : entity work.foo
    generic map (n => 4, p => 32)
    port map (clk, l, res);
end behav;
