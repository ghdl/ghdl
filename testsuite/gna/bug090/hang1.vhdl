library ieee;
use ieee.std_logic_1164.all;

entity hello is
  generic (constant l : natural := 8);
  port`(a : in std_logic_vector (l - 1 downto 0));
end hello;

architecture behav of hello is
  signal clk : std_logic;
  signal q : std_logic_vector (l - 1 downto 0);
begin
  process
  begin
    clk <= '0';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;
  end process;

  process (clk)
  begin
    if rising_edge(clk) then
      q <= a;
    end if;
  end process;
  assert false report "Hello world" severity note;
end behav;
