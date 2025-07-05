entity tb_top_module is
end tb_top_module;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_top_module is
  signal clk : std_logic;
  signal X   : std_logic_vector(15 downto 0);
  signal Y   : std_logic_vector(15 downto 0);
  signal R   : std_logic_vector(31 downto 0);
begin
  dut: entity work.top_module
    port map (
      clk => clk,
      X   => X,
      Y   => Y,
      R   => R); 

  process
  begin
    X <= x"0003";
    Y <= x"0123";
    wait for 1 ns;
    assert R = x"0000_0369" severity failure;
    wait;
  end process;
end behav;
