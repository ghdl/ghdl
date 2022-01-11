entity tb_alias02 is
end tb_alias02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_alias02 is
  signal o : std_logic_vector(7 downto 0);
  signal i : std_logic;
begin
  dut: entity work.alias02
    port map (i => i, o => o);

  process
  begin
    i <= '1';
    wait for 1 ns;
    assert o = x"ff" severity failure;

    i <= '0';
    wait for 1 ns;
    assert o = x"c3" severity failure;

    wait;
  end process;
end behav;
