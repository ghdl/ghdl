entity tb_uns is
end tb_uns;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_uns is
  signal o1 : std_logic_vector(1 downto 0);
  signal o2 : std_logic_vector (31 downto 0);
begin
  ent_1: entity work.uns
    port map (
      o1 => o1,
      o2 => o2);

  process
  begin
    wait for 1 ns;
    assert o1 = "01" severity failure;
    assert o2 = x"0000_0003" severity failure;
    wait;
  end process;
end behav;
