entity tb_comp06 is
end tb_comp06;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_comp06 is
  signal v : std_logic_vector (7 downto 0);
  signal r : std_logic_vector (7 downto 0);
begin
  comp06_1: entity work.comp06
    port map (
      v => v,
      r => r);

  process
  begin
    v <= x"0f";
    wait for 1 ns;
    assert r = x"0c" severity failure;

    v <= x"f0";
    wait for 1 ns;
    assert r = x"70" severity failure;

    wait;
  end process;
end behav;
