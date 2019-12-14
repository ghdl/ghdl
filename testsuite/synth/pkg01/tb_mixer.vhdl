library ieee;
use ieee.std_logic_1164.all;

entity tb_mixer is
end tb_mixer;

architecture behav of tb_mixer is
  signal h, l, o : std_logic_vector (7 downto 0);
begin
  dut : entity work.mixer
    port map (h => h, l => l, o => o);

  process
  begin
    h <= x"00";
    l <= x"ab";
    wait for 1 ns;
    assert o = x"0b" severity failure;

    h <= x"50";
    l <= x"a6";
    wait for 1 ns;
    assert o = x"56" severity failure;

    h <= x"a3";
    l <= x"5c";
    wait for 1 ns;
    assert o = x"af" severity failure;

    wait;
  end process;
end behav;
