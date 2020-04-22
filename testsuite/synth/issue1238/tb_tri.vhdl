entity tb_tri is
end tb_tri;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_tri is
  signal i : std_logic;
  signal en : std_logic;
  signal o : std_logic;
begin
  dut: entity work.tri
    port map (i, en, o);

  process
  begin
    i <= '1';
    en <= '1';
    wait for 1 ns;
    assert o = '1' severity failure;

    i <= '0';
    en <= '1';
    wait for 1 ns;
    assert o = '0' severity failure;

    i <= '1';
    en <= '0';
    wait for 1 ns;
    assert o = 'Z' severity failure;

    wait;
  end process;
end behav;
