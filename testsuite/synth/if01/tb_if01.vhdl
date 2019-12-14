entity tb_if01 is
end tb_if01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_if01 is
  signal c0, c1 : std_logic;
  signal r : std_logic;
begin
  dut: entity work.if01
    port map (c0, c1, r);

  process
  begin
    c0 <= '1';
    c1 <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;

    c0 <= '0';
    c1 <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;

    c0 <= '1';
    c1 <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;

    c0 <= '0';
    c1 <= '1';
    wait for 1 ns;
    assert r = '0' severity failure;

    wait;
  end process;
end behav;
