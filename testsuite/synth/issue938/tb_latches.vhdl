entity tb_latches is
end tb_latches;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_latches is
  signal g : std_logic;
  signal d : std_logic;
  signal clr : std_logic;
  signal q : std_logic;
begin
  dut: entity work.latches
    port map (g, d, clr, q);

  process
  begin
    clr <= '1';
    g <= '0';
    wait for 1 ns;
    assert q = '0' severity failure;

    clr <= '0';
    wait for 1 ns;
    assert q = '0' severity failure;

    g <= '1';
    d <= '1';
    wait for 1 ns;
    assert q = '1' severity failure;

    g <= '0';
    d <= '0';
    wait for 1 ns;
    assert q = '1' severity failure;

    g <= '1';
    d <= '0';
    wait for 1 ns;
    assert q = '0' severity failure;

    g <= '1';
    d <= '1';
    wait for 1 ns;
    assert q = '1' severity failure;

    clr <= '1';
    wait for 1 ns;
    assert q = '0' severity failure;
    wait;
  end process;
end behav;
