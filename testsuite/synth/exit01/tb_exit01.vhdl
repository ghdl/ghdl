entity tb_exit01 is
end tb_exit01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_exit01 is
  signal v : std_logic_vector(3 downto 0);
  signal r : integer;
begin
  dut: entity work.exit01
    port map (val => v, res => r);

  process
  begin
    v <= "0001";
    wait for 1 ns;
    assert r = 0 severity failure;

    v <= "0010";
    wait for 1 ns;
    assert r = 1 severity failure;

    v <= "0100";
    wait for 1 ns;
    assert r = 2 severity failure;

    v <= "1000";
    wait for 1 ns;
    assert r = 3 severity failure;

    v <= "0000";
    wait for 1 ns;
    assert r = 4 severity failure;

    v <= "0110";
    wait for 1 ns;
    assert r = 1 severity failure;

    v <= "1001";
    wait for 1 ns;
    assert r = 0 severity failure;

    wait;
  end process;
end behav;
