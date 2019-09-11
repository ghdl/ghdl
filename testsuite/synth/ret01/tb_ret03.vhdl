entity tb_ret03 is
end tb_ret03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ret03 is
  signal d : std_logic_vector (7 downto 0);
  signal r : integer;
begin
  dut: entity work.ret03
    port map (d, r);

  process
  begin
    d <= x"01";
    wait for 1 ns;
    assert r = 0 severity failure;

    d <= x"1f";
    wait for 1 ns;
    assert r = 4 severity failure;

    d <= x"e2";
    wait for 1 ns;
    assert r = 7 severity failure;

    d <= x"00";
    wait for 1 ns;
    assert r = -1 severity failure;

    wait;
  end process;
end behav;
