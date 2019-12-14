entity tb_ret01 is
end tb_ret01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ret01 is
  signal d : std_logic_vector (7 downto 0);
  signal r : integer;
begin
  dut: entity work.ret01
    port map (d, r);

  process
  begin
    d <= x"01";
    wait for 1 ns;
    assert r = 1 severity failure;

    d <= x"f1";
    wait for 1 ns;
    assert r = -1 severity failure;

    wait;
  end process;
end behav;
