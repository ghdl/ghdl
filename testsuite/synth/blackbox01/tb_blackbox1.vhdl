entity tb_blackbox1 is
end tb_blackbox1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_blackbox1 is
  signal a, b : std_logic_vector(7 downto 0);
  signal r :std_logic_vector(7 downto 0);
begin
  dut: entity work.blackbox1
    port map (a, b, r);

  process
  begin
    a <= x"40";
    b <= x"04";
    wait for 1 ns;
    assert r = x"44" severity failure;

    a <= x"b5";
    b <= x"11";
    wait for 1 ns;
    assert r = x"c6" severity failure;

    a <= x"b5";
    b <= x"23";
    wait for 1 ns;
    assert r = x"c8" severity failure;

    wait;
  end process;
end behav;
