entity tb_func06 is
end tb_func06;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func06 is
  signal r : std_logic_vector(15 downto 0);
  signal s : natural;
begin
  dut: entity work.func06
    port map (s, r);

  process
  begin
    s <= 2;
    wait for 1 ns;
    assert r = x"1234" severity failure;

    s <= 0;
    wait for 1 ns;
    assert r = x"0000" severity failure;

    s <= 3;
    wait for 1 ns;
    assert r = x"5678" severity failure;

    s <= 4;
    wait for 1 ns;
    assert r = x"0000" severity failure;

    wait;
  end process;
end behav;
