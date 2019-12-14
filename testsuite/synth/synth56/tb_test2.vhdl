entity tb_test2 is
end tb_test2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test2 is
  signal v : std_logic_vector(1 downto 0);
  signal s : std_logic_vector(1 downto 0);
begin
  dut: entity work.test2
    port map (s, v);

  process
  begin
    s <= "00";
    wait for 1 ns;
    assert v = "00" severity failure;

    s <= "11";
    wait for 1 ns;
    assert v = "10" severity failure;

    wait;
  end process;
end behav;
