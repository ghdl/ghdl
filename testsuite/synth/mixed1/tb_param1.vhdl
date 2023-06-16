entity tb_param1 is
end tb_param1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_param1 is
  signal a, r : std_logic_vector(3 downto 0);
begin
  dut: entity work.param1t
    port map (a, r);

  process
  begin
    a <= "0000";
    wait for 1 ns;
    assert r = "1001" severity failure;

    a <= "0001";
    wait for 1 ns;
    assert r = "1010" severity failure;

    wait;
  end process;
end behav;
