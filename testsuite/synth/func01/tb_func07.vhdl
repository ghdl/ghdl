entity tb_func07 is
end tb_func07;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func07 is
  signal v, r : std_ulogic_vector(7 downto 0);
begin
  dut: entity work.func07
    port map (v, r);

  process
  begin
    v <= "00000000";
    wait for 1 ns;
    assert r = x"00" severity failure;

    v <= "00100100";
    wait for 1 ns;
    assert r = x"02" severity failure;

    v <= "11100111";
    wait for 1 ns;
    assert r = x"06" severity failure;

    wait;
  end process;
end behav;
