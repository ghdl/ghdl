entity tb_tc3 is
end tb_tc3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_tc3 is
  signal state : std_ulogic;
  signal o     : std_ulogic_vector(3 downto 0);
begin
  dut: entity work.tc3
    port map (state, o);

  process
  begin
    state <= '0';
    wait for 1 ns;
    assert o = "0111" severity failure;

    state <= '1';
    wait for 1 ns;
    assert o = "1000" severity failure;

    wait;
  end process;
end behav;
