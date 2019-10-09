entity tb_aggr02 is
end tb_aggr02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_aggr02 is
  signal i0 : natural range 0 to 5;
  signal o : std_logic_vector(7 downto 0);
begin
  dut: entity work.aggr02
    port map (i0, o);

  process
  begin
    i0 <= 0;
    wait for 1 ns;
    assert o = x"00" severity failure;

    i0 <= 1;
    wait for 1 ns;
    assert o = x"81" severity failure;

    i0 <= 2;
    wait for 1 ns;
    assert o = x"02" severity failure;

    wait;
  end process;
end behav;
