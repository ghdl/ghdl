entity tb_sub02 is
end tb_sub02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_sub02 is
  signal i, o : std_logic_vector(3 downto 0) := x"0";
begin
  dut: entity work.sub02
    port map (i => i, o => o);

  process
  begin
    i <= x"0";
    wait for 1 ns;
    assert o = x"7" severity failure;

    i <= x"8";
    wait for 1 ns;
    assert o = x"f" severity failure;

    i <= x"9";
    wait for 1 ns;
    assert o = x"0" severity failure;

    wait;
  end process;
end behav;
