entity tb_sub04 is
end tb_sub04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_sub04 is
  signal i, o : std_logic_vector(3 downto 0) := x"0";
begin
  dut: entity work.sub04
    port map (i => i, o => o);

  process
  begin
    i <= x"0";
    wait for 1 ns;
    assert o = x"9" severity failure;

    i <= x"8";
    wait for 1 ns;
    assert o = x"1" severity failure;

    i <= x"9";
    wait for 1 ns;
    assert o = x"2" severity failure;

    wait;
  end process;
end behav;
