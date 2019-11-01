entity tb_forgen03 is
end tb_forgen03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_forgen03 is
  signal a : std_logic_vector (7 downto 0);
  signal b : std_logic_vector (7 downto 0);
  signal o : std_logic_vector (7 downto 0);
begin
  dut: entity work.forgen03
    port map (a, b, o);

  process
  begin
    a <= x"30";
    b <= x"28";
    wait for 1 ns;
    assert o = x"58" severity failure;

    a <= x"11";
    b <= x"f7";
    wait for 1 ns;
    assert o = x"08" severity failure;

    wait;
  end process;
end behav;
