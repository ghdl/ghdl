entity tb_conv01 is
end tb_conv01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_conv01 is
  signal i : std_logic_vector(19 downto 0);
  signal o : std_logic_vector(31 downto 0);
begin
  dut: entity work.conv01
    port map (i => i, o => o);

  process
  begin
    i <= x"abcde";
    wait for 1 ns;
    assert o = x"000_abcde" severity failure;

    wait;
  end process;
end behav;
