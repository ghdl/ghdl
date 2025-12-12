entity tb_leftmost04 is
end tb_leftmost04;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_leftmost04 is
  signal a : unsigned (0 downto 1);
  signal ra : integer;
begin
  dut_a: entity work.leftmost04
    port map (a, ra);

  process
  begin
    a <= "";
    wait for 1 ns;

    assert ra = -1 severity failure;
    wait;
  end process;
end behav;
