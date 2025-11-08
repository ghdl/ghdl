library ieee;
use ieee.std_logic_1164.all;

entity tb_top_test is
end;

architecture behav of tb_top_test is
  signal a  : std_logic;
  signal x1 : std_logic;
  signal x2 : std_logic;
begin
  top_test_1: entity work.top_test
    port map (
      a  => a,
      x1 => x1,
      x2 => x2);

  process
  begin
    a <= '0';
    wait for 1 ns;
    assert x1 = '0' severity failure;
    assert x2 = '1' severity failure;

    a <= '1';
    wait for 1 ns;
    assert x1 = '1' severity failure;
    assert x2 = '0' severity failure;

    wait;
  end process;
end behav;
