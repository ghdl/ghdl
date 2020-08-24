entity tb_test is
end tb_test;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_test is
  signal s : std_logic := '0';
  signal a : unsigned(7 downto 0);
  signal r : integer range 0 to 255;
begin
  dut: entity work.test
    port map (s, a, r);

  process
  begin
    wait for 1 ns;
    assert r = 0 severity failure;

    s <= '1';
    a <= x"a5";
    wait for 1 ns;
    assert r = 165 severity failure;
    wait;
  end process;
end behav;
