entity tb_testcase1 is
end tb_testcase1;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_testcase1 is
  signal sel : unsigned (1 downto 0);
  signal det : std_logic;
begin
  dut: entity work.testcase1
    port map (sel, det);

  process
  begin
    sel <= "00";
    wait for 1 ns;
    assert det = '0' severity failure;

    sel <= "01";
    wait for 1 ns;
    assert det = '0' severity failure;

    sel <= "10";
    wait for 1 ns;
    assert det = '1' severity failure;

    sel <= "11";
    wait for 1 ns;
    assert det = '1' severity failure;

    wait;
  end process;
end behav;
