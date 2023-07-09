entity tb_comparator2 is
end tb_comparator2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_comparator2 is
  signal a, b : std_logic_vector(1 downto 0);
  signal r : std_logic;
begin
  dut: entity work.comparator2
    port map (a, b, r);

  process
  begin
    a <= "00";
    b <= "00";
    wait for 1 ns;
    assert r = '1' severity failure;

    a <= "01";
    b <= "10";
    wait for 1 ns;
    assert r = '0' severity failure;

    a <= "11";
    b <= "01";
    wait for 1 ns;
    assert r = '0' severity failure;

    a <= "10";
    b <= "10";
    wait for 1 ns;
    assert r = '1' severity failure;

    wait;
  end process;
end behav;
