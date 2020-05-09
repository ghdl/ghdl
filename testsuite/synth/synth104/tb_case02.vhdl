entity tb_case02 is
end tb_case02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_case02 is
  signal sel : unsigned (3 downto 0);
  signal det : std_logic_vector (1 downto 0);
begin
  dut: entity work.case02
    port map (sel, det);

  process
  begin
    sel <= "0000";
    wait for 1 ns;
    assert det = "00" severity failure;

    sel <= "0010";
    wait for 1 ns;
    assert det = "01" severity failure;

    sel <= "0110";
    wait for 1 ns;
    assert det = "01" severity failure;

    sel <= "1010";
    wait for 1 ns;
    assert det = "10" severity failure;

    sel <= "1111";
    wait for 1 ns;
    assert det = "11" severity failure;

    wait;
  end process;
end behav;
