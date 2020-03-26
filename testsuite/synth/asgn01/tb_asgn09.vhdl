entity tb_asgn09 is
end tb_asgn09;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_asgn09 is
  signal a, b, c, d : std_logic_vector (1 downto 0);
  signal sel        : std_logic_vector(1 downto 0);
  signal o          : std_logic_vector (3 downto 0);
begin
  dut: entity work.asgn09
    port map (a, b, c, d, sel, o);

  process
  begin
    a <= "10";
    b <= "01";
    c <= "00";
    d <= "11";

    sel <= "00";
    wait for 1 ns;
    assert o = "1110" severity failure;

    sel <= "01";
    wait for 1 ns;
    assert o = "1101" severity failure;

    sel <= "10";
    wait for 1 ns;
    assert o = "1100" severity failure;

    sel <= "11";
    wait for 1 ns;
    assert o = "1111" severity failure;

    wait;
  end process;
end behav;
