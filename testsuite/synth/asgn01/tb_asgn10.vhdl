entity tb_asgn10 is
end tb_asgn10;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_asgn10 is
  signal a, b, c : std_logic_vector (1 downto 0);
  signal sel_a, sel_b : std_logic;
  signal o        : std_logic_vector (1 downto 0);
begin
  dut: entity work.asgn10
    port map (a, b, sel_a, sel_b, o);

  process
  begin
    a <= "10";
    b <= "01";
    c <= "00";

    sel_a <= '1';
    sel_b <= '0';
    wait for 1 ns;
    assert o = "10" severity failure;

    sel_a <= '0';
    sel_b <= '1';
    wait for 1 ns;
    assert o = "01" severity failure;

    sel_a <= '0';
    sel_b <= '0';
    wait for 1 ns;
    assert o = "01" severity failure;

    wait;
  end process;
end behav;
