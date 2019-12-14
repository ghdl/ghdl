entity tb_subprg01 is
end tb_subprg01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_subprg01 is
  signal a, na : std_logic_vector (3 downto 0);
begin
  dut: entity work.subprg01
    port map (a, na);

  process
  begin
    a <= x"0";
    wait for 1 ns;
    assert na = x"f" severity failure;

    a <= x"5";
    wait for 1 ns;
    assert na = x"a" severity failure;

    wait;
  end process;
end behav;
