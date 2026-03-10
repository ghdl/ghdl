entity tb_my_is_x is
end tb_my_is_x;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_my_is_x is
  signal a : std_logic;
  signal r1, r2 : boolean;
begin
  dut: entity work.my_is_x
    port map (a, r1, r2);

  process
  begin
    a <= '0';
    wait for 1 ns;
    assert r1 = false severity failure;
    assert r2 = false severity failure;

    a <= '1';
    wait for 1 ns;
    assert r1 = false severity failure;
    assert r2 = false severity failure;

    wait;
  end process;
end behav;
