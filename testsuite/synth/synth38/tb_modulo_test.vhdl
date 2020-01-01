entity tb_modulo_test is
end tb_modulo_test;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_modulo_test is
  signal a, b, c : integer := 0;
begin
  dut: entity work.modulo_test
    port map (a, b, c);

  process
  begin
    a <= 7;
    wait for 1 ns;
    assert b = 7 severity failure;
    assert c = 7 severity failure;
    
    a <= 8;
    wait for 1 ns;
    assert b = 0 severity failure;
    assert c = 0 severity failure;

    a <= -7;
    wait for 1 ns;
    assert b = 1 severity failure;
    assert c = -7 severity failure;
    wait;
  end process;
end behav;
