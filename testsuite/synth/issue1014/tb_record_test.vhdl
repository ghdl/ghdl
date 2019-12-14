entity tb_record_test is
end tb_record_test;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_record_test is
  signal v : integer;
begin
  dut: entity work.record_test
    port map (o => v);

  process
  begin
    wait for 1 ns;
    assert v = 333 severity failure;
    wait;
  end process;
end behav;
