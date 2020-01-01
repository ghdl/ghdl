entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal a : std_logic;
begin
  dut: entity work.ent
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = '0' severity failure;
    wait;
  end process;
end behav;
