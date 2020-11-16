entity tb_pass01 is
end tb_pass01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_pass01 is
  subtype int_rng is integer range -4 to 4;
  signal i, o: int_rng;
begin
  dut: entity work.pass01
    port map (i, o);

  process
  begin
    for k in int_rng loop
      i <= k;
      wait for 1 ns;
      assert o = k severity failure;
    end loop;
    wait;
  end process;
end behav;
