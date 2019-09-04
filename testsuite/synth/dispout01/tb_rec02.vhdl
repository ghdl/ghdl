entity tb_rec02 is
end tb_rec02;

library ieee;
use ieee.std_logic_1164.all;
use work.rec02_pkg.all;

architecture behav of tb_rec02 is
  signal inp : std_logic;
  signal r : myrec;
begin
  dut: entity work.rec02
    port map (inp => inp, o => r);

  process
  begin
    inp <= '0';
    wait for 1 ns;
    assert r = (b => '1', a => 5)  severity failure;

    inp <= '1';
    wait for 1 ns;
    assert r = (b => '0', a => 3)  severity failure;

    wait;
  end process;
end behav;
