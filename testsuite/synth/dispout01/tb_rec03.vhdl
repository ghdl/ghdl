entity tb_rec03 is
end tb_rec03;

library ieee;
use ieee.std_logic_1164.all;
use work.rec03_pkg.all;

architecture behav of tb_rec03 is
  signal inp : std_logic;
  signal r : myrec;
begin
  dut: entity work.rec03
    port map (inp => inp, o => r);

  process
  begin
    inp <= '1';
    wait for 1 ns;
    assert r = (a => s0, b => '0') severity failure;

    inp <= '0';
    wait for 1 ns;
    assert r = (a => s3, b => '1') severity failure;

    wait;
  end process;
end behav;
