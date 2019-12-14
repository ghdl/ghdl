entity tb_rec01 is
end tb_rec01;

library ieee;
use ieee.std_logic_1164.all;
use work.rec01_pkg.all;

architecture behav of tb_rec01 is
  signal inp : std_logic;
  signal r : myrec;
begin
  dut: entity work.rec01
    port map (inp => inp, o => r);

  process
  begin
    inp <= '0';
    wait for 1 ns;
    assert r = (a => '0', b => '1') severity failure;

    inp <= '1';
    wait for 1 ns;
    assert r = (a => '1', b => '0') severity failure;

    wait;
  end process;
end behav;
