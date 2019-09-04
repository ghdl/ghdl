entity tb_rec08 is
end tb_rec08;

library ieee;
use ieee.std_logic_1164.all;
use work.rec08_pkg.all;

architecture behav of tb_rec08 is
  signal inp : std_logic;
  signal r : myrec;
begin
  dut: entity work.rec08
    port map (inp => inp, o => r);

  process
  begin
    inp <= '1';
    wait for 1 ns;
    assert r = (a => "1", b => '0') severity failure;

    inp <= '0';
    wait for 1 ns;
    assert r = (a => "0", b => '1') severity failure;

    wait;
  end process;
end behav;
