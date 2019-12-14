entity tb_rec06 is
end tb_rec06;

library ieee;
use ieee.std_logic_1164.all;
use work.rec06_pkg.all;

architecture behav of tb_rec06 is
  signal inp : std_logic;
  signal r : myrec;
begin
  dut: entity work.rec06
    port map (inp => inp, o => r);

  process
  begin
    inp <= '1';
    wait for 1 ns;
    assert r = (a => (c => 2, d => "1000"), b => '0') severity failure;

    inp <= '0';
    wait for 1 ns;
    assert r = (a => (c => 3, d => "0000"), b => '1') severity failure;

    wait;
  end process;
end behav;
