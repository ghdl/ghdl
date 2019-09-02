entity tb_rec02 is
end tb_rec02;

library ieee;
use ieee.std_logic_1164.all;
use work.rec02_pkg.all;

architecture behav of tb_rec02 is
  signal inp : myrec;
  signal r : std_logic;
begin
  dut: entity work.rec02
    port map (inp => inp, o => r);

  process
  begin
    inp.a <= 5;
    inp.b <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;

    inp.a <= 2;
    inp.b <= '1';
    wait for 1 ns;
    assert r = '0' severity failure;

    inp.a <= 1;
    inp.b <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;

    inp.a <= 4;
    inp.b <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;

    wait;
  end process;
end behav;
