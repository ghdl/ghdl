entity tb_rec03 is
end tb_rec03;

library ieee;
use ieee.std_logic_1164.all;
use work.rec03_pkg.all;

architecture behav of tb_rec03 is
  signal inp : myrec;
  signal r : std_logic;
begin
  dut: entity work.rec03
    port map (inp => inp, o => r);

  process
  begin
    inp.a <= s0;
    inp.b <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;

    inp.a <= s2;
    inp.b <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;

    inp.a <= s2;
    inp.b <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;

    inp.a <= s3;
    inp.b <= '0';
    wait for 1 ns;
    assert r = '1' severity failure;

    wait;
  end process;
end behav;
