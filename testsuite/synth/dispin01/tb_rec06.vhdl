entity tb_rec06 is
end tb_rec06;

library ieee;
use ieee.std_logic_1164.all;
use work.rec06_pkg.all;

architecture behav of tb_rec06 is
  signal inp : myrec;
  signal r : std_logic;
begin
  dut: entity work.rec06
    port map (inp => inp, o => r);

  process
  begin
    inp.a <= (2, "0000");
    inp.b <= '1';
    wait for 1 ns;
    assert r = '0' severity failure;

    inp.a <= (2, "0110");
    inp.b <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;

    inp.a <= (3, "1001");
    inp.b <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;

    inp.a <= (3, "0001");
    inp.b <= '1';
    wait for 1 ns;
    assert r = '0' severity failure;

    wait;
  end process;
end behav;
