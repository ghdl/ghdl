entity tb_rec09 is
end tb_rec09;

library ieee;
use ieee.std_logic_1164.all;
use work.rec09_pkg.all;

architecture behav of tb_rec09 is
  signal inp : std_logic;
  signal r : myrec;
begin
  dut: entity work.rec09
    port map (inp => inp, o => r);

  process
  begin
    inp <= '1';
    wait for 1 ns;
    assert r.b = '0' severity failure;

    inp <= '0';
    wait for 1 ns;
    assert r.b = '1' severity failure;

    wait;
  end process;
end behav;
