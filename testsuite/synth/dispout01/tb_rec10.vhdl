entity tb_rec10 is
end tb_rec10;

library ieee;
use ieee.std_logic_1164.all;
use work.rec10_pkg.all;

architecture behav of tb_rec10 is
  signal inp : std_logic;
  signal r : myrec;
begin
  dut: entity work.rec10
    port map (inp => inp, o => r);

  process
  begin
    inp <= '1';
    wait for 1 ns;
    assert r.b (1) = '0' severity failure;

    inp <= '0';
    wait for 1 ns;
    assert r.b (1) = '1' severity failure;

    wait;
  end process;
end behav;
