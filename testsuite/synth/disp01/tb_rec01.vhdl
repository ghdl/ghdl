entity tb_rec01 is
end tb_rec01;

library ieee;
use ieee.std_logic_1164.all;
use work.rec01_pkg.all;

architecture behav of tb_rec01 is
  signal inp : myrec;
  signal r : std_logic;
begin
  dut: entity work.rec01
    port map (inp => inp, o => r);

  process
    constant av : std_logic_vector := b"11001";
    constant bv : std_logic_vector := b"01011";
    constant rv : std_logic_vector := b"11011";
  begin
    for i in av'range loop
      inp.a <= av (i);
      inp.b <= bv (i);
      wait for 1 ns;
      assert r = rv(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
