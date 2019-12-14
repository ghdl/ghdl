entity tb_vhd02 is
end tb_vhd02;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

architecture behav of tb_vhd02 is
  signal i1, o1 : my_rec;
begin
  dut: entity work.vhd02
    port map (i1 => i1, o1 => o1);

  process
  begin
    i1.b <= '1';
    wait for 1 ns;
    assert o1.b = '1' severity failure;

    i1.b <= '0';
    wait for 1 ns;
    assert o1.b = '0' severity failure;

    wait;
  end process;
end behav;
