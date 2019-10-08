entity tb_vhd01 is
end tb_vhd01;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

architecture behav of tb_vhd01 is
  signal i1, o1 : std_logic_vector(1 to 1);
begin
  dut: entity work.vhd01
    port map (i1 => i1, o1 => o1);

  process
  begin
    i1 <= "1";
    wait for 1 ns;
    assert o1 = "1" severity failure;

    i1 <= "0";
    wait for 1 ns;
    assert o1 = "0" severity failure;

    wait;
  end process;
end behav;
