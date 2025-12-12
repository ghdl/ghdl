entity tb_nand01 is
end tb_nand01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_nand01 is
  signal l, r, o  : std_logic_vector(3 downto 0);
begin
  nand01_1: entity work.nand01
    port map (l, r, o);

  process
  begin
    l <= b"0011";
    r <= b"1010";
    wait for 1 ns;
    assert o = b"1101" severity failure;

    wait;
  end process;
end behav;
