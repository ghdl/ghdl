entity tb_comp05 is
end tb_comp05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_comp05 is
  signal v : std_logic_vector (7 downto 0);
  signal r : std_logic_vector (7 downto 0);
begin
  comp05_1: entity work.comp05
    port map (
      v => v,
      r => r);

  process
  begin
    v <= b"1100_0011";
    wait for 1 ns;
    assert r = b"0011_1100" severity failure;

    v <= b"1100_0010";
    wait for 1 ns;
    assert r = b"0011_1101" severity failure;

    wait;
  end process;
end behav;
