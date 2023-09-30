entity tb_shift1 is
end tb_shift1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_shift1 is
  signal n : natural;
  signal res : std_logic_vector(31 downto 0);
begin
  dut: entity work.shift1
    port map (n, res);

  process
  begin
    n <= 0;
    wait for 1 ns;
    assert res = x"0000_0001" severity failure;

    n <= 17;
    wait for 1 ns;
    assert res = x"0002_0000" severity failure;

    wait;
  end process;
end behav;
