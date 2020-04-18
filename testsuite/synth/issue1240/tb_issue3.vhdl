entity tb_issue3 is
end tb_issue3;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_issue3 is
  signal i : integer := 0;
  signal o : signed (3 downto 0);
begin
  dut: entity work.issue3
    port map (i, o);

  process
  begin
    i <= 0;
    wait for 1 ns;
    assert o = "0010" severity failure;

    i <= 1;
    wait for 1 ns;
    assert o = "0011" severity failure;

    i <= -1;
    wait for 1 ns;
    assert o = "0001" severity failure;

    wait;
  end process;
end behav;
