entity tb_srot01 is
end tb_srot01;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_srot01 is
  signal v    : signed(7 downto 0);
  signal ro   : signed(7 downto 0);
  signal lo   : signed(7 downto 0);
begin
  dut: entity work.srot01
    port map (v, ro, lo);

  process
  begin
    v <= x"14";
    wait for 1 ns;
    assert ro = x"0a" severity failure;
    assert lo = x"28" severity failure;

    wait;
  end process;
end behav;
