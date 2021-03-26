entity tb_patgen is
end tb_patgen;

library ieee;
use ieee.std_logic_1164.all;

use work.pkg.all;

architecture behav of tb_patgen is
  signal clk : std_logic;
  signal rst : std_logic;
  signal bo : bus_rec_out_t;
begin
  dut: entity work.patgen
    port map (clk, rst, bo);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '1';
    pulse;
    assert bo.dat = x"01" severity failure;
    assert bo.rst = '1' severity failure;
    assert bo.stb = '0' severity failure;

    rst <= '0';
    pulse;
    assert bo.dat = x"02" severity failure;
    assert bo.rst = '0' severity failure;
    assert bo.stb = '1' severity failure;

    pulse;
    assert bo.dat = x"03" severity failure;
    assert bo.rst = '0' severity failure;
    assert bo.stb = '1' severity failure;

    pulse;
    assert bo.dat = x"05" severity failure;
    assert bo.rst = '0' severity failure;
    assert bo.stb = '1' severity failure;

    pulse;
    assert bo.dat = x"01" severity failure;
    assert bo.rst = '0' severity failure;
    assert bo.stb = '1' severity failure;
    wait;
  end process;
end behav;
