entity tb_rec2 is
end tb_rec2;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_rec2 is
  signal clk      : std_logic;
  signal sl_in    : std_logic;
  signal slv_in   : std_logic_vector(7 downto 0);
  signal int_in   : integer range 0 to 15;
  signal usig_in  : unsigned(7 downto 0);
  signal sl_out   : std_logic;
  signal slv_out  : std_logic_vector(7 downto 0);
  signal int_out  : integer range 0 to 15;
  signal usig_out : unsigned(7 downto 0);
begin
  dut: entity work.rec2
    port map (
      clk      => clk,
      sl_in    => sl_in,
      slv_in   => slv_in,
      int_in   => int_in,
      usig_in  => usig_in,
      sl_out   => sl_out,
      slv_out  => slv_out,
      int_out  => int_out,
      usig_out => usig_out);

  process
  begin
    clk <= '0';
    sl_in <= '1';
    slv_in <= x"12";
    int_in <= 13;
    usig_in <= x"d5";

    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;

    assert sl_out = '1' severity failure;
    assert slv_out = x"12" severity failure;
    assert int_out = 13 severity failure;
    assert usig_out = x"d5" severity failure;

    sl_in <= '0';
    slv_in <= x"9b";
    int_in <= 3;
    usig_in <= x"72";

    clk <= '0';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;

    assert sl_out = '0' severity failure;
    assert slv_out = x"9b" severity failure;
    assert int_out = 3 severity failure;
    assert usig_out = x"72" severity failure;

    wait;
  end process;
end behav;
