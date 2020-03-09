entity tb_arr01 is
end tb_arr01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_arr01 is
  signal a : std_logic_vector (31 downto 0);
  signal sel : natural range 0 to 3;
  signal clk : std_logic;
  signal res : std_logic_vector (7 downto 0);
begin
  dut: entity work.arr01
    port map (a, sel, clk, res);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    a <= x"a1b2c3d4";
    sel <= 0;
    pulse;
    pulse;
    assert res = x"a1" severity failure;

    sel <= 1;
    pulse;
    assert res = x"a1" severity failure;

    sel <= 2;
    pulse;
    assert res = x"b2" severity failure;

    sel <= 3;
    pulse;
    assert res = x"c3" severity failure;

    sel <= 0;
    pulse;
    assert res = x"d4" severity failure;

    wait;
  end process;
end behav;
