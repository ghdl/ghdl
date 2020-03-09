entity tb_arr02 is
end tb_arr02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_arr02 is
  signal a : std_logic_vector (31 downto 0);
  signal sel : natural range 0 to 3;
  signal clk : std_logic;
  signal res : std_logic_vector (3 downto 0);
begin
  dut: entity work.arr02
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
    assert res = x"1" severity failure;

    sel <= 1;
    pulse;
    assert res = x"1" severity failure;

    sel <= 2;
    pulse;
    assert res = x"2" severity failure;

    sel <= 3;
    pulse;
    assert res = x"3" severity failure;

    sel <= 0;
    pulse;
    assert res = x"4" severity failure;

    wait;
  end process;
end behav;
