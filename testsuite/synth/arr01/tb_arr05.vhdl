entity tb_arr05 is
end tb_arr05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_arr05 is
  signal clk : std_logic;
  signal val : std_logic_vector(7 downto 0);
  signal res : std_logic_vector(7 downto 0);
  signal par : std_logic;
begin
  dut: entity work.arr05
    port map (clk => clk, val => val, res => res, par => par);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    val <= x"a0";
    pulse;
    val <= x"71";
    pulse;
    val <= x"82";
    pulse;
    val <= x"23";
    pulse;
    val <= x"fe";
    pulse;
    assert res = x"a0" severity failure;

    val <= x"e4";
    pulse;
    assert res = x"71" severity failure;

    val <= x"c5";
    pulse;
    assert res = x"82" severity failure;

    val <= x"f6";
    pulse;
    assert res = x"23" severity failure;

    val <= x"57";
    pulse;
    assert res = x"fe" severity failure;

    wait;
  end process;
end behav;
