entity tb_arr07 is
end tb_arr07;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_arr07 is
  signal clk : std_logic;
  signal val : std_logic_vector(7 downto 0);
  signal res : std_logic_vector(7 downto 0);
  signal par : std_logic;
begin
  dut: entity work.arr07
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
    for i in 0 to 15 loop
      val <= std_logic_vector (to_unsigned(i, 4) & to_unsigned (15 - i, 4));
      pulse;
    end loop;

    assert res = x"0f" severity failure;

    val <= x"e4";
    pulse;
    assert res = x"1e" severity failure;

    val <= x"c5";
    pulse;
    assert res = x"2d" severity failure;

    val <= x"f6";
    pulse;
    assert res = x"3c" severity failure;

    val <= x"57";
    pulse;
    assert res = x"4b" severity failure;

    wait;
  end process;
end behav;
