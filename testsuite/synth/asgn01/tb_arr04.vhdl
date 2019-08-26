entity tb_arr04 is
end tb_arr04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_arr04 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal sel_i : std_logic;
  signal v : std_logic;
  signal r : std_logic_vector(0 to 1);
begin
  dut: entity work.arr04
    port map (clk => clk, rst => rst, sel_i => sel_i, v => v, res => r);

  process
    constant siv : std_logic_vector := b"0010";
    constant v_v : std_logic_vector := b"0011";
    constant r1v : std_logic_vector := b"0011";
    constant r0v : std_logic_vector := b"0001";
  begin
    clk <= '0';
    rst <= '1';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;
    rst <= '0';
    for i in siv'range loop
      sel_i <= siv (i);
      v <= v_v (i);
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      assert r(0) = r0v(i) severity failure;
      assert r(1) = r1v(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
