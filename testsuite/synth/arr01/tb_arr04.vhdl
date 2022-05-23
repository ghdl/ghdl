entity tb_arr04 is
end tb_arr04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_arr04 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal sel_i : std_logic;
  signal sel_o : std_logic;
  signal v : std_logic;
  signal r : std_logic;
begin
  dut: entity work.arr04
    port map (clk => clk, rst => rst, sel_i => sel_i, sel_o => sel_o,
              v => v, res => r);

  process
    constant siv : std_logic_vector := b"0010";
    constant sov : std_logic_vector := b"0101";
    constant v_v : std_logic_vector := b"0011";
    constant r_v : std_logic_vector := b"0001";
    --  reg0                             0001
    --  reg1                             0011
  begin
    clk <= '0';
    rst <= '1';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;
    rst <= '0';
    for i in siv'range loop
      sel_i <= siv (i);
      sel_o <= sov (i);
      v <= v_v (i);
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      assert r = r_v(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
