entity tb_issue is
end tb_issue;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_issue is
  signal clk, reset, state : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.issue
    port map (
      reset => reset,
      state => state,
      out1 => dout,
      in1 => din,
      clk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    reset <= '0';
    din <= '0';
    pulse;
    assert dout = '0' severity failure;
    din <= '1';
    pulse;
    assert dout = '1' severity failure;
    reset <= '1';
    state <= '0';
    pulse;
    assert dout = '0' severity failure;
    state <= '1';
    wait for 1 ns;
    assert dout = '1' severity failure;
    wait;
  end process;
end behav;
