entity tb_repro1 is
end tb_repro1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro1 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.repro1
    port map (
      sig_out => dout,
      sig_in => din,
      clock => clk,
      reset => rst);

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
    wait for 1 ns;
    assert dout = '0' severity failure;
    rst <= '0';
    din <= '1';
    pulse;
    assert dout = '1' severity failure;
    din <= '0';
    pulse;
    assert dout = '0' severity failure;
    din <= '1';
    pulse;
    assert dout = '1' severity failure;
    rst <= '1';
    wait for 1 ns;
    assert dout = '0' severity failure;
    wait;
  end process;
end behav;
