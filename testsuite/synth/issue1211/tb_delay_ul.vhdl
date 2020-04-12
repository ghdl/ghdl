entity tb_delay_ul is
end tb_delay_ul;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_delay_ul is
  signal clk : std_logic;
  signal rst : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
  signal en : std_logic;
begin
  dut: entity work.delay_ul
    port map (
      sig_out => dout,
      sig_in => din,
      clock => clk,
      reset => rst,
      Enable => En);

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
    en <= '1';
    wait for 1 ns;
    assert dout = '0' severity failure;
    rst <= '0';

    din <= '1';
    pulse;
    assert dout = '0' severity failure;

    din <= '1';
    pulse;
    assert dout = '0' severity failure;

    din <= '0';
    pulse;
    assert dout = '0' severity failure;

    din <= '1';
    pulse;
    assert dout = '1' severity failure;

    din <= '1';
    pulse;
    assert dout = '1' severity failure;

    din <= '1';
    pulse;
    assert dout = '0' severity failure;

    din <= '1';
    pulse;
    assert dout = '1' severity failure;

    din <= '1';
    rst <= '1';
    pulse;
    assert dout = '0' severity failure;

    rst <= '0';
    din <= '1';
    pulse;
    assert dout = '0' severity failure;
    
    wait;
  end process;
end behav;
