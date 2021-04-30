entity tb_dff02 is
end tb_dff02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff02 is
  signal clk : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
  signal rst : std_logic;
begin
  dut: entity work.dff02
    port map (
      q => dout,
      d => din,
      clk => clk,
      rst => rst);

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
