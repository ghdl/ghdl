entity tb_dff08 is
end tb_dff08;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff08 is
  signal clk : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.dff08
    port map (
      q => dout,
      d => din,
      clk => clk);

  process
    procedure pulse is
    begin
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
      wait for 1 ns;
    end pulse;
  begin
    din <= '0';
    pulse;
    assert dout = '0' severity failure;
    din <= '1';
    clk <= '1';
    wait for 1 ns;
    assert dout = '0' severity failure;
    clk <= '0';
    wait for 1 ns;
    assert dout = '1' severity failure;
    pulse;
    assert dout = '1' severity failure;
    din <= '0';
    pulse;
    assert dout = '0' severity failure;
    wait;
  end process;
end behav;
