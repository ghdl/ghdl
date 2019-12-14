entity tb_dff12 is
end tb_dff12;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff12 is
  signal clk : std_logic;
  signal rstn : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.dff12
    port map (
      q => dout,
      d => din,
      clk => clk,
      rstn => rstn);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rstn <= '1';
    wait for 1 ns;
    assert dout = '1' severity failure;
    rstn <= '0';
    pulse;
    assert dout = '0' severity failure;
    rstn <= '1';
    din <= '1';
    pulse;
    assert dout = '1' severity failure;
    din <= '0';
    pulse;
    assert dout = '0' severity failure;
    din <= '1';
    pulse;
    assert dout = '1' severity failure;
    wait;
  end process;
end behav;
