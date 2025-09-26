entity tb_dff01b is
end tb_dff01b;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff01b is
  signal clk : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.dff01b
    port map (
      q => dout,
      d => din,
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
    assert dout = '0' severity failure;
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
    wait;
  end process;
end behav;
