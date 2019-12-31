entity tb_dff02 is
end tb_dff02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff02 is
  signal clk : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
  signal en : std_logic := '0';
  signal rst : std_logic := '0';
begin
  dut: entity work.dff02
    port map (
      q => dout,
      d => din,
      en => en,
      rst => rst,
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
    din <= '0';
    pulse;
    assert dout = '1' severity failure;
    en <= '1';
    pulse;
    assert dout = '0' severity failure;
    en <= '1';
    rst <= '1';
    wait for 1 ns;
    assert dout = '1' severity failure;
    pulse;
    assert dout = '1' severity failure;
    rst <= '0';
    pulse;
    assert dout = '0' severity failure;
    wait;
  end process;
end behav;
