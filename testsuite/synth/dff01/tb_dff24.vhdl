entity tb_dff24 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff24 is
  signal clk : std_logic_vector(0 to 1);
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.dff24
    port map (
      q => dout,
      d => din,
      clk => clk);

  clk(1) <= 'X';

  process
    procedure pulse is
    begin
      clk(0) <= '1';
      wait for 1 ns;
      clk(0) <= '0';
      wait for 1 ns;
    end pulse;
  begin
    din <= '0';
    pulse;
    assert dout = '0' severity failure;
    din <= '1';
    pulse;
    assert dout = '1' severity failure;
    pulse;
    assert dout = '1' severity failure;
    din <= '0';
    pulse;
    assert dout = '0' severity failure;
    wait;
  end process;
end behav;
