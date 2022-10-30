entity tb_areset is
end tb_areset;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_areset is
  signal clk : std_logic;
  signal rst_n : std_logic;
  signal din : std_logic;
  signal dout : std_logic_vector(1 downto 0);
begin
  dut: entity work.areset
    port map (
      clk_sys => clk,
      reset_n => rst_n,
      d => din,
      q => dout);

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
    rst_n <= '0';
    pulse;
    assert dout (0) = '0' severity failure;

    din <= '1';
    pulse;
    assert dout (0) = '0' severity failure;

    rst_n <= '1';
    pulse;
    assert dout (0) = '1' severity failure;
    assert dout (1) = '1' severity failure;

    din <= '0';
    pulse;
    assert dout (0) = '1' severity failure;
    assert dout (1) = '0' severity failure;

    din <= '1';
    rst_n <= '0';
    pulse;
    assert dout (0) = '0' severity failure;
    assert dout (1) = '0' severity failure;
wait;
  end process;
end behav;
