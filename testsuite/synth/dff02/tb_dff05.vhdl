entity tb_dff05 is
end tb_dff05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff05 is
  signal clk : std_logic;
  signal din : std_logic_vector (7 downto 0);
  signal dout : std_logic_vector (7 downto 0);
begin
  dut: entity work.dff05
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
    din <= b"1_00000_00";
    pulse;
    assert dout (0) = '0' severity failure;

    din <= b"0_00001_00";
    pulse;
    assert dout (2) = '1' severity failure;

    din <= b"0_00000_01";
    pulse;
    assert dout (2) = '0' severity failure;

    din <= b"1_00000_01";
    pulse;
    assert dout (0) = '1' severity failure;

    wait;
  end process;
end behav;
