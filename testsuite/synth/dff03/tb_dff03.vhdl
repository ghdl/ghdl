entity tb_dff03 is
end tb_dff03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff03 is
  signal clk : std_logic;
  signal en1 : std_logic;
  signal en2 : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.dff03
    port map (
      q => dout,
      d => din,
      en1 => en1,
      en2 => en2,
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
    en1 <= '1';
    en2 <= '1';
    din <= '0';
    pulse;
    assert dout = '0' severity failure;

    din <= '1';
    pulse;
    assert dout = '1' severity failure;

    en1 <= '0';
    din <= '0';
    pulse;
    assert dout = '1' severity failure;

    en1 <= '1';
    din <= '0';
    pulse;
    assert dout = '0' severity failure;

    en2 <= '0';
    din <= '1';
    pulse;
    assert dout = '0' severity failure;

    en2 <= '1';
    din <= '1';
    pulse;
    assert dout = '1' severity failure;

    wait;
  end process;
end behav;
