entity tb_dff3 is
end tb_dff3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_dff3 is
  signal clk : std_logic;
  signal en1, en2 : std_logic;
  signal din : std_logic;
  signal dout : std_logic;
begin
  dut: entity work.dff3
    port map (
      q => dout,
      d => din,
      clk => clk,
      en1 => en1,
      en2 => en2);

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

    en1 <= '1';
    en2 <= '1';
    din <= '1';
    pulse;
    assert dout = '1' severity failure;

    en1 <= '0';
    en2 <= '1';
    din <= '0';
    pulse;
    assert dout = '1' severity failure;

    en1 <= '1';
    en2 <= '0';
    din <= '0';
    pulse;
    assert dout = '1' severity failure;

    en1 <= '1';
    en2 <= '1';
    din <= '0';
    pulse;
    assert dout = '0' severity failure;

    wait;
  end process;
end behav;
