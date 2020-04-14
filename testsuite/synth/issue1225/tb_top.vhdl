entity tb_top is
end tb_top;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_top is
  signal clk : std_logic;
  signal en : std_logic;
  signal a, b : std_logic;
  signal p, q : std_logic;
begin
  dut: entity work.top
    port map (clk, en, a, b, p, q);

  process
    procedure pulse is
    begin
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
    end pulse;
  begin
    clk <= '0';

    a <= '1';
    b <= '0';
    en <= '0';
    pulse;
    assert p = '0' severity failure;
    assert q = '0' severity failure;

    a <= '1';
    b <= '1';
    en <= '0';
    pulse;
    assert p = '0' severity failure;
    assert q = '1' severity failure;

    a <= '1';
    b <= '1';
    en <= '1';
    pulse;
    assert p = '1' severity failure;
    assert q = '1' severity failure;

    wait;
  end process;
end behav;
