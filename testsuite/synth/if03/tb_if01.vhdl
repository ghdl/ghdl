entity tb_if01 is
end tb_if01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_if01 is
  signal a    : std_logic;
  signal b    : std_logic;
  signal en1  : std_logic;
  signal sel1 : std_logic;
  signal clk  : std_logic;
  signal s1   : std_logic;
  signal s2   : std_logic;
begin
  dut: entity work.if01
    port map (
      a    => a,
      b    => b,
      en1  => en1,
      sel1 => sel1,
      clk  => clk,
      s1   => s1,
      s2   => s2);

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
    b <= '1';
    a <= '0';
    pulse;
    assert s1 = '0' severity failure;
    assert s2 = '1' severity failure;

    en1 <= '1';
    b <= '0';
    a <= '1';
    pulse;
    assert s1 = '1' severity failure;
    assert s2 = '0' severity failure;

    en1 <= '0';
    b <= 'X';
    a <= 'X';
    pulse;
    assert s1 = '1' severity failure;
    assert s2 = '0' severity failure;

    wait;
  end process;
end behav;
