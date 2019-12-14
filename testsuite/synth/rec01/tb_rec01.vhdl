entity tb_rec01 is
end tb_rec01;

library ieee;
use ieee.std_logic_1164.all;
use work.rec01_pkg.all;

architecture behav of tb_rec01 is
  signal inp : myrec;
  signal clk : std_logic;
  signal rst : std_logic;
  signal r : std_logic;
begin
  dut: entity work.rec01
    port map (inp => inp, clk => clk, rst => rst, o => r);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '1';
    pulse;
    assert r = '1' severity failure;

    rst <= '0';
    inp <= (a => "0010", b => '1');
    pulse;
    assert r = '1' severity failure;

    rst <= '0';
    inp <= (a => "0001", b => '1');
    pulse;
    assert r = '0' severity failure;

    wait;
  end process;
end behav;
