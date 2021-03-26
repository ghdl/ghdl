entity tb_accumwr is
end tb_accumwr;

library ieee;
use ieee.std_logic_1164.all;

use work.pkg.all;

architecture behav of tb_accumwr is
  signal clk : std_logic;
  signal rst : std_logic;
  signal en : std_logic;
  signal res : std_logic_vector(15 downto 0);
begin
  dut: entity work.accumwr
    port map (clk, rst, en, res);

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
    en <= '0';
    pulse;
    assert res = x"0000" severity failure;

    rst <= '0';
    pulse;
    assert res = x"0000" severity failure;

    en <= '1';
    pulse;
    assert res = x"0001" severity failure;
    wait;
  end process;
end behav;
