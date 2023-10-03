library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
  generic (
    clk_period: time
  );
end top;

architecture str of top
is
  signal rst: std_logic := '1';
begin
  RELEASE_RESET: process
  begin
    wait for clk_period * 10;
    rst <= '0';
    wait for clk_period * 1;
    wait;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.finish;

entity top_tb is
end top_tb;

architecture sim of top_tb
is
  constant clk_hz: integer := 100e6;
  constant clk_period: time := 1 sec / clk_hz;
begin
  DUT: entity work.top (str)
    generic map (clk_period => clk_period);
  SEQUENCER_PROC: process
  begin
    wait until <<signal dut.rst: std_logic>> = '0';
    finish;
  end process;
end architecture;

