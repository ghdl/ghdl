library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.finish;
entity shift_register_tb is
end shift_register_tb;
architecture sim of shift_register_tb
is
  constant clk_hz: integer := 100e6;
  constant clk_period: time := 1 sec / clk_hz;
  -- number of stages
  constant NUM_STAGES: natural := 5;
  -- number of bits
  constant BITS: natural := 4;
  signal clk: std_logic := '1';
  signal rst: std_logic := '1';
  signal x: std_logic_vector (BITS - 1 downto 0) := (others => '0');
  signal y: std_logic_vector (BITS - 1 downto 0);
begin
  clk <= not clk after clk_period / 2;
  DUT: entity work.shift_register (rtl)
    generic map (NUM_STAGES => NUM_STAGES, BITS => BITS)
    port map (clk => clk, rst => rst, x => x, y => y);
  SEQUENCER_PROC: process
  begin
    wait for clk_period * 2;
    rst <= '0';
    wait for clk_period;
    for i in - 2** (BITS - 1) to 2** (BITS - 1) - 1 loop
      x <= std_logic_vector (to_signed (i, BITS));
      wait for clk_period;
    end loop;
    wait;
  end process;
  CHECK_PROC: process
  begin
    wait on rst;
    wait for (NUM_STAGES + 1) * clk_period;
    for i in - 2** (BITS - 1) to 2** (BITS - 1) - 1 loop
      assert to_integer (signed (y)) = i report "y: " & to_string (to_integer (signed (y))) & " is not equal to " & to_string (i) severity failure;
      wait for clk_period;
    end loop;
    wait for clk_period;
    finish;
  end process;
end architecture;

