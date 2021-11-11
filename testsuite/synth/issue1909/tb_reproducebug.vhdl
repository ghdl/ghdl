entity tb_reproducebug is
end tb_reproducebug;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_reproducebug is
  signal clk      : std_logic;
  signal input    : unsigned(7 downto 0);
  signal output   : unsigned(7 downto 0);
begin
  dut: entity work.reproducebug
    port map (clk, input, output);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end;
  begin
    input <= x"14";
    pulse;
    assert output = x"0a" severity failure;

    wait;
  end process;
end behav;
