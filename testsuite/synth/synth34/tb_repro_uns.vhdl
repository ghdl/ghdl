entity tb_repro_uns is
end tb_repro_uns;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_repro_uns is
  signal clk : std_logic;
  signal a : unsigned(7 downto 0);
  signal b : unsigned(7 downto 0);
begin
  dut: entity work.repro_uns
    port map (
      clk => clk, a => a, b => b);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    a <= x"ab";
    pulse;
    assert b = x"ab" severity failure;

    a <= x"12";
    pulse;
    assert b = x"12" severity failure;
    wait;
  end process;
end behav;
