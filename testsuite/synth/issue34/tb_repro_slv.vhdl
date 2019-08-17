entity tb_repro_slv is
end tb_repro_slv;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro_slv is
  signal clk : std_logic;
  signal a : std_logic_vector(7 downto 0);
  signal b : std_logic_vector(7 downto 0);
begin
  dut: entity work.repro_slv
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
