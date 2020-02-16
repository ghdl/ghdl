entity tb_cnt04 is
end tb_cnt04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_cnt04 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal counter : std_logic_vector (7 downto 0);
begin
  dut: entity work.cnt04
    port map (clk => clk, rst => rst, counter => counter);
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
    assert counter = x"01" severity failure;

    rst <= '0';
    pulse;
    assert counter = x"02" severity failure;

    pulse;
    assert counter = x"03" severity failure;

    wait;
  end process;
end behav;
