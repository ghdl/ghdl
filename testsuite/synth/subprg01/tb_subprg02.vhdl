entity tb_subprg02 is
end tb_subprg02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_subprg02 is
  signal a, na : std_logic_vector (3 downto 0);
  signal n : natural range 0 to 1;
  signal clk : std_logic;
begin
  dut: entity work.subprg02
    port map (a, n, clk, na);

  process
    procedure pulse is
    begin
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
    end pulse;
  begin
    n <= 0;
    clk <= '0';

    a <= x"0";
    pulse;
    assert na = x"f" severity failure;

    a <= x"5";
    pulse;
    assert na = x"a" severity failure;

    wait;
  end process;
end behav;
