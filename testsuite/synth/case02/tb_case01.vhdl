entity tb_case01 is
end tb_case01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_case01 is
  signal a : std_logic_vector (1 downto 0);
  signal o : std_logic_vector (1 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.case01
    port map (a, clk, o);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    a <= "00";
    pulse;
    a <= "10";
    pulse;
    assert o = "00" severity failure;

    wait;
  end process;
end behav;
