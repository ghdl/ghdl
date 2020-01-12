entity tb_repro3_1 is
end tb_repro3_1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro3_1 is
  signal clk : std_logic;
  signal led : std_logic_vector(7 downto 0);
begin
  dut: entity work.repro3_1
    port map (clk, led);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    pulse;
    assert led = x"01" severity failure;

    pulse;
    assert led = x"02" severity failure;
    wait;
  end process;
end behav;
