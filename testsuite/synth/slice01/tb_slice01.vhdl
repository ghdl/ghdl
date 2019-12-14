entity tb_slice01 is
end tb_slice01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_slice01 is
  signal rst : std_logic;
  signal clk : std_logic;
  signal di : std_logic;
  signal do : std_logic_vector (3 downto 0);
begin
  dut: entity work.slice01
    generic map (w => 4)
    port map (rst, clk, di, do);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
    constant b0 : std_logic_vector (3 downto 0) := "1101";
  begin
    rst <= '1';
    pulse;
    rst <= '0';
    for i in b0'reverse_range loop
      di <= b0 (i);
      pulse;
    end loop;
    assert do = b0 severity error;
    wait;
  end process;
end behav;
