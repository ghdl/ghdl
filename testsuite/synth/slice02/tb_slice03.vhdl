entity tb_slice03 is
end tb_slice03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_slice03 is
  signal addr : std_logic_vector (3 downto 0);
  signal di : std_logic_vector (7 downto 0);
  signal do : std_logic_vector (1 downto 0);
  signal clk : std_logic;
begin
  dut: entity work.slice03
    port map (raddr => addr, din => b"1001_1011", rdat => do, rclk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    addr <= x"4";
    pulse;
    assert do = b"01" severity error;

    addr <= x"5";
    pulse;
    assert do = b"11" severity error;

    addr <= x"6";
    pulse;
    assert do = b"00" severity error;

    wait;
  end process;
end behav;
