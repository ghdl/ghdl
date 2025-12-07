entity tb_slice04 is
end tb_slice04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_slice04 is
  signal addr : std_logic_vector (3 downto 0);
  signal di : std_logic_vector (0 to 3);
  signal do : std_logic_vector (0 to 1);
  signal clk : std_logic;
begin
  dut: entity work.slice04
    port map (raddr => addr, din => b"1001", rdat => do, rclk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    addr <= x"f";
    pulse;
    assert do = b"10" severity error;

    addr <= x"0";
    pulse;
    assert do = b"01" severity error;

    wait;
  end process;
end behav;
