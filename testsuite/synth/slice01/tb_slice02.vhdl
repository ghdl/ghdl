entity tb_slice02 is
end tb_slice02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_slice02 is
  signal clk : std_logic;
  signal di : std_logic_vector (7 downto 0);
  signal mask : std_logic_vector (1 downto 0);
  signal do : std_logic_vector (7 downto 0);
begin
  dut: entity work.slice02
    generic map (w => 4)
    port map (clk, di, mask, do);

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
    di <= x"12";
    mask <= "11";
    pulse;
    assert do = x"12" severity error;
    wait;
  end process;
end behav;
