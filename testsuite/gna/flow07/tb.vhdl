library ieee;
use ieee.std_logic_1164.all;

entity tb is
end tb;

architecture sim of tb is
  signal clk     : std_logic := '0';
  signal rst     : std_logic := '1';
  signal data_in : std_logic_vector (3 downto 0) := "0001";
  signal count   : std_logic_vector (3 downto 0);
  signal ready   : std_logic;
  signal bus_io  : std_logic;
begin
  uut : entity work.dut
    port map (clk => clk, rst => rst, data_in => data_in,
              count => count, ready => ready, bus_io => bus_io);

  clk <= not clk after 5 ns;

  stim : process is
  begin
    rst     <= '1';
    data_in <= "0001";
    wait for 12 ns;
    rst <= '0';
    wait for 40 ns;
    std.env.finish;
  end process;
end sim;
