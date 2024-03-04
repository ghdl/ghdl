library ieee;
use ieee.std_logic_1164.all;

entity tb1 is
end tb1;

architecture behav of tb1 is
  signal clk  : std_logic;
  signal rst  : std_logic;
  signal lim  : std_logic_vector (7 downto 0);
  signal zero : std_logic;
  signal tick : std_logic;
begin
  dut: entity work.ecounter
    port map (
      clk  => clk,
      rst  => rst,
      lim  => lim,
      zero => zero,
      tick => tick);

  process
  begin
    for i in 1 to 10 loop
      clk <= '0';
      wait for 5 ns;
      clk <= '1';
      wait for 5 ns;
    end loop;
    wait;
  end process;

  rst <= '1', '0' after 10 ns;

  lim <= "00010010"; -- x"12"
end behav;
