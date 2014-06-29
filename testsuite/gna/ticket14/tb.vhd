
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is

end tb;

architecture behav of tb is
  signal clk : std_logic;
begin  -- behav

  process
  begin
    for i in 1 to 5 loop
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end loop;  -- i
  end process;

  inst : entity work.scrambler port map (
    clk   => clk,
    en    => '0',
    reset => '0',
    seed  => '0',
    d_in  => '0',
    d_out => open);

end behav;
