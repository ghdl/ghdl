library ieee;
use ieee.std_logic_1164.all;

entity err_process02 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic);
end;

architecture behav of err_process02 is
begin
  process
  begin
    wait for 1 ns; -- until rising_edge (clk);
    q <= d;
  end process;
end behav;
