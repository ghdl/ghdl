library ieee;
use ieee.std_logic_1164.all;

entity dff11 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic);
end dff11;

architecture behav of dff11 is
begin
  process
  begin
    wait until rising_edge (clk);
    q <= d;
  end process;
end behav;
