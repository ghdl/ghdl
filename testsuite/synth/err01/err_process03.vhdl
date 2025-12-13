library ieee;
use ieee.std_logic_1164.all;

entity err_process03 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic);
end;

architecture behav of err_process03 is
begin
  process
  begin
    q <= d;
    wait until rising_edge (clk);
    q <= not d;
  end process;
end behav;
