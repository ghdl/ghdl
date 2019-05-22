library ieee;
use ieee.std_logic_1164.all;

entity dff08 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff08;

architecture behav of dff08 is
begin
  process (clk) is
  begin
    if falling_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
