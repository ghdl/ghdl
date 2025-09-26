library ieee;
use ieee.std_logic_1164.all;

entity dff01b is
  port (q : inout std_logic := '0';
        d : std_logic;
        clk : std_logic);
end dff01b;

architecture behav of dff01b is
begin
  process (clk) is
  begin
    if rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
