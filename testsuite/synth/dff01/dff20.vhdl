library ieee;
use ieee.std_logic_1164.all;

entity dff20 is
  port (q : inout std_logic := '0';
        d : std_logic;
        clk : std_logic);
end;

architecture behav of dff20 is
begin
  process (clk) is
  begin
    if clk'event and clk = d then
      q <= d;
    end if;
  end process;
end behav;
