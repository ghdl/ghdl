library ieee;
use ieee.std_logic_1164.all;

entity dff18 is
  port (q : inout std_logic := '0';
        d : std_logic;
        clk : std_logic);
end;

architecture behav of dff18 is
begin
  process (clk) is
  begin
    if clk'event and clk /= '0' then
      q <= d;
    end if;
  end process;
end behav;
