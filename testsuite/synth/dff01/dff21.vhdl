library ieee;
use ieee.std_logic_1164.all;

entity dff21 is
  port (q : inout std_logic := '0';
        d : std_logic;
        clk : std_logic);
end;

architecture behav of dff21 is
begin
  process (clk) is
  begin
    if clk'event and clk = 'L' then
      q <= d;
    end if;
  end process;
end behav;
