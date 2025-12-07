library ieee;
use ieee.std_logic_1164.all;

entity dff23 is
  port (q : out std_logic;
        d : std_logic;
        clk, clk1 : std_logic);
end;

architecture behav of dff23 is
begin
  process (clk) is
  begin
    if clk'event and clk1 = '0' then
      q <= d;
    end if;
  end process;
end behav;
