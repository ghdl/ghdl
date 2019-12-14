entity tc3 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tc3 is
  signal clk : std_logic;
  signal tg : std_logic;
begin
  process (clk) is
  begin
    if falling_edge(clk) and (tg) then
      null;
    end if;
  end process;
end behav;
