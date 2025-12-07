library ieee;
use ieee.std_logic_1164.all;

entity dff22 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end;

architecture behav of dff22 is
begin
  process (clk) is
  begin
    if clk'event and clk = '0' then
      q <= d;
    end if;
  end process;
end behav;
