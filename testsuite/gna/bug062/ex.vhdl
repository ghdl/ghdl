library ieee;
use ieee.std_logic_1164.all;

entity ex is
  port (clk, en : std_ulogic;
        r1: std_ulogic;
        r0: out std_ulogic);
end ex;

architecture behav of ex is
begin
  process(clk)
  begin
    if rising_edge(clk) then
    if en = '1' then
            r0 <= r1;
        end if;
    end if;
  end process;
end behav;
