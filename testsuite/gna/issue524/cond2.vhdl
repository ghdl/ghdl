library ieee;
use ieee.std_logic_1164.all;
entity cond is
  port (i, rst, clk: std_logic;
        o: out std_logic);
end;


architecture behav of cond is
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if ?? rst then
        o <= '0';
      else
        o <= i;
      end if;
    end if;
  end process;
end;


