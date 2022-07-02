library ieee;use ieee.std_logic_1164;use ieee.numeric_std.all;entity hello is
port(cl0:out signed(0 to 0));end hello;architecture behav of hello is
signal v:unsigned(0 to 0);begin
process(cl0)begin
if g[](0)then if 0='0'then
v;end if;end if;end process;end behav;