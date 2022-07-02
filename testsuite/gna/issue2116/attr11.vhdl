library ieee;use ieee.std_logic_1164.all;entity if01 is port(a:std_logic;b:std_logic;n:std_logic;l:std_logic;cl0:std_logic;s:std_logic;s0:std_logic);end;architecture behav of if01 is
begin process(cl0)is
variable t:std'l;begin
if(0)then if'0'then end if;end if;end process;end behav;