library ieee;
use ieee.std_logic_1164.all;
library mylib;

entity and3 is
  port (i0, i1, i2 : std_logic;
        o : out std_logic);
end and3;

architecture behav of and3 is
  signal t1 : std_logic;
begin
  a1: entity mylib.and2
    port map (i0, i1, t1);
  a2: entity mylib.and2
    port map (t1, i2, o);
end behav;
