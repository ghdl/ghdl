library ieee;
use ieee.std_logic_1164.all;

entity targ02 is
  port (o0, o1, o2 : out std_logic);
end targ02;

architecture behav of targ02 is
begin
  (o2, o1, o0) <= std_logic_vector'("001");
end behav;
