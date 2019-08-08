library ieee;
use ieee.std_logic_1164.all;

entity targ01 is
  port (v : std_logic_vector (2 downto 0);
        o0, o1, o2 : out std_logic);
end targ01;

architecture behav of targ01 is
begin
  (o2, o1, o0) <= v;
end behav;
