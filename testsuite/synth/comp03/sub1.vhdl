library ieee;
use ieee.std_logic_1164.all;

entity sub1 is
  port (p : std_logic_vector (7 downto 0);
        o : out std_logic);
end sub1;

architecture behav of sub1 is
begin
  o <= p (0);
end behav;
