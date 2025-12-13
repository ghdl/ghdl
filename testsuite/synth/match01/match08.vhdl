library ieee;
use ieee.std_logic_1164.all;

entity match08 is
  port (a : in std_logic_vector (3 downto 0);
        z : out std_logic);
end match08;

architecture behav of match08 is
begin
  z <= a ?/= "1--Z";
end behav;
