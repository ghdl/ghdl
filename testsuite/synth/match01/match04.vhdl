library ieee;
use ieee.std_logic_1164.all;

entity match04 is
  port (a : in std_logic_vector (3 downto 0);
        z : out std_logic);
end match04;

architecture behav of match04 is
begin
  z <= a ?= "-1--0";
end behav;
