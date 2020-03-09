library ieee;
use ieee.std_logic_1164.all;

entity match01 is
  port (a : in std_logic_vector (3 downto 0);
        z : out std_logic);
end match01;

architecture behav of match01 is
begin
  z <= a ?= "1--0";
end behav;
