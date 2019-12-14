library ieee;
use ieee.std_logic_1164.all;

entity concat01 is
  port (a, b : in std_logic;
        z : out std_logic_vector(1 downto 0));
end concat01;

architecture behav of concat01 is
begin
  z <= a & b;
end behav;
