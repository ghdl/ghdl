library ieee;
use ieee.std_logic_1164.all;

entity arr02 is
  port (v : std_logic_vector(0 to 7);
        h : out std_logic_vector(0 to 3);
        l : out std_logic_vector(3 downto 0));
end arr02;

architecture behav of arr02 is
begin
  l <= v (4 to 7);
  h <= v (0 to 3);
end behav;
