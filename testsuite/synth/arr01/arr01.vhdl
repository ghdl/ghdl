library ieee;
use ieee.std_logic_1164.all;

entity arr01 is
  port (v : std_logic_vector(7 downto 0);
        h : out std_logic_vector(3 downto 0);
        l : out std_logic_vector(3 downto 0));
end arr01;

architecture behav of arr01 is
begin
  l <= v (3 downto 0);
  h <= v (7 downto 4);
end behav;
