library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;

entity sns02 is
  port (a : std_logic_vector(7 downto 0);
        b : out std_logic);
end sns02;

architecture behav of sns02 is
begin
  b <= or_reduce(a);
end behav;
