library ieee;
use ieee.std_logic_1164.all;

entity aggr01 is
  port (a : std_logic_vector (7 downto 0);
        b : out std_logic_vector (7 downto 0));
end aggr01;

architecture behav of aggr01 is
  constant mask : std_logic_vector (7 downto 0) :=
    (0 => '1', others => '0');
begin
  b <= a and mask;
end behav;
    
