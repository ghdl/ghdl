library ieee;
use ieee.std_logic_1164.all;

entity conv01 is
  port (a, b : in std_logic;
        z : out std_logic);
end conv01;

architecture behav of conv01 is
begin
  z <= not a and std_logic(b);
end behav;
