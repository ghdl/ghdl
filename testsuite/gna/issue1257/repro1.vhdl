entity repro1 is
end repro1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of repro1 is
   signal left : std_logic_vector(1 downto 0);
   signal right : std_logic_vector(2 downto 0);
begin
    left <= right;
end;
