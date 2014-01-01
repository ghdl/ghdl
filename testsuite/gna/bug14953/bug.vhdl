library ieee;
use ieee.std_logic_1164.all;

entity bug is
end entity;

architecture a of bug is
    signal irunning :natural range 0 to 1 := 2;  -- reports no error
begin
    irunning <= 2;  -- reports error, but no information
end architecture;
