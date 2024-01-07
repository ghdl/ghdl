----------------------------------
-- Generates 3 events.
----------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity test is
end entity;

architecture beh of test is

  signal a : std_logic;

begin

  TB: process 
  begin
    a <= '0';
    wait for 10 ns;
    a <= '1';
    wait for 10 ns;
    a <= '0';
    wait;
  end process TB;

end architecture beh;
