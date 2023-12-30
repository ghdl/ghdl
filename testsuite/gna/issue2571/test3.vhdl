----------------------------------
-- This works.
----------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity test3 is
end entity;

architecture beh of test3 is
  signal g : std_logic := '1';
  signal s : std_logic bus;
  
begin

  BLK : block (g = '1')
  begin
    s <= guarded '1';
  end block BLK;
  
  TB: process 
  begin
    report "Arrived here.";
    g <= '1';
    wait;
  end process TB;

end architecture beh;
