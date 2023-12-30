----------------------------------
-- Crashes WITH reaching the 
-- report statement.
----------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity test2 is
end entity;

architecture beh of test2 is
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
    g <= '0';
    wait;
  end process TB;

end architecture beh;
