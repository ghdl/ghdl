library ieee;
use ieee.std_logic_1164.all;

entity driver is
  generic (val : string);
  port (o : out std_logic);
end driver;

architecture behav of driver is
begin
  drv1: if val = "one" generate
    o <= '1';
  end generate;

  drv0: if val = "zero" generate
    o <= '0';
  end generate;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity string01 is
  port (o : out std_logic);
end string01;

architecture behav of string01 is
begin
  e : entity work.driver
    generic map (val => "one")
    port map (o => o);
end behav;

