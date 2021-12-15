library ieee;
use ieee.std_logic_1164.all;

entity name4 is
  port (leds_o : std_logic_vector(3 downto 0);
        leds_init : std_logic_vector(3 downto 0);
        led_init_en : std_logic);
end;

architecture behav of name4 is
begin
  leds_o <= (led_init());
end behav;

