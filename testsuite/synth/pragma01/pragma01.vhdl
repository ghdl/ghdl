library ieee;
use ieee.std_logic_1164.all;

entity pragma01 is
  port (is_sim : out std_logic);
end pragma01;

architecture behav of pragma01 is
begin
  is_sim <= '0'
  -- pragma translate_off
       or '1'
  -- pragma translate_on
       ;
end behav;
