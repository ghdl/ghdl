library ieee;
use ieee.std_logic_1164.all;

entity genlogic is
  generic (val : std_logic := '0');
end genlogic;

architecture behav of genlogic is
begin
  assert val = '1' or val = 'H' severity failure;
end behav;
