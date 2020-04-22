library ieee;
use ieee.std_logic_1164.all;

entity tri is
  port (i, en : std_logic;
        o : out std_logic);
end tri;

architecture behav of tri is
begin
  o <= i when en = '1' else 'Z';
end behav;
