library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture a of ent is
   signal sig : std_logic_vector(1 downto 0);
   variable var : std_logic_vector(0 to 1);
begin
  sig <= (var'reverse_range => '0');
end;
