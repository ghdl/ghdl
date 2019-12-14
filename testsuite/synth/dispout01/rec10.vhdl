library ieee;
use ieee.std_logic_1164.all;
use work.rec10_pkg.all;

entity rec10 is
  port (inp : std_logic;
        o : out myrec);
end rec10;

architecture behav of rec10 is
begin
  o.b (1) <= not inp;
end behav;
