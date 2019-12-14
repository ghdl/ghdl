library ieee;
use ieee.std_logic_1164.all;
use work.rec09_pkg.all;

entity rec09 is
  port (inp : std_logic;
        o : out myrec);
end rec09;

architecture behav of rec09 is
begin
  o.b <= not inp;
end behav;
