library ieee;
use ieee.std_logic_1164.all;
use work.rec01_pkg.all;

entity rec01 is
  port (inp : std_logic;
        o : out myrec);
end rec01;

architecture behav of rec01 is
begin
  o.a <= inp;
  o.b <= not inp;
end behav;
