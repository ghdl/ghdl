library ieee;
use ieee.std_logic_1164.all;
use work.rec02_pkg.all;

entity rec02 is
  port (inp : std_logic;
        o : out myrec);
end rec02;

architecture behav of rec02 is
begin
  o.b <= not inp;
  o.a <= 3 when inp = '1' else 5;
end behav;
