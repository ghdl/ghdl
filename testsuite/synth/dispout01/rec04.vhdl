library ieee;
use ieee.std_logic_1164.all;
use work.rec04_pkg.all;

entity rec04 is
  port (inp : std_logic;
        o : out myrec);
end rec04;

architecture behav of rec04 is
begin
  o.b <= not inp;
  o.a <= "0001" when inp = '1' else "1000";
end behav;
