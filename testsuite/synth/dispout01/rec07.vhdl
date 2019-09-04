library ieee;
use ieee.std_logic_1164.all;
use work.rec07_pkg.all;

entity rec07 is
  port (inp : std_logic;
        o : out myrec);
end rec07;

architecture behav of rec07 is
begin
  o.b <= not inp;
  o.a <= "0001" when inp = '1' else "1000";
end behav;
