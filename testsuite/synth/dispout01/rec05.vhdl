library ieee;
use ieee.std_logic_1164.all;
use work.rec05_pkg.all;

entity rec05 is
  port (inp : std_logic;
        o : out myrec);
end rec05;

architecture behav of rec05 is
begin
  o.b <= not inp;
  o.a <= "0101" when inp = '0' else "1010";
end behav;
