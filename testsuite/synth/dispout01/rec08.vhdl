library ieee;
use ieee.std_logic_1164.all;
use work.rec08_pkg.all;

entity rec08 is
  port (inp : std_logic;
        o : out myrec);
end rec08;

architecture behav of rec08 is
begin
  o.b <= not inp;
  o.a <= "1" when inp = '1' else "0";
end behav;
