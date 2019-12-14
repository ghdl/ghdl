library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rec06_pkg.all;

entity rec06 is
  port (inp : std_logic;
        o : out myrec);
end rec06;

architecture behav of rec06 is
begin
  o.b <= not inp;
  o.a.c <= 2 when inp = '1' else 3;
  o.a.d <= "0000" when inp = '0' else "1000";
end behav;
