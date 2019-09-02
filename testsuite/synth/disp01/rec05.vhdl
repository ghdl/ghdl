library ieee;
use ieee.std_logic_1164.all;
use work.rec05_pkg.all;

entity rec05 is
  port (inp : myrec;
        o : out std_logic);
end rec05;

architecture behav of rec05 is
begin
  o <= '1' when inp.a (1) = inp.b else '0';
end behav;
