library ieee;
use ieee.std_logic_1164.all;
use work.rec04_pkg.all;

entity rec04 is
  port (inp : myrec;
        o : out std_logic);
end rec04;

architecture behav of rec04 is
begin
  o <= '1' when inp.a (1) = inp.b else '0';
end behav;
