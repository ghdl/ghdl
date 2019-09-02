library ieee;
use ieee.std_logic_1164.all;
use work.rec03_pkg.all;

entity rec03 is
  port (inp : myrec;
        o : out std_logic);
end rec03;

architecture behav of rec03 is
begin
  o <= inp.b when inp.a = s2 else '1';
end behav;
