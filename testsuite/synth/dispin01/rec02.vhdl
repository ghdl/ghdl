library ieee;
use ieee.std_logic_1164.all;
use work.rec02_pkg.all;

entity rec02 is
  port (inp : myrec;
        o : out std_logic);
end rec02;

architecture behav of rec02 is
begin
  o <= inp.b when inp.a > 3 else '0';
end behav;
