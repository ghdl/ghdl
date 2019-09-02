library ieee;
use ieee.std_logic_1164.all;
use work.rec01_pkg.all;

entity rec01 is
  port (inp : myrec;
        o : out std_logic);
end rec01;

architecture behav of rec01 is
begin
  o <= inp.a or inp.b;
end behav;
