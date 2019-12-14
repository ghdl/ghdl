library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rec06_pkg.all;

entity rec06 is
  port (inp : myrec;
        o : out std_logic);
end rec06;

architecture behav of rec06 is
begin
  o <= inp.b when inp.a.d > inp.a.c else '0';
end behav;
