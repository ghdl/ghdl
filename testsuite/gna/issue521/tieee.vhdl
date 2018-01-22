library ieee;
use ieee.pkg.all;

entity tieee is
end;

architecture behav of tieee is
begin
  assert cst = 5 severity failure;
end behav;
