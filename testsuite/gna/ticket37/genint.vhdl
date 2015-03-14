package pkg is
  subtype myint is integer range integer'low to 169;
end pkg;

use work.pkg.all;

entity genint is
  generic (val : myint := 5);
end genint;

architecture behav of genint is
begin
  assert val = -159 or val = 9 severity failure;
end behav;
