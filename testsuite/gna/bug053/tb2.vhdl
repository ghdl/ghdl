package pkg2 is
  package pkg1 is
    constant c : natural := 5;
--    function f return natural;
  end pkg1;
end pkg2;

entity tb2 is
end tb2;

use work.pkg2.all;

architecture behav of tb2 is
begin
  assert pkg1.c = 5 severity failure;
  assert pkg1.c /= 5 report "value is correct" severity note;
end behav;
