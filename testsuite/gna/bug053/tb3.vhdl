package pkg2 is
  package pkg1 is
    constant c : natural := 5;
    function f return natural;
  end pkg1;
end pkg2;

package body pkg2 is
  package body pkg1 is
    function f return natural is
    begin
      return 3;
    end f;
  end pkg1;
end pkg2;

entity tb3 is
end tb3;

use work.pkg2.all;

architecture behav of tb3 is
begin
  assert pkg1.c = 5 severity failure;
  assert pkg1.c /= 5 report "value is correct" severity note;
  assert pkg1.f = 3 severity failure;
  assert pkg1.f /= 3 report "value is correct" severity note;
end behav;
