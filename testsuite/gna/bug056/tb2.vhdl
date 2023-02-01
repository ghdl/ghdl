package pkg2 is
  generic (c : natural);

  function f return natural;
end pkg2;

package body pkg2 is
  constant d : natural := c;

  function f return natural is
  begin
    return d;
  end f;
end pkg2;

entity tb2 is
end;

architecture behav of tb2 is
  package p is new work.pkg2 generic map (c => 3);
begin
  assert p.f = 3;
end behav;
