package pkg is
  generic (c : natural);

  function f return natural;
end pkg;

package body pkg is
  function f return natural is
  begin
    return c;
  end f;
end pkg;

entity tb is
end tb;

architecture behav of tb is
  package p is new work.pkg generic map (c => 3);
begin
  assert p.f = 3;
end behav;
