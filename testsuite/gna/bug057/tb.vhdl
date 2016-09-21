package pkg1 is
  generic (c : natural);
  generic map (c => 5);

  function f return natural;
end pkg1;

package body pkg1 is
  function f return natural is
  begin
    return c;
  end f;
end pkg1;

entity tb is
end tb;

architecture behav of tb is
begin
  assert work.pkg1.f = 5;
end behav;
