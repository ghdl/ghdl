package SortListGenericPkg is
  generic (
    type ElementType;
    function "<"(L : ElementType; R : ElementType) return boolean;
    function "<="(L : ElementType; R : ElementType) return boolean
  );
  function f (a, b : ElementType) return boolean;
end package;

package body SortListGenericPkg is
  function f (a, b : ElementType) return boolean is
  begin
    return a <= b;
  end f;
end;

package mysort is new work.SortListGenericPkg generic map (natural, "<", "<=");

entity repro is
end repro;

use work.mysort.all;
architecture behav of repro
is
begin
  process
    variable ok : boolean;
  begin
    ok := f (3, 12);
    assert ok report "bad comparaison" severity failure;
    wait;
  end process;
end behav;
