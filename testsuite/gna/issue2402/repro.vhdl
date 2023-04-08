package gpkg1 is
  generic (type t;
           init : t;
           length : natural);

  function get return t;
end gpkg1;

package body gpkg1 is
  type my_rec is record
    bv : bit_vector (1 to length);
  end record;

  constant c : t := init;

  function get return t is
  begin
    return c;
  end get;
end gpkg1;

package gpkg2 is
--  generic (type t1; init1 : t);

  package g1 is new work.gpkg1
    generic map (t => natural, init => 11, length => 7);

  function get return natural;
end gpkg2;

package body gpkg2 is
  function get return natural is
  begin
    return g1.get;
  end;
end;

use work.gpkg2.all;

entity repro is
end;

architecture behav of repro is
begin
  assert get = 11 report "done" severity note;
end behav;
