package g1pkg1 is
  generic (type t;
           init : t;
           length : natural);

  function get return t;
end;

package body g1pkg1 is
  constant c : t := init;

  type my_rec is record
    bv : bit_vector (1 to length);
  end record;

  function get return t is
  begin
    return c;
  end get;
end;

package g1pkg2 is
--  generic (type t1; init1 : t);

  package g1 is new work.g1pkg1
    generic map (t => natural, init => 11, length => 7);

  function get return natural;
end;

package body g1pkg2 is
  function get return natural is
  begin
    return g1.get;
  end;
end;

use work.g1pkg2.all;

entity repro1 is
end;

architecture behav of repro1 is
begin
  assert get = 11 report "done" severity note;
end behav;
