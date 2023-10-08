package repro1_pkg1 is
  function get_const return natural;
end;

package body repro1_pkg1 is
  function get_const return natural is
  begin
    return 3;
  end;
end;


use work.repro1_pkg1.all;

package repro1_pkg2 is
  type prot_t is protected
    impure function get return natural;
    procedure set (v : natural);
  end protected;

  shared variable sh : prot_t;

  constant c : natural := get_const;
end repro1_pkg2;

package body repro1_pkg2 is
  type prot_t is protected body
    variable val : natural := c;
    impure function get return natural is
    begin
      return val;
    end;
  
    procedure set (v : natural) is
    begin
      val := v;
    end;
  end protected body;
end repro1_pkg2;

use work.repro1_pkg2.all;

entity repro1 is
end;

architecture arch of repro1 is
begin
  assert sh.get = 3 severity failure;
end;
