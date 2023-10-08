package repro3_pkg1 is
  generic (type t;
           init : t);
  type prot_t is protected
    impure function get return t;
    procedure set (v : t);
  end protected;
end;

package body repro3_pkg1 is
  type my_Rec is record
    val : t;
  end record;
  
  type prot_t is protected body
    variable val : my_rec := (val => init);
    impure function get return t is
    begin
      return val.val;
    end;
  
    procedure set (v : t) is
    begin
      val.val := v;
    end;
  end protected body;
end;


package repro3_pkg2 is

  function my_func return natural;
  
  package my_pkg1 is new work.repro3_pkg1
    generic map (t => natural, init => 6);
  shared variable sh : my_pkg1.prot_t;
end repro3_pkg2;

package body repro3_pkg2 is
  function my_func return natural is
  begin
    return 7;
  end;
end repro3_pkg2;

use work.repro3_pkg2.all;

entity repro3 is
end;

architecture arch of repro3 is
begin
  assert sh.get = 6 severity failure;
end;
