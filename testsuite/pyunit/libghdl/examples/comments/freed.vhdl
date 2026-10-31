package freed is
  type rec is record
    a : bit;
  end record;

  view vw of rec is
    a : in;
  end view;

  view vw2 of rec is
    --  Comment for :a: (a mode view element, renamed after its mode)
    a : view vw;
  end view;

  procedure p (
    --  Comment for :x: (an out parameter without class, renamed)
    x : out bit
  );

  function g generic (type T) parameter (v : T) return T;

  --  Comment for :gi: (a subprogram instantiation)
  function gi is new g generic map (T => bit);

  --  Comment for :pi: (a package instantiation)
  package pi is new work.gp generic map (G => 1);
end package;

library ieee;

--  Comment for :pu: (a package instantiation after a context clause)
package pu is new work.gp generic map (G => 1);

entity freed is
  port (
    --  Comment for :v: (a port with a mode view, renamed)
    v : view work.freed.vw
  );
end entity;
